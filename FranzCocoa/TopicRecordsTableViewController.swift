import Cocoa
import Dispatch
import Foundation
import NoiseSerde
import OSLog
import SwiftUI
import UniformTypeIdentifiers

fileprivate let logger = Logger(
  subsystem: Bundle.main.bundleIdentifier!,
  category: "TopicRecordsTable"
)

class TopicRecordsTableViewController: NSViewController {
  @IBOutlet weak var scrollView: NSScrollView!
  @IBOutlet weak var clipView: NSClipView!
  @IBOutlet weak var tableView: NSTableView!
  @IBOutlet weak var segmentedControl: NSSegmentedControl!
  @IBOutlet weak var statsLabel: NSTextField!

  private var contextMenu = NSMenu()

  weak var delegate: WorkspaceDetailDelegate?

  private var id: UVarint!
  private var topic: String!
  private var iteratorId: UVarint?
  private var items = [Item]()
  private var cookie = 0
  private var liveModeOn = false

  private var longestKey = -1
  private var longestKeyLen = 0
  private var longestValue = -1
  private var longestValueLen = 0

  private var optionsDefaultsKey: String?
  private var options = TopicRecordsOptions()

  private var bytesFmt: ByteCountFormatter = {
    let fmt = ByteCountFormatter()
    fmt.countStyle = .memory
    return fmt
  }()

  private lazy var recordWindowCtls = [RecordWindowController]()
  private var jumpNotificationObserver: AnyObject?

  deinit {
    guard let iteratorId else { return }
    Error.wait(Backend.shared.closeIterator(withId: iteratorId))
  }

  override func viewDidLoad() {
    super.viewDidLoad()

    contextMenu.delegate = self

    tableView.allowsMultipleSelection = true
    tableView.menu = contextMenu
    tableView.dataSource = self
    tableView.delegate = self
    tableView.doubleAction = #selector(didDoubleClickRow(_:))
    tableView.setDraggingSourceOperationMask(.copy, forLocal: false)
    tableView.target = self
    if let connection = delegate?.getConnectionName(), let topic {
      tableView.autosaveName = "Franz:\(connection):TopicRecords:\(topic)"
      tableView.autosaveTableColumns = true
    }

    segmentedControl.isEnabled = false
    segmentedControl.target = self
    segmentedControl.action = #selector(didPressSegmentedControl(_:))

    statsLabel.font = .monospacedDigitSystemFont(ofSize: 10, weight: .regular)
    statsLabel.textColor = .secondaryLabelColor
    statsLabel.stringValue = ""
  }

  override func viewDidAppear() {
    super.viewDidAppear()

    guard let id, let topic else { return }
    NotificationCenter.default.post(
      name: .TopicRecordsTableAppeared,
      object: self,
      userInfo: [
        "id": id,
        "topic": topic,
      ]
    )

    jumpNotificationObserver = NotificationCenter.default.addObserver(
      forName: .TopicRecordsTableJumpRequested,
      object: nil,
      queue: .main
    ) { [weak self] notification in
      guard let self else { return }
      guard notification.userInfo?["ID"] as? UVarint == id else { return }
      self.didRequestJumpToOffset()
    }
  }

  override func viewDidDisappear() {
    super.viewDidDisappear()
    if let jumpNotificationObserver {
      NotificationCenter.default.removeObserver(jumpNotificationObserver)
    }

    guard let id, let topic else { return }
    NotificationCenter.default.post(
      name: .TopicRecordsTableDisappeared,
      object: self,
      userInfo: [
        "id": id,
        "topic": topic,
      ]
    )
  }

  func configure(withId id: UVarint, andTopic topic: String) {
    guard let delegate else { return }

    self.id = id
    self.topic = topic

    let connectionOptionsKey = "Franz:\(delegate.getConnectionName()):TopicRecordsOptions"
    self.optionsDefaultsKey = "\(connectionOptionsKey):\(topic)"
    self.options = (
      Defaults.shared.get(codable: self.optionsDefaultsKey!) ??
      Defaults.shared.get(codable: connectionOptionsKey) ??
      TopicRecordsOptions()
    )

    let status = delegate.makeStatusProc()
    status("Opening Iterator")
    Backend.shared.openIterator(
      forTopic: topic,
      andOffset: .recent(20),
      inWorkspace: id
    ).onComplete { [weak self] iteratorId in
      guard let self else {
        status("Ready")
        Error.wait(Backend.shared.closeIterator(withId: iteratorId))
        return
      }
      self.iteratorId = iteratorId
      self.segmentedControl?.isEnabled = true
      self.toggleLiveMode(resettingIterator: false)
    }
  }

  private func loadRecords(byAppending appending: Bool = true) {
    assert(Thread.isMainThread)
    guard let delegate else { return }
    let status = delegate.makeStatusProc()
    status("Fetching Records")
    loadRecords { [weak self] records in
      guard let records else {
        status("Ready")
        return
      }
      status("Processing Records")
      self?.setRecords(records, byAppending: appending) {
        status("Ready")
      }
    }
  }

  private func loadRecords(completionHandler: @escaping ([IteratorResult]?) -> Void) {
    assert(Thread.isMainThread)
    guard let iteratorId else { return }
    let cookie = self.cookie
    Backend.shared.getRecords(
      iteratorId,
      withMaxBytes: options.maxBytes
    ).sink(
      onError: { error in
        Error.alert(withError: error)
        completionHandler(nil)
      },
      onComplete: { [weak self] records in
        guard cookie == self?.cookie else {
          logger.debug("loadRecords: cookie expired")
          completionHandler(nil)
          return
        }
        completionHandler(records)
      })
  }

  private func setRecords(_ records: [IteratorRecord],
                          byAppending appending: Bool = true,
                          completionHandler: @escaping () -> Void = { }) {
    setRecords(records.map(IteratorResult.original), byAppending: appending) {
      completionHandler()
    }
  }

  // Invariant: completionHandler is always called on the main thread.
  private func setRecords(_ records: [IteratorResult],
                          byAppending appending: Bool = true,
                          completionHandler: @escaping () -> Void = { }) {
    assert(Thread.isMainThread)

    if records.isEmpty && appending {
      completionHandler()
      return
    }

    let selectedRow = self.tableView.selectedRow
    var selectedItem: Item?
    if selectedRow >= 0 {
      selectedItem = items[selectedRow]
    }

    let sortDirection = self.options.sortDirection
    let keepBytes = self.options.keepBytes

    DispatchQueue.uiBackground.async { [weak self] in
      guard let self else {
        DispatchQueue.main.sync {
          completionHandler()
        }
        return
      }

      var items = [Item]()
      if appending {
        var added = [Item]()
        added.reserveCapacity(records.count)
        for r in records {
          added.append(Item(r))
        }

        Timed.block(named: "setRecords.sort") {
          switch sortDirection {
          case .desc:
            added.sort { $0 > $1 }
          case .asc:
            added.sort { $0 < $1 }
          }
        }

        Timed.block(named: "setRecords.concat") {
          switch sortDirection {
          case .asc:
            items = self.items
            items.reserveCapacity(added.count)
            items.append(contentsOf: added)
          case .desc:
            items = added
            items.reserveCapacity(items.count)
            items.append(contentsOf: self.items)
          }
        }
      } else {
        Timed.block(named: "setRecords.fill") {
          items.reserveCapacity(records.count)
          for r in records {
            items.append(Item(r))
          }
        }

        Timed.block(named: "setRecords.sort") {
          switch sortDirection {
          case .desc:
            items.sort { $0 > $1 }
          case .asc:
            items.sort { $0 < $1 }
          }
        }
      }

      var totalBytes = UVarint(0)
      self.longestKey = -1
      self.longestKeyLen = 0
      self.longestValue = -1
      self.longestValueLen = 0
      switch sortDirection {
      case .asc:
        for (row, it) in items.reversed().enumerated() {
          let size = it.record.size
          if totalBytes + size > keepBytes {
            let n = items.count - row
            logger.debug("setRecords: removing first \(n) items")
            items.removeFirst(n)
            break
          }
          if let key = it.record.key, self.longestKeyLen < key.count {
            self.longestKeyLen = key.count
            self.longestKey = row
          }
          if let value = it.record.value, self.longestValueLen < value.count {
            self.longestValueLen = value.count
            self.longestValue = row
          }
          totalBytes += size
        }
      case .desc:
        for (row, it) in items.enumerated() {
          let size = it.record.size
          if totalBytes + size > keepBytes {
            let n = items.count - row
            logger.debug("setRecords: removing last \(n) items")
            items.removeLast(n)
            break
          }
          if let key = it.record.key, self.longestKeyLen < key.count {
            self.longestKeyLen = key.count
            self.longestKey = row
          }
          if let value = it.record.value, self.longestValueLen < value.count {
            self.longestValueLen = value.count
            self.longestValue = row
          }
          totalBytes += size
        }
      }

      DispatchQueue.main.sync { [weak self] in
        guard let self else { return }
        var scrollToBottom = false
        if let documentView = self.scrollView.documentView {
          scrollToBottom = (
            sortDirection == .asc &&
            self.scrollView.hasVerticalScroller &&
            self.clipView.bounds.origin.y + self.clipView.bounds.height == documentView.bounds.height
          )
        }

        self.items = items
        self.tableView.reloadData()
        if let selectedItem, let selectedRow = items.firstIndex(of: selectedItem) {
          self.tableView.selectRowIndexes([selectedRow], byExtendingSelection: false)
        }
        if scrollToBottom {
          self.tableView.scrollToEndOfDocument(self)
        }

        let bytesStr = self.bytesFmt.string(fromByteCount: Int64(totalBytes))
        self.statsLabel.stringValue = "Records: \(items.count) (\(bytesStr))"
        completionHandler()
      }
    }
  }

  private func scheduleLoad(withCookie cookie: Int) {
    assert(Thread.isMainThread)
    guard let delegate else { return }
    guard cookie == self.cookie else {
      logger.debug("scheduleLoad: cookie expired")
      return
    }
    let status = delegate.makeStatusProc()
    status("Fetching Records")
    loadRecords { [weak self] records in
      guard let records else {
        status("Ready")
        return
      }
      status("Processing Records")
      self?.setRecords(records) { [weak self] in
        status("Ready")
        var deadline = DispatchTime.now()
        if records.isEmpty {
          deadline = deadline.advanced(by: .seconds(1))
        }
        DispatchQueue.main.asyncAfter(deadline: deadline) { [weak self] in
          self?.scheduleLoad(withCookie: cookie)
        }
      }
    }
  }

  private func toggleLiveMode(resettingIterator resetIterator: Bool = true) {
    if liveModeOn {
      stopLiveMode()
    } else {
      startLiveMode(resettingIterator: resetIterator)
    }
  }

  private func stopLiveMode() {
    assert(Thread.isMainThread)
    cookie += 1
    liveModeOn = false
    segmentedControl.setSelected(false, forSegment: 0)
    segmentedControl.setEnabled(true, forSegment: 1)
    segmentedControl.setEnabled(true, forSegment: 2)
  }

  private func startLiveMode(resettingIterator resetIterator: Bool) {
    assert(Thread.isMainThread)
    cookie += 1
    let cookie = cookie
    setRecords([IteratorResult](), byAppending: false)
    liveModeOn = true
    segmentedControl.setSelected(true, forSegment: 0)
    segmentedControl.setEnabled(false, forSegment: 1)
    segmentedControl.setEnabled(false, forSegment: 2)
    if resetIterator {
      reset(offset: .latest) { [weak self] in
        self?.scheduleLoad(withCookie: cookie)
      }
    } else {
      self.scheduleLoad(withCookie: cookie)
    }
  }

  private func reset(offset: IteratorOffset, _ completionHandler: @escaping () -> Void = {}) {
    assert(Thread.isMainThread)
    guard let delegate else { return }
    guard let iteratorId else { return }

    let status = delegate.makeStatusProc()
    status("Resetting Iterator")
    Backend.shared.resetIterator(withId: iteratorId, toOffset: offset).onComplete {
      status("Ready")
      completionHandler()
    }
  }

  private func view(record: IteratorRecord) {
    var tab = RecordDetailViewController.Tab.key
    if tableView.clickedColumn >= 0 &&
        tableView.tableColumns[tableView.clickedColumn].identifier == .TopicRecordsValue {
      tab = .value
    }
    let ctl = RecordWindowController()
    ctl.delegate = self
    ctl.configure(
      withRecord: record,
      andTopic: topic,
      andKeyFormat: options.keyFormat.dataFormat,
      andValueFormat: options.valueFormat.dataFormat,
      andTab: tab
    )
    ctl.show(self)
    recordWindowCtls.append(ctl)
  }

  @objc func didPressSegmentedControl(_ sender: NSSegmentedControl) {
    guard let delegate else { return }
    let segment = sender.selectedSegment
    guard let tag = ControlTag(rawValue: sender.tag(forSegment: segment)) else { return }
    switch tag {
    case .play:
      toggleLiveMode()

    case .options:
      sender.setSelected(false, forSegment: segment)
      let bounds = sender.relativeBounds(forSegment: segment)
      showOptionsPopover(relativeTo: bounds, of: sender)

    case .more:
      sender.setSelected(false, forSegment: segment)
      sender.isEnabled = false

      let status = delegate.makeStatusProc()
      status("Fetching Records")
      loadRecords { [weak self] records in
        guard let self, let records else {
          sender.isEnabled = true
          status("Ready")
          return
        }
        if records.isEmpty {
          let bounds = sender.relativeBounds(forSegment: segment)
          let popover = NSPopover()
          popover.behavior = .transient
          popover.contentSize = NSSize(width: 200, height: 50)
          popover.contentViewController = NSHostingController(
            rootView: Text("No more records.")
              .padding()
              .frame(
                width: popover.contentSize.width,
                height: popover.contentSize.height))
          popover.show(relativeTo: bounds, of: sender, preferredEdge: .minY)
          sender.isEnabled = true
          status("Ready")
          return
        }

        status("Processing Records")
        self.setRecords(records) { [weak self] in
          guard let self else { return }
          switch self.options.sortDirection {
          case .asc:
            self.tableView.scrollToEndOfDocument(self)
          case .desc:
            self.tableView.scrollToBeginningOfDocument(self)
          }
          sender.isEnabled = true
          status("Ready")
        }
      }
    }
  }

  private func showOptionsPopover(relativeTo bounds: NSRect, of view: NSView) {
    let popover = NSPopover()
    let form = TopicRecordsOptionsForm(model: self.options) {
      if let key = self.optionsDefaultsKey {
        try? Defaults.shared.set(codable: self.options, forKey: key)
      }
      self.setRecords(self.items.map(\.record), byAppending: false)
      popover.close()
    } jumpAction: {
      popover.animates = false
      popover.close()
      self.showJumpPopover(popover, relativeTo: bounds, of: view)
    }
    popover.behavior = .semitransient
    popover.contentSize = NSSize(width: 270, height: 260)
    popover.contentViewController = NSHostingController(
      rootView: form.frame(
        width: popover.contentSize.width,
        height: popover.contentSize.height))
    popover.show(relativeTo: bounds, of: view, preferredEdge: .minY)
  }

  private func showJumpPopover(_ popover: NSPopover, relativeTo bounds: NSRect, of view: NSView) {
    let resetForm = IteratorResetForm { offset in
      self.reset(offset: offset) { [weak self] in
        self?.loadRecords(byAppending: false)
      }
      popover.close()
    }
    popover.contentSize = NSSize(width: 270, height: 120)
    popover.contentViewController = NSHostingController(
      rootView: resetForm.frame(
        width: popover.contentSize.width,
        height: popover.contentSize.height))
    popover.show(relativeTo: bounds, of: view, preferredEdge: .minY)
    popover.animates = true
  }

  @IBAction func didPressScriptButton(_ sender: NSButton) {
    WindowManager.shared.showScriptWindow(forWorkspace: id, andTopic: topic, withDelegate: self)
  }

  @objc func didDoubleClickRow(_ sender: NSTableView) {
    guard sender.clickedRow >= 0 else { return }
    view(record: items[sender.clickedRow].record)
  }

  func didRequestJumpToOffset() {
    if liveModeOn {
      stopLiveMode()
    }
    let segment = ControlTag.options.rawValue
    let bounds = segmentedControl.relativeBounds(forSegment: segment)
    let popover = NSPopover()
    popover.behavior = .semitransient
    showJumpPopover(popover, relativeTo: bounds, of: segmentedControl)
    NSApp.keyWindow?.makeFirstResponder(popover)
  }
}

// MARK: - RecordWindowDelegate
extension TopicRecordsTableViewController: RecordWindowDelegate {
  func recordWindowWillClose(_ sender: AnyObject) {
    guard let ctl = sender as? RecordWindowController else { return }
    recordWindowCtls.removeAll { $0 == ctl }
  }
}

// MARK: - ScriptWindowDelegate
extension TopicRecordsTableViewController: ScriptWindowDelegate {
  func scriptWindow(willActivate script: String) -> Bool {
    return Error.wait(Backend.shared.activateScript(script, forTopic: topic, inWorkspace: id)) != nil
  }

  func scriptWindow(willApply script: String) {
    Backend.shared.applyScript(
      script,
      toRecords: items.map { $0.original },
      inWorkspace: id
    ).sink( onError: { err in
      Error.alert(withError: err)
    }, onComplete: { [weak self] result in
      self?.setRecords(
        result.items,
        byAppending: false
      ) {
        if result.reduced != nil || result.output.count > 0 {
          WindowManager.shared.showResultWindow(result)
        }
      }
    })
  }

  func scriptWindowWillDeactivate() {
    Error.wait(Backend.shared.deactivateScript(forTopic: topic, inWorkspace: id))
  }

  func scriptWindowWillClose() {
    Error.wait(Backend.shared.deactivateScript(forTopic: topic, inWorkspace: id))
  }
}

// MARK: - NSMenuDelegate
extension TopicRecordsTableViewController: NSMenuDelegate {
  func menuNeedsUpdate(_ menu: NSMenu) {
    menu.removeAllItems()
    guard !liveModeOn else { return }
    guard tableView.clickedRow >= 0 else { return }
    let record = items[tableView.clickedRow].record
    menu.addItem(.init(
      title: "View",
      action: #selector(didPressViewRecordItem(_:)),
      keyEquivalent: ""
    ))
    if record.key != nil {
      menu.addItem(.separator())
      menu.addItem(.init(
        title: "Publish Tombstone...",
        action: #selector(didPressPublishTombstoneItem(_:)),
        keyEquivalent: String.backspaceKeyEquivalent
      ))
    }
  }

  @objc func didPressViewRecordItem(_ sender: NSMenu) {
    guard !liveModeOn else { return }
    guard tableView.clickedRow >= 0 else { return }
    view(record: items[tableView.clickedRow].record)
  }

  @objc func didPressPublishTombstoneItem(_ sender: NSMenu) {
    guard !liveModeOn else { return }
    guard let delegate else { return }
    guard tableView.clickedRow >= 0 else { return }
    let record = items[tableView.clickedRow].record
    guard let key = record.key else { return }

    let alert = NSAlert()
    alert.alertStyle = .informational
    alert.messageText = "Really publish tombstone?"
    alert.informativeText = "This action cannot be undone."
    alert.addButton(withTitle: "Publish")
    alert.addButton(withTitle: "Cancel")
    switch alert.runModal() {
    case .alertFirstButtonReturn:
      let status = delegate.makeStatusProc()
      status("Publishing Tombstone")
      Backend.shared.publishRecord(
        toTopic: topic,
        andPartition: record.partitionId,
        withKey: key,
        andValue: nil,
        inWorkspace: id
      ).onComplete { [weak self] record in
        status("Processing Records")
        self?.setRecords([record], byAppending: true) {
          status("Ready")
        }
      }
    default:
      ()
    }
  }
}

// MARK: - ControlTag
fileprivate enum ControlTag: Int {
  case play = 0
  case options
  case more
}

// MARK: - Item
fileprivate class Item: NSObject, Comparable {
  struct ID: Hashable, Equatable {
    let pid: UVarint
    let offset: UVarint
  }

  var id: ID
  var record: IteratorRecord
  var original: IteratorRecord

  init(_ r: IteratorResult) {
    switch r {
    case .original(let record):
      self.id = ID(pid: record.partitionId, offset: record.offset)
      self.record = record
      self.original = record
    case .transformed(let record, let original):
      self.id = ID(pid: original.partitionId, offset: original.offset)
      self.record = record
      self.original = original
    }
  }

  static func > (lhs: Item, rhs: Item) -> Bool {
    return lhs.record > rhs.record
  }

  static func < (lhs: Item, rhs: Item) -> Bool {
    return lhs.record < rhs.record
  }

  override func isEqual(to object: Any?) -> Bool {
    guard let other = object as? Item else { return false }
    return id == other.id
  }
}

// MARK: - NSSegmentedControl
extension NSSegmentedControl {
  func relativeBounds(forSegment index: Int) -> NSRect {
    let w = bounds.width / CGFloat(segmentCount)
    var r = bounds
    r.size.width = w
    r.origin.x += w * CGFloat(index)
    return r
  }
}

// MARK: - NSTableViewDataSource
extension TopicRecordsTableViewController: NSTableViewDataSource {
  func numberOfRows(in tableView: NSTableView) -> Int {
    return items.count
  }
}

// MARK: - NSTableViewDelegate
extension TopicRecordsTableViewController: NSTableViewDelegate {
  func tableView(_ tableView: NSTableView, viewFor tableColumn: NSTableColumn?, row: Int) -> NSView? {
    guard let id = tableColumn?.identifier else { return nil }
    guard let view = tableView.makeView(withIdentifier: id, owner: self) as? NSTableCellView else { return nil }
    guard let textField = view.textField else { return nil }
    let record = items[row].record
    textField.font = dataFont
    textField.textColor = tableView.selectedRow == row ? .selectedControlTextColor : .controlTextColor
    if id == .TopicRecordsPartitionId {
      textField.stringValue = "\(record.partitionId)"
      textField.font = numberFont
    } else if id == .TopicRecordsTimestamp {
      textField.stringValue = DateFormatter.localizedString(
        from: .init(timeIntervalSince1970: Double(record.timestamp)/1000.0),
        dateStyle: .short,
        timeStyle: .medium
      )
      textField.font = numberFont
    } else if id == .TopicRecordsOffset {
      textField.stringValue = "\(record.offset)"
      textField.font = numberFont
    } else if id == .TopicRecordsKey {
      setTextFieldData(textField, data: record.key)
    } else if id == .TopicRecordsValue {
      setTextFieldData(textField, data: record.value)
    }
    return view
  }

  private func setTextFieldData(_ textField: NSTextField, data: Data?) {
    if let data {
      if let string = String.text(from: data, withMaxLength: 4096) {
        textField.stringValue = string
        return
      }
      textField.stringValue = "BINARY DATA"
      textField.textColor = .secondaryLabelColor
    } else {
      textField.stringValue = "NULL"
      textField.textColor = .secondaryLabelColor
    }
  }

  func tableView(_ tableView: NSTableView, pasteboardWriterForRow row: Int) -> NSPasteboardWriting? {
    guard !liveModeOn else { return nil }
    guard let event = NSApplication.shared.currentEvent, event.type == .leftMouseDragged else { return nil }
    let columnPoint = tableView.convert(event.locationInWindow, from: nil)
    let columnIdx = tableView.column(at: columnPoint)
    guard columnIdx >= 0 else { return nil }
    let column = tableView.tableColumns[columnIdx]
    if column.identifier == .TopicRecordsKey {
      guard items[row].record.key != nil else { return nil }
      let provider = NSFilePromiseProvider(fileType: options.keyFormat.utType.identifier, delegate: self)
      provider.userInfo = DragInfo(row: row, target: .key)
      return provider
    } else if column.identifier == .TopicRecordsValue {
      guard items[row].record.value != nil else { return nil }
      let provider = NSFilePromiseProvider(fileType: options.valueFormat.utType.identifier, delegate: self)
      provider.userInfo = DragInfo(row: row, target: .value)
      return provider
    }
    return nil
  }

  func tableView(_ tableView: NSTableView, sizeToFitWidthOfColumn column: Int) -> CGFloat {
    var str = String(repeating: "m", count: 6)
    var font = dataFont
    switch tableView.tableColumns[column].identifier {
    case .TopicRecordsPartitionId:
      str = String(repeating: "m", count: 6)
      font = numberFont
    case .TopicRecordsOffset:
      str = String(repeating: "m", count: 8)
      font = numberFont
    case .TopicRecordsTimestamp:
      str = String(repeating: "m", count: 10)
      font = numberFont
    case .TopicRecordsKey:
      if longestKey >= 0,
         let data = items[longestKey].record.key,
         let decoded = String.text(from: data, withMaxLength: 300) {
        str = decoded
      } else {
        str = String(repeating: "m", count: longestKeyLen)
      }
    case .TopicRecordsValue:
      if longestValue >= 0,
         let data = items[longestValue].record.value,
         let decoded = String.text(from: data, withMaxLength: 300) {
        str = decoded
      } else {
        str = String(repeating: "m", count: longestValueLen)
      }
    default:
      str = String(repeating: "m", count: 10)
      font = numberFont
    }

    let insets = CGFloat(12)
    let rect = str.boundingRect(
      with: NSSize(width: 2000, height: 24),
      options: [.usesDeviceMetrics, .truncatesLastVisibleLine],
      attributes: [NSAttributedString.Key.font: font]
    )
    tableView.reloadData()  // force redisplay
    return rect.width + insets
  }

  private var dataFont: NSFont {
    .monospacedSystemFont(ofSize: 12, weight: .regular)
  }

  private var numberFont: NSFont {
    .monospacedDigitSystemFont(ofSize: 12, weight: .regular)
  }
}

// MARK: - NSFilePromiseProviderDelegate
fileprivate struct DragInfo {
  enum Target {
    case key
    case value
  }

  let row: Int
  let target: Target

  var suffix: String {
    switch target {
    case .key:
      return "key"
    case .value:
      return "value"
    }
  }

  func data(fromItem item: Item) -> Data? {
    switch target {
    case .key:
      return item.record.key
    case .value:
      return item.record.value
    }
  }
}

extension TopicRecordsTableViewController: NSFilePromiseProviderDelegate {
  func filePromiseProvider(_ provider: NSFilePromiseProvider,
                           fileNameForType fileType: String) -> String {
    guard let topic, let info = provider.userInfo as? DragInfo else {
      preconditionFailure()
    }
    let ext = UTType(fileType)?.preferredFilenameExtension ?? ""
    let record = items[info.row].record
    let filename = "\(topic)@\(record.partitionId)-\(record.offset)-\(info.suffix)"
    if ext.isEmpty {
      return filename
    }
    return "\(filename).\(ext)"
  }

  func filePromiseProvider(_ provider: NSFilePromiseProvider,
                           writePromiseTo url: URL,
                           completionHandler: @escaping (Error?) -> Void) {
    guard let info = provider.userInfo as? DragInfo,
          let data = info.data(fromItem: items[info.row]) else {
      completionHandler(nil)
      return
    }
    FileManager.default.createFile(atPath: url.path, contents: data)
    completionHandler(nil)
  }

  // ??? Shouldn't this shit be automatic?
  @MainActor
  func filePromiseProvider(_ provider: NSFilePromiseProvider,
                           writePromiseTo url: URL) async throws {
    await withUnsafeContinuation { k in
      self.filePromiseProvider(provider, writePromiseTo: url) { _ in
        k.resume()
      }
    }
  }
}

// MARK: - TopicRecordsTable
struct TopicRecordsTable: NSViewControllerRepresentable {
  typealias NSViewControllerType = TopicRecordsTableViewController

  let id: UVarint
  let topic: String
  weak var delegate: WorkspaceDetailDelegate?

  func makeNSViewController(context: Context) -> TopicRecordsTableViewController {
    let ctl = TopicRecordsTableViewController()
    ctl.delegate = delegate
    ctl.configure(withId: id, andTopic: topic)
    return ctl
  }

  func updateNSViewController(_ nsViewController: TopicRecordsTableViewController, context: Context) {
  }
}

// MARK: - IteratorResetForm
fileprivate struct IteratorResetForm: View {
  enum Offset {
    case earliest
    case latest
    case recent
    case timestamp
    case offset
  }

  @State var target = Offset.recent
  @State var timestamp = Date()
  @State var delta = UVarint(20)
  @State var offset = UVarint(0)

  var resetAction: (IteratorOffset) -> Void

  var offsetFormatter: NumberFormatter = {
    let fmt = NumberFormatter()
    fmt.allowsFloats = false
    fmt.minimum = 0
    return fmt
  }()

  enum Field: Hashable {
    case picker
    case timestamp
    case delta
    case offset
  }

  @FocusState private var focused: Field?

  var body: some View {
    Form {
      Picker("Target:", selection: $target) {
        Text("Earliest").tag(Offset.earliest)
        Text("Timestamp").tag(Offset.timestamp)
        Text("Recent").tag(Offset.recent)
        Text("Latest").tag(Offset.latest)
        Text("Offset").tag(Offset.offset)
      }
      .focused($focused, equals: .picker)
      .onChange(of: target) { target in
        // JANK: fields might not be visible when this fires, so add a delay.
        DispatchQueue.main.asyncAfter(deadline: .now()+0.05) {
          switch target {
          case .timestamp:
            focused = .timestamp
          case .recent:
            focused = .delta
          case .offset:
            focused = .offset
          default:
            ()
          }
        }
      }
      if target == .timestamp {
        DatePicker(
          "Timestamp:",
          selection: $timestamp,
          displayedComponents: [.date, .hourAndMinute]
        )
        .focused($focused, equals: .timestamp)
      } else if target == .recent {
        TextField("Delta:", value: $delta, formatter: offsetFormatter)
          .focused($focused, equals: .delta)
          .onSubmit {
            resetAction(.recent(delta))
          }
      } else if target == .offset {
        TextField("Offset:", value: $offset, formatter: offsetFormatter)
          .focused($focused, equals: .offset)
          .onSubmit {
            resetAction(.exact(offset))
          }
      }
      Button("Jump") {
        switch target {
        case .earliest:
          resetAction(.earliest)
        case .timestamp:
          // The time component of the picker defaults to the current
          // second, so we have to truncate that part off until the
          // .hourMinuteAndSecond style becomes available on macOS, or
          // until we write a custom component for this.
          let timestamp = UVarint((timestamp.timeIntervalSince1970/60).rounded()) * 60 * 1000
          resetAction(.timestamp(timestamp))
        case .recent:
          resetAction(.recent(delta))
        case .latest:
          resetAction(.latest)
        case .offset:
          resetAction(.exact(offset))
        }
      }
        .buttonStyle(.borderedProminent)
        .keyboardShortcut(.defaultAction)
    }
    .padding()
  }
}
