import Cocoa
import Dispatch
import Foundation
import NoiseSerde
import SwiftUI
import UniformTypeIdentifiers

class TopicRecordsTableViewController: NSViewController {
  @IBOutlet weak var tableView: NSTableView!
  @IBOutlet weak var segmentedControl: NSSegmentedControl!
  @IBOutlet weak var statsLabel: NSTextField!

  private var contextMenu = NSMenu()

  weak var delegate: WorkspaceDetailDelegate?

  private var id: UVarint!
  private var topic: String!
  private var iteratorId: UVarint?
  private var items = [Item]()
  private var liveModeOn = false
  private var liveModeCookie = 0
  private var backpressureSema = DispatchSemaphore(value: 1)

  private var optionsDefaultsKey: String?
  private var options = TopicRecordsOptions()

  private var bytesFmt: ByteCountFormatter = {
    let fmt = ByteCountFormatter()
    fmt.countStyle = .memory
    return fmt
  }()

  override func viewDidLoad() {
    super.viewDidLoad()

    contextMenu.delegate = self

    tableView.menu = contextMenu
    tableView.dataSource = self
    tableView.delegate = self
    tableView.setDraggingSourceOperationMask(.copy, forLocal: false)
    if let connection = delegate?.getConnectionName(), let topic {
      tableView.autosaveName = "Franz:\(connection):TopicRecords:\(topic)"
      tableView.autosaveTableColumns = true
    }

    segmentedControl.target = self
    segmentedControl.action = #selector(didPressSegmentedControl(_:))

    statsLabel.font = .monospacedDigitSystemFont(ofSize: 10, weight: .regular)
    statsLabel.textColor = .secondaryLabelColor
    statsLabel.stringValue = ""
  }

  func configure(withId id: UVarint, andTopic topic: String) {
    guard let delegate else { return }

    self.id = id
    self.topic = topic
    self.optionsDefaultsKey = "Franz:\(delegate.getConnectionName()):TopicRecordsOptions:\(topic)"
    self.options = Defaults.shared.get(codable: self.optionsDefaultsKey!) ?? TopicRecordsOptions()

    let status = delegate.makeStatusProc()
    status("Opening Iterator")
    Backend.shared.openIterator(
      forTopic: topic,
      andOffset: .earliest,
      inWorkspace: id
    ).onComplete { iteratorId in
      self.iteratorId = iteratorId
      self.loadRecords(withCookie: self.liveModeCookie) { _ in
        return true
      }
    }
  }

  private func setRecords(_ records: [IteratorRecord],
                          byAppending appending: Bool = true,
                          completionHandler: @escaping () -> Void = { }) {
    assert(Thread.isMainThread)
    weak var ctl = self

    let selectedRow = self.tableView.selectedRow
    var selectedItem: Item?
    if selectedRow >= 0 {
      selectedItem = items[selectedRow]
    }

    let sortDirection = self.options.sortDirection
    let keepBytes = self.options.keepBytes

    self.backpressureSema.wait()
    DispatchQueue.uiBackground.async {
      guard let ctl else {
        completionHandler()
        return
      }

      var items = ctl.items
      var known: [Item.ID: Item] = Dictionary(minimumCapacity: items.count)
      for item in items {
        known[item.id] = item
      }

      if !appending {
        items.removeAll()
      }
      for r in sortDirection == .asc ? records : records.reversed() {
        var it = Item(record: r)
        if let item = known[it.id] {
          item.record = r
          it = item
        }
        items.append(it)
      }

      switch sortDirection {
      case .desc:
        items.sort { $0.id > $1.id }
      case .asc:
        items.sort { $0.id < $1.id }
      }

      var totalBytes = UVarint(0)
      for (row, it) in items.enumerated() {
        totalBytes += UVarint(it.record.key?.count ?? 0)
        totalBytes += UVarint(it.record.value?.count ?? 0)
        if totalBytes > keepBytes {
          items.removeLast(items.count-row)
          break
        }
      }

      ctl.backpressureSema.signal()
      DispatchQueue.main.sync {
        ctl.items = items
        ctl.tableView.reloadData()
        if let selectedItem, let selectedRow = items.firstIndex(of: selectedItem) {
          ctl.tableView.selectRowIndexes([selectedRow], byExtendingSelection: false)
        }

        let bytesStr = ctl.bytesFmt.string(fromByteCount: Int64(totalBytes))
        ctl.statsLabel.stringValue = "Records: \(items.count) (\(bytesStr))"
        completionHandler()
      }
    }
  }

  private func loadRecords(byAppending appending: Bool = true,
                           completionHandler: @escaping ([IteratorRecord]) -> Bool = { _ in true }) {
    guard let delegate else { return }
    guard let iteratorId else { return }

    weak var ctl = self
    let status = delegate.makeStatusProc()
    status("Fetching Records")
    Backend.shared.getRecords(
      iteratorId,
      withMaxBytes: options.maxBytes
    ).onComplete { records in
      guard let ctl else { return }
      if !completionHandler(records) {
        status("Ready")
        return
      }

      if records.isEmpty && appending {
        status("Ready")
        return
      }

      status("Processing Records")
      ctl.setRecords(records, byAppending: appending) {
        status("Ready")
      }
    }
  }

  private func loadRecords(withCookie cookie: Int,
                           completionHandler: @escaping ([IteratorRecord]) -> Bool) {
    weak var ctl = self
    loadRecords { records in
      guard let ctl, ctl.liveModeCookie == cookie else { return false }
      return completionHandler(records)
    }
  }

  private func scheduleLoad(withCookie cookie: Int) {
    loadRecords(withCookie: cookie) { records in
      var deadline = DispatchTime.now()
      if records.isEmpty {
        deadline = deadline.advanced(by: .seconds(1))
      }
      weak var ctl = self
      DispatchQueue.main.asyncAfter(deadline: deadline) {
        ctl?.scheduleLoad(withCookie: cookie)
      }
      return true
    }
  }

  private func toggleLiveMode() {
    assert(Thread.isMainThread)
    if liveModeOn {
      liveModeCookie += 1
      liveModeOn = false
      segmentedControl.setSelected(false, forSegment: 0)
      segmentedControl.setEnabled(true, forSegment: 1)
      segmentedControl.setEnabled(true, forSegment: 2)
      return
    }

    liveModeCookie += 1
    let cookie = liveModeCookie
    setRecords([], byAppending: false)
    options.sortDirection = .desc
    liveModeOn = true
    segmentedControl.setEnabled(false, forSegment: 1)
    segmentedControl.setEnabled(false, forSegment: 2)
    reset(offset: .latest) { [weak self] in
      self?.scheduleLoad(withCookie: cookie)
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

  @objc func didPressSegmentedControl(_ sender: NSSegmentedControl) {
    let segment = sender.selectedSegment
    guard let tag = ControlTag(rawValue: sender.tag(forSegment: segment)) else { return }
    switch tag {
    case .play:
      toggleLiveMode()

    case .options:
      sender.setSelected(false, forSegment: segment)
      let bounds = sender.relativeBounds(forSegment: segment)
      let popover = NSPopover()
      let form = TopicRecordsOptionsForm(model: self.options) {
        if let key = self.optionsDefaultsKey {
          try? Defaults.shared.set(codable: self.options, forKey: key)
        }
        self.setRecords(self.items.map(\.record), byAppending: false)
        popover.close()
      } resetAction: {
        let resetForm = IteratorResetForm { offset in
          self.reset(offset: offset) {
            self.loadRecords(byAppending: false)
          }
          popover.close()
        }
        popover.animates = false
        popover.close()
        popover.contentSize = NSSize(width: 250, height: 100)
        popover.contentViewController = NSHostingController(
          rootView: resetForm.frame(
            width: popover.contentSize.width,
            height: popover.contentSize.height))
        popover.show(relativeTo: bounds, of: sender, preferredEdge: .minY)
        popover.animates = true
      }
      popover.behavior = .semitransient
      popover.contentSize = NSSize(width: 250, height: 260)
      popover.contentViewController = NSHostingController(
        rootView: form.frame(
          width: popover.contentSize.width,
          height: popover.contentSize.height))
      popover.show(relativeTo: bounds, of: sender, preferredEdge: .minY)

    case .more:
      sender.setSelected(false, forSegment: segment)
      sender.isEnabled = false
      loadRecords { records in
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
        }
        switch self.options.sortDirection {
        case .asc:
          self.tableView.scrollToEndOfDocument(self)
        case .desc:
          self.tableView.scrollToBeginningOfDocument(self)
        }
        sender.isEnabled = true
        return true
      }
    }
  }
}

// MARK: - NSMenuDelegate
extension TopicRecordsTableViewController: NSMenuDelegate {
  func menuNeedsUpdate(_ menu: NSMenu) {
    menu.removeAllItems()
    guard !liveModeOn else { return }
    guard tableView.clickedRow >= 0 else { return }
    let record = items[tableView.clickedRow].record
    if record.key != nil {
      menu.addItem(.init(
        title: "Publish Tombstone...",
        action: #selector(didPressPublishTombstoneItem(_:)),
        keyEquivalent: ""))
    }
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
      ).onComplete { record in
        status("Ready")
        self.setRecords([record], byAppending: true)
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
fileprivate class Item: NSObject {
  struct ID: Hashable, Equatable, Comparable {
    let pid: UVarint
    let offset: UVarint

    static func < (lhs: Item.ID, rhs: Item.ID) -> Bool {
      return lhs.offset == rhs.offset ? lhs.pid < rhs.pid : lhs.offset < rhs.offset
    }
  }

  var id: ID
  var record: IteratorRecord

  init(record: IteratorRecord) {
    self.id = ID(pid: record.partitionId, offset: record.offset)
    self.record = record
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
    textField.font = .monospacedSystemFont(ofSize: 12, weight: .regular)
    textField.textColor = tableView.selectedRow == row ? .selectedControlTextColor : .controlTextColor
    if id == .TopicRecordsPartitionId {
      textField.stringValue = "\(record.partitionId)"
      textField.font = .monospacedDigitSystemFont(ofSize: 12, weight: .regular)
    } else if id == .TopicRecordsTimestamp {
      textField.stringValue = DateFormatter.localizedString(
        from: .init(timeIntervalSince1970: Double(record.timestamp)/1000.0),
        dateStyle: .short,
        timeStyle: .medium
      )
      textField.font = .monospacedDigitSystemFont(ofSize: 12, weight: .regular)
    } else if id == .TopicRecordsOffset {
      textField.stringValue = "\(record.offset)"
      textField.font = .monospacedDigitSystemFont(ofSize: 12, weight: .regular)
    } else if id == .TopicRecordsKey {
      setTextFieldData(textField, data: record.key)
    } else if id == .TopicRecordsValue {
      setTextFieldData(textField, data: record.value)
    }
    return view
  }

  private func setTextFieldData(_ textField: NSTextField, data: Data?) {
    if let data {
      if let string = String.text(from: data, withMaxLength: 150) {
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
    guard let origin = tableView.superview?.convert(tableView.frame.origin, to: nil) else { return nil }
    let relativeLocation = NSPoint(
      x: event.locationInWindow.x - origin.x,
      y: event.locationInWindow.y + origin.y)
    let columnIdx = tableView.column(at: relativeLocation)
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

// MARK: - TopicRecordsOptions+Form
fileprivate enum ContentType: Codable {
  case binary
  case json
  case text

  var utType: UTType {
    switch self {
    case .binary:
      return .data
    case .json:
      return .json
    case .text:
      return .plainText
    }
  }
}

fileprivate enum SortDirection: Codable {
  case asc
  case desc
}

fileprivate final class TopicRecordsOptions: ObservableObject, Codable {
  @Published var keyFormat = ContentType.binary
  @Published var valueFormat = ContentType.binary
  @Published var sortDirection = SortDirection.asc
  @Published var maxMBScaled = 0.0
  @Published var keepMBScaled = 1.0

  var maxBytes: UVarint {
    Self.descale(maxMBScaled)*1024*1024
  }

  var keepBytes: UVarint {
    Self.descale(keepMBScaled)*1024*1024
  }

  struct Data: Codable {
    let keyFormat: ContentType
    let valueFormat: ContentType
    let sortDirection: SortDirection
    let maxMBScaled: Double
    let keepMBScaled: Double
  }

  init() {

  }

  init(from decoder: Decoder) throws {
    let data = try Data(from: decoder)
    self.keyFormat = data.keyFormat
    self.valueFormat = data.valueFormat
    self.sortDirection = data.sortDirection
    self.maxMBScaled = data.maxMBScaled
    self.keepMBScaled = data.keepMBScaled
  }

  func encode(to encoder: Encoder) throws {
    let data = Data(
      keyFormat: keyFormat,
      valueFormat: valueFormat,
      sortDirection: sortDirection,
      maxMBScaled: maxMBScaled,
      keepMBScaled: keepMBScaled
    )
    try data.encode(to: encoder)
  }

  static func descale(_ mbScaled: Double) -> UVarint {
    return UVarint(pow(2, mbScaled))
  }
}

fileprivate struct TopicRecordsOptionsForm: View {
  @StateObject var model: TopicRecordsOptions

  let saveAction: () -> Void
  let resetAction: () -> Void

  var bytesFmt: ByteCountFormatter = {
    let fmt = ByteCountFormatter()
    fmt.countStyle = .memory
    return fmt
  }()
  var formattedMaxMB: String {
    bytesFmt.string(fromByteCount: Int64(model.maxBytes))
  }
  var formattedKeepMB: String {
    bytesFmt.string(fromByteCount: Int64(model.keepBytes))
  }

  var body: some View {
    Form {
      Picker("Key Format:", selection: $model.keyFormat) {
        Text("Binary").tag(ContentType.binary)
        Text("JSON").tag(ContentType.json)
        Text("Text").tag(ContentType.text)
      }
      Picker("Value Format:", selection: $model.valueFormat) {
        Text("Binary").tag(ContentType.binary)
        Text("JSON").tag(ContentType.json)
        Text("Text").tag(ContentType.text)
      }
      Picker("Sort:", selection: $model.sortDirection) {
        Text("Ascending").tag(SortDirection.asc)
        Text("Descending").tag(SortDirection.desc)
      }
      VStack(alignment: .trailing, spacing: 0) {
        Slider(value: $model.maxMBScaled, in: 0...7, step: 1) {
          Text("Request Size:")
        }
        HStack(spacing: 2) {
          Text(formattedMaxMB)
            .font(.system(size: 10).monospacedDigit())
            .foregroundColor(.secondary)
          Image(systemName: "info.circle")
            .font(.system(size: 10))
            .foregroundColor(.secondary)
            .help("The maximum amount of data that will be retrieved per partition.")
        }
      }
      VStack(alignment: .trailing, spacing: 0) {
        Slider(value: $model.keepMBScaled, in: 0...10, step: 1) {
          Text("Buffer Size:")
        }
        HStack(spacing: 2) {
          Text(formattedKeepMB)
            .font(.system(size: 10).monospacedDigit())
            .foregroundColor(.secondary)
          Image(systemName: "info.circle")
            .font(.system(size: 10))
            .foregroundColor(.secondary)
            .help("The maximum amount of data that will be buffered in memory.")
        }
      }
      HStack {
        Button("Save") {
          saveAction()
        }
        .buttonStyle(.borderedProminent)
        .keyboardShortcut(.defaultAction)

        Button("Reset...", role: .destructive) {
          resetAction()
        }
      }
    }
    .padding()
  }
}

// MARK: - IteratorResetForm
fileprivate struct IteratorResetForm: View {
  enum Offset {
    case earliest
    case offset
    case latest
  }

  @State var target = Offset.latest
  @State var offset = UVarint(0)

  var resetAction: (IteratorOffset) -> Void

  var offsetFormatter: NumberFormatter = {
    let fmt = NumberFormatter()
    fmt.allowsFloats = false
    fmt.minimum = 0
    return fmt
  }()

  var body: some View {
    Form {
      Picker("Target:", selection: $target) {
        Text("Earliest").tag(Offset.earliest)
        Text("Offset").tag(Offset.offset)
        Text("Latest").tag(Offset.latest)
      }
      if target == .offset {
        TextField("Offset:", value: $offset, formatter: offsetFormatter)
          .onSubmit {
            resetAction(.exact(offset))
          }
      }
      Button("Reset") {
        switch target {
        case .earliest:
          resetAction(.earliest)
        case .offset:
          resetAction(.exact(offset))
        case .latest:
          resetAction(.latest)
        }
      }
        .buttonStyle(.borderedProminent)
        .keyboardShortcut(.defaultAction)
    }
    .padding()
  }
}
