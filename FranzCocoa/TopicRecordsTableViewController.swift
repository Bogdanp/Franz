import Cocoa
import Dispatch
import Foundation
import NoiseSerde
import SwiftUI
import UniformTypeIdentifiers

class TopicRecordsTableViewController: NSViewController {
  @IBOutlet weak var tableView: NSTableView!
  @IBOutlet weak var segmentedControl: NSSegmentedControl!

  weak var delegate: WorkspaceDetailDelegate?

  private var id: UVarint!
  private var topic: String!
  private var iteratorId: UVarint?
  private var items = [Item]()
  private var liveModeOn = false
  private var liveModeCookie = 0

  private var optionsDefaultsKey: String?
  private var options = TopicRecordsOptions()

  override func viewDidLoad() {
    super.viewDidLoad()

    tableView.dataSource = self
    tableView.delegate = self
    tableView.setDraggingSourceOperationMask(.copy, forLocal: false)
    if let connection = delegate?.getConnectionName(), let topic {
      tableView.autosaveName = "Franz:\(connection):TopicRecords:\(topic)"
      tableView.autosaveTableColumns = true
    }

    segmentedControl.target = self
    segmentedControl.action = #selector(didPressSegmentedControl(_:))
  }

  func configure(withId id: UVarint, andTopic topic: String) {
    guard let delegate else { return }

    self.id = id
    self.topic = topic
    self.optionsDefaultsKey = "Franz:\(delegate.getConnectionName()):TopicRecordsOptions:\(topic)"
    self.options = Defaults.shared.get(codable: self.optionsDefaultsKey!) ?? TopicRecordsOptions()

    let status = delegate.makeStatusProc()
    status("Opening Iterator...")
    Backend.shared.openIterator(
      forTopic: topic,
      andOffset: .earliest,
      inWorkspace: id
    ).onComplete { iteratorId in
      self.iteratorId = iteratorId
      self.loadRecords()
    }
  }

  func teardown() {
    assert(Thread.isMainThread)
    liveModeCookie += 1
  }

  private func setRecords(_ records: [IteratorRecord], byAppending appending: Bool = true) {
    var known = [Item.Ident: Item]()
    for item in self.items {
      known[item.id] = item
    }

    var selectedItem: Item?
    if self.tableView.selectedRow >= 0 {
      selectedItem = self.items[self.tableView.selectedRow]
    }

    if !appending {
      self.items.removeAll(keepingCapacity: true)
    }
    for r in records {
      var it = Item(record: r)
      if let item = known[it.id] {
        item.record = r
        it = item
      }
      self.items.append(it)
    }

    self.items.sort { a, b in
      switch self.options.sortDirection {
      case .desc:
        return a.id > b.id
      case .asc:
        return a.id < b.id
      }
    }

    var totalBytes = UVarint(0)
    for (row, it) in self.items.enumerated() {
      totalBytes += UVarint(it.record.key?.count ?? 0)
      totalBytes += UVarint(it.record.value?.count ?? 0)
      if totalBytes > self.options.keepBytes {
        self.items.removeLast(self.items.count-row)
        break
      }
    }

    self.tableView.reloadData()
    if let selectedItem, let selectedRow = self.items.firstIndex(of: selectedItem) {
      self.tableView.selectRowIndexes([selectedRow], byExtendingSelection: false)
    }
  }

  private func loadRecords(byAppending: Bool = true,
                           completionHandler: @escaping ([IteratorRecord]) -> Bool = { _ in true }) {
    guard let delegate else { return }
    guard let iteratorId else { return }

    weak var ctl = self
    let status = delegate.makeStatusProc()
    status("Fetching records...")
    Backend.shared.getRecords(iteratorId).onComplete { records in
      guard let ctl else { return }
      status("Ready")
      if !completionHandler(records) {
        return
      }

      ctl.setRecords(records, byAppending: byAppending)
    }
  }

  private func loadRecords(withCookie cookie: Int, _ proc: @escaping ([IteratorRecord]) -> Bool) {
    loadRecords { records in
      guard self.liveModeCookie == cookie else { return false }
      return proc(records)
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
    guard let delegate else { return }
    guard let iteratorId else { return }

    if liveModeOn {
      liveModeCookie += 1
      liveModeOn = false
      segmentedControl.setSelected(false, forSegment: 0)
      segmentedControl.setEnabled(true, forSegment: 1)
      segmentedControl.setEnabled(true, forSegment: 2)
      return
    }

    let status = delegate.makeStatusProc()
    let cookie = liveModeCookie
    setRecords([], byAppending: false)
    options.sortDirection = .desc
    liveModeOn = true
    segmentedControl.setEnabled(false, forSegment: 1)
    segmentedControl.setEnabled(false, forSegment: 2)
    status("Resetting iterator...")
    Backend.shared.resetIterator(withId: iteratorId, toOffset: .latest).onComplete {
      self.scheduleLoad(withCookie: cookie)
    }
  }

  @objc func didPressSegmentedControl(_ sender: NSSegmentedControl) {
    let segment = sender.selectedSegment
    switch segment {
    case 0: // play/pause
      toggleLiveMode()
    case 1: // settings
      sender.setSelected(false, forSegment: segment)
      let bounds = sender.relativeBounds(forSegment: segment)
      let popover = NSPopover()
      let form = TopicRecordsOptionsForm(model: self.options) {
        if let key = self.optionsDefaultsKey {
          do {
            try Defaults.shared.set(codable: self.options, forKey: key)
          } catch { }
        }
        self.loadRecords()
        popover.close()
      }
      popover.behavior = .semitransient
      popover.contentSize = NSSize(width: 250, height: 200)
      popover.contentViewController = NSHostingController(
        rootView: form.frame(
          width: popover.contentSize.width,
          height: popover.contentSize.height))
      popover.show(relativeTo: bounds, of: sender, preferredEdge: .minY)
    case 2: // more
      sender.setSelected(false, forSegment: segment)
      sender.isEnabled = false
      loadRecords { records in
        if records.isEmpty {
          let bounds = sender.relativeBounds(forSegment: segment)
          let popover = NSPopover()
          popover.behavior = .transient
          popover.contentSize = NSSize(width: 200, height: 50)
          popover.contentViewController = NSHostingController(rootView: Text("No more records.").padding())
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
    default:
      ()
    }
  }
}

// MARK: - SortDirection
fileprivate enum SortDirection: Codable {
  case asc
  case desc
}

// MARK: - Item
fileprivate class Item: NSObject {
  struct Ident: Hashable, Equatable, Comparable {
    let pid: UVarint
    let offset: UVarint

    static func < (lhs: Item.Ident, rhs: Item.Ident) -> Bool {
      return lhs.pid == rhs.pid ? lhs.offset < rhs.offset : lhs.pid < rhs.pid
    }
  }

  var record: IteratorRecord
  var id: Ident {
    Ident(pid: record.partitionId, offset: record.offset)
  }

  init(record: IteratorRecord) {
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
      if let string = String.utf8(from: data, withMaxLength: 150) {
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
    let provider = NSFilePromiseProvider(fileType: options.valueFormat.utType.identifier, delegate: self)
    provider.userInfo = row
    return provider
  }
}

// MARK: - NSFilePromiseProviderDelegate
extension TopicRecordsTableViewController: NSFilePromiseProviderDelegate {
  func filePromiseProvider(_ filePromiseProvider: NSFilePromiseProvider,
                           fileNameForType fileType: String) -> String {
    guard let topic, let row = filePromiseProvider.userInfo as? Int else {
      preconditionFailure()
    }
    let ext = UTType(fileType)?.preferredFilenameExtension
    let record = items[row].record
    let filename = "\(topic)@\(record.partitionId)-\(record.offset)"
    if let ext, ext != "" {
      return "\(filename).\(ext)"
    }
    return filename
  }

  func filePromiseProvider(_ filePromiseProvider: NSFilePromiseProvider,
                           writePromiseTo url: URL,
                           completionHandler: @escaping (Error?) -> Void) {
    guard let row = filePromiseProvider.userInfo as? Int,
          let data = items[row].record.value else {
      completionHandler(nil)
      return
    }
    FileManager.default.createFile(atPath: url.path, contents: data)
    completionHandler(nil)
  }

  // ??? Shouldn't this shit be automatic?
  func filePromiseProvider(_ filePromiseProvider: NSFilePromiseProvider,
                           writePromiseTo url: URL) async throws {
    await withUnsafeContinuation { k in
      self.filePromiseProvider(filePromiseProvider, writePromiseTo: url) { _ in
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

fileprivate final class TopicRecordsOptions: ObservableObject, Codable {
  @Published var keyFormat = ContentType.binary
  @Published var valueFormat = ContentType.binary
  @Published var sortDirection = SortDirection.asc
  @Published var maxBytes = UVarint(1 * 1024 * 1024)
  @Published var keepBytes = UVarint(10 * 1024 * 1024)

  struct Data: Codable {
    let keyFormat: ContentType
    let valueFormat: ContentType
    let sortDirection: SortDirection
    let maxBytes: UVarint
    let keepBytes: UVarint
  }

  init() {

  }

  init(from decoder: Decoder) throws {
    let data = try Data(from: decoder)
    self.keyFormat = data.keyFormat
    self.valueFormat = data.valueFormat
    self.sortDirection = data.sortDirection
    self.maxBytes = data.maxBytes
    self.keepBytes = data.keepBytes
  }

  func encode(to encoder: Encoder) throws {
    let data = Data(
      keyFormat: keyFormat,
      valueFormat: valueFormat,
      sortDirection: sortDirection,
      maxBytes: maxBytes,
      keepBytes: keepBytes
    )
    try data.encode(to: encoder)
  }
}

fileprivate struct TopicRecordsOptionsForm: View {
  @StateObject var model: TopicRecordsOptions

  let save: () -> Void

  var bytesFormatter: NumberFormatter = {
    let fmt = NumberFormatter()
    fmt.allowsFloats = false
    fmt.minimum = 1
    return fmt
  }()

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
      TextField("Max Bytes:", value: $model.maxBytes, formatter: bytesFormatter)
      TextField("Keep Bytes:", value: $model.keepBytes, formatter: bytesFormatter)
      Button("Save") {
        save()
      }
      .buttonStyle(.borderedProminent)
    }
    .padding()
  }
}
