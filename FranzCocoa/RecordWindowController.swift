import Cocoa

class RecordWindowController: NSWindowController {
  private var topic: String?
  private var record: IteratorRecord?
  private lazy var detailCtl = RecordDetailViewController()

  weak var delegate: RecordWindowDelegate?

  convenience init() {
    self.init(windowNibName: NSNib.Name("RecordWindowController"))
  }

  override func windowDidLoad() {
    super.windowDidLoad()
    window?.delegate = self
    reset()
  }

  private func reset() {
    guard let record, let topic else { return }
    window?.title = "\(topic): Record \(record.offset)@\(record.partitionId)"
    window?.contentViewController = detailCtl
  }

  func configure(
    withRecord record: IteratorRecord,
    andTopic topic: String,
    andKeyFormat keyFormat: DataFormat,
    andValueFormat valueFormat: DataFormat,
    andTab tab: RecordDetailViewController.Tab
  ) {
    self.record = record
    self.topic = topic
    self.detailCtl.configure(
      withRecord: record,
      andKeyFormat: keyFormat,
      andValueFormat: valueFormat,
      andTab: tab
    )
    self.reset()
  }

  func show(_ sender: Any) {
    showWindow(sender)
    window?.center()
  }
}

// MARK: - RecordWindowDelegate
protocol RecordWindowDelegate: AnyObject {
  func recordWindowWillClose(_ sender: AnyObject)
}

// MARK: - NSWindowDelegate
extension RecordWindowController: NSWindowDelegate {
  func windowWillClose(_ notification: Notification) {
    delegate?.recordWindowWillClose(self)
  }

  func windowDidBecomeKey(_ notification: Notification) {
    guard let item = MainMenu.shared.find(itemByPath: [.FileMenuItem, .SaveAsMenuItem]) else {
      return
    }
    item.action = #selector(didPushSaveAsItem(_:))
    item.target = self
  }

  func windowDidResignKey(_ notification: Notification) {
    guard let item = MainMenu.shared.find(itemByPath: [.FileMenuItem, .SaveAsMenuItem]) else {
      return
    }
    item.action = nil
    item.target = nil
  }

  @objc func didPushSaveAsItem(_ sender: NSMenuItem) {
    detailCtl.save()
  }
}
