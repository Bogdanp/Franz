import Cocoa

class RecordWindowController: NSWindowController {
  private var topic: String?
  private var record: IteratorRecord?
  private lazy var detailCtl = RecordDetailViewController()

  convenience init() {
    self.init(windowNibName: NSNib.Name("RecordWindowController"))
  }

  override func windowDidLoad() {
    super.windowDidLoad()
    reset()
  }

  private func reset() {
    guard let record, let topic else { return }
    window?.title = "\(topic): Record \(record.partitionId)@\(record.offset)"
    window?.contentViewController = detailCtl
  }

  func configure(
    withRecord record: IteratorRecord,
    andTopic topic: String,
    andKeyFormat keyFormat: DataFormat,
    andValueFormat valueFormat: DataFormat
  ) {
    self.record = record
    self.topic = topic
    self.detailCtl.configure(
      withRecord: record,
      andKeyFormat: keyFormat,
      andValueFormat: valueFormat
    )
    self.reset()
  }

  func show(_ sender: Any) {
    showWindow(sender)
    window?.center()
  }
}
