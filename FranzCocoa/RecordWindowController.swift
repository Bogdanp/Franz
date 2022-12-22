import Cocoa

class RecordWindowController: NSWindowController {
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
    guard let record else { return }
    window?.title = "Record@\(record.offset)"
    window?.contentViewController = detailCtl
  }

  func configure(
    withRecord record: IteratorRecord,
    andKeyFormat keyFormat: DataFormat,
    andValueFormat valueFormat: DataFormat
  ) {
    self.record = record
    self.detailCtl.configure(
      withRecord: record,
      andKeyFormat: keyFormat,
      andValueFormat: valueFormat
    )
    self.reset()
  }
}
