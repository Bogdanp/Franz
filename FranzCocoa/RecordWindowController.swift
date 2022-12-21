import Cocoa

class RecordWindowController: NSWindowController {
  private var record: IteratorRecord!
  private var detailCtl: RecordDetailViewController?

  convenience init() {
    self.init(windowNibName: NSNib.Name("RecordWindowController"))
  }

  override func windowDidLoad() {
    super.windowDidLoad()
  }

  private func reset() {
    guard let record else { return }
    window?.title = "Record@\(record.offset)"

    if detailCtl == nil {
      detailCtl = RecordDetailViewController()
      detailCtl?.configure(withRecord: record)
      window?.contentViewController = detailCtl
    }
  }

  func configure(withRecord record: IteratorRecord) {
    self.record = record
    self.reset()
  }
}
