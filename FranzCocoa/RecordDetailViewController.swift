import Cocoa

class RecordDetailViewController: NSViewController {
  private var record: IteratorRecord?

  @IBOutlet weak var partitionIdField: NSTextField!
  @IBOutlet weak var offsetField: NSTextField!
  @IBOutlet weak var timestampField: NSTextField!

  @IBOutlet weak var tabView: NSTabView!
  @IBOutlet weak var keyButton: NSButton!
  @IBOutlet weak var valueButton: NSButton!

  private enum Tab {
    case key
    case value
  }

  private var currentTab = Tab.key

  override func viewDidLoad() {
    super.viewDidLoad()

    partitionIdField.isSelectable = true
    offsetField.isSelectable = true
    timestampField.isSelectable = true

    let keyDataCtl = DataViewController()
    keyDataCtl.configure(withData: record?.key ?? Data())
    tabView.addTabViewItem(.init(viewController: keyDataCtl))

    let valueDataCtl = DataViewController()
    valueDataCtl.configure(withData: record?.value ?? Data())
    tabView.addTabViewItem(.init(viewController: valueDataCtl))

    reset()
  }

  private func reset() {
    guard let record else { return }

    partitionIdField.stringValue = String(describing: record.partitionId)
    offsetField.stringValue = String(describing: record.offset)
    timestampField.stringValue = DateFormatter.localizedString(
      from: Date(timeIntervalSince1970: TimeInterval(Double(record.timestamp)/1000.0)),
      dateStyle: .long,
      timeStyle: .long
    )

    switch currentTab {
    case .key:
      keyButton.state = .on
      valueButton.state = .off
      tabView.selectTabViewItem(at: 0)
    case .value:
      keyButton.state = .off
      valueButton.state = .on
      tabView.selectTabViewItem(at: 1)
    }
  }

  func configure(withRecord record: IteratorRecord) {
    self.record = record
  }

  @IBAction func didPushKeyButton(_ sender: Any) {
    currentTab = .key
    reset()
  }

  @IBAction func didPushValueButton(_ sender: Any) {
    currentTab = .value
    reset()
  }
}
