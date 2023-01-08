import Cocoa

class RecordDetailViewController: NSViewController {
  private var record: IteratorRecord?
  private var keyFormat = DataFormat.binary
  private var valueFormat = DataFormat.binary

  @IBOutlet weak var partitionIdField: NSTextField!
  @IBOutlet weak var offsetField: NSTextField!
  @IBOutlet weak var timestampField: NSTextField!

  @IBOutlet weak var tabView: NSTabView!
  @IBOutlet weak var keyButton: NSButton!
  @IBOutlet weak var valueButton: NSButton!
  @IBOutlet weak var headersButton: NSButton!

  enum Tab {
    case key
    case value
    case headers
  }

  private var currentTab = Tab.key

  override func viewDidLoad() {
    super.viewDidLoad()

    partitionIdField.isSelectable = true
    offsetField.isSelectable = true
    timestampField.isSelectable = true

    guard let record else { return }
    let keyDataCtl = DataViewController()
    keyDataCtl.configure(withData: record.key ?? Data(), andFormat: keyFormat)
    tabView.addTabViewItem(.init(viewController: keyDataCtl))

    let valueDataCtl = DataViewController()
    valueDataCtl.configure(withData: record.value ?? Data(), andFormat: valueFormat)
    tabView.addTabViewItem(.init(viewController: valueDataCtl))

    let headersDataCtl = HeadersTableViewController()
    headersDataCtl.configure(record.headers)
    tabView.addTabViewItem(.init(viewController: headersDataCtl))

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

    keyButton.state = .off
    valueButton.state = .off
    headersButton.state = .off
    switch currentTab {
    case .key:
      keyButton.state = .on
      tabView.selectTabViewItem(at: 0)
    case .value:
      valueButton.state = .on
      tabView.selectTabViewItem(at: 1)
    case .headers:
      headersButton.state = .on
      tabView.selectTabViewItem(at: 2)
    }
  }

  func configure(
    withRecord record: IteratorRecord,
    andKeyFormat keyFormat: DataFormat = .binary,
    andValueFormat valueFormat: DataFormat = .binary,
    andTab tab: Tab = .key
  ) {
    self.record = record
    self.keyFormat = keyFormat
    self.valueFormat = valueFormat
    self.currentTab = tab
  }

  func save() {
    var title = ""
    var data: Data?
    switch currentTab {
    case .key:
      title = "Save Record Key"
      data = record?.key
    case .value:
      title = "Save Record Value"
      data = record?.value
    default:
      ()
    }
    guard let data else { return }

    let dialog = NSSavePanel()
    dialog.isExtensionHidden = false
    dialog.title = title
    switch dialog.runModal() {
    case .OK:
      guard let url = dialog.url else { return }
      Error.block {
        try data.write(to: url, options: .atomic)
      }
    default:
      return
    }
  }

  @IBAction func didPushKeyButton(_ sender: Any) {
    currentTab = .key
    reset()
  }

  @IBAction func didPushValueButton(_ sender: Any) {
    currentTab = .value
    reset()
  }

  @IBAction func didPushHeadersButton(_ sender: Any) {
    currentTab = .headers
    reset()
  }
}
