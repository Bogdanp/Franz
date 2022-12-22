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

  private var keyEditorCtl = EditorViewController()
  private var valueEditorCtl = EditorViewController()

  override func viewDidLoad() {
    super.viewDidLoad()

    partitionIdField.isSelectable = true
    offsetField.isSelectable = true
    timestampField.isSelectable = true

    if let data = record?.key, let code = String(data: data, encoding: .utf8) {
      keyEditorCtl.configure(code: code, language: .json, border: .noBorder)
    } else {
      keyEditorCtl.configure(code: "", language: .json)
    }
    keyEditorCtl.isEditable = false

    if let data = record?.value, let code = String(data: data, encoding: .utf8) {
      valueEditorCtl.configure(code: code, language: .json, border: .noBorder)
    } else {
      valueEditorCtl.configure(code: "", language: .json)
    }
    valueEditorCtl.isEditable = false
    tabView.addTabViewItem(.init(viewController: keyEditorCtl))
    tabView.addTabViewItem(.init(viewController: valueEditorCtl))

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

  @IBAction func didPushFormatButton(_ sender: Any) {
    switch currentTab {
    case .key:
      if let data = record?.key,
         let code = String(data: data, encoding: .utf8),
         let formatted = Error.wait(Backend.shared.ppJson(code)) {
        keyEditorCtl.configure(code: formatted, language: .json)
      }
    case .value:
      if let data = record?.value,
         let code = String(data: data, encoding: .utf8),
         let formatted = Error.wait(Backend.shared.ppJson(code)) {
        valueEditorCtl.configure(code: formatted, language: .json)
      }
    }
  }
}
