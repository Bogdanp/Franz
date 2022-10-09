import Cocoa
import NoiseSerde

class NewTopicFormViewController: NSViewController {
  private var id: UVarint!
  fileprivate var options = [EditableTopicOption]()

  @IBOutlet weak var nameField: NSTextField!
  @IBOutlet weak var partitionsField: NSTextField!
  @IBOutlet weak var optionsTable: NSTableView!

  @IBOutlet weak var cancelButton: NSButton!
  @IBOutlet weak var createButton: NSButton!

  var delegate: NewTopicFormDelegate?

  override func viewDidLoad() {
    super.viewDidLoad()

    optionsTable.doubleAction = #selector(didDoubleClickOptionsTable(_:))
    optionsTable.delegate = self
    optionsTable.dataSource = self
    optionsTable.reloadData()

    let partitionsFormatter = NumberFormatter()
    partitionsFormatter.allowsFloats = false
    partitionsFormatter.minimum = 1
    partitionsField.formatter = partitionsFormatter
  }

  override func viewDidAppear() {
    view.window?.setFrame(NSRect(x: 0, y: 0, width: 480, height: 280), display: true)
    view.window?.styleMask.remove(.resizable)
    view.window?.styleMask.update(with: .fullSizeContentView)
  }

  func configure(withId id: UVarint) {
    self.id = id
  }

  @objc func didDoubleClickOptionsTable(_ sender: NSTableView) {
    options.append(EditableTopicOption())
    optionsTable.reloadData()
    optionsTable.selectRowIndexes([options.count-1], byExtendingSelection: false)
    optionsTable.editColumn(0, row: options.count-1, with: nil, select: true)
  }

  @objc func didFinishEditingOptionName(_ sender: NSTextField) {
    let option = options[optionsTable.selectedRow]
    option.key = sender.stringValue
  }

  @objc func didFinishEditingOptionValue(_ sender: NSTextField) {
    let option = options[optionsTable.selectedRow]
    option.value = sender.stringValue
  }

  @IBAction func didPressCancelButton(_ sender: NSButton) {
    dismiss(self)
    delegate?.didCancelNewTopicForm(self)
  }

  @IBAction func didPressCreateButton(_ sender: NSButton) {
    if nameField.stringValue == "" {
      nameField.becomeFirstResponder()
      return
    }
    if partitionsField.stringValue == "" {
      partitionsField.becomeFirstResponder()
      return
    }
    let name = nameField.stringValue
    let partitions = partitionsField.integerValue
    let options = self.options.map { TopicOption(key: $0.key, value: $0.value) }

    cancelButton.isEnabled = false
    createButton.isEnabled = false
    Backend.shared.createTopic(
      withId: id,
      named: name,
      partitions: UVarint(partitions),
      andOptions: options
    ).onComplete { error in
      self.cancelButton.isEnabled = true
      self.createButton.isEnabled = true
      guard let error else {
        self.dismiss(self)
        self.delegate?.newTopicFormCompleted(withName: name, partitions: partitions, andOptions: options)
        return
      }

      let alert = NSAlert()
      alert.messageText = "Error"
      alert.informativeText = error
      alert.runModal()
    }
  }
}

// MARK: -NewTopicFormDelegate
protocol NewTopicFormDelegate {
  func didCancelNewTopicForm(_ sender: NewTopicFormViewController)
  func newTopicFormCompleted(withName name: String, partitions: Int, andOptions options: [TopicOption])
}

// MARK: -EditableTopicOption
fileprivate class EditableTopicOption: NSObject {
  var key = ""
  var value = ""
}

// MARK: -NSTableViewDataSource
extension NewTopicFormViewController: NSTableViewDataSource {
  func numberOfRows(in tableView: NSTableView) -> Int {
    return options.count
  }
}

// MARK: -NSTableViewDelegate
extension NewTopicFormViewController: NSTableViewDelegate {
  func tableView(_ tableView: NSTableView, shouldEdit tableColumn: NSTableColumn?, row: Int) -> Bool {
    return true
  }

  func tableView(_ tableView: NSTableView, viewFor tableColumn: NSTableColumn?, row: Int) -> NSView? {
    guard let view = tableView.makeView(withIdentifier: tableColumn!.identifier, owner: nil) as? NSTableCellView else {
      return nil
    }
    switch tableColumn?.identifier {
    case NSUserInterfaceItemIdentifier("TopicOptionName"):
      view.textField?.stringValue = options[row].key
      view.textField?.action = #selector(didFinishEditingOptionName(_:))
      view.textField?.isAutomaticTextCompletionEnabled = true
    case NSUserInterfaceItemIdentifier("TopicOptionValue"):
      view.textField?.stringValue = options[row].value
      view.textField?.action = #selector(didFinishEditingOptionValue(_:))
      view.textField?.isAutomaticTextCompletionEnabled = true
    default:
      ()
    }
    return view
  }
}
