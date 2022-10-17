import Cocoa
import NoiseSerde

class NewTopicFormViewController: NSViewController {
  private var id: UVarint!
  fileprivate var options = [EditableTopicOption]()

  @IBOutlet weak var nameField: NSTextField!
  @IBOutlet weak var partitionsField: NSTextField!
  @IBOutlet weak var optionsTable: TopicOptionsTableView!

  @IBOutlet weak var cancelButton: NSButton!
  @IBOutlet weak var createButton: NSButton!

  var delegate: NewTopicFormDelegate?

  private var completionTimers = [TopicOptionTextField: Timer]()

  override func viewDidLoad() {
    super.viewDidLoad()

    optionsTable.target = self
    optionsTable.doubleAction = #selector(didDoubleClickOptionsTable(_:))
    optionsTable.deleteAction = #selector(didDeleteOption(_:))
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
    if let view = optionsTable.view(atColumn: 0, row: options.count-1, makeIfNecessary: false) as? NSTableCellView {
      view.textField?.currentEditor()?.complete(self)
    }
  }

  @objc func didDeleteOption(_ sender: NSTableView) {
    if optionsTable.selectedRow >= 0 {
      options.remove(at: optionsTable.selectedRow)
      optionsTable.reloadData()
    }
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
      named: name,
      withPartitions: UVarint(partitions),
      andOptions: options,
      inWorkspace: id
    ).sink(onError: { message in
      self.cancelButton.isEnabled = true
      self.createButton.isEnabled = true

      let alert = NSAlert()
      alert.messageText = "Error"
      alert.informativeText = message
      alert.runModal()
    }) {
      self.cancelButton.isEnabled = true
      self.createButton.isEnabled = true
      self.dismiss(self)
      self.delegate?.didCreateNewTopic(named: name)
    }
  }
}

// MARK: -NewTopicFormDelegate
protocol NewTopicFormDelegate {
  func didCancelNewTopicForm(_ sender: NewTopicFormViewController)
  func didCreateNewTopic(named name: String)
}

// MARK: -EditableTopicOption
fileprivate class EditableTopicOption: NSObject {
  var key = ""
  var value = ""
}

// MARK: -TopicOptionsTableView
class TopicOptionsTableView: NSTableView {
  var deleteAction: Selector?

  override func keyDown(with event: NSEvent) {
    switch event.keyCode {
    case 51:
      if selectedRow >= 0 {
        let _ = target?.perform(deleteAction, with: self)
        return
      }
    default:
      super.keyDown(with: event)
    }
  }
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
    guard let textField = view.textField as? TopicOptionTextField else {
      return nil
    }
    switch tableColumn?.identifier {
    case NSUserInterfaceItemIdentifier("TopicOptionName"):
      textField.stringValue = options[row].key
      textField.action = #selector(didFinishEditingOptionName(_:))
      textField.isAutomaticTextCompletionEnabled = true
      textField.delegate = self
      textField.kind = .key
      textField.row = row
    case NSUserInterfaceItemIdentifier("TopicOptionValue"):
      textField.stringValue = options[row].value
      textField.action = #selector(didFinishEditingOptionValue(_:))
      textField.isAutomaticTextCompletionEnabled = true
      textField.delegate = self
      textField.kind = .value
      textField.row = row
    default:
      ()
    }
    return view
  }
}

// MARK: -TopicOptionTextField
enum TopicOptionTextFieldKind {
  case key
  case value
}

class TopicOptionTextField: NSTextField {
  var kind: TopicOptionTextFieldKind = .key
  var row: Int = -1
}

// MARK: -NSTextFieldDelegate
extension NewTopicFormViewController: NSTextFieldDelegate {
  static let completions: [String: [String]] = [
    "cleanup.policy": [
      "compact",
      "delete"
    ],
    "compression.type": [
      "gzip",
      "lz4",
      "producer",
      "snappy",
      "uncompressed",
      "zstd",
    ],
    "delete.retention.ms": [],
    "file.delete.delay.ms": [],
    "flush.messages": [],
    "flush.ms": [],
    "follower.replication.throttled.replicas": [],
    "index.interval.bytes": [],
    "leader.replication.throttled.replicas": [],
    "max.compaction.lag.ms": [],
    "max.message.bytes": [],
    "message.timestamp.difference.max.ms": [],
    "message.timestamp.type": [
      "CreateTime",
      "LogAppendTime",
    ],
    "min.cleanable.dirty.ratio": [],
    "min.compaction.lag.ms": [],
    "min.insync.replicas": [],
    "preallocate": [],
    "retention.bytes": [],
    "retention.ms": [],
    "segment.bytes": [],
    "segment.index.bytes": [],
    "segment.jitter.ms": [],
    "segment.ms": [],
    "unclean.leader.election.enable": [],
    "message.downconversion.enable": [],
  ]

  func control(_ control: NSControl, textView: NSTextView, completions words: [String], forPartialWordRange charRange: NSRange, indexOfSelectedItem index: UnsafeMutablePointer<Int>) -> [String] {
    guard let textField = control as? TopicOptionTextField else {
      return []
    }
    guard let range = Range(charRange, in: textView.string) else {
      return []
    }
    let substr = String(textView.string[range])
    switch textField.kind {
    case .key:
      if let _ = Self.completions[substr] {
        return []
      }
      return Self.completions.keys.filter { k in
        k.starts(with: substr)
      }.sorted()
    case .value:
      let option = options[textField.row]
      guard let completions = Self.completions[option.key] else {
        return []
      }
      if let _ = completions.first(where: { $0 == substr }) {
        return []
      }
      return completions.filter { k in
        k.starts(with: substr)
      }.sorted()
    }
  }

  func controlTextDidChange(_ notification: Notification) {
    guard let editor = notification.userInfo?["NSFieldEditor"] as? NSTextView else {
      return
    }
    guard let textField = notification.object as? TopicOptionTextField else {
      return
    }
    if let timer = completionTimers[textField] {
      timer.invalidate()
    }
    completionTimers[textField] = Timer.scheduledTimer(withTimeInterval: 0.5, repeats: false) { _ in
      editor.complete(self)
      self.completionTimers.removeValue(forKey: textField)
    }
  }
}
