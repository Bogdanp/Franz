import Cocoa
import NoiseSerde

class WorkspaceSidebarViewController: NSViewController {
  private var id: UVarint!
  private var metadata = Metadata(brokers: [], topics: [], groups: [])
  private var entries = [SidebarEntry]()
  private var filteredEntries = [SidebarEntry]()
  private var selectedEntry: SidebarEntry?
  private var contextMenu = NSMenu()

  @IBOutlet weak var tableView: NSTableView!
  @IBOutlet weak var noTopicsField: NSTextField!
  @IBOutlet weak var filterField: NSSearchField!

  weak var delegate: WorkspaceSidebarDelegate?

  override func viewDidLoad() {
    super.viewDidLoad()

    contextMenu.delegate = self

    tableView.menu = contextMenu
    tableView.register(.init(nibNamed: "SidebarEntryCellView", bundle: nil), forIdentifier: .entry)
    tableView.register(.init(nibNamed: "SidebarGroupCellView", bundle: nil), forIdentifier: .group)
    tableView.delegate = self
    tableView.dataSource = self
    tableView.reloadData()

    filterField.focusRingType = .none
  }

  func configure(withId id: UVarint, andMetadata metadata: Metadata) {
    assert(Thread.isMainThread)

    self.id = id
    self.metadata = metadata
    self.updateEntries()
  }

  private func updateEntries() {
    var oldBrokers = [String: SidebarEntry]()
    var oldTopics = [String: SidebarEntry]()
    var oldGroups = [String: SidebarEntry]()
    for e in entries {
      switch e.kind {
      case .broker:
        oldBrokers[e.label] = e
      case .topic:
        oldTopics[e.label] = e
      case .consumerGroup:
        oldGroups[e.label] = e
      default:
        ()
      }
    }

    entries.removeAll(keepingCapacity: true)
    entries.append(SidebarEntry(withKind: .group, label: "Brokers"))
    for b in metadata.brokers {
      if let e = oldBrokers[b.address] {
        e.data = b
        entries.append(e)
      } else {
        let e = SidebarEntry(withKind: .broker, label: b.address, andData: b)
        entries.append(e)
      }
    }

    entries.append(SidebarEntry(withKind: .group, label: "Topics"))
    for t in metadata.topics {
      if let e = oldTopics[t.name] {
        e.data = t
        e.count = "\(t.partitions.count)"
        entries.append(e)
      } else {
        let e = SidebarEntry(withKind: .topic, label: t.name, count: "\(t.partitions.count)", andData: t)
        entries.append(e)
      }
    }

    entries.append(SidebarEntry(withKind: .group, label: "Consumer Groups"))
    for g in metadata.groups {
      if let e = oldGroups[g.id] {
        e.data = g
        entries.append(e)
      } else {
        let e = SidebarEntry(withKind: .consumerGroup, label: g.id, andData: g)
        entries.append(e)
      }
    }

    filterEntries()
    updateSelection()
  }

  private func filterEntries() {
    filteredEntries = entries
    if filterField.stringValue != "" {
      let filter = filterField.stringValue
      filteredEntries = []
      for e in entries where e.kind == .group || e.label.localizedCaseInsensitiveContains(filter) {
        filteredEntries.append(e)
      }
    }
  }

  private func updateSelection() {
    var selectedRow = -1
    for (row, e) in filteredEntries.enumerated() where e == selectedEntry {
      selectedRow = row
      break
    }

    noTopicsField.isHidden = !(metadata.topics.isEmpty && metadata.brokers.isEmpty)
    tableView.reloadData()
    if selectedRow >= 0 {
      if selectedRow != tableView.selectedRow {
        tableView.selectRowIndexes([selectedRow], byExtendingSelection: false)
      }
    } else {
      delegate?.sidebar(didDeselectEntry: selectedEntry)
    }
  }

  func selectEntry(withKind kind: SidebarEntryKind, andLabel label: String) {
    for (i, e) in filteredEntries.enumerated() {
      if e.kind == kind && e.label == label {
        tableView.selectRowIndexes([i], byExtendingSelection: false)
        return
      }
    }
  }

  @IBAction func didPressNewTopicButton(_ sender: Any) {
    let ctl = NewTopicFormViewController()
    ctl.delegate = self
    ctl.configure(withId: id)
    presentAsSheet(ctl)
  }

  @IBAction func didFilterSidebarItems(_ sender: NSSearchField) {
    filterEntries()
    updateSelection()
  }
}

// MARK: - WorkspaceSidebarDelegate
protocol WorkspaceSidebarDelegate: AnyObject {
  func sidebar(didSelectEntry entry: Any, withKind kind: SidebarEntryKind)
  func sidebar(didDeselectEntry entry: Any?)
  func sidebar(didDeleteTopic topic: Topic)
  func sidebar(didDeleteConsumerGroup group: Group)
  func sidebarRequestsReload(withNewTopic name: String)
}

// MARK: - NewTopicFormDelegate
extension WorkspaceSidebarViewController: NewTopicFormDelegate {
  func didCancelNewTopicForm(_ sender: NewTopicFormViewController) {
  }

  func didCreateNewTopic(named name: String) {
    delegate?.sidebarRequestsReload(withNewTopic: name)
  }
}

// MARK: - NSMenuDelegate
extension WorkspaceSidebarViewController: NSMenuDelegate {
  func menuNeedsUpdate(_ menu: NSMenu) {
    menu.removeAllItems()
    if tableView.clickedRow >= 0 {
      let entry = filteredEntries[tableView.clickedRow]
      switch entry.kind {
      case .topic:
        menu.addItem(.init(
          title: "Delete...",
          action: #selector(didPressDeleteTopic(_:)),
          keyEquivalent: .backspaceKeyEquivalent
        ))
      case .consumerGroup:
        menu.addItem(.init(
          title: "Delete...",
          action: #selector(didPressDeleteConsumerGroup(_:)),
          keyEquivalent: .backspaceKeyEquivalent
        ))
      default:
        ()
      }
    }
  }

  @objc func didPressDeleteTopic(_ sender: NSMenuItem) {
    assert(tableView.clickedRow >= 0)
    guard let topic = filteredEntries[tableView.clickedRow].data as? Topic else {
      return
    }
    let alert = NSAlert()
    alert.alertStyle = .warning
    alert.messageText = "Delete topic \(topic.name)?"
    alert.informativeText = "This action cannot be undone."
    alert.addButton(withTitle: "Delete")
    alert.addButton(withTitle: "Cancel")
    switch alert.runModal() {
    case .alertFirstButtonReturn:
      delegate?.sidebar(didDeleteTopic: topic)
    default:
      ()
    }
  }

  @objc func didPressDeleteConsumerGroup(_ sender: NSMenuItem) {
    assert(tableView.clickedRow >= 0)
    guard let group = filteredEntries[tableView.clickedRow].data as? Group else {
      return
    }
    let alert = NSAlert()
    alert.alertStyle = .warning
    alert.messageText = "Delete consumer group \(group.id)?"
    alert.informativeText = "This action cannot be undone."
    alert.addButton(withTitle: "Delete")
    alert.addButton(withTitle: "Cancel")
    switch alert.runModal() {
    case .alertFirstButtonReturn:
      delegate?.sidebar(didDeleteConsumerGroup: group)
    default:
      ()
    }
  }
}

// MARK: - NSTableViewDelegate
extension WorkspaceSidebarViewController: NSTableViewDelegate {
  func tableView(_ tableView: NSTableView, viewFor tableColumn: NSTableColumn?, row: Int) -> NSView? {
    let entry = filteredEntries[row]
    switch entry.kind {
    case .broker, .topic, .consumerGroup:
      guard let view = tableView.makeView(withIdentifier: .entry, owner: nil) as? SidebarEntryCellView else {
        return nil
      }

      if let count = entry.count {
        view.configure(withText: entry.label, andCount: count)
      } else {
        view.configure(withText: entry.label)
      }

      var image: NSImage?
      switch entry.kind {
      case .broker:
        image = NSImage(systemSymbolName: "xserve", accessibilityDescription: "Broker")
      case .topic:
        image = NSImage(systemSymbolName: "tray.full.fill", accessibilityDescription: "Topic")
      case .consumerGroup:
        image = NSImage(systemSymbolName: "circle.grid.3x3.fill", accessibilityDescription: "Consumer Group")
      default:
        image = nil
      }
      view.imageView?.image = image
      return view
    case .group:
      guard let view = tableView.makeView(withIdentifier: .group, owner: nil) as? SidebarGroupCellView else {
        return nil
      }

      view.textField.stringValue = entry.label
      return view
    }
  }

  func tableView(_ tableView: NSTableView, isGroupRow row: Int) -> Bool {
    return filteredEntries[row].kind == .group
  }

  func tableView(_ tableView: NSTableView, shouldSelectRow row: Int) -> Bool {
    return filteredEntries[row].kind != .group
  }

  func tableViewSelectionDidChange(_ notification: Notification) {
    let e = filteredEntries[tableView.selectedRow]
    guard e.kind != .group else { return }
    guard let data = e.data else { return }
    selectedEntry = e
    delegate?.sidebar(didSelectEntry: data, withKind: e.kind)
  }
}

// MARK: - NSTableViewDataSource
extension WorkspaceSidebarViewController: NSTableViewDataSource {
  func numberOfRows(in tableView: NSTableView) -> Int {
    return filteredEntries.count
  }

  func tableView(_ tableView: NSTableView, objectValueFor tableColumn: NSTableColumn?, row: Int) -> Any? {
    return filteredEntries[row]
  }
}

// MARK: - NSUserInterfaceItemIdentifier
extension NSUserInterfaceItemIdentifier {
  static let entry = NSUserInterfaceItemIdentifier("Entry")
  static let group = NSUserInterfaceItemIdentifier("Group")
}

// MARK: - SidebarEntry
enum SidebarEntryKind {
  case group
  case broker
  case topic
  case consumerGroup
}

class SidebarEntry: NSObject {
  let kind: SidebarEntryKind
  let label: String
  var count: String?
  var data: Any?

  init(withKind kind: SidebarEntryKind, label: String, count: String? = nil, andData data: Any? = nil) {
    self.kind = kind
    self.label = label
    self.count = count
    self.data = data
  }
}
