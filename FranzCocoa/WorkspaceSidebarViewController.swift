import Cocoa
import NoiseSerde
import OSLog

fileprivate let logger = Logger(
  subsystem: "io.defn.Franz",
  category: "WorkspaceSidebar"
)

class WorkspaceSidebarViewController: NSViewController {
  private var id: UVarint!
  private var conn: ConnectionDetails!
  private var metadata = Metadata(brokers: [], topics: [], groups: [], schemas: [])
  private var entries = [SidebarEntry]()
  private var filteredEntries = [SidebarEntry]()
  private var selectedEntry: SidebarEntry?
  private var contextMenu = NSMenu()
  private var defaultsKey: String?

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

  func configure(withId id: UVarint, andConn conn: ConnectionDetails, andMetadata metadata: Metadata) {
    assert(Thread.isMainThread)

    self.id = id
    self.conn = conn
    self.metadata = metadata
    self.defaultsKey = "WorkspaceSidebar:\(conn.id!)"
    self.updateEntries()
  }

  private func getState() -> WorkspaceSidebarState {
    guard let defaultsKey else { return WorkspaceSidebarState() }
    return Defaults.shared.get(codable: defaultsKey) ?? WorkspaceSidebarState()
  }

  private func setState(_ state: WorkspaceSidebarState) {
    guard let defaultsKey else { return }
    do {
      try Defaults.shared.set(codable: state, forKey: defaultsKey)
    } catch {
      logger.error("failed to save sidebar state: \(error)")
    }
  }

  private func updateEntries() {
    var oldBrokers = [String: SidebarEntry]()
    var oldTopics = [String: SidebarEntry]()
    var oldConsumerGroups = [String: SidebarEntry]()
    var oldSchemas = [String: SidebarEntry]()
    for e in entries {
      switch e.kind {
      case .broker:
        oldBrokers[e.label] = e
      case .topic:
        oldTopics[e.label] = e
      case .consumerGroup:
        oldConsumerGroups[e.label] = e
      case .schema:
        oldSchemas[e.label] = e
      default:
        continue
      }
    }

    let state = getState()
    entries.removeAll(keepingCapacity: true)
    entries.append(SidebarEntry(
      withKind: .group,
      label: "Brokers",
      andTag: SidebarGroup.brokers.rawValue,
      andCollapsed: state.groupCollapsedStates[.brokers] ?? false
    ))
    for b in metadata.brokers {
      if let e = oldBrokers[b.address] {
        e.data = b
        entries.append(e)
      } else {
        let e = SidebarEntry(withKind: .broker, label: b.address, andData: b)
        entries.append(e)
      }
    }

    let options = Defaults.shared.get(codable: "Franz:\(conn.name):SidebarOptions") ?? SidebarOptions()

    entries.append(SidebarEntry(
      withKind: .group,
      label: "Topics",
      andTag: SidebarGroup.topics.rawValue,
      andCollapsed: state.groupCollapsedStates[.topics] ?? false
    ))
    for t in metadata.topics {
      let e: SidebarEntry = oldTopics[t.name] ?? SidebarEntry(withKind: .topic, label: t.name)
      e.data = t
      switch options.topicStat {
      case .none:
        e.count = nil
      case .partitionCount:
        e.count = format(stat: Int64(t.partitions.count))
      case .minLag:
        e.count = format(stat: t.stats.minLag)
      case .maxLag:
        e.count = format(stat: t.stats.maxLag)
      case .sumLag:
        e.count = format(stat: t.stats.sumLag)
      }
      entries.append(e)
    }

    entries.append(SidebarEntry(
      withKind: .group,
      label: "Consumer Groups",
      andTag: SidebarGroup.consumerGroups.rawValue,
      andCollapsed: state.groupCollapsedStates[.consumerGroups] ?? false
    ))
    for g in metadata.groups {
      let e = oldConsumerGroups[g.id] ?? SidebarEntry(withKind: .consumerGroup, label: g.id)
      e.data = g
      switch options.groupStat {
      case .none:
        e.count = nil
      case .minLag:
        e.count = format(stat: g.stats.minLag)
      case .maxLag:
        e.count = format(stat: g.stats.maxLag)
      case .sumLag:
        e.count = format(stat: g.stats.sumLag)
      }
      entries.append(e)
    }

    if conn?.schemaRegistryId != nil {
      entries.append(SidebarEntry(
        withKind: .group,
        label: "Schemas",
        andTag: SidebarGroup.schemas.rawValue,
        andCollapsed: state.groupCollapsedStates[.schemas] ?? false
      ))
      for s in metadata.schemas {
        if let e = oldSchemas[s.name] {
          e.data = s
          entries.append(e)
        } else {
          let e = SidebarEntry(withKind: .schema, label: s.name, andData: s)
          entries.append(e)
        }
      }
    }

    filterEntries()
    updateSelection()
    updateCollapsed()
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

  func updateCollapsed(animatingItems animate: Bool = false) {
    var collapsed = Set<SidebarEntryKind>()
    var hidden = IndexSet()
    var notHidden = IndexSet()
    for (idx, e) in filteredEntries.enumerated() {
      if e.kind == .group {
        guard let group = SidebarGroup(rawValue: e.tag) else {
          continue
        }
        if e.collapsed {
          switch group {
          case .brokers:
            collapsed.insert(.broker)
          case .topics:
            collapsed.insert(.topic)
          case .consumerGroups:
            collapsed.insert(.consumerGroup)
          case .schemas:
            collapsed.insert(.schema)
          }
        }
        continue
      }
      if collapsed.contains(e.kind) {
        hidden.insert(idx)
      } else {
        notHidden.insert(idx)
      }
    }

    if !tableView.hiddenRowIndexes.isSuperset(of: hidden) {
      tableView.hideRows(at: hidden, withAnimation: animate ? .slideUp : [])
    }

    if !tableView.hiddenRowIndexes.isDisjoint(with: notHidden) {
      tableView.unhideRows(at: notHidden, withAnimation: animate ? .slideDown : [])
    }
  }

  func clearFilter() {
    filterField.stringValue = ""
    updateEntries()
  }

  @discardableResult
  func selectEntry(withKind kind: SidebarEntryKind, andLabel label: String) -> Bool {
    for (i, e) in filteredEntries.enumerated() {
      if e.kind == kind && e.label == label {
        tableView.selectRowIndexes([i], byExtendingSelection: false)
        return true
      }
    }
    return false
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
    updateCollapsed()
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
    case .broker, .topic, .consumerGroup, .schema:
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
      case .schema:
        image = NSImage(systemSymbolName: "doc.plaintext.fill", accessibilityDescription: "Schema")
      default:
        image = nil
      }
      view.imageView?.image = image
      return view
    case .group:
      guard let view = tableView.makeView(withIdentifier: .group, owner: nil) as? SidebarGroupCellView else {
        return nil
      }

      view.collapsed = entry.collapsed
      view.delegate = self
      view.tag = entry.tag
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
    if tableView.selectedRow == -1 {
      delegate?.sidebar(didDeselectEntry: selectedEntry)
      selectedEntry = nil
      return
    }
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
enum SidebarGroup: Int, Codable, Hashable {
  case brokers
  case topics
  case consumerGroups
  case schemas
}

enum SidebarEntryKind {
  case group
  case broker
  case topic
  case consumerGroup
  case schema
}

class SidebarEntry: NSObject {
  let kind: SidebarEntryKind
  let label: String
  var count: String?
  var data: Any?
  var tag = -1
  var collapsed = false

  init(
    withKind kind: SidebarEntryKind,
    label: String,
    count: String? = nil,
    andData data: Any? = nil,
    andTag tag: Int = -1,
    andCollapsed collapsed: Bool = false
  ) {
    self.kind = kind
    self.label = label
    self.count = count
    self.data = data
    self.tag = tag
    self.collapsed = collapsed
  }
}

// MARK: - SidebarGroupCellViewDelegate
extension WorkspaceSidebarViewController: SidebarGroupCellViewDelegate {
  func sidebarGroupCellView(withTag tag: Int, collapsed: Bool) {
    guard let group = SidebarGroup(rawValue: tag) else { return }
    var state = getState()
    state.groupCollapsedStates[group] = collapsed
    setState(state)
    guard let entry = entries.first(where: { $0.tag == tag }) else { return }
    entry.collapsed = collapsed
    updateCollapsed(animatingItems: true)
  }
}

// MARK: - WorkspaceSidebarState
struct WorkspaceSidebarState: Codable {
  var groupCollapsedStates = [SidebarGroup: Bool]()
}

// MARK: - format
fileprivate func format(stat amount: Int64) -> String {
  if amount >= 1_000_000_000 {
    return "\(amount / 1_000_000_000)B"
  } else if amount >= 1_000_000 {
    return "\(amount / 1_000_000)M"
  } else if amount >= 1_000 {
    return "\(amount / 1_000)K"
  }
  return "\(amount)"
}
