import Cocoa
import NoiseSerde

class WorkspaceSidebarViewController: NSViewController {
  private var metadata = Metadata(brokers: [], topics: [], groups: [])
  private var entries = [SidebarEntry]()

  @IBOutlet weak var tableView: NSTableView!
  @IBOutlet weak var noTopicsField: NSTextField!

  var delegate: WorkspaceSidebarDelegate?

  override func viewDidLoad() {
    super.viewDidLoad()

    tableView.register(.init(nibNamed: "SidebarEntryCellView", bundle: nil), forIdentifier: .entry)
    tableView.register(.init(nibNamed: "SidebarGroupCellView", bundle: nil), forIdentifier: .group)
    tableView.delegate = self
    tableView.dataSource = self
    tableView.reloadData()
  }

  func configure(withMetadata metadata: Metadata) {
    self.metadata = metadata

    self.entries.removeAll(keepingCapacity: true)
    self.entries.append(SidebarEntry(withKind: .group, label: "Brokers"))
    for b in metadata.brokers {
      self.entries.append(SidebarEntry(withKind: .broker, label: "\(b.host):\(b.port)"))
    }

    self.entries.append(SidebarEntry(withKind: .group, label: "Topics"))
    for t in metadata.topics {
      self.entries.append(SidebarEntry(withKind: .topic, label: t.name, andCount: "\(t.partitions.count)"))
    }

    self.entries.append(SidebarEntry(withKind: .group, label: "Consumer Groups"))
    for g in metadata.groups {
      self.entries.append(SidebarEntry(withKind: .consumerGroup, label: g.id))
    }

    self.noTopicsField.isHidden = !(self.metadata.topics.isEmpty && self.metadata.brokers.isEmpty)
    self.tableView.reloadData()
  }
}

// MARK: -NSTableViewDelegate
extension WorkspaceSidebarViewController: NSTableViewDelegate {
  func tableView(_ tableView: NSTableView, viewFor tableColumn: NSTableColumn?, row: Int) -> NSView? {
    let entry = entries[row]
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
    return entries[row].kind == .group
  }

  func tableView(_ tableView: NSTableView, shouldSelectRow row: Int) -> Bool {
    return entries[row].kind != .group
  }

  func tableViewSelectionDidChange(_ notification: Notification) {
    delegate?.sidebar(didSelectEntry: entries[tableView.selectedRow])
  }
}

// MARK: -NSTableViewDataSource
extension WorkspaceSidebarViewController: NSTableViewDataSource {

  func numberOfRows(in tableView: NSTableView) -> Int {
    return entries.count
  }

  func tableView(_ tableView: NSTableView, objectValueFor tableColumn: NSTableColumn?, row: Int) -> Any? {
    return entries[row]
  }
}

// MARK: -NSUserInterfaceItemIdentifier
extension NSUserInterfaceItemIdentifier {
  static let entry = NSUserInterfaceItemIdentifier("Entry")
  static let group = NSUserInterfaceItemIdentifier("Group")
}

// MARK: -SidebarEntry
enum SidebarEntryKind {
  case group
  case broker
  case topic
  case consumerGroup
}

class SidebarEntry: NSObject {
  let kind: SidebarEntryKind
  let label: String
  let count: String?

  init(withKind kind: SidebarEntryKind, label: String, andCount count: String? = nil) {
    self.kind = kind
    self.label = label
    self.count = count
  }
}

// MARK: -WorkspaceSidebarDelegate
protocol WorkspaceSidebarDelegate {
  func sidebar(didSelectEntry entry: SidebarEntry)
}
