import Cocoa
import NoiseSerde
import SwiftUI

enum GroupOffsetsItemKind {
  case topic
  case partition
}

struct GroupOffsetsItem: Hashable, Identifiable {
  var id: Self { self }
  var kind: GroupOffsetsItemKind
  var label: String
  var offset: String = ""
  var lag: String = ""
  var memberId: String = ""
  var clientId: String = ""
  var clientHost: String = ""
  var children: [GroupOffsetsItem]? = nil
}

class GroupOffsetsOutlineViewController: NSViewController {
  private var offsets: GroupOffsets!
  private var items = [GroupOffsetsItem]()
  private var itemsSeq = [GroupOffsetsItem]()

  private var contextMenu = NSMenu()

  @IBOutlet weak var outlineView: NSOutlineView!

  override func viewDidLoad() {
    super.viewDidLoad()

    contextMenu.delegate = self

    outlineView?.menu = contextMenu
    outlineView?.delegate = self
    outlineView?.dataSource = self
    outlineView?.expandItem(nil, expandChildren: true)
  }

  func configure(withOffsets offsets: GroupOffsets) {
    self.offsets = offsets

    items.removeAll(keepingCapacity: true)
    itemsSeq.removeAll(keepingCapacity: true)
    for t in offsets.topics {
      var item = GroupOffsetsItem(kind: .topic, label: t.name, children: [])
      var lag = Varint(0)
      for p in t.partitions {
        lag += p.lag
        item.children?.append(GroupOffsetsItem(
          kind: .partition,
          label: "Partition \(p.partitionId)",
          offset: String(p.offset),
          lag: String(p.lag),
          memberId: p.memberId ?? "",
          clientId: p.clientId ?? "",
          clientHost: p.clientHost ?? ""
        ))
      }
      item.lag = String(lag)
      items.append(item)
      itemsSeq.append(item)
      itemsSeq.append(contentsOf: item.children!)
    }
  }
}

// MARK: -NSOutlineViewDelegate
extension GroupOffsetsOutlineViewController: NSOutlineViewDelegate {
  func outlineView(_ outlineView: NSOutlineView, viewFor tableColumn: NSTableColumn?, item: Any) -> NSView? {
    guard let id = tableColumn?.identifier else { return nil }
    let view = outlineView.makeView(withIdentifier: id, owner: self)
    guard let view = view as? NSTableCellView else { return nil }
    guard let item = item as? GroupOffsetsItem else { return nil }
    guard let textField = view.textField else { return nil }
    var text = ""
    textField.font = .systemFont(ofSize: 12, weight: .regular)
    switch item.kind {
    case .topic:
      if id == .GroupOffsetsTopic {
        text = item.label
      } else if id == .GroupOffsetsLag {
        text = item.lag
      }
    case .partition:
      if id == .GroupOffsetsTopic {
        text = item.label
      } else if id == .GroupOffsetsOffset {
        text = item.offset
        textField.font = .monospacedDigitSystemFont(ofSize: 12, weight: .regular)
      } else if id == .GroupOffsetsLag {
        text = item.lag
        textField.font = .monospacedDigitSystemFont(ofSize: 12, weight: .regular)
      } else if id == .GroupOffsetsConsumerId {
        text = item.memberId
      } else if id == .GroupOffsetsHost {
        text = item.clientHost
      } else if id == .GroupOffsetsClientId {
        text = item.clientId
      }
    }
    textField.stringValue = text
    return view
  }
}

// MARK: -NSMenuDelegate
extension GroupOffsetsOutlineViewController: NSMenuDelegate {
  func menuNeedsUpdate(_ menu: NSMenu) {
    menu.removeAllItems()
    guard let row = outlineView?.clickedRow, row >= 0 else { return }
    let item = itemsSeq[row]
    switch item.kind {
    case .topic:
      menu.addItem(.init(title: "Copy Name", action: #selector(didPressCopyTopicNameItem(_:)), keyEquivalent: "c"))
      menu.addItem(.separator())
      menu.addItem(.init(title: "Reset Offsets...", action: #selector(didPressResetTopicOffsetsItem(_:)), keyEquivalent: .backspaceKeyEquivalent))
    case .partition:
      menu.addItem(.init(title: "Copy Offset", action: #selector(didPressCopyPartitionOffsetItem(_:)), keyEquivalent: "c"))
      menu.addItem(.init(title: "Copy Member ID", action: #selector(didPressCopyPartitionMemberIDItem(_:)), keyEquivalent: "C"))
      menu.addItem(.init(title: "Copy Client ID", action: #selector(didPressCopyPartitionClientIDItem(_:)), keyEquivalent: ""))
    }
  }

  @objc func didPressCopyTopicNameItem(_ sender: Any) {
    guard let row = outlineView?.clickedRow, row >= 0 else { return }
    NSPasteboard.general.clearContents()
    NSPasteboard.general.setString(itemsSeq[row].label, forType: .string)
  }

  @objc func didPressResetTopicOffsetsItem(_ sender: Any) {
    guard let row = outlineView?.clickedRow, row >= 0 else { return }
    let item = itemsSeq[row]
    guard item.kind == .topic else { return }

  }

  @objc func didPressCopyPartitionOffsetItem(_ sender: Any) {
    guard let row = outlineView?.clickedRow, row >= 0 else { return }
    NSPasteboard.general.clearContents()
    NSPasteboard.general.setString(itemsSeq[row].offset, forType: .string)
  }

  @objc func didPressCopyPartitionMemberIDItem(_ sender: Any) {
    guard let row = outlineView?.clickedRow, row >= 0 else { return }
    NSPasteboard.general.clearContents()
    NSPasteboard.general.setString(itemsSeq[row].memberId, forType: .string)
  }

  @objc func didPressCopyPartitionClientIDItem(_ sender: Any) {
    guard let row = outlineView?.clickedRow, row >= 0 else { return }
    NSPasteboard.general.clearContents()
    NSPasteboard.general.setString(itemsSeq[row].clientId, forType: .string)
  }
}

// MARK: -NSOutlineViewDataSource
extension GroupOffsetsOutlineViewController: NSOutlineViewDataSource {
  func outlineView(_ outlineView: NSOutlineView, numberOfChildrenOfItem item: Any?) -> Int {
    guard let item = item as? GroupOffsetsItem else { return items.count }
    return item.children?.count ?? 0
  }

  func outlineView(_ outlineView: NSOutlineView, child index: Int, ofItem item: Any?) -> Any {
    guard let item = item as? GroupOffsetsItem else { return items[index] }
    return item.children![index]
  }

  func outlineView(_ outlineView: NSOutlineView, isItemExpandable item: Any) -> Bool {
    guard let item = item as? GroupOffsetsItem else { return false }
    return item.children != nil
  }
}

// MARK: -GroupOffsetsTable
struct GroupOffsetsTable: NSViewControllerRepresentable {
  typealias NSViewController = GroupOffsetsOutlineViewController

  let offsets: GroupOffsets

  func makeNSViewController(context: Context) -> some NSViewController {
    let ctl = GroupOffsetsOutlineViewController()
    ctl.configure(withOffsets: offsets)
    return ctl
  }

  func updateNSViewController(_ nsViewController: NSViewControllerType, context: Context) {
  }
}
