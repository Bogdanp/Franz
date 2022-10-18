import Cocoa
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
  var memberId: String = ""
  var clientId: String = ""
  var clientHost: String = ""
  var children: [GroupOffsetsItem]? = nil
}

class GroupOffsetsOutlineViewController: NSViewController {
  private var offsets: GroupOffsets!
  private var items: [GroupOffsetsItem]!

  @IBOutlet weak var outlineView: NSOutlineView!

  override func viewDidLoad() {
    super.viewDidLoad()

    outlineView?.delegate = self
    outlineView?.dataSource = self
  }

  func configure(withOffsets offsets: GroupOffsets) {
    self.offsets = offsets

    var items = [GroupOffsetsItem]()
    for t in offsets.topics {
      var item = GroupOffsetsItem(kind: .topic, label: t.name, children: [])
      for p in t.partitions {
        item.children?.append(GroupOffsetsItem(
          kind: .partition,
          label: "Partition \(p.partitionId)",
          offset: String(p.offset),
          memberId: p.memberId ?? "",
          clientId: p.clientId ?? "",
          clientHost: p.clientHost ?? ""
        ))
      }
      items.append(item)
    }
    self.items = items
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
      }
    case .partition:
      if id == .GroupOffsetsTopic {
        text = item.label
      } else if id == .GroupOffsetsOffset {
        text = item.offset
        textField.font = .monospacedDigitSystemFont(ofSize: 12, weight: .regular)
      } else if id == .GroupOffsetsLag {
        text = "1024"
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
