import Cocoa
import NoiseSerde
import SwiftUI

class GroupOffsetsOutlineViewController: NSViewController {
  private var id: UVarint!
  private var offsets: GroupOffsets!
  private var items = [GroupOffsetsItem]()
  private var itemsSeq = [GroupOffsetsItem]()
  private var reloadAction: (() -> Void)!

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

  func configure(withId id: UVarint,
                 andOffsets offsets: GroupOffsets,
                 andReloadAction reloadAction: @escaping () -> Void) {
    self.id = id
    self.offsets = offsets
    self.reloadAction = reloadAction

    var itemsById = [String: GroupOffsetsItem]()
    for item in itemsSeq {
      itemsById[item.id] = item
    }

    var selection: GroupOffsetsItem?
    if let row = outlineView?.selectedRow, row >= 0 {
      selection = itemsSeq[row]
    }

    items.removeAll(keepingCapacity: true)
    itemsSeq.removeAll(keepingCapacity: true)
    for t in offsets.topics {
      var item: GroupOffsetsItem!
      if let it = itemsById[GroupOffsetsItem.id(forTopic: t.name)] {
        item = it
        item.children?.removeAll(keepingCapacity: true)
      } else {
        item = GroupOffsetsItem(
          topic: t.name,
          children: [])
      }
      var lag = Varint(0)
      for p in t.partitions {
        let partitionId = Int(p.partitionId)
        var partitionItem: GroupOffsetsItem!
        if let it = itemsById[GroupOffsetsItem.id(forPartition: partitionId, onTopic: t.name)] {
          partitionItem = it
        } else {
          partitionItem = GroupOffsetsItem(
            topic: t.name,
            partitionId: partitionId)
        }
        partitionItem.offset = p.offset
        partitionItem.lag = p.lag
        partitionItem.memberId = p.memberId
        partitionItem.clientId = p.clientId
        partitionItem.clientHost = p.clientHost
        item.children?.append(partitionItem)
        lag += p.lag
      }
      item.lag = lag
      items.append(item)
      itemsSeq.append(item)
      itemsSeq.append(contentsOf: item.children!)
    }

    outlineView?.reloadData()
    outlineView?.expandItem(nil, expandChildren: true)

    if let selection {
      for (i, item) in itemsSeq.enumerated() where item == selection {
        outlineView.selectRowIndexes([i], byExtendingSelection: false)
        break
      }
    }
  }
}

// MARK: - GroupOffsetsItem
fileprivate class GroupOffsetsItem: NSObject {
  var topic: String
  var partitionId: Int?

  var id: String {
    if let partitionId {
      return Self.id(forPartition: partitionId, onTopic: topic)
    }
    return Self.id(forTopic: topic)
  }

  var kind: GroupOffsetsItemKind {
    if partitionId != nil {
      return .partition
    }
    return .topic
  }

  var label: String {
    if let partitionId {
      return "Partition \(partitionId)"
    }
    return topic
  }

  var offset: Int64?
  var lag: Int64?
  var memberId: String?
  var clientId: String?
  var clientHost: String?
  var children: [GroupOffsetsItem]?

  init(topic: String,
       partitionId: Int? = nil,
       children: [GroupOffsetsItem]? = nil) {
    self.topic = topic
    self.partitionId = partitionId
    self.children = children
  }

  override func isEqual(to object: Any?) -> Bool {
    guard let other = object as? GroupOffsetsItem else { return false }
    return id == other.id
  }

  static func id(forTopic name: String) -> String {
    return "topic:\(name)"
  }

  static func id(forPartition pid: Int, onTopic topic: String) -> String {
    return "\(id(forTopic: topic)), pid:\(pid)"
  }
}

fileprivate enum GroupOffsetsItemKind {
  case topic
  case partition
}

// MARK: - GroupOffsetsTable
struct GroupOffsetsTable: NSViewControllerRepresentable {
  typealias NSViewController = GroupOffsetsOutlineViewController

  var id: UVarint
  @Binding var offsets: GroupOffsets?
  var reloadAction: () -> Void = {}

  func makeNSViewController(context: Context) -> some NSViewController {
    let ctl = GroupOffsetsOutlineViewController()
    if let offsets {
      ctl.configure(withId: id, andOffsets: offsets, andReloadAction: reloadAction)
    }
    return ctl
  }

  func updateNSViewController(_ nsViewController: NSViewControllerType, context: Context) {
    guard let offsets else { return }
    nsViewController.configure(withId: id, andOffsets: offsets, andReloadAction: reloadAction)
  }
}

// MARK: - NSOutlineViewDataSource
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

// MARK: - NSOutlineViewDelegate
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
        if let lag = item.lag {
          text = String(lag)
          textField.font = .monospacedDigitSystemFont(ofSize: 12, weight: .regular)
        }
      }
    case .partition:
      if id == .GroupOffsetsTopic {
        text = item.label
      } else if id == .GroupOffsetsOffset {
        if let offset = item.offset {
          text = String(offset)
          textField.font = .monospacedDigitSystemFont(ofSize: 12, weight: .regular)
        }
      } else if id == .GroupOffsetsLag {
        if let lag = item.lag {
          text = String(lag)
          textField.font = .monospacedDigitSystemFont(ofSize: 12, weight: .regular)
        }
      } else if id == .GroupOffsetsConsumerId {
        text = item.memberId ?? ""
      } else if id == .GroupOffsetsHost {
        text = item.clientHost ?? ""
      } else if id == .GroupOffsetsClientId {
        text = item.clientId ?? ""
      }
    }
    textField.stringValue = text
    return view
  }
}

// MARK: - NSMenuDelegate
extension GroupOffsetsOutlineViewController: NSMenuDelegate {
  func menuNeedsUpdate(_ menu: NSMenu) {
    menu.removeAllItems()
    guard let row = outlineView?.clickedRow, row >= 0 else { return }
    let item = itemsSeq[row]
    switch item.kind {
    case .topic:
      menu.addItem(.init(
        title: "Copy Name",
        action: #selector(didPressCopyTopicNameItem(_:)),
        keyEquivalent: "c"))
      menu.addItem(.separator())
      menu.addItem(.init(
        title: "Reset Offsets...",
        action: #selector(didPressResetTopicOffsetsItem(_:)),
        keyEquivalent: .backspaceKeyEquivalent))
    case .partition:
      menu.addItem(.init(
        title: "Copy Offset",
        action: #selector(didPressCopyPartitionOffsetItem(_:)),
        keyEquivalent: "c"))
      menu.addItem(.init(
        title: "Copy Member ID",
        action: #selector(didPressCopyPartitionMemberIDItem(_:)),
        keyEquivalent: "C"))
      menu.addItem(.init(
        title: "Copy Client ID",
        action: #selector(didPressCopyPartitionClientIDItem(_:)),
        keyEquivalent: ""))
      menu.addItem(.separator())
      menu.addItem(.init(
        title: "Reset Offset...",
        action: #selector(didPressResetPartitionOffsetItem(_:)),
        keyEquivalent: .backspaceKeyEquivalent))
    }
  }

  @objc func didPressCopyTopicNameItem(_ sender: Any) {
    guard let row = outlineView?.clickedRow, row >= 0 else { return }
    Pasteboard.put(itemsSeq[row].label)
  }

  @objc func didPressResetTopicOffsetsItem(_ sender: Any) {
    guard let row = outlineView?.clickedRow, row >= 0 else { return }
    let item = itemsSeq[row]
    guard item.kind == .topic else { return }
    guard let id else { return }
    guard let groupId = self.offsets?.groupId else { return }

    let ctl = NSHostingController(rootView: ResetTopicOffsets(parent: self) { target in
      Backend.shared.resetTopicOffsets(
        forGroupNamed: groupId,
        andTopic: item.label,
        andTarget: target,
        inWorkspace: id
      ).onComplete { _ in
        self.reloadAction()
      }
    })
    presentAsSheet(ctl)
  }

  @objc func didPressResetPartitionOffsetItem(_ sender: Any) {
    guard let row = outlineView?.clickedRow, row >= 0 else { return }
    let item = itemsSeq[row]
    guard item.kind == .partition else { return }
    guard let id else { return }
    guard let groupId = self.offsets?.groupId else { return }
    guard let partitionId = item.partitionId else { return }

    let view = ResetPartitionOffset(
      offset: item.offset,
      parent: self
    ) { target, offset in
      Backend.shared.resetPartitionOffset(
        forGroupNamed: groupId,
        andTopic: item.topic,
        andPartitionId: UVarint(partitionId),
        andTarget: target,
        andOffset: offset.map { UVarint($0) },
        inWorkspace: id
      ).onComplete { _ in
        self.reloadAction()
      }
    }
    let ctl = NSHostingController(rootView: view)
    presentAsSheet(ctl)
  }

  @objc func didPressCopyPartitionOffsetItem(_ sender: Any) {
    guard let row = outlineView?.clickedRow, row >= 0 else { return }
    guard let offset = itemsSeq[row].offset else { return }
    Pasteboard.put(String(offset))
  }

  @objc func didPressCopyPartitionMemberIDItem(_ sender: Any) {
    guard let row = outlineView?.clickedRow, row >= 0 else { return }
    guard let memberId = itemsSeq[row].memberId else { return }
    Pasteboard.put(memberId)
  }

  @objc func didPressCopyPartitionClientIDItem(_ sender: Any) {
    guard let row = outlineView?.clickedRow, row >= 0 else { return }
    guard let clientId = itemsSeq[row].clientId else { return }
    Pasteboard.put(clientId)
  }
}

// MARK: - ResetTopicOffsets
fileprivate struct ResetTopicOffsets: View {
  @State var target = Symbol("earliest")

  var parent: NSViewController
  var resetAction: (Symbol) -> Void

  var body: some View {
    Form {
      Picker("Reset to:", selection: $target) {
        Text("Earliest").tag(Symbol("earliest"))
        Text("Latest").tag(Symbol("latest"))
      }
      HStack {
        Button("Cancel", role: .cancel) {
          dismiss()
        }
        .keyboardShortcut(.cancelAction)
        Button("Reset", role: .destructive) {
          dismiss()
          resetAction(target)
        }
        .keyboardShortcut(.defaultAction)
      }
    }
    .padding()
    .frame(minWidth: 240)
  }

  private func dismiss() {
    parent.presentedViewControllers?.forEach { ctl in
      if let ctl = ctl as? NSHostingController<Self> {
        ctl.dismiss(self)
      }
    }
  }
}

// MARK: - ResetPartitionOffset
fileprivate struct ResetPartitionOffset: View {
  @State var target = Symbol("offset")
  @State var offset: Int64?

  var parent: NSViewController
  var resetAction: (Symbol, Int64?) -> Void

  var body: some View {
    Form {
      Picker("Reset to:", selection: $target) {
        Text("Earliest").tag(Symbol("earliest"))
        Text("Offset").tag(Symbol("offset"))
        Text("Latest").tag(Symbol("latest"))
      }

      TextField("Offset:", value: $offset, format: .number.grouping(.never))
        .disabled(target != "offset")
        .onSubmit {
          submit()
        }

      HStack {
        Button("Cancel", role: .cancel) {
          dismiss()
        }
        .keyboardShortcut(.cancelAction)
        Button("Reset", role: .destructive) {
          submit()
        }
        .keyboardShortcut(.defaultAction)
        .disabled(target == "offset" && offset == nil)
      }
    }
    .padding()
    .frame(minWidth: 240)
  }

  private func submit() {
    guard let offset, offset >= 0 else { return }
    dismiss()
    resetAction(target, offset)
  }

  private func dismiss() {
    parent.presentedViewControllers?.forEach { ctl in
      if let ctl = ctl as? NSHostingController<Self> {
        ctl.dismiss(self)
      }
    }
  }
}
