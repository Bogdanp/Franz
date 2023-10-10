import Cocoa
import Foundation
import SwiftUI

class ResourceConfigTableViewController: NSViewController {
  private var entries = [ResourceConfigEntry]()

  @IBOutlet weak var tableView: NSTableView!
  private var contextMenu: NSMenu!

  override func viewDidLoad() {
    super.viewDidLoad()

    contextMenu = NSMenu()
    contextMenu.delegate = self

    tableView?.menu = contextMenu
    tableView?.delegate = self
    tableView?.dataSource = self
    tableView?.reloadData()
  }

  func configure(withData data: [ResourceConfig]) {
    var known = [String: ResourceConfigEntry]()
    for e in entries {
      known[e.name] = e
    }

    entries.removeAll(keepingCapacity: true)
    for item in data {
      if let e = known[item.name] {
        e.value = item.nonnullValue
        entries.append(e)
      } else {
        let e = ResourceConfigEntry(
          name: item.name,
          value: item.nonnullValue,
          isDefault: item.isDefault,
          isSensitive: item.isSensitive,
          docUrl: item.docUrl)
        entries.append(e)
      }
    }

    tableView?.reloadData()
  }
}

// MARK: - ResourceConfigEntry
class ResourceConfigEntry: NSObject {
  var name: String
  var value: String
  var isDefault = true
  var isSensitive = false
  var isRevealed = false
  var docUrl: String?

  var safeValue: String {
    !isSensitive || isRevealed ? value : "••••••••"
  }

  init(name: String,
       value: String,
       isDefault: Bool = false,
       isSensitive: Bool = false,
       isRevealed: Bool = false,
       docUrl: String? = nil) {
    self.name = name
    self.value = value
    self.isDefault = isDefault
    self.isSensitive = isSensitive
    self.isRevealed = isRevealed
    self.docUrl = docUrl
  }

  override func isEqual(to object: Any?) -> Bool {
    guard let other = object as? ResourceConfigEntry else { return false }
    return other.name == name
  }
}

// MARK: - ResourceConfigTable
struct ResourceConfigTable: NSViewControllerRepresentable {
  typealias NSViewControllerType = ResourceConfigTableViewController

  @Binding var configs: [ResourceConfig]

  func makeNSViewController(context: Context) -> ResourceConfigTableViewController {
    let ctl = ResourceConfigTableViewController()
    ctl.configure(withData: configs)
    return ctl
  }

  func updateNSViewController(_ nsViewController: ResourceConfigTableViewController, context: Context) {
    nsViewController.configure(withData: configs)
  }
}

// MARK: - NSMenuDelegate
extension ResourceConfigTableViewController: NSMenuDelegate {
  func menuNeedsUpdate(_ menu: NSMenu) {
    guard tableView.clickedRow >= 0 else { return }
    menu.removeAllItems()
    menu.addItem(.init(
      title: "Copy Key",
      action: #selector(copyKey(_:)),
      keyEquivalent: ""
    ))
    menu.addItem(.init(
      title: "Copy Value",
      action: #selector(copy(_:)),
      keyEquivalent: "c"
    ))
    let entry = entries[tableView.clickedRow]
    if entry.docUrl != nil {
      menu.addItem(.init(
        title: "Open Docs...",
        action: #selector(openDocs(_:)),
        keyEquivalent: ""
      ))
    }
    guard entry.isSensitive else { return }
    menu.addItem(.separator())
    menu.addItem(.init(
      title: entry.isRevealed ? "Hide" : "Reveal",
      action: #selector(didPressRevealEntryItem(_:)),
      keyEquivalent: ""))
  }

  @objc func didPressRevealEntryItem(_ sender: Any) {
    guard tableView.clickedRow >= 0 else { return }
    let row = tableView.clickedRow
    let entry = entries[row]
    guard entry.isSensitive else { return }
    entry.isRevealed.toggle()
    tableView.reloadData(forRowIndexes: [row], columnIndexes: [0, 1])
  }

  @objc func copyKey(_ sender: Any) {
    guard let entry = currentEntry else { return }
    Pasteboard.put(entry.name)
  }

  @objc func copy(_ sender: Any) {
    guard let entry = currentEntry else { return }
    Pasteboard.put(entry.value)
  }

  @objc func openDocs(_ sender: Any) {
    guard let entry = currentEntry,
          let docUrl = entry.docUrl,
          let url = URL(string: docUrl) else { return }
    NSWorkspace.shared.open(url)
  }

  private var currentEntry: ResourceConfigEntry? {
    let row = (tableView.clickedRow >= 0
               ? tableView.clickedRow
               : tableView.selectedRow)
    guard row >= 0 else { return nil }
    return entries[row]
  }
}

// MARK: - NSMenuItemValidation
extension ResourceConfigTableViewController: NSMenuItemValidation {
  func validateMenuItem(_ menuItem: NSMenuItem) -> Bool {
    if menuItem.action == #selector(copy(_:)) {
      return tableView.clickedRow >= 0 || tableView.selectedRow >= 0
    }
    return true
  }
}

// MARK: - NSTableViewDataSource
extension ResourceConfigTableViewController: NSTableViewDataSource {
  func numberOfRows(in tableView: NSTableView) -> Int {
    return entries.count
  }

  func tableView(_ tableView: NSTableView, objectValueFor tableColumn: NSTableColumn?, row: Int) -> Any? {
    return entries[row]
  }
}

// MARK: - NSTableVideDelegate
extension ResourceConfigTableViewController: NSTableViewDelegate {
  func tableView(_ tableView: NSTableView, viewFor tableColumn: NSTableColumn?, row: Int) -> NSView? {
    guard let identifier = tableColumn?.identifier else { return nil }
    guard let view = tableView.makeView(
      withIdentifier: identifier,
      owner: self
    ) as? NSTableCellView else { return nil }
    let entry = entries[row]
    var font = NSFont.systemFont(ofSize: 12)
    if identifier == .ResourceConfigConfig {
      view.textField?.stringValue = entry.name
    } else if identifier == .ResourceConfigValue {
      view.textField?.stringValue = entry.safeValue
      if (entry.isSensitive && !entry.isRevealed) || !entry.isDefault {
        font = .systemFont(ofSize: 12, weight: .semibold)
      }
    }
    view.textField?.font = font
    return view
  }
}
