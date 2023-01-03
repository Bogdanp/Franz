import Cocoa

class HeadersTableViewController: NSViewController {
  private var headers: [(String, Data)]?

  @IBOutlet var tableMenu: NSMenu!
  @IBOutlet weak var tableView: NSTableView!

  override func viewDidLoad() {
    super.viewDidLoad()

    tableMenu.delegate = self
    tableView.dataSource = self
    tableView.delegate = self
    tableView.menu = tableMenu
  }

  func configure(_ headers: [String: Data]) {
    self.headers = headers.map { ($0, $1) }.sorted { $0.0 < $1.0 }
    self.tableView?.reloadData()
  }

  private func sendToPasteboard(_ accessor: ((String, Data)) -> String) {
    guard let tableView, tableView.clickedRow >= 0,
          let header = headers?[tableView.clickedRow] else {
      return
    }
    Pasteboard.put(accessor(header))
  }

  @IBAction func didPushCopyKeyButton(_ sender: Any) {
    sendToPasteboard { $0.0 }
  }

  @IBAction func didPushCopyValueButton(_ sender: Any) {
    sendToPasteboard { String(decoding: $0.1, as: UTF8.self) }
  }
}

// MARK: - NSMenuDelegate
extension HeadersTableViewController: NSMenuDelegate {
  func menuNeedsUpdate(_ menu: NSMenu) {
    guard let tableView else { return }
    menu.autoenablesItems = false
    if tableView.clickedRow == -1 {
      for it in menu.items {
        it.isEnabled = false
      }
    } else {
      for it in menu.items {
        it.isEnabled = true
      }
    }
  }
}

// MARK: - NSTableViewDataSource
extension HeadersTableViewController: NSTableViewDataSource {
  func numberOfRows(in tableView: NSTableView) -> Int {
    return headers?.count ?? 0
  }
}

// MARK: - NSTableViewDelegate
extension HeadersTableViewController: NSTableViewDelegate {
  func tableView(_ tableView: NSTableView, viewFor tableColumn: NSTableColumn?, row: Int) -> NSView? {
    guard let ident = tableColumn?.identifier,
          let view = tableView.makeView(withIdentifier: ident, owner: nil),
          let view = view as? NSTableCellView,
          let header = headers?[row] else {
      return nil
    }

    switch ident {
    case NSUserInterfaceItemIdentifier("HeaderKey"):
      view.textField?.stringValue = header.0
    case NSUserInterfaceItemIdentifier("HeaderValue"):
      view.textField?.stringValue = String(decoding: header.1, as: UTF8.self)
    default:
      ()
    }

    return view
  }
}
