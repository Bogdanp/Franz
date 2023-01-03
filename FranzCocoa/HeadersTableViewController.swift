import Cocoa

class HeadersTableViewController: NSViewController {
  private var headers: [(String, Data)]?

  @IBOutlet weak var tableView: NSTableView!

  override func viewDidLoad() {
    super.viewDidLoad()

    tableView.dataSource = self
    tableView.delegate = self
  }

  func configure(_ headers: [String: Data]) {
    self.headers = headers.map { ($0, $1) }.sorted { $0.0 < $1.0 }
    self.tableView?.reloadData()
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
