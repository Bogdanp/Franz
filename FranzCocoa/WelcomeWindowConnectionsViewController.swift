import Cocoa

class WelcomeWindowConnectionsViewController: NSViewController {

  @IBOutlet weak var recentConnectionsLabel: NSTextField!
  @IBOutlet weak var connectionsTable: NSTableView!

  private var connections = [ConnectionDetails]()

  override func viewDidLoad() {
    super.viewDidLoad()

    connectionsTable.register(.init(nibNamed: "ConnectionTableCellView", bundle: nil), forIdentifier: .connectionColumn)
    connectionsTable.dataSource = self
    connectionsTable.delegate = self
    connectionsTable.doubleAction = #selector(didDoubleClickConnection(_:))

    let menu = NSMenu()
    menu.addItem(NSMenuItem(title: "Edit...", action: nil, keyEquivalent: ""))
    menu.addItem(.separator())
    menu.addItem(NSMenuItem(title: "Delete", action: #selector(didPressDeleteMenuItem(_:)), keyEquivalent: ""))
    connectionsTable.menu = menu
  }

  override func viewDidAppear() {
    super.viewDidAppear()
    reload()
  }

  private func reload() {
    connections = Backend.shared.getConnections().wait()
    recentConnectionsLabel.isHidden = !connections.isEmpty
    connectionsTable.reloadData()
  }

  @objc func didDoubleClickConnection(_ sender: NSTableView) {
    let conn = connections[sender.selectedRow]
    let _ = Backend.shared.touchConnection(conn)
    WindowManager.shared.launchWorkspace(withConn: conn)
    WindowManager.shared.closeWelcomeWindow()
  }

  @objc func didPressDeleteMenuItem(_ sender: NSMenuItem) {
    let conn = connections[connectionsTable.clickedRow]
    if Backend.shared.deleteConnection(conn).wait() {
      reload()
    }
  }
}

// MARK: -NSTableViewDataSource
extension WelcomeWindowConnectionsViewController: NSTableViewDataSource {
  func numberOfRows(in tableView: NSTableView) -> Int {
    return connections.count
  }

  func tableView(_ tableView: NSTableView, objectValueFor tableColumn: NSTableColumn?, row: Int) -> Any? {
    return connections[row]
  }
}

// MARK: -NSTableViewDelegate
extension WelcomeWindowConnectionsViewController: NSTableViewDelegate {
  func tableView(_ tableView: NSTableView, viewFor tableColumn: NSTableColumn?, row: Int) -> NSView? {
    let view = tableView.makeView(withIdentifier: .connectionColumn, owner: self)
    guard let view = view as? ConnectionTableCellView else {
      return nil
    }

    let conn = connections[row]
    view.imageView?.image = NSImage(systemSymbolName: "server.rack", accessibilityDescription: "Server")?
      .withSymbolConfiguration(.init(pointSize: 24, weight: .light))
    view.textField?.stringValue = conn.name
    view.detailsView?.stringValue = conn.detailsString()
    return view
  }
}

// MARK: -NSUserInterfaceItemIdentifier
fileprivate extension NSUserInterfaceItemIdentifier {
  static let connectionColumn = NSUserInterfaceItemIdentifier("Connection")
}
