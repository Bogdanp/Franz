import Cocoa

class WelcomeWindowConnectionsViewController: NSViewController {

  @IBOutlet weak var recentConnectionsLabel: NSTextField!
  @IBOutlet weak var connectionsTableView: NSTableView!

  private var connections = [ConnectionDetails]()

  override func viewDidLoad() {
    super.viewDidLoad()

    connectionsTableView.register(.init(nibNamed: "ConnectionTableCellView", bundle: nil), forIdentifier: .connectionColumn)
    connectionsTableView.dataSource = self
    connectionsTableView.delegate = self
    connectionsTableView.doubleAction = #selector(didDoubleClickConnection(_:))
  }

  override func viewDidAppear() {
    super.viewDidAppear()
    connections = Backend.shared.getConnections().wait()
    recentConnectionsLabel.isHidden = !connections.isEmpty
    connectionsTableView.reloadData()
  }

  @objc func didDoubleClickConnection(_ sender: NSTableView) {
    let conn = connections[sender.selectedRow]
    let _ = Backend.shared.touchConnection(conn)
    WindowManager.shared.launchWorkspace(withConn: conn)
    WindowManager.shared.closeWelcomeWindow()
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
