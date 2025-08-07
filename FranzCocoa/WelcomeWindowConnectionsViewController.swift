import Cocoa

class WelcomeWindowConnectionsViewController: NSViewController {
  @IBOutlet weak var recentConnectionsLabel: NSTextField!
  @IBOutlet weak var connectionsTable: ConnectionsTableView!

  private var connections = [ConnectionDetails]()

  override func viewDidLoad() {
    super.viewDidLoad()

    connectionsTable.register(.init(nibNamed: "ConnectionTableCellView", bundle: nil), forIdentifier: .connectionColumn)
    connectionsTable.dataSource = self
    connectionsTable.delegate = self
    connectionsTable.doubleAction = #selector(didRequestSelectConnection(_:))
    connectionsTable.deleteAction = #selector(didRequestDeleteConnection(_:))
    connectionsTable.target = self

    let menu = NSMenu()
    menu.addItem(NSMenuItem(
      title: "Edit...",
      action: #selector(didPressEditConnection(_:)),
      keyEquivalent: ""
    ))
    menu.addItem(.separator())
    menu.addItem(NSMenuItem(
      title: "Delete",
      action: #selector(didRequestDeleteConnection(_:)),
      keyEquivalent: .backspaceKeyEquivalent
    ))
    connectionsTable.menu = menu
  }

  override func viewDidAppear() {
    super.viewDidAppear()
    reload()
  }

  private func reload() {
    guard let conns = Error.wait(Backend.shared.getConnections()) else { return }
    connections = conns
    recentConnectionsLabel.isHidden = !connections.isEmpty
    connectionsTable.reloadData()
  }

  @objc func didRequestSelectConnection(_ sender: NSTableView) {
    assert(Thread.isMainThread)
    guard sender.selectedRow >= 0 else { return }
    guard sender.selectedRow < connections.count else { return }
    let conn = connections[sender.selectedRow]
    Error.wait(Backend.shared.touchConnection(conn))
    guard let passwordId = conn.passwordId else {
      WindowManager.shared.launchWorkspace(withConn: conn, andPassword: nil)
      WindowManager.shared.closeWelcomeWindow()
      return
    }
    switch Keychain.shared.get(passwordWithId: passwordId) {
    case .aborted:
      ()
    case .error(let code):
      let alert = NSAlert()
      alert.messageText = "Error"
      alert.informativeText = "Failed to retrieve password from Keychain (code: \(code))."
      alert.runModal()
    case .success(let password):
      WindowManager.shared.launchWorkspace(withConn: conn, andPassword: password)
      WindowManager.shared.closeWelcomeWindow()
    }
  }

  @objc func didPressEditConnection(_ sender: NSMenuItem) {
    assert(Thread.isMainThread)
    let conn = connections[connectionsTable.clickedRow]
    let formController = ConnectionDetailsFormViewController()
    formController.configure(actionLabel: "Save", details: conn, { changedConn in
      Error.wait(Backend.shared.updateConnection(changedConn))
      if let password = changedConn.password, let id = changedConn.passwordId {
        Keychain.shared.upsert(password: password, withId: id)
      }
      self.reload()
    })
    presentAsSheet(formController)
  }

  @objc func didRequestDeleteConnection(_ sender: NSMenuItem) {
    assert(Thread.isMainThread)
    let conn = connections[
      connectionsTable.clickedRow < 0
      ? connectionsTable.selectedRow
      : connectionsTable.clickedRow]
    let alert = NSAlert()
    alert.alertStyle = .warning
    alert.messageText = "Delete \(conn.name)?"
    alert.informativeText = "This action cannot be undone."
    alert.addButton(withTitle: "Delete")
    alert.addButton(withTitle: "Cancel")
    switch alert.runModal() {
    case .alertFirstButtonReturn:
      Error.wait(Backend.shared.deleteConnectionAndSystemResources(conn))
      reload()
    default:
      ()
    }
  }
}

// MARK: - ConnectionsTableView
class ConnectionsTableView: NSTableView {
  var deleteAction: Selector?

  override func keyDown(with event: NSEvent) {
    if event.characters?.count == 1 {
      switch event.keyCode {
      case 36: // RET
        _ = target?.perform(doubleAction, with: self)
        return
      case 51: // BKSPC
        if event.modifierFlags.contains(.command) {
          _ = target?.perform(deleteAction, with: self)
          return
        }
      default:
        ()
      }
    }
    super.keyDown(with: event)
  }
}

// MARK: - NSTableViewDataSource
extension WelcomeWindowConnectionsViewController: NSTableViewDataSource {
  func numberOfRows(in tableView: NSTableView) -> Int {
    return connections.count
  }

  func tableView(_ tableView: NSTableView, objectValueFor tableColumn: NSTableColumn?, row: Int) -> Any? {
    return connections[row]
  }
}

// MARK: - NSTableViewDelegate
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
    view.detailsView?.stringValue = conn.bootstrapAddress
    return view
  }
}

// MARK: - NSUserInterfaceItemIdentifier
fileprivate extension NSUserInterfaceItemIdentifier {
  static let connectionColumn = NSUserInterfaceItemIdentifier("Connection")
}
