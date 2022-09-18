import Cocoa
import NoiseSerde

class WorkspaceWindowController: NSWindowController {
  private var conn: ConnectionDetails!
  private var id: UVarint!

  private let splitCtl = NSSplitViewController()

  convenience init(withConn conn: ConnectionDetails) {
    self.init(windowNibName: "WorkspaceWindowController")
    self.conn = conn
    self.id = Backend.shared.openWorkspace(withConn: conn).wait()
  }

  override func windowDidLoad() {
    super.windowDidLoad()

    splitCtl.addSplitViewItem(NSSplitViewItem(sidebarWithViewController: WorkspaceSidebarViewController(withClientID: id)))
    splitCtl.addSplitViewItem(NSSplitViewItem(viewController: WorkspaceDetailViewController()))
    window?.contentViewController = splitCtl
    window?.title = "\(conn.name) : \(conn.detailsString())"
    window?.center()
  }

  @IBAction func didPressToggleSidebarButton(_ sender: Any) {
    splitCtl.toggleSidebar(sender)
  }
}
