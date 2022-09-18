import Cocoa
import NoiseSerde

class WorkspaceWindowController: NSWindowController {
  private var conn: ConnectionDetails!
  private var clientID: UVarint!

  convenience init() {
    self.init(windowNibName: "WorkspaceWindowController")
  }

  override func windowDidLoad() {
    super.windowDidLoad()
  }

  func configure(withConn conn: ConnectionDetails) {
    self.conn = conn
    self.clientID = Backend.shared.openWorkspace(withConn: conn).wait()
  }
}
