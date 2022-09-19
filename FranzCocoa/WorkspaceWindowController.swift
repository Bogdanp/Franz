import Cocoa
import NoiseBackend
import NoiseSerde

class WorkspaceWindowController: NSWindowController {
  private var conn: ConnectionDetails!
  private var id: UVarint!

  @IBOutlet weak var toolbar: NSToolbar!
  @IBOutlet weak var statusBarView: NSView!
  @IBOutlet weak var statusBarNameField: NSTextField!
  @IBOutlet weak var statusBarStatusField: NSTextField!

  private let splitCtl = NSSplitViewController()
  private let sidebarCtl = WorkspaceSidebarViewController()

  convenience init(withConn conn: ConnectionDetails) {
    self.init(windowNibName: "WorkspaceWindowController")
    self.conn = conn
  }

  override func windowDidLoad() {
    super.windowDidLoad()

    statusBarNameField.stringValue = conn.name

    status("Connecting...")
    Backend.shared.openWorkspace(withConn: conn).onComplete { id in
      self.id = id
      self.status("Getting metadata...")
      Backend.shared.getMetadata(id).onComplete { meta in
        self.sidebarCtl.configure(withMetadata: meta)
        self.status("Ready")
      }
    }

    self.splitCtl.addSplitViewItem(NSSplitViewItem(sidebarWithViewController: sidebarCtl))
    self.splitCtl.addSplitViewItem(NSSplitViewItem(viewController: WorkspaceDetailViewController()))

    window?.contentViewController = splitCtl
    window?.title = "\(conn.name) : \(conn.detailsString())"
    window?.setFrame(NSRect(x: 0, y: 0, width: 800, height: 600), display: true)
    window?.center()
  }

  private func status(_ s: String) {
    statusBarStatusField.stringValue = s
  }
}

// MARK: -NSToolbarDelegate
extension WorkspaceWindowController: NSToolbarDelegate {
  func toolbar(
    _ toolbar: NSToolbar,
    itemForItemIdentifier itemIdentifier: NSToolbarItem.Identifier,
    willBeInsertedIntoToolbar flag: Bool
  ) -> NSToolbarItem? {
      switch itemIdentifier {
      case .toggleSidebar:
        let item = NSToolbarItem(itemIdentifier: itemIdentifier)
        item.image = NSImage(systemSymbolName: "sidebar.left", accessibilityDescription: "Toggle Sidebar")?
          .withSymbolConfiguration(.init(pointSize: 18, weight: .light))
        item.label = "Toggle Sidebar"
        item.action = #selector(didPressToggleSidebarButton(_:))
        return item
      case .statusBar:
        let item = NSToolbarItem(itemIdentifier: itemIdentifier)
        item.view = statusBarView
        return item
      default:
        return nil
      }
  }

  func toolbarDefaultItemIdentifiers(_ toolbar: NSToolbar) -> [NSToolbarItem.Identifier] {
    return [.toggleSidebar, .flexibleSpace, .statusBar, .flexibleSpace]
  }

  func toolbarAllowedItemIdentifiers(_ toolbar: NSToolbar) -> [NSToolbarItem.Identifier] {
    return [.toggleSidebar, .statusBar, .flexibleSpace]
  }

  @objc func didPressToggleSidebarButton(_ sender: Any) {
    splitCtl.toggleSidebar(sender)
  }
}

// MARK: -NSToolbarItem.Identifier
extension NSToolbarItem.Identifier {
  static let toggleSidebar = NSToolbarItem.Identifier("toggleSidebar")
  static let statusBar = NSToolbarItem.Identifier("statusBar")
}
