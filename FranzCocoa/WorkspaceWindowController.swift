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
  private let detailCtl = WorkspaceDetailViewController()

  convenience init(withConn conn: ConnectionDetails) {
    self.init(windowNibName: "WorkspaceWindowController")
    self.conn = conn
  }

  override func windowDidLoad() {
    super.windowDidLoad()

    sidebarCtl.delegate = self

    statusBarNameField.stringValue = conn.name
    connect()

    let sidebarItem = NSSplitViewItem(sidebarWithViewController: sidebarCtl)
    sidebarItem.minimumThickness = 200

    splitCtl.addSplitViewItem(sidebarItem)
    splitCtl.addSplitViewItem(NSSplitViewItem(viewController: detailCtl))

    shouldCascadeWindows = false
    window?.delegate = self
    window?.title = "\(conn.name) : \(conn.detailsString())"
    window?.contentViewController = splitCtl
    window?.setFrameAutosaveName("Franz:\(conn.name)")
    if let didSet = window?.setFrameUsingName(window!.frameAutosaveName), !didSet {
      window?.setFrame(NSRect(x: 0, y: 0, width: 960, height: 600), display: true)
      window?.center()
    }
  }

  private func connect() {
    status("Connecting...")
    Backend.shared.openWorkspace(withConn: conn).sink(
      onError: { err in
        self.status("Connection Failed")
        let alert = NSAlert()
        alert.messageText = "Connection Error"
        alert.informativeText = err
        alert.alertStyle = .critical
        alert.addButton(withTitle: "OK")
        alert.addButton(withTitle: "Retry")
        switch alert.runModal() {
        case .alertFirstButtonReturn:
          return
        case .alertSecondButtonReturn:
          self.connect()
        default:
          preconditionFailure()
        }
      }) { id in
        self.id = id
        self.loadMetadata(forcingReload: false)
      }
  }

  private func loadMetadata(forcingReload reload: Bool = true, andThen proc: @escaping () -> Void = {}) {
    self.status("Getting metadata...")
    Backend.shared.getMetadata(id, forcingReload: reload).onComplete { meta in
      self.sidebarCtl.configure(withId: self.id, andMetadata: meta)
      self.detailCtl.configure(withId: self.id)
      self.status("Ready")
      proc()
    }
  }

  private func status(_ s: String) {
    assert(Thread.isMainThread)
    statusBarStatusField.stringValue = s
  }
}

// MARK: NSWindowDelegate
extension WorkspaceWindowController: NSWindowDelegate {
  func windowWillClose(_ notification: Notification) {
    let _ = Backend.shared.closeWorkspace(withId: id)
    WindowManager.shared.removeWorkspace(withId: conn.id!)
  }
}

// MARK: NSToolbarDelegate
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
        item.minSize.width = 200
        item.maxSize.width = .infinity
        return item
      case .reloadButton:
        let item = NSToolbarItem(itemIdentifier: itemIdentifier)
        item.image = NSImage(systemSymbolName: "arrow.clockwise", accessibilityDescription: "Reload")?
          .withSymbolConfiguration(.init(pointSize: 18, weight: .light))
        item.label = "Reload Metadata"
        item.action = #selector(didPressReloadButton(_:))
        return item
      default:
        return nil
      }
  }

  func toolbarDefaultItemIdentifiers(_ toolbar: NSToolbar) -> [NSToolbarItem.Identifier] {
    return [.toggleSidebar, .statusBar, .reloadButton]
  }

  func toolbarAllowedItemIdentifiers(_ toolbar: NSToolbar) -> [NSToolbarItem.Identifier] {
    return [.toggleSidebar, .statusBar, .reloadButton, .flexibleSpace]
  }

  @objc func didPressToggleSidebarButton(_ sender: Any) {
    splitCtl.toggleSidebar(sender)
  }

  @objc func didPressReloadButton(_ sender: Any) {
    loadMetadata()
  }
}

// MARK: NSToolbarItem.Identifier
extension NSToolbarItem.Identifier {
  static let toggleSidebar = NSToolbarItem.Identifier("toggleSidebar")
  static let statusBar = NSToolbarItem.Identifier("statusBar")
  static let reloadButton = NSToolbarItem.Identifier("reloadButton")
}

// MARK: WorkspaceSidebarDelegate
extension WorkspaceWindowController: WorkspaceSidebarDelegate {
  func sidebar(didSelectEntry entry: Any, withKind kind: SidebarEntryKind) {
    detailCtl.show(entry: entry, withKind: kind)
  }

  func sidebar(didDeselectEntry entry: Any?) {
    detailCtl.clear()
  }

  func sidebar(didDeleteTopic topic: Topic) {
    status("Deleting topic \(topic.name)...")
    Backend.shared.deleteTopic(named: topic.name, forClient: id).onComplete { _ in
      self.loadMetadata()
    }
  }

  func sidebar(didDeleteConsumerGroup group: Group) {
    status("Deleting consumer group \(group.id)...")
    Backend.shared.deleteGroup(withId: group.id, forClient: id).onComplete { _ in
      self.loadMetadata()
    }
  }

  func sidebarRequestsReload(withNewTopic name: String) {
    loadMetadata {
      self.sidebarCtl.selectEntry(withKind: .topic, andLabel: name)
    }
  }
}
