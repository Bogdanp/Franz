import Cocoa
import NoiseBackend
import NoiseSerde

class WorkspaceWindowController: NSWindowController {
  private var conn: ConnectionDetails!
  private var pass: String?
  private var id: UVarint!

  @IBOutlet weak var toolbar: NSToolbar!
  @IBOutlet weak var statusBarView: NSView!
  @IBOutlet weak var statusBarNameField: NSTextField!
  @IBOutlet weak var statusBarStatusField: NSTextField!

  private let splitCtl = NSSplitViewController()
  private let sidebarCtl = WorkspaceSidebarViewController()
  private let detailCtl = WorkspaceDetailViewController()

  private weak var newTopicMenuItem: NSMenuItem?

  convenience init(withConn conn: ConnectionDetails, andPassword password: String?) {
    self.init(windowNibName: "WorkspaceWindowController")
    self.conn = conn
    self.pass = password
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

    newTopicMenuItem = MainMenu.shared.find(itemByPath: [.FileMenuItem, .NewTopicMenuItem])

    shouldCascadeWindows = false
    window?.delegate = self
    window?.title = "\(conn.name) : \(conn.bootstrapAddress)"
    window?.contentViewController = splitCtl
    window?.setFrameAutosaveName("Franz:\(conn.name)")
    if let didSet = window?.setFrameUsingName(window!.frameAutosaveName), !didSet {
      window?.setFrame(NSRect(x: 0, y: 0, width: 960, height: 600), display: true)
      window?.center()
    }
  }

  private func connect() {
    status("Connecting...")
    Backend.shared.openWorkspace(withConn: conn, andPassword: pass).sink(
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
    Backend.shared.getMetadata(forcingReload: reload, inWorkspace: id).onComplete { meta in
      self.sidebarCtl.configure(withId: self.id, andMetadata: meta)
      self.detailCtl.configure(withId: self.id)
      self.status("Ready")
      proc()
    }
  }

  private func loadMetadata(andSelectTopic name: String) {
    loadMetadata {
      self.sidebarCtl.selectEntry(withKind: .topic, andLabel: name)
    }
  }

  private func status(_ s: String) {
    assert(Thread.isMainThread)
    statusBarStatusField.stringValue = s
  }

  @objc func didPressNewTopicItem(_ sender: Any) {
    let ctl = NewTopicFormViewController()
    ctl.delegate = self
    ctl.configure(withId: id)
    window?.contentViewController?.presentAsSheet(ctl)
  }
}

// MARK: NSWindowDelegate
extension WorkspaceWindowController: NSWindowDelegate {
  func windowDidBecomeKey(_ notification: Notification) {
    guard let item = newTopicMenuItem else {
      return
    }
    item.target = self
    item.action = #selector(didPressNewTopicItem(_:))
  }

  func windowDidResignKey(_ notification: Notification) {
    guard let item = newTopicMenuItem else {
      return
    }
    item.target = nil
    item.action = nil
  }

  func windowWillClose(_ notification: Notification) {
    guard let id else { return }
    if WindowManager.shared.removeWorkspace(withId: conn.id!) {
      let _ = Backend.shared.closeWorkspace(id)
    }
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

// MARK: -NSToolbarItem.Identifier
extension NSToolbarItem.Identifier {
  static let toggleSidebar = NSToolbarItem.Identifier("toggleSidebar")
  static let statusBar = NSToolbarItem.Identifier("statusBar")
  static let reloadButton = NSToolbarItem.Identifier("reloadButton")
}

// MARK: -NewTopicFormDelegate
extension WorkspaceWindowController: NewTopicFormDelegate {
  func didCancelNewTopicForm(_ sender: NewTopicFormViewController) {
  }

  func didCompleteNewTopicForm(withName name: String, partitions: Int, andOptions options: [TopicOption]) {
    loadMetadata(andSelectTopic: name)
  }
}

// MARK: -WorkspaceSidebarDelegate
extension WorkspaceWindowController: WorkspaceSidebarDelegate {
  func sidebar(didSelectEntry entry: Any, withKind kind: SidebarEntryKind) {
    detailCtl.show(entry: entry, withKind: kind)
  }

  func sidebar(didDeselectEntry entry: Any?) {
    detailCtl.clear()
  }

  func sidebar(didDeleteTopic topic: Topic) {
    status("Deleting topic \(topic.name)...")
    Backend.shared.deleteTopic(named: topic.name, inWorkspace: id).onComplete { _ in
      self.loadMetadata()
    }
  }

  func sidebar(didDeleteConsumerGroup group: Group) {
    status("Deleting consumer group \(group.id)...")
    Backend.shared.deleteGroup(named: group.id, inWorkspace: id).onComplete { _ in
      self.loadMetadata()
    }
  }

  func sidebarRequestsReload(withNewTopic name: String) {
    loadMetadata(andSelectTopic: name)
  }
}
