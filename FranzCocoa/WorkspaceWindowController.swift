import Cocoa
import NoiseBackend
import NoiseSerde
import OSLog
import SwiftUI

fileprivate var logger = Logger(
  subsystem: "io.defn.Franz",
  category: "WorkspaceWindowController"
)

class WorkspaceWindowController: NSWindowController {
  private var conn: ConnectionDetails!
  private var pass: String?
  private var id: UVarint!
  private weak var connectFuture: Future<String, UVarint>?

  private var metadata: Metadata?
  private var topic: String?

  @IBOutlet weak var toolbar: NSToolbar!
  @IBOutlet weak var statusBarView: NSView!
  @IBOutlet weak var statusBarNameField: NSTextField!
  @IBOutlet weak var statusBarStatusField: NSTextField!

  private let splitCtl = NSSplitViewController()
  private let sidebarCtl = WorkspaceSidebarViewController()
  private let detailCtl = WorkspaceDetailViewController()

  private var statusMu = DispatchSemaphore(value: 1)
  private var statusCookie = 0

  convenience init(withConn conn: ConnectionDetails, andPassword password: String?) {
    self.init(windowNibName: "WorkspaceWindowController")
    self.conn = conn
    self.pass = password
  }

  override func windowDidLoad() {
    super.windowDidLoad()

    toolbar.showsBaselineSeparator = true

    sidebarCtl.delegate = self
    detailCtl.delegate = self

    statusBarNameField.stringValue = conn.name
    connect()

    let sidebarItem = NSSplitViewItem(sidebarWithViewController: sidebarCtl)
    sidebarItem.minimumThickness = 200

    let detailItem = NSSplitViewItem(viewController: detailCtl)
    detailItem.minimumThickness = 400

    splitCtl.addSplitViewItem(sidebarItem)
    splitCtl.addSplitViewItem(detailItem)
    splitCtl.splitView.dividerStyle = .thin

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
    status("Connecting")
    if let connectFuture {
      logger.debug("canceling previous connect future")
      connectFuture.cancel()
    }
    connectFuture = Backend.shared.openWorkspace(withConn: conn, andPassword: pass)
    connectFuture?.sink(
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
      },
      onComplete: { id in
        self.id = id
        if let schemaRegistryId = self.conn.schemaRegistryId,
           let registry = Error.wait(Backend.shared.getSchemaRegistry(schemaRegistryId)) {
          self.activateRegistry(registry)
        }
        self.loadMetadata(forcingReload: false)
      }
    )
  }

  private func activateRegistry(_ registry: SchemaRegistry) {
    var password: String?
    if let passwordId = registry.passwordId {
      switch Keychain.shared.get(passwordWithId: passwordId, forResource: "schema registry") {
      case .success(let p):
        password = p
      default:
        ()
      }
    }
    Error.wait(Backend.shared.activateSchemaRegistry(
      registry,
      withPassword: password,
      inWorkspace: id
    ))
  }

  private func deactivateRegistry() {
    Error.wait(Backend.shared.deactivateSchemaRegistry(inWorkspace: id))
  }

  private func loadMetadata(forcingReload reload: Bool = true, andThen proc: @escaping () -> Void = {}) {
    guard let id else {
      connect()
      return
    }
    self.status("Fetching Metadata")
    Backend.shared.getMetadata(forcingReload: reload, inWorkspace: id).onComplete { meta in
      self.metadata = meta
      self.sidebarCtl.configure(withId: self.id, andConn: self.conn, andMetadata: meta)
      self.detailCtl.configure(withId: self.id)
      self.status("Ready")
      proc()
    }
  }

  private func loadMetadata(andSelectTopic name: String) {
    loadMetadata { [weak self] in
      self?.sidebarCtl.selectEntry(withKind: .topic, andLabel: name)
    }
  }

  private func loadMetadata(andSelectGroup name: String) {
    loadMetadata { [weak self] in
      self?.sidebarCtl.selectEntry(withKind: .group, andLabel: name)
    }
  }

  private func status(_ s: String) {
    assert(Thread.isMainThread)
    statusBarStatusField.stringValue = s
  }

  @objc func toggleSidebar(_ sender: Any) {
    splitCtl.toggleSidebar(sender)
  }

  @objc func performReload(_ sender: Any) {
    loadMetadata()
  }

  @objc func newTopic(_ sender: Any) {
    let ctl = NewTopicFormViewController()
    ctl.delegate = self
    ctl.configure(withId: id)
    window?.contentViewController?.presentAsSheet(ctl)
  }

  @objc func newRecord(_ sender: Any) {
    guard let metadata else { return }
    let ctl = PublishRecordFormViewController()
    ctl.delegate = self
    ctl.configure(withMetadata: metadata, andTopic: metadata.topics.first(where: { $0.name == topic }))
    contentViewController?.presentAsSheet(ctl)
  }

  @objc func configureSchemaRegistry(_ sender: Any) {
    let ctl = ConfigureSchemaRegistryFormViewController()
    let reg = conn.schemaRegistryId.flatMap { Error.wait(Backend.shared.getSchemaRegistry($0)) }
    ctl.delegate = self
    ctl.configure(withId: id, andRegistry: reg)
    window?.contentViewController?.presentAsSheet(ctl)
  }

  @objc func jumpToOffset(_ sender: Any) {
    guard let id else { return }
    NotificationCenter.default.post(
      name: .TopicRecordsTableJumpRequested,
      object: self,
      userInfo: ["ID": id]
    )
  }
}

// MARK: NSMenuItemValidation
extension WorkspaceWindowController: NSMenuItemValidation {
  func validateMenuItem(_ menuItem: NSMenuItem) -> Bool {
    if menuItem.action == #selector(jumpToOffset(_:)) {
      // Using TabState here is fairly piggy.
      return detailCtl.currentEntryKind == .topic &&
        TabState.shared.get(.topicDetail) as? TopicDetailView.Tab == .messages
    }
    return true
  }
}

// MARK: NSWindowDelegate
extension WorkspaceWindowController: NSWindowDelegate {
  func windowWillClose(_ notification: Notification) {
    if WindowManager.shared.removeWorkspace(withId: conn.id!), let id {
      WindowManager.shared.closeScriptWindows(forWorkspace: id)
      Error.wait(Backend.shared.closeWorkspace(id))
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
          .withSymbolConfiguration(.init(pointSize: 18, weight: .regular))
        item.label = "Toggle Sidebar"
        item.action = #selector(toggleSidebar(_:))
        return item
      case .statusBar:
        let item = NSToolbarItem(itemIdentifier: itemIdentifier)
        item.view = statusBarView
        if #unavailable(macOS 13) {
          item.minSize.width = 200
          item.maxSize.width = .infinity
        }
        return item
      case .reloadButton:
        let item = NSToolbarItem(itemIdentifier: itemIdentifier)
        item.image = NSImage(systemSymbolName: "arrow.clockwise", accessibilityDescription: "Reload")?
          .withSymbolConfiguration(.init(pointSize: 16, weight: .regular))
        item.label = "Reload"
        item.action = #selector(performReload(_:))
        return item
      case .publishButton:
        let item = NSToolbarItem(itemIdentifier: itemIdentifier)
        item.image = NSImage(systemSymbolName: "plus", accessibilityDescription: "Publish")?
          .withSymbolConfiguration(.init(pointSize: 18, weight: .regular))
        item.label = "Publish Record..."
        item.action = #selector(newRecord(_:))
        return item
      default:
        return nil
      }
  }

  func toolbarDefaultItemIdentifiers(_ toolbar: NSToolbar) -> [NSToolbarItem.Identifier] {
    return [.toggleSidebar, .sidebarTrackingSeparator, .statusBar, .reloadButton, .publishButton]
  }

  func toolbarAllowedItemIdentifiers(_ toolbar: NSToolbar) -> [NSToolbarItem.Identifier] {
    return [.toggleSidebar, .sidebarTrackingSeparator, .statusBar, .reloadButton, .publishButton]
  }
}

// MARK: - NSToolbarItem.Identifier
extension NSToolbarItem.Identifier {
  static let toggleSidebar = NSToolbarItem.Identifier("toggleSidebar")
  static let statusBar = NSToolbarItem.Identifier("statusBar")
  static let publishButton = NSToolbarItem.Identifier("publishButton")
  static let reloadButton = NSToolbarItem.Identifier("reloadButton")
}

// MARK: - NewTopicFormDelegate
extension WorkspaceWindowController: NewTopicFormDelegate {
  func didCancelNewTopicForm(_ sender: NewTopicFormViewController) {
  }

  func didCreateNewTopic(named name: String) {
    loadMetadata(andSelectTopic: name)
  }
}

// MARK: - PublishRecordFormDelegate
extension WorkspaceWindowController: PublishRecordFormDelegate {
  func didCancelPublishRecordForm(_ sender: PublishRecordFormViewController) {
    contentViewController?.dismiss(sender)
  }

  func didSubmitPublishRecordForm(
    _ sender: PublishRecordFormViewController,
    withTopic topic: Topic,
    partitionId pid: UVarint,
    key: String?,
    andValue value: String?) {

      let status = makeStatusProc()
      status("Publishing Record")
      Backend.shared.publishRecord(
        toTopic: topic.name,
        andPartition: pid,
        withKey: key.flatMap { $0.data(using: .utf8) },
        andValue: value.flatMap { $0.data(using: .utf8) },
        inWorkspace: id
      ).onComplete { _ in
        status("Ready")
        self.contentViewController?.dismiss(sender)
      }
  }
}

// MARK: - ConfigureSchemaRegistryFormDelegate
extension WorkspaceWindowController: ConfigureSchemaRegistryFormDelegate {
  func didCancelConfigureSchemaRegistryForm(_ sender: ConfigureSchemaRegistryFormViewController) {
  }

  func didSaveConfigureSchemaRegistryForm(registry: SchemaRegistry?) {
    if let registry {
      if registry.id == nil {
        if let registry = Error.wait(Backend.shared.saveSchemaRegistry(registry)) {
          conn.schemaRegistryId = registry.id
          Error.wait(Backend.shared.updateConnection(conn))
        }
      } else {
        Error.wait(Backend.shared.updateSchemaRegistry(registry))
      }

      activateRegistry(registry)
    } else if let registryId = conn.schemaRegistryId {
      conn.schemaRegistryId = nil
      Error.wait(Backend.shared.updateConnection(conn))
      Error.wait(Backend.shared.deleteSchemaRegistry(registryId))
      deactivateRegistry()
    }

    loadMetadata(forcingReload: true)
  }
}

// MARK: - WorkspaceDetailDelegate
extension WorkspaceWindowController: WorkspaceDetailDelegate {
  func getConnectionName() -> String {
    return conn.name
  }

  func clearStatusCookie() {
    _ = makeStatusCookie()
  }

  func makeStatusProc() -> ((String) -> Void) {
    let cookie = makeStatusCookie()
    return { message in
      self.status(message, withCookie: cookie)
    }
  }

  private func makeStatusCookie() -> Int {
    statusMu.wait()
    statusCookie += 1
    let cookie = statusCookie
    statusMu.signal()
    return cookie
  }

  private func status(_ message: String, withCookie cookie: Int) {
    statusMu.wait()
    guard cookie == statusCookie else {
      statusMu.signal()
      return
    }
    statusMu.signal()
    status(message)
  }

  func request(topicNamed name: String) {
    if let metadata, metadata.topics.contains(where: { $0.name == name }) {
      sidebarCtl.selectEntry(withKind: .topic, andLabel: name)
    } else {
      loadMetadata(andSelectTopic: name)
    }
  }

  func request(groupNamed name: String) {
    if let metadata, metadata.groups.contains(where: { $0.id == name }) {
      sidebarCtl.selectEntry(withKind: .consumerGroup, andLabel: name)
    } else {
      loadMetadata(andSelectGroup: name)
    }
  }
}

// MARK: - WorkspaceSidebarDelegate
extension WorkspaceWindowController: WorkspaceSidebarDelegate {
  func sidebar(didSelectEntry entry: Any, withKind kind: SidebarEntryKind) {
    if kind == .topic, let t = entry as? Topic {
      topic = t.name
    } else {
      topic = nil
    }
    detailCtl.show(entry: entry, withKind: kind)
  }

  func sidebar(didDeselectEntry entry: Any?) {
    detailCtl.clear()
  }

  func sidebar(didDeleteTopic topic: Topic) {
    status("Deleting Topic \(topic.name)")
    Backend.shared.deleteTopic(named: topic.name, inWorkspace: id).onComplete { _ in
      self.loadMetadata()
    }
  }

  func sidebar(didDeleteConsumerGroup group: Group) {
    status("Deleting Group \(group.id)")
    Backend.shared.deleteGroup(named: group.id, inWorkspace: id).onComplete { _ in
      self.loadMetadata()
    }
  }

  func sidebarRequestsReload(withNewTopic name: String) {
    loadMetadata(andSelectTopic: name)
  }
}
