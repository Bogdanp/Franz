import Cocoa
import NoiseBackend
import NoiseSerde
import SwiftUI

class WorkspaceWindowController: NSWindowController {
  private var conn: ConnectionDetails!
  private var pass: String?
  private var id: UVarint!

  private var metadata: Metadata?
  private var topic: String?

  @IBOutlet weak var toolbar: NSToolbar!
  @IBOutlet weak var statusBarView: NSView!
  @IBOutlet weak var statusBarNameField: NSTextField!
  @IBOutlet weak var statusBarStatusField: NSTextField!

  private let splitCtl = NSSplitViewController()
  private let sidebarCtl = WorkspaceSidebarViewController()
  private let detailCtl = WorkspaceDetailViewController()

  private weak var reloadMetadataMenuItem: NSMenuItem?
  private weak var newTopicMenuItem: NSMenuItem?
  private weak var publishMenuItem: NSMenuItem?

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

    reloadMetadataMenuItem = MainMenu.shared.find(itemByPath: [.ConnectionMenuItem, .ReloadMetadataMenuItem])
    newTopicMenuItem = MainMenu.shared.find(itemByPath: [.TopicMenuItem, .NewTopicMenuItem])
    publishMenuItem = MainMenu.shared.find(itemByPath: [.TopicMenuItem, .PublishMenuItem])

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
      },
      onComplete: { id in
        self.id = id
        self.loadMetadata(forcingReload: false)
      }
    )
  }

  private func loadMetadata(forcingReload reload: Bool = true, andThen proc: @escaping () -> Void = {}) {
    guard let id else {
      connect()
      return
    }
    self.status("Fetching Metadata")
    Backend.shared.getMetadata(forcingReload: reload, inWorkspace: id).onComplete { meta in
      self.metadata = meta
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
    let items = [
      reloadMetadataMenuItem: #selector(didPressReloadButton(_:)),
      newTopicMenuItem: #selector(didPressNewTopicItem(_:)),
      publishMenuItem: #selector(didPressPublishButton(_:)),
    ]
    for (item, selector) in items {
      guard let item else { continue }
      item.target = self
      item.action = selector
    }
  }

  func windowDidResignKey(_ notification: Notification) {
    for item in [reloadMetadataMenuItem, newTopicMenuItem, publishMenuItem] {
      guard let item else { continue }
      item.target = nil
      item.action = nil
    }
  }

  func windowWillClose(_ notification: Notification) {
    guard let id else { return }
    if WindowManager.shared.removeWorkspace(withId: conn.id!) {
      _ = Backend.shared.closeWorkspace(id)
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
          .withSymbolConfiguration(.init(pointSize: 16, weight: .regular))
        item.label = "Reload"
        item.action = #selector(didPressReloadButton(_:))
        return item
      case .publishButton:
        let item = NSToolbarItem(itemIdentifier: itemIdentifier)
        item.image = NSImage(systemSymbolName: "plus", accessibilityDescription: "Publish")?
          .withSymbolConfiguration(.init(pointSize: 18, weight: .regular))
        item.label = "Publish Record..."
        item.action = #selector(didPressPublishButton(_:))
        return item
      default:
        return nil
      }
  }

  func toolbarDefaultItemIdentifiers(_ toolbar: NSToolbar) -> [NSToolbarItem.Identifier] {
    return [.toggleSidebar, .statusBar, .reloadButton, .publishButton]
  }

  func toolbarAllowedItemIdentifiers(_ toolbar: NSToolbar) -> [NSToolbarItem.Identifier] {
    return [.toggleSidebar, .statusBar, .reloadButton, .publishButton]
  }

  @objc func didPressToggleSidebarButton(_ sender: Any) {
    splitCtl.toggleSidebar(sender)
  }

  @objc func didPressReloadButton(_ sender: Any) {
    loadMetadata()
  }

  @objc func didPressPublishButton(_ sender: Any) {
    guard let metadata else { return }
    let ctl = PublishRecordFormViewController()
    ctl.delegate = self
    ctl.configure(withMetadata: metadata, andTopic: metadata.topics.first(where: { $0.name == topic }))
    contentViewController?.presentAsSheet(ctl)
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
        // TODO: Notify the topic's records table to show the record if it's currently being displayed.
      }
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
    loadMetadata(andSelectTopic: name)
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
