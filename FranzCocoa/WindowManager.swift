import AppKit
import Foundation
import NoiseSerde

class WindowManager {
  static var shared = WindowManager()

  private var welcomeWindowController: WelcomeWindowController?
  private var preferencesWindowController: PreferencesWindowController?
  private var updatesWindowController: UpdatesWindowController?
  private var updatesProgressWindowController: UpdatesProgressWindowController?
  private var workspaces = [WorkspaceWindowController]()
  private var scripts = [ScriptWindowKey: ScriptWindowController]() // workspace id -> ctl
  private var secureURLs = [UVarint: [URL]]() // connection id -> URLs

  @discardableResult
  func launchWorkspace(
    withConn conn: ConnectionDetails,
    andPassword password: String?,
    preferringExisting preferExisting: Bool = true
  ) -> WorkspaceWindowController? {
    assert(Thread.isMainThread)
#if !MAC_APP_STORE_BUILD
    guard checkLicense() else {
      return nil
    }
#endif
    guard let id = conn.id else {
      preconditionFailure()
    }
    if let workspace = workspaces.first(where: { $0.connectionId == id }), preferExisting {
      workspace.showWindow(self)
      return workspace
    }
    // If the connection has SSL key and cert paths, convert their
    // bookmarks to paths in order for the Racket code to be able to
    // read them.
    var conn = conn
    if conn.sslKeyPath != nil && conn.sslCertPath != nil {
      guard let keyURL = bookmarkDataToURL(conn.sslKeyPath!) else {
        Error.alert(withError: "Failed to access SSL key. Please edit this connection and pick a new key.")
        return nil
      }
      guard let certURL = bookmarkDataToURL(conn.sslCertPath!) else {
        Error.alert(withError: "Failed to access SSL certificate. Please edit this connection and pick a new cert.")
        return nil
      }
      conn.sslKeyPath = keyURL.relativePath
      conn.sslCertPath = certURL.relativePath
      secureURLs[id] = [keyURL, certURL]
    }
    let workspace = WorkspaceWindowController(withConn: conn, andPassword: password)
    workspace.showWindow(self)
    workspaces.append(workspace)
    return workspace
  }

  func removeAllWorkspaces() {
    assert(Thread.isMainThread)
    workspaces.removeAll()
    for connectionId in secureURLs.keys {
      removeSecureURLs(forConnectionWithId: connectionId)
    }
  }

  func removeWorkspace(_ workspace: WorkspaceWindowController) -> Bool {
    assert(Thread.isMainThread)
    if let index = workspaces.firstIndex(of: workspace) {
      workspaces.remove(at: index)
      if let connectionId = workspace.connectionId {
        removeSecureURLs(forConnectionWithId: connectionId)
      }
      return true
    }
    return false
  }

  func showWelcomeWindow() {
    assert(Thread.isMainThread)
    if welcomeWindowController == nil {
      welcomeWindowController = WelcomeWindowController()
    }
    welcomeWindowController?.window?.center()
    welcomeWindowController?.showWindow(self)
  }

  func closeWelcomeWindow() {
    assert(Thread.isMainThread)
    welcomeWindowController?.close()
    welcomeWindowController = nil
  }

  func showPreferencesWindow(selectingTab tab: PreferencesTab = .general) {
    assert(Thread.isMainThread)
    if preferencesWindowController == nil {
      preferencesWindowController = PreferencesWindowController()
    }
    preferencesWindowController?.window?.center()
    preferencesWindowController?.showWindow(self)
    preferencesWindowController?.display(tab: tab)
  }

  func showScriptWindow(
    forWorkspace id: UVarint,
    andTopic topic: String,
    withDelegate delegate: ScriptWindowDelegate? = nil
  ) {
    assert(Thread.isMainThread)
    let key = ScriptWindowKey(workspaceId: id, topic: topic)
    if let ctl = scripts[key] {
      ctl.delegate = delegate
      ctl.showWindow(self)
      return
    }
    let ctl = ScriptWindowController(withId: id, andTopic: topic)
    ctl.delegate = delegate
    ctl.showWindow(self)
    scripts[key] = ctl
  }

  func closeScriptWindows(forWorkspace id: UVarint) {
    for (k, ctl) in scripts where k.workspaceId == id {
      ctl.close()
      scripts.removeValue(forKey: k)
    }
  }

  func showUpdatesWindow(withChangelog changelog: String, andRelease release: Release) {
    assert(Thread.isMainThread)
    if updatesWindowController == nil {
      updatesWindowController = UpdatesWindowController()
    }
    updatesWindowController?.showWindow(self)
    updatesWindowController?.configure(withChangelog: changelog, andRelease: release)
  }

  func closeUpdatesWindow() {
    updatesWindowController?.close()
  }

  func showUpdatesProgressWindow(withCloseHandler hdl: @escaping () -> Void = { }) {
    assert(Thread.isMainThread)
    if updatesProgressWindowController == nil {
      updatesProgressWindowController = UpdatesProgressWindowController()
    }
    updatesProgressWindowController?.configure(withCloseHandler: hdl)
    updatesProgressWindowController?.showWindow(self)
  }

  func closeUpdatesProgressWindow() {
    updatesProgressWindowController?.close()
    updatesProgressWindowController = nil
  }

  // XXX: Kind of a strange place for this to be in.
  func openManual() {
    if let url = Bundle.main.url(forResource: "resources/manual/index", withExtension: "html") {
      NSWorkspace.shared.open(url)
    }
  }

  private func checkLicense() -> Bool {
    if let ok = Error.wait(Backend.shared.isLicenseValid()), ok {
      return true
    }

    showPreferencesWindow(selectingTab: .license)
    return false
  }

  private func removeSecureURLs(forConnectionWithId id: UVarint) {
    guard let urls = secureURLs[id] else { return }
    for url in urls {
      url.stopAccessingSecurityScopedResource()
    }
    secureURLs.removeValue(forKey: id)
  }
}

fileprivate struct ScriptWindowKey: Hashable, Equatable {
  let workspaceId: UVarint
  let topic: String
}

fileprivate func bookmarkDataToURL(_ string: String) -> URL? {
  var isStale = false
  guard let base64Data = string.data(using: .utf8) else { return nil}
  guard let bookmarkData = Data(base64Encoded: base64Data) else { return nil }
  guard let url = try? URL(
    resolvingBookmarkData: bookmarkData,
    options: [.withSecurityScope],
    bookmarkDataIsStale: &isStale
  ) else {
    return nil
  }
  if isStale {
    return nil
  }
  if !url.startAccessingSecurityScopedResource() {
    return nil
  }
  return url
}
