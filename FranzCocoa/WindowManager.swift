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

  @discardableResult
  func launchWorkspace(
    withConn conn: ConnectionDetails,
    andPassword password: String?,
    preferringExisting preferExisting: Bool = true
  ) -> WorkspaceWindowController? {
    assert(Thread.isMainThread)
    guard checkLicense() else {
      return nil
    }
    guard let id = conn.id else {
      preconditionFailure()
    }
    if let workspace = workspaces.first(where: { $0.connectionId == id }), preferExisting {
      workspace.showWindow(self)
      return workspace
    }
    let workspace = WorkspaceWindowController(withConn: conn, andPassword: password)
    workspace.showWindow(self)
    workspaces.append(workspace)
    return workspace
  }

  func removeAllWorkspaces() {
    assert(Thread.isMainThread)
    workspaces.removeAll()
  }

  func removeWorkspace(_ workspace: WorkspaceWindowController) -> Bool {
    assert(Thread.isMainThread)
    if let index = workspaces.firstIndex(of: workspace) {
      workspaces.remove(at: index)
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
}

fileprivate struct ScriptWindowKey: Hashable, Equatable {
  let workspaceId: UVarint
  let topic: String
}
