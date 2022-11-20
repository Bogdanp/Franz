import AppKit
import Foundation
import NoiseSerde

class WindowManager {
  static var shared = WindowManager()

  private var welcomeWindowController: WelcomeWindowController?
  private var preferencesWindowController: PreferencesWindowController?
  private var workspaces = [UInt64: WorkspaceWindowController]() // conn id -> ctl
  private var scripts = [ScriptWindowKey: ScriptWindowController]() // workspace id -> ctl

  func launchWorkspace(withConn conn: ConnectionDetails, andPassword password: String?) {
    assert(Thread.isMainThread)
    guard let id = conn.id else {
      preconditionFailure()
    }
    if let workspace = workspaces[id] {
      workspace.showWindow(self)
      return
    }
    let workspace = WorkspaceWindowController(withConn: conn, andPassword: password)
    workspace.showWindow(self)
    workspaces[id] = workspace
  }

  func closeWorkspace(withId id: UInt64) {
    assert(Thread.isMainThread)
    guard let workspace = workspaces[id] else {
      return
    }
    workspaces.removeValue(forKey: id)
    workspace.close()
  }

  func removeAllWorkspaces() {
    assert(Thread.isMainThread)
    workspaces.removeAll()
  }

  func removeWorkspace(withId id: UInt64) -> Bool {
    assert(Thread.isMainThread)
    return workspaces.removeValue(forKey: id) != nil
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

  func showPreferencesWindow() {
    assert(Thread.isMainThread)
    if preferencesWindowController == nil {
      preferencesWindowController = PreferencesWindowController()
    }
    preferencesWindowController?.window?.center()
    preferencesWindowController?.showWindow(self)
  }

  func showScriptWindow(
    forWorkspace id: UVarint,
    andTopic topic: String,
    withDelegate delegate: ScriptWindowControllerDelegate? = nil
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

  // XXX: Kind of a strange place for this to be in.
  func openManual() {
    if let url = Bundle.main.url(forResource: "resources/manual/index", withExtension: "html") {
      NSWorkspace.shared.open(url)
    }
  }
}

fileprivate struct ScriptWindowKey: Hashable, Equatable {
  let workspaceId: UVarint
  let topic: String
}
