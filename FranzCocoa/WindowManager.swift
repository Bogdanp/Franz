import Foundation
import AppKit

class WindowManager {
  static var shared = WindowManager()

  private var welcomeWindowController: WelcomeWindowController?
  private var workspaces = [UInt64: WorkspaceWindowController]()

  func launchWorkspace(withConn conn: ConnectionDetails) {
    assert(Thread.isMainThread)
    guard let id = conn.id else {
      preconditionFailure()
    }
    if let workspace = workspaces[id] {
      workspace.showWindow(self)
      return
    }
    let workspace = WorkspaceWindowController(withConn: conn)
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

  func removeWorkspace(withId id: UInt64) {
    assert(Thread.isMainThread)
    workspaces.removeValue(forKey: id)
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
}
