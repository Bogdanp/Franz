import Foundation
import AppKit

class WindowManager {
  static var shared = WindowManager()

  private var welcomeWindowController: WelcomeWindowController?
//  private var workspaces = [UInt64: WorkspaceWindowController]()
//
//  func launchWorkspace(forConf c: ConnectionConf) {
//    assert(Thread.isMainThread)
//    guard c.id != nil else {
//      preconditionFailure()
//    }
//    if let workspace = workspaces[c.id!] {
//      workspace.showWindow(self)
//      return
//    }
//    let workspace = WorkspaceWindowController()
//    workspace.configure(withConf: c)
//    workspace.showWindow(self)
//    workspaces[c.id!] = workspace
//  }
//
//  func closeWorkspace(withId id: UInt64) {
//    assert(Thread.isMainThread)
//    guard let workspace = workspaces[id] else {
//      return
//    }
//    workspaces.removeValue(forKey: id)
//    workspace.close()
//  }

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
