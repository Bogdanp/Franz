import Cocoa
import Foundation
import NoiseBackend
import SwiftUI

@main
class AppDelegate: NSObject, NSApplicationDelegate {
  func applicationDidFinishLaunching(_ aNotification: Notification) {
    FutureUtil.set(defaultErrorHandler: Error.alert(withError:))
    assert(Error.wait(Backend.shared.ping()) == "pong")
    WindowManager.shared.showWelcomeWindow()

    let codeWindow = NSWindow()
    codeWindow.contentView = NSHostingView(rootView: Editor(code: """
local script = {}

function script.filter(record)
  return true
end

function script.map(record)
  return record
end

return script
"""))
    codeWindow.setFrame(NSRect(x: 0, y: 0, width: 800, height: 600), display: true)
    codeWindow.center()
    codeWindow.makeKeyAndOrderFront(self)
  }

  func applicationWillTerminate(_ aNotification: Notification) {
    WindowManager.shared.removeAllWorkspaces()
    Error.wait(Backend.shared.closeAllWorkspaces())
  }

  func applicationSupportsSecureRestorableState(_ app: NSApplication) -> Bool {
    return true
  }

  @IBAction func didPushWelcomeToFranzButton(_ sender: Any) {
    WindowManager.shared.showWelcomeWindow()
  }

  @IBAction func didPushNewConnectionButton(_ sender: Any) {
    WindowManager.shared.showWelcomeWindow()
    NotificationCenter.default.post(name: .NewConnectionRequested, object: nil)
  }

  @IBAction func didPushPreferencesButton(_ sender: Any) {
    WindowManager.shared.showPreferencesWindow()
  }
}
