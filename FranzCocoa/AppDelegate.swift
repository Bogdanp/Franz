import Cocoa
import NoiseBackend
import os

let logger = Logger(
  subsystem: Bundle.main.bundleIdentifier!,
  category: "network"
)

@main
class AppDelegate: NSObject, NSApplicationDelegate {
  func applicationDidFinishLaunching(_ aNotification: Notification) {
    FutureUtil.set(defaultErrorHandler: { err in
      let textField = NSTextField()
      textField.isEditable = true
      textField.isSelectable = true
      textField.stringValue = "\(err)"
      textField.frame = NSRect(x: 0, y: 0, width: 400, height: 200)
      textField.font = .monospacedSystemFont(ofSize: 12, weight: .regular)
      let alert = NSAlert()
      alert.alertStyle = .critical
      alert.messageText = "Error"
      alert.informativeText = "Franz encountered an unexpected error.  Most likely, this is a bug, so please report it!"
      alert.accessoryView = textField
      alert.runModal()
    })
    assert(try! Backend.shared.ping().wait() == "pong")
    WindowManager.shared.showWelcomeWindow()
  }

  func applicationWillTerminate(_ aNotification: Notification) {
    WindowManager.shared.removeAllWorkspaces()
    try! Backend.shared.closeAllWorkspaces().wait()
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
