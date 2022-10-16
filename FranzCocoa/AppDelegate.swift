import Cocoa

@main
class AppDelegate: NSObject, NSApplicationDelegate {
  func applicationDidFinishLaunching(_ aNotification: Notification) {
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
}
