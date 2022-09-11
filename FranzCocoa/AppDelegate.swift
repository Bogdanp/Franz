import Cocoa

@main
class AppDelegate: NSObject, NSApplicationDelegate {
  func applicationDidFinishLaunching(_ aNotification: Notification) {
    WindowManager.shared.showWelcomeWindow()
  }

  func applicationWillTerminate(_ aNotification: Notification) {
  }

  func applicationSupportsSecureRestorableState(_ app: NSApplication) -> Bool {
    return true
  }

  @IBAction func didPushWelcomeToFranzButton(_ sender: Any) {
    WindowManager.shared.showWelcomeWindow()
  }
}
