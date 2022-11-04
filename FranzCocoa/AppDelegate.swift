import Cocoa
import NoiseBackend

@main
class AppDelegate: NSObject, NSApplicationDelegate {
  func applicationDidFinishLaunching(_ aNotification: Notification) {
    FutureUtil.set(defaultErrorHandler: Error.alert(withError:))
    assert(Error.wait(Backend.shared.ping()) == "pong")
    WindowManager.shared.showWelcomeWindow()
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
