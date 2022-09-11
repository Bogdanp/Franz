import Cocoa

@main
class AppDelegate: NSObject, NSApplicationDelegate {
  func applicationDidFinishLaunching(_ aNotification: Notification) {
    let res = Backend.shared.hello().wait()
    print("res=\(res)")
  }

  func applicationWillTerminate(_ aNotification: Notification) {
  }

  func applicationSupportsSecureRestorableState(_ app: NSApplication) -> Bool {
    return true
  }
}
