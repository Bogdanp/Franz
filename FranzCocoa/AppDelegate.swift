import Cocoa
import Foundation
import NoiseBackend
import SwiftUI

@main
class AppDelegate: NSObject, NSApplicationDelegate {
  func applicationDidFinishLaunching(_ aNotification: Notification) {
    if Defaults.shared.checkForUpdates {
      AutoUpdater.shared.start(withInterval: Double(Defaults.shared.updateInterval.seconds)) { changes, version in
        WindowManager.shared.showUpdatesWindow(withChangelog: changes, andRelease: version)
      }
    }

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

  @IBAction func didPushPreferencesButton(_ sender: Any) {
    WindowManager.shared.showPreferencesWindow()
  }

  @IBAction func didPushManualButton(_ sender: Any) {
    WindowManager.shared.openManual()
  }

  @IBAction func didPushCheckForUpdatesButton(_ sender: Any) {
    var canceled = false
    WindowManager.shared.showUpdatesProgressWindow {
      canceled = true
    }
    AutoUpdater.shared.resetCaches {
      AutoUpdater.shared.checkForUpdates(
        rejectionHandler: {
          guard !canceled else { return }
          WindowManager.shared.closeUpdatesProgressWindow()
          let alert = NSAlert()
          alert.messageText = "You're up to date!"
          alert.informativeText = "You are running the latest version of Franz available."
          alert.runModal()
        },
        completionHandler: { changes, version in
          guard !canceled else { return }
          WindowManager.shared.closeUpdatesProgressWindow()
          WindowManager.shared.showUpdatesWindow(withChangelog: changes, andRelease: version)
        }
      )
    }
  }
}
