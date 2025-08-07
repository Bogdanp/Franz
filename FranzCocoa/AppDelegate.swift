import Cocoa
import Foundation
import NoiseBackend
import NoiseSerde
import SwiftUI

@main
class AppDelegate: NSObject, NSApplicationDelegate {
  func applicationDidFinishLaunching(_ aNotification: Notification) {
    // XXX: Escape squircle jail.
    if #available(macOS 26, *) {
      if let image = NSImage(named: "AppIcon") {
        NSApplication.shared.dockTile.contentView = NSImageView(image: image)
        NSApplication.shared.dockTile.display()
      }
    }

    FutureUtil.set(defaultErrorHandler: { err in
      Error.alert(withError: err)
    })
    assert(Error.wait(Backend.shared.ping()) == "pong")

#if !MAC_APP_STORE_BUILD
    AutoUpdater.shared.start(
      withInterval: Defaults.shared.checkForUpdates ? Defaults.shared.updateInterval.seconds * 1000 : nil,
      checkingImmediately: Defaults.shared.checkForUpdates
    ) { changes, release in
      WindowManager.shared.showUpdatesWindow(
        withChangelog: changes,
        andRelease: release
      )
    }
#endif

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

  @IBAction func didPushSettingsButton(_ sender: Any) {
    WindowManager.shared.showSettingsWindow()
  }

  @IBAction func didPushManualButton(_ sender: Any) {
    WindowManager.shared.openManual()
  }

  @IBAction func didPushExportDebugInfoButton(_ sender: Any) {
    Backend.shared.getBreadcrumbs().onComplete { crumbs in
      guard crumbs.count > 0 else { return }
      let dialog = NSSavePanel()
      dialog.allowedContentTypes = [.utf8PlainText]
      dialog.nameFieldStringValue = "debug.txt"
      dialog.title = "Save Debug Info"
      switch dialog.runModal() {
      case .OK:
        guard let url = dialog.url else { return }
        let data = crumbs.map { $0.description }.joined(separator: "\n\n").data(using: .utf8)
        do {
          try data?.write(to: url, options: .atomic)
        } catch {
          Error.alert(withError: error)
        }
      default:
        ()
      }
    }
  }

#if !MAC_APP_STORE_BUILD
  @IBAction func didPushCheckForUpdatesButton(_ sender: Any) {
    var canceled = false
    WindowManager.shared.showUpdatesProgressWindow {
      canceled = true
    }

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
#endif
}
