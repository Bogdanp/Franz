import Cocoa
import SwiftUI

class WelcomeWindowContentViewController: NSViewController {
  @IBOutlet weak var versionLabel: NSTextField!

  override func viewDidLoad() {
    super.viewDidLoad()

    let version = Bundle.main.object(forInfoDictionaryKey: "CFBundleShortVersionString") ?? "1"
    let build = Bundle.main.object(forInfoDictionaryKey: "CFBundleVersion") ?? "Unknown"
    versionLabel.stringValue = "Version \(version) (Build \(build))"
  }

  func newConnection() {
    assert(Thread.isMainThread)
    let formController = ConnectionDetailsFormViewController()
    formController.configure(actionLabel: "Connect") { details in
      guard let conn = Error.wait(Backend.shared.saveConnection(details)) else { return }
      if let password = details.password, let id = details.passwordId {
        _ = Keychain.shared.upsert(password: password, withId: id)
      }
      WindowManager.shared.launchWorkspace(withConn: conn, andPassword: details.password)
      WindowManager.shared.closeWelcomeWindow()
    }
    presentAsSheet(formController)
  }

  @IBAction func didPushCloseButton(_ sender: Any) {
    WindowManager.shared.closeWelcomeWindow()
  }

  @IBAction func didPushNewConnectionButton(_ sender: Any) {
    newConnection()
  }

  @IBAction func didPushDocumentationButton(_ sender: Any) {
    WindowManager.shared.openManual()
  }

  @IBAction func didPushSupportButton(_ sender: Any) {
    NSWorkspace.shared.open(URL(string: "mailto:bogdan@defn.io?subject=Franz%20Support")!)
  }

  @IBAction func didPushMastodonButton(_ sender: Any) {
    NSWorkspace.shared.open(URL(string: "https://hachyderm.io/@franz_app")!)
  }
}
