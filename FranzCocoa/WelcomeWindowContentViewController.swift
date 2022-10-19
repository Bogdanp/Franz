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
    formController.configure(actionLabel: "Connect", { details in
      let conn = try! Backend.shared.saveConnection(details).wait()
      if let password = details.password, let id = details.passwordId {
        _ = Keychain.shared.upsert(password: password, withId: id)
      }
      WindowManager.shared.launchWorkspace(withConn: conn, andPassword: details.password)
      WindowManager.shared.closeWelcomeWindow()
    })
    presentAsSheet(formController)
  }

  @IBAction func didPushCloseButton(_ sender: Any) {
    WindowManager.shared.closeWelcomeWindow()
  }

  @IBAction func didPushNewConnectionButton(_ sender: Any) {
    newConnection()
  }
}
