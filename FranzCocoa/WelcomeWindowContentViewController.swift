import Cocoa
import SwiftUI

class WelcomeWindowContentViewController: NSViewController {
  @IBOutlet weak var versionLabel: NSTextField!
  @IBOutlet weak var trialButton: NSButton!

  override func viewDidLoad() {
    super.viewDidLoad()

    let version = Bundle.main.object(forInfoDictionaryKey: "CFBundleShortVersionString") ?? "1"
    let build = Bundle.main.object(forInfoDictionaryKey: "CFBundleVersion") ?? "Unknown"
    versionLabel.stringValue = "Version \(version) (Build \(build))"

    trialButton.isHidden = true
    if let valid = Error.wait(Backend.shared.isLicenseValid()), !valid {
      trialButton.isHidden = false
      if let deadlineSeconds =  Error.wait(Backend.shared.getTrialDeadline()) {
        let now = Date()
        let deadline = Date(timeIntervalSince1970: TimeInterval(Int(deadlineSeconds)))
        let remaining = Int(deadline.timeIntervalSince(now)) / 86400
        if remaining <= 0 {
          trialButton.title = "Trial Expired!"
        } else {
          trialButton.title = "\(remaining) Days Remaining in Trial"
        }
      }
    }
  }

  func newConnection() {
    assert(Thread.isMainThread)
    let formController = ConnectionDetailsFormViewController()
    formController.configure(actionLabel: "Connect") { details in
      guard let conn = Error.wait(Backend.shared.saveConnection(details)) else { return }
      if let password = details.password, let id = details.passwordId {
        Keychain.shared.upsert(password: password, withId: id)
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

  @IBAction func didPushTrialButton(_ sender: Any) {
    WindowManager.shared.closeWelcomeWindow()
    WindowManager.shared.showPreferencesWindow(selectingTab: .license)
  }
}
