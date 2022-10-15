import Cocoa
import SwiftUI

class WelcomeWindowContentViewController: NSViewController {
  @IBAction func didPushCloseButton(_ sender: Any) {
    WindowManager.shared.closeWelcomeWindow()
  }
  
  @IBAction func didPushNewConnectionButton(_ sender: Any) {
    let formController = ConnectionDetailsFormViewController()
    formController.configure(actionLabel: "Connect", { details in
      let conn = try! Backend.shared.saveConnection(details).wait()
      if let password = details.password, let id = details.passwordId {
        let _ = Keychain.shared.upsert(password: password, withId: id)
      }
      WindowManager.shared.launchWorkspace(withConn: conn, andPassword: details.password)
      WindowManager.shared.closeWelcomeWindow()
    })
    presentAsSheet(formController)
  }
}
