import Cocoa
import SwiftUI

class WelcomeWindowContentViewController: NSViewController {
  @IBAction func didPushCloseButton(_ sender: Any) {
    WindowManager.shared.closeWelcomeWindow()
  }
  
  @IBAction func didPushNewConnectionButton(_ sender: Any) {
    let formController = ConnectionDetailsFormViewController()
    formController.configure(actionLabel: "Connect", { details in
      let conn = Backend.shared.saveConnection(details).wait()
      WindowManager.shared.launchWorkspace(withConn: conn)
      WindowManager.shared.closeWelcomeWindow()
    })
    presentAsSheet(formController)
  }
}
