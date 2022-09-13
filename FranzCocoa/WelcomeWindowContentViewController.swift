import Cocoa
import SwiftUI

class WelcomeWindowContentViewController: NSViewController {
  private var connections = [ConnectionDetails]()

  override func viewDidAppear() {
    connections = Backend.shared.getConnections().wait()
    print("connections=\(connections)")
  }

  @IBAction func didPushCloseButton(_ sender: Any) {
    WindowManager.shared.closeWelcomeWindow()
  }
  
  @IBAction func didPushNewConnectionButton(_ sender: Any) {
    let formController = ConnectionDetailsFormViewController()
    formController.configure(actionLabel: "Connect", { details in
      let id = Backend.shared.saveConnection(details).wait()
      print("id=\(id)")
    })
    presentAsSheet(formController)
  }
}
