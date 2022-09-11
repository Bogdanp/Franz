import Cocoa

class WelcomeWindowContentViewController: NSViewController {
  @IBAction func didPushCloseButton(_ sender: Any) {
    WindowManager.shared.closeWelcomeWindow()
  }

  @IBAction func didPushNewConnectionButton(_ sender: Any) {
//    presentAsSheet(NewConnectionViewController())
  }
}
