import Cocoa

class ConnectionDetailsFormViewController: NSViewController {

  @IBOutlet weak var nameField: NSTextField!
  @IBOutlet weak var bootstrapHostField: NSTextField!
  @IBOutlet weak var bootstrapPortField: NSTextField!
  @IBOutlet weak var usernameField: NSTextField!
  @IBOutlet weak var passwordField: NSTextField!
  @IBOutlet weak var enableSSLCheckbox: NSButton!
  @IBOutlet weak var cancelButton: NSButton!
  @IBOutlet weak var actionButton: NSButton!

  private var actionLabel: String!
  private var actionProc: ((ConnectionDetails) -> Void)!

  override func viewDidLoad() {
    super.viewDidLoad()
    actionButton.title = actionLabel
    actionButton.sizeToFit()
  }

  override func viewDidAppear() {
    view.window?.setFrame(NSRect(x: 0, y: 0, width: 480, height: 190), display: true)
    view.window?.styleMask.remove(.resizable)
    view.window?.styleMask.update(with: .fullSizeContentView)
  }

  func configure(actionLabel label: String, _ proc: @escaping (ConnectionDetails) -> Void) {
    self.actionLabel = label
    self.actionProc = proc
  }

  @IBAction func didPushCancelButton(_ sender: Any) {
    self.dismiss(self)
  }

  @IBAction func didPushActionButton(_ sender: Any) {
    self.dismiss(self)
    actionProc(ConnectionDetails(
      id: nil,
      name: nameField.stringValue == "" ? "Unnamed Connection" : nameField.stringValue,
      bootstrapHost: bootstrapHostField.stringValue == "" ? "127.0.0.1" : bootstrapHostField.stringValue,
      bootstrapPort: bootstrapPortField.stringValue == "" ? 9092 : 9092,
      username: usernameField.stringValue == "" ? nil : usernameField.stringValue,
      password: passwordField.stringValue == "" ? nil : passwordField.stringValue,
      useSsl: enableSSLCheckbox.state == .on
    ))
  }
}
