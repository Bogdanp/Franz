import Cocoa
import NoiseSerde
import os

fileprivate let logger = Logger(
  subsystem: Bundle.main.bundleIdentifier!,
  category: "ConnectionDetailsForm"
)

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

  private var details: ConnectionDetails?

  override func viewDidLoad() {
    super.viewDidLoad()

    actionButton.title = actionLabel
    actionButton.sizeToFit()

    let portFormatter = NumberFormatter()
    portFormatter.minimum = 1
    portFormatter.maximum = 65535
    portFormatter.allowsFloats = false
    bootstrapPortField.formatter = portFormatter

    if let details {
      nameField.stringValue = details.name
      bootstrapHostField.stringValue = details.bootstrapHost
      bootstrapPortField.integerValue = Int(details.bootstrapPort)
      if let username = details.username {
        usernameField.stringValue = username
      }
      if let passwordId = details.passwordId {
        switch Keychain.shared.get(passwordWithId: passwordId) {
        case .success(let password):
          passwordField.stringValue = password
        default:
          ()
        }
      }
      enableSSLCheckbox.state = details.useSsl ? .on : .off
    }
  }

  override func viewDidAppear() {
    view.window?.setFrame(NSRect(x: 0, y: 0, width: 480, height: 190), display: true)
    view.window?.styleMask.remove(.resizable)
    view.window?.styleMask.update(with: .fullSizeContentView)
  }

  func configure(actionLabel label: String,
                 details: ConnectionDetails? = nil,
                 _ proc: @escaping (ConnectionDetails) -> Void) {
    self.actionLabel = label
    self.actionProc = proc
    self.details = details
  }

  @IBAction func didPushCancelButton(_ sender: Any) {
    self.dismiss(self)
  }

  @IBAction func didPushActionButton(_ sender: Any) {
    self.dismiss(self)
    var passwordId: String?
    if !passwordField.stringValue.isEmpty {
      do {
        passwordId = try Backend.shared.generatePasswordId().wait()
      } catch {
        logger.error("failed to generate password id: \(error.localizedDescription)")
        return
      }
    }
    actionProc(ConnectionDetails(
      id: details?.id,
      name: nameField.stringValue == "" ? "Unnamed Connection" : nameField.stringValue,
      bootstrapHost: bootstrapHostField.stringValue == "" ? "127.0.0.1" : bootstrapHostField.stringValue,
      bootstrapPort: bootstrapPortField.stringValue == "" ? 9092 : UVarint(bootstrapPortField.integerValue),
      authMechanism: .plain,
      username: usernameField.stringValue == "" ? nil : usernameField.stringValue,
      password: passwordField.stringValue == "" ? nil : passwordField.stringValue,
      passwordId: passwordField.stringValue == "" ? nil : passwordId,
      awsRegion: nil,
      awsAccessKeyId: nil,
      useSsl: enableSSLCheckbox.state == .on
    ))
  }
}
