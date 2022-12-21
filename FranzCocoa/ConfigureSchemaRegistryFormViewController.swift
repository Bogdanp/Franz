import Cocoa
import NoiseSerde

class ConfigureSchemaRegistryFormViewController: NSViewController {
  private var id: UVarint!

  @IBOutlet weak var registryURLField: NSTextField!
  @IBOutlet weak var usernameField: NSTextField!
  @IBOutlet weak var passwordField: NSTextField!

  weak var delegate: ConfigureSchemaRegistryFormDelegate?

  override func viewDidLoad() {
    super.viewDidLoad()
  }

  func configure(withId id: UVarint) {
    self.id = id
  }

  @IBAction func didPressCancelButton(_ sender: NSButton) {
    dismiss(self)
    delegate?.didCancelConfigureSchemaRegistryForm(self)
  }

  @IBAction func didPressSaveButton(_ sender: NSButton) {
    delegate?.didSaveConfigureSchemaRegistryForm(
      withURL: registryURLField.stringValue == "" ? nil : registryURLField.stringValue,
      andUsername: usernameField.stringValue == "" ? nil : usernameField.stringValue,
      andPassword: passwordField.stringValue == "" ? nil : passwordField.stringValue
    )
  }
}

// MARK: - ConfigureSchemaRegistryFormDelegate
protocol ConfigureSchemaRegistryFormDelegate: AnyObject {
  func didCancelConfigureSchemaRegistryForm(_ sender: ConfigureSchemaRegistryFormViewController)
  func didSaveConfigureSchemaRegistryForm(
    withURL url: String?,
    andUsername username: String?,
    andPassword password: String?
  )
}
