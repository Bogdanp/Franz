import Cocoa
import NoiseSerde

class ConfigureSchemaRegistryFormViewController: NSViewController {
  private var id: UVarint!
  private var registry: SchemaRegistry?

  @IBOutlet weak var registryURLField: NSTextField!
  @IBOutlet weak var usernameField: NSTextField!
  @IBOutlet weak var passwordField: NSTextField!

  weak var delegate: ConfigureSchemaRegistryFormDelegate?

  override func viewDidLoad() {
    super.viewDidLoad()

    if let registry {
      registryURLField.stringValue = registry.url
      usernameField.stringValue = registry.username ?? ""
      if let passwordId = registry.passwordId {
        switch Keychain.shared.get(passwordWithId: passwordId) {
        case .success(let password):
          passwordField.stringValue = password
        default:
          ()
        }
      }
    }
  }

  func configure(withId id: UVarint, andRegistry registry: SchemaRegistry? = nil) {
    self.id = id
    self.registry = registry
  }

  @IBAction func didPressCancelButton(_ sender: NSButton) {
    dismiss(self)
    delegate?.didCancelConfigureSchemaRegistryForm(self)
  }

  @IBAction func didPressSaveButton(_ sender: NSButton) {
    dismiss(self)
    if registryURLField.stringValue != "" {
      let password = passwordField.stringValue
      let passwordId = password == "" ? nil : Error.wait(Backend.shared.generatePasswordId())
      let registry = SchemaRegistry(
        id: self.registry?.id,
        kind: .confluent,
        url: registryURLField.stringValue,
        username: usernameField.stringValue == "" ? nil : usernameField.stringValue,
        passwordId: passwordId
      )
      if let passwordId {
        _ = Keychain.shared.upsert(password: password, withId: passwordId)
      }
      delegate?.didSaveConfigureSchemaRegistryForm(registry: registry)
    } else {
      delegate?.didSaveConfigureSchemaRegistryForm(registry: nil)
    }
  }
}

// MARK: - ConfigureSchemaRegistryFormDelegate
protocol ConfigureSchemaRegistryFormDelegate: AnyObject {
  func didCancelConfigureSchemaRegistryForm(_ sender: ConfigureSchemaRegistryFormViewController)
  func didSaveConfigureSchemaRegistryForm(registry: SchemaRegistry?)
}
