import Cocoa
import NoiseSerde
import UniformTypeIdentifiers
import os

fileprivate let logger = Logger(
  subsystem: Bundle.main.bundleIdentifier!,
  category: "ConnectionDetailsForm"
)

class ConnectionDetailsFormViewController: NSViewController {
  @IBOutlet weak var nameField: NSTextField!
  @IBOutlet weak var bootstrapHostField: NSTextField!
  @IBOutlet weak var bootstrapPortField: NSTextField!
  @IBOutlet weak var authMechanismButton: NSPopUpButton!
  @IBOutlet weak var usernameField: NSTextField!
  @IBOutlet weak var passwordField: NSTextField!
  @IBOutlet weak var enableHttpProxyCheckbox: NSButton!
  @IBOutlet weak var httpProxyHostField: NSTextField!
  @IBOutlet weak var httpProxyPortField: NSTextField!
  @IBOutlet weak var enableSslCheckbox: NSButton!
  @IBOutlet weak var sslKeyButton: NSButton!
  @IBOutlet weak var sslCertButton: NSButton!
  @IBOutlet weak var awsRegionField: NSTextField!
  @IBOutlet weak var awsAccessKeyIdField: NSTextField!
  @IBOutlet weak var awsAccessKeySecretField: NSSecureTextField!
  @IBOutlet weak var cancelButton: NSButton!
  @IBOutlet weak var testButton: NSButton!
  @IBOutlet weak var actionButton: NSButton!

  @IBOutlet weak var proxyViewHeightConstraint: NSLayoutConstraint!
  @IBOutlet weak var proxyView: NSView!
  @IBOutlet var httpProxyView: NSView!

  @IBOutlet weak var authViewHeightConstraint: NSLayoutConstraint!
  @IBOutlet weak var authView: NSView!
  @IBOutlet var plainAuthView: NSView!
  @IBOutlet var awsAuthView: NSView!

  private var actionLabel: String!
  private var actionProc: ((ConnectionDetails) -> Void)!

  private var details: ConnectionDetails?
  private var sslKeyPathBookmark: String?
  private var sslCertPathBookmark: String?

  override func viewDidLoad() {
    super.viewDidLoad()

    actionButton.title = actionLabel
    actionButton.sizeToFit()

    let portFormatter = NumberFormatter()
    portFormatter.minimum = 1
    portFormatter.maximum = 65535
    portFormatter.allowsFloats = false
    bootstrapPortField.formatter = portFormatter
    httpProxyPortField.formatter = portFormatter

    if let details {
      nameField.stringValue = details.name
      bootstrapHostField.stringValue = details.bootstrapHost
      bootstrapPortField.integerValue = Int(details.bootstrapPort)
      if let httpProxyAddr = details.httpProxyAddr {
        enableHttpProxyCheckbox.state = .on
        let parts = httpProxyAddr.split(separator: ":")
        if parts.count == 2 {
          httpProxyHostField.stringValue = String(parts[0])
          httpProxyPortField.integerValue = Int(parts[1]) ?? 1080
        }
      }
      authMechanism = details.authMechanism
      if let username = details.username {
        usernameField.stringValue = username
      }
      if let passwordId = details.passwordId {
        switch Keychain.shared.get(passwordWithId: passwordId) {
        case .success(let password):
          switch details.authMechanism {
          case .plain, .scramSHA256, .scramSHA512:
            passwordField.stringValue = password
          case .aws:
            awsAccessKeySecretField.stringValue = password
          }
        default:
          ()
        }
      }
      if let region = details.awsRegion {
        awsRegionField.stringValue = region
      }
      if let accessKeyId = details.awsAccessKeyId {
        awsAccessKeyIdField.stringValue = accessKeyId
      }
      enableSslCheckbox.state = details.useSsl ? .on : .off
      sslKeyPathBookmark = details.sslKeyPath
      sslCertPathBookmark = details.sslCertPath
    }

    reset()
  }

  override func viewDidAppear() {
    view.window?.setContentSize(NSSize(width: 490, height: 200))
    view.window?.styleMask.remove(.resizable)
    view.window?.styleMask.update(with: .fullSizeContentView)
    reset()
  }

  func configure(actionLabel label: String,
                 details: ConnectionDetails? = nil,
                 _ proc: @escaping (ConnectionDetails) -> Void) {
    self.actionLabel = label
    self.actionProc = proc
    self.details = details
  }

  private var authMechanism: AuthMechanism {
    get {
      switch authMechanismButton.selectedTag() {
      case 0:
        return .plain
      case 1:
        return .scramSHA256
      case 2:
        return .scramSHA512
      case 3:
        return .aws
      default:
        preconditionFailure()
      }
    }
    set {
      switch newValue {
      case .plain:
        authMechanismButton.selectItem(withTag: 0)
      case .scramSHA256:
        authMechanismButton.selectItem(withTag: 1)
      case .scramSHA512:
        authMechanismButton.selectItem(withTag: 2)
      case .aws:
        authMechanismButton.selectItem(withTag: 3)
      }
    }
  }

  private func reset() {
    httpProxyView.removeFromSuperview()
    if enableHttpProxyCheckbox.state == .on {
      proxyView.setFrameSize(NSSize(width: 490, height: 26))
      proxyView.addSubview(httpProxyView)
      proxyViewHeightConstraint.constant = 26
      httpProxyView.setFrameOrigin(proxyView.bounds.origin)
    } else {
      proxyView.setFrameSize(NSSize(width: 490, height: 0))
      proxyViewHeightConstraint.constant = 0
    }

    plainAuthView.removeFromSuperview()
    awsAuthView.removeFromSuperview()
    switch authMechanism {
    case .plain, .scramSHA256, .scramSHA512:
      authView.setFrameSize(NSSize(width: 490, height: 26))
      authView.addSubview(plainAuthView)
      authViewHeightConstraint.constant = 26
      plainAuthView.setFrameOrigin(authView.bounds.origin)
    case .aws:
      authView.setFrameSize(NSSize(width: 490, height: 52))
      authView.addSubview(awsAuthView)
      awsAuthView.setFrameOrigin(authView.bounds.origin)
      authViewHeightConstraint.constant = 52
    }

    view.window?.setContentSize(NSSize(
      width: 490,
      height: 185 + proxyView.frame.height + authView.bounds.height
    ))
    sslKeyButton.isEnabled = enableSslCheckbox.state == .on
    sslKeyButton.title = sslKeyPathBookmark == nil ? "SSL Key..." : "SSL Key*..."
    sslCertButton.isEnabled = enableSslCheckbox.state == .on
    sslCertButton.title = sslCertPathBookmark == nil ? "SSL Cert...": "SSL Cert*..."

    testButton.image = nil
  }

  @IBAction func didToggleEnableHttpProxy(_ sender: Any) {
    reset()
  }

  @IBAction func didChangeAuthMechanism(_ sender: Any) {
    reset()
  }

  @IBAction func didToggleEnableSsl(_ sender: Any) {
    reset()
  }

  @IBAction func didPushSSLKeyButton(_ sender: Any) {
    displayOpenPanel(
      withTitle: "Select SSL Key",
      andAllowedContentTypes: [.pkcs12, .x509Certificate],
      andInitialPathBookmark: sslKeyPathBookmark
    ) { bookmark in
      sslKeyPathBookmark = bookmark
    }
  }

  @IBAction func didPushSSLCertButton(_ sender: Any) {
    displayOpenPanel(
      withTitle: "Select SSL Certificate",
      andAllowedContentTypes: [.pkcs12, .x509Certificate],
      andInitialPathBookmark: sslCertPathBookmark
    ) { bookmark in
      sslCertPathBookmark = bookmark
    }
  }

  private func displayOpenPanel(
    withTitle title: String,
    andAllowedContentTypes allowedContentTypes: [UTType],
    andInitialPathBookmark pathBookrmark: String?,
    completionHandler handler: (String?) -> Void
  ) {
    let panel = NSOpenPanel()
    panel.title = title
    panel.canChooseFiles = true
    panel.allowedContentTypes = allowedContentTypes
    if let pathBookrmark {
      var isStale = false
      let url = try? URL(
        resolvingBookmarkData: Data(base64Encoded: pathBookrmark.data(using: .utf8)!)!,
        options: [.withSecurityScope],
        bookmarkDataIsStale: &isStale
      )
      if let url, !isStale {
        panel.directoryURL = url.deletingLastPathComponent()
      }
    }
    let res = panel.runModal()
    switch res {
    case .OK:
      guard let url = panel.url else { return }
      guard let bookmark = try? url.bookmarkData(
        options: [.withSecurityScope, .securityScopeAllowOnlyReadAccess],
        includingResourceValuesForKeys: nil,
        relativeTo: nil
      ) else { return }
      handler(bookmark.base64EncodedString())
    default:
      handler(nil)
    }
    reset()
  }

  @IBAction func didPushCancelButton(_ sender: Any) {
    self.dismiss(self)
  }

  @IBAction func didPushTestButton(_ sender: Any) {
    guard let details = getDetails() else { return }
    testButton.image = nil
    testButton.isEnabled = false
    actionButton.isEnabled = false
    Backend.shared.testConnection(details).onComplete { [weak self] message in
      guard let self else { return }
      self.testButton.isEnabled = true
      self.actionButton.isEnabled = true
      if let message {
        Error.alert(withError: message)
      } else {
        testButton.image = NSImage(systemSymbolName: "checkmark.circle", accessibilityDescription: nil)
        testButton.imagePosition = .imageTrailing
      }
    }
  }

  @IBAction func didPushActionButton(_ sender: Any) {
    guard let details = getDetails() else { return }
    self.dismiss(self)
    actionProc(details)
  }

  private func getDetails() -> ConnectionDetails? {
    var username: String?
    var password: String?
    var awsRegion: String?
    var awsAccessKeyId: String?
    let authMechanism = self.authMechanism
    switch authMechanism {
    case .plain, .scramSHA256, .scramSHA512:
      username = usernameField.stringValue == "" ? nil : usernameField.stringValue
      password = passwordField.stringValue == "" ? nil : passwordField.stringValue
    case .aws:
      awsRegion = awsRegionField.stringValue == "" ? "us-east-1" : awsRegionField.stringValue
      if awsAccessKeyIdField.stringValue == "" {
        awsAccessKeyIdField.becomeFirstResponder()
        return nil
      }
      awsAccessKeyId = awsAccessKeyIdField.stringValue
      if awsAccessKeySecretField.stringValue == "" {
        awsAccessKeySecretField.becomeFirstResponder()
        return nil
      }
      password = awsAccessKeySecretField.stringValue
    }

    var passwordId = details?.passwordId
    if passwordId == nil {
      do {
        passwordId = try Backend.shared.generatePasswordId().wait()
      } catch {
        logger.error("failed to generate password id: \(error.localizedDescription)")
        return nil
      }
    }

    let bootstrapHost = (
      bootstrapHostField.stringValue == ""
      ? "127.0.0.1"
      : bootstrapHostField.stringValue.trimmingCharacters(in: .whitespacesAndNewlines)
    )
    let bootstrapPort = (
      bootstrapPortField.stringValue == ""
      ? 9092
      : UVarint(bootstrapPortField.integerValue)
    )

    var httpProxyAddr: String?
    if enableHttpProxyCheckbox.state == .on {
      let host = (
        httpProxyHostField.stringValue == ""
        ? "127.0.0.1"
        : httpProxyHostField.stringValue.trimmingCharacters(in: .whitespacesAndNewlines)
      )
      let port = (
        httpProxyPortField.stringValue == ""
        ? 1080
        : UVarint(httpProxyPortField.integerValue)
      )
      httpProxyAddr = "\(host):\(port)"
    }

    return ConnectionDetails(
      id: details?.id,
      name: nameField.stringValue == "" ? "Unnamed Connection" : nameField.stringValue,
      httpProxyAddr: httpProxyAddr,
      bootstrapHost: bootstrapHost,
      bootstrapPort: bootstrapPort,
      authMechanism: authMechanism,
      username: username,
      password: password,
      passwordId: password != nil ? passwordId : nil,
      awsRegion: awsRegion,
      awsAccessKeyId: awsAccessKeyId,
      useSsl: enableSslCheckbox.state == .on,
      sslKeyPath: sslKeyPathBookmark,
      sslCertPath: sslCertPathBookmark,
      schemaRegistryId: nil
    )
  }
}
