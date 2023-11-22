import Cocoa
import SpriteKit
import SwiftUI

class WelcomeWindowContentViewController: NSViewController {
  @IBOutlet weak var logoContainer: NSImageView!
  @IBOutlet weak var versionLabel: NSTextField!
  @IBOutlet weak var trialButton: NSButton!

  override func viewDidLoad() {
    super.viewDidLoad()
    logoContainer.addSubview(makeLogoView())

    let version = Bundle.main.object(forInfoDictionaryKey: "CFBundleShortVersionString") ?? "1"
    let build = Bundle.main.object(forInfoDictionaryKey: "CFBundleVersion") ?? "Unknown"
    versionLabel.stringValue = "Version \(version) (Build \(build))"

#if MAC_APP_STORE_BUILD
    trialButton.isHidden = true
#else
    trialButton.isHidden = true
    if Error.wait(Backend.shared.getLicense()) == nil {
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
#endif
  }

  private func makeLogoView() -> NSView {
    let xmasTime: Bool = {
      let components = Calendar.current.dateComponents([.month, .day], from: .now)
      guard let month = components.month, let day = components.day else {
        return false
      }
      let composite = month*100+day
      return composite <= 115 || composite >= 1115
    }()
    let sceneView = SKView()
    sceneView.allowsTransparency = true
    sceneView.frame = .init(x: 0, y: 0, width: 128, height: 128)
    sceneView.presentScene(makeLogoScene(forXmas: xmasTime))
    return sceneView
  }

  private func makeLogoScene(forXmas xmas: Bool = false) -> SKScene {
    let size = CGSize(width: 128, height: 128)
    let scene = SKScene(size: size)
    scene.anchorPoint = .init(x: 0, y: 0)
    scene.backgroundColor = .clear
    let sprite = SKSpriteNode(imageNamed: "AppIcon")
    if xmas {
      let frames = 25
      let animation = SKAction.animate(
        with: (0..<frames).map {
          SKTexture(imageNamed: String(format: "AppIconXmas-%02d", $0))
        },
        timePerFrame: 1/Double(frames)*2
      )
      sprite.run(SKAction.repeatForever(animation))
    }
    sprite.anchorPoint = .init(x: 0, y: 0)
    sprite.size = size
    scene.addChild(sprite)
    return scene
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
