import Cocoa

class DataViewController: NSViewController {
  private var data: Data!

  @IBOutlet weak var contentView: NSView!
  @IBOutlet weak var formatPopup: NSPopUpButton!
  @IBOutlet weak var formatButton: NSButton!

  private var contentViewConstraints = [NSLayoutConstraint]()

  private enum Format: Int {
    case binary
    case plain
    case json
  }

  private var format = Format.binary

  private var editorCtl = EditorViewController()

  override func viewDidLoad() {
    super.viewDidLoad()
    editorCtl.isEditable = false
    reset()
  }

  private func reset() {
    switch format {
    case .binary:
      formatButton.isEnabled = false
    case .plain:
      if let text = String(data: data, encoding: .utf8) {
        editorCtl.configure(code: text, language: .plain)
      }
      formatButton.isEnabled = false
    case .json:
      if let code = String(data: data, encoding: .utf8) {
        editorCtl.configure(code: code, language: .json)
      }
      formatButton.isEnabled = true
    }

    editorCtl.view.removeFromSuperviewWithoutNeedingDisplay()
    editorCtl.removeFromParent()
    NSLayoutConstraint.deactivate(contentViewConstraints)
    switch format {
    case .binary:
      ()
    case .plain, .json:
      addChild(editorCtl)
      contentView.addSubview(editorCtl.view)
      editorCtl.view.setFrameOrigin(.zero)
      editorCtl.view.setFrameSize(contentView.frame.size)
      editorCtl.view.translatesAutoresizingMaskIntoConstraints = false
      contentViewConstraints = fullSizeConstraints(
        forSubview: editorCtl.view,
        relativeTo: contentView
      )
      NSLayoutConstraint.activate(contentViewConstraints)
    }
  }

  func configure(withData data: Data) {
    self.data = data
  }

  @IBAction func didChangeFormat(_ sender: NSPopUpButton) {
    guard let format = Format(rawValue: sender.selectedTag()) else {
      return
    }
    self.format = format
    self.reset()
  }

  @IBAction func didPushFormatButton(_ sender: Any) {
    switch format {
    case .json:
      if let code = String(data: data, encoding: .utf8),
         let formatted = Error.wait(Backend.shared.ppJson(code)) {
        data = formatted.data(using: .utf8)
        reset()
      }
    default:
      ()
    }
  }
}
