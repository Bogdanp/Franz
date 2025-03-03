import Cocoa

enum DataFormat: Int {
  case binary
  case json
  case text
}

class DataViewController: NSViewController {
  private var data: Data!

  @IBOutlet weak var contentView: NSView!
  @IBOutlet weak var formatPopup: NSPopUpButton!
  @IBOutlet weak var formatButton: NSButton!

  private var contentViewConstraints = [NSLayoutConstraint]()

  private var format = DataFormat.binary

  private var binaryCtl = HexViewerViewController()
  private var editorCtl = EditorViewController()

  override func viewDidLoad() {
    super.viewDidLoad()
    editorCtl.isEditable = false
    reset()
  }

  private func reset() {
    formatPopup.selectItem(withTag: format.rawValue)
    formatButton.isEnabled = false
    switch format {
    case .binary:
      binaryCtl.configure(withData: data)
    case .json:
      if let code = String(data: data, encoding: .utf8) {
        editorCtl.configure(code: code, language: .json)
      } else {
        editorCtl.configure(code: "", language: .json)
      }
      formatButton.isEnabled = true
    case .text:
      editorCtl.configure(code: String(decoding: data, as: UTF8.self), language: .plain)
    }

    binaryCtl.view.removeFromSuperviewWithoutNeedingDisplay()
    binaryCtl.removeFromParent()
    editorCtl.view.removeFromSuperviewWithoutNeedingDisplay()
    editorCtl.removeFromParent()
    NSLayoutConstraint.deactivate(contentViewConstraints)
    switch format {
    case .binary:
      addChild(binaryCtl)
      contentView.addSubview(binaryCtl.view)
      binaryCtl.view.setFrameOrigin(.zero)
      binaryCtl.view.setFrameSize(contentView.frame.size)
    case  .json, .text:
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

  func configure(withData data: Data, andFormat format: DataFormat = .binary) {
    self.data = data
    self.format = format
  }

  @IBAction func didChangeFormat(_ sender: NSPopUpButton) {
    guard let format = DataFormat(rawValue: sender.selectedTag()) else {
      return
    }
    self.format = format
    self.reset()
  }

  @IBAction func didPushFormatButton(_ sender: Any) {
    switch format {
    case .json:
      if let code = String(data: data, encoding: .utf8),
         let formatted = Error.wait(Backend.shared.ppJson(code: code)) {
        data = formatted.data(using: .utf8)
        reset()
      }
    default:
      ()
    }
  }
}
