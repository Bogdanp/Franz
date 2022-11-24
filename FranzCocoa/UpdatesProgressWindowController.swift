import Cocoa

class UpdatesProgressWindowController: NSWindowController {
  @IBOutlet weak var progressIndicator: NSProgressIndicator!

  private var closeHandler: (() -> Void)?

  convenience init() {
    self.init(windowNibName: "UpdatesProgressWindowController")
  }

  func configure(withCloseHandler hdl: @escaping () -> Void) {
    closeHandler = hdl
  }

  override func windowDidLoad() {
    super.windowDidLoad()

    window?.delegate = self
    window?.center()
  }
}

// MARK: - NSWindowDelegate
extension UpdatesProgressWindowController: NSWindowDelegate {
  func windowDidBecomeKey(_ notification: Notification) {
    progressIndicator.startAnimation(self)
  }

  func windowWillClose(_ notification: Notification) {
    progressIndicator.stopAnimation(self)
    closeHandler?()
  }
}
