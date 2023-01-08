import Cocoa

class WelcomeWindowController: NSWindowController {
  private var contentCtl: WelcomeWindowContentViewController!
  private var splitCtl: SplitViewController!

  convenience init() {
    self.init(windowNibName: "WelcomeWindowController")
  }

  override func windowDidLoad() {
    super.windowDidLoad()

    window?.isMovableByWindowBackground = true
    window?.setFrameAutosaveName("Welcome to Franz")
    window?.title = "Welcome to Franz"
    window?.center()

    contentCtl = WelcomeWindowContentViewController()
    splitCtl = SplitViewController()
    splitCtl.addSplitViewItem(NSSplitViewItem(viewController: contentCtl))
    splitCtl.addSplitViewItem(NSSplitViewItem(sidebarWithViewController: WelcomeWindowConnectionsViewController()))
    contentViewController = splitCtl
  }

  @objc func newConnection(_ sender: Any) {
    contentCtl.newConnection()
  }
}

// MARK: - WelcomeWindow
class WelcomeWindow: NSWindow {
  override func keyDown(with event: NSEvent) {
    switch event.keyCode {
    case 53: // ESC
      WindowManager.shared.closeWelcomeWindow()
    default:
      super.keyDown(with: event)
    }
  }
}

// MARK: - NSSplitView
fileprivate class SplitView: NSSplitView {
  override var dividerThickness: CGFloat {
    0
  }
}

// MARK: - NSSplitViewController
fileprivate class SplitViewController: NSSplitViewController {
  override func loadView() {
    splitView = SplitView()
    splitView.isVertical = true // true = horizontal
    super.loadView()
  }

  override func splitView(
    _ splitView: NSSplitView,
    effectiveRect proposedEffectiveRect: NSRect,
    forDrawnRect drawnRect: NSRect,
    ofDividerAt dividerIndex: Int
  ) -> NSRect {
      return .zero
  }
}
