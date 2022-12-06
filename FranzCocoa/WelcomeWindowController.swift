import Cocoa

class WelcomeWindowController: NSWindowController {
  private var split: SplitViewController!
  private var newConnectionObserver: Any?

  convenience init() {
    self.init(windowNibName: "WelcomeWindowController")
  }

  override func windowDidLoad() {
    super.windowDidLoad()

    window?.delegate = self
    window?.isMovableByWindowBackground = true
    window?.setFrameAutosaveName("Welcome to Franz")
    window?.title = "Welcome to Franz"
    window?.center()

    let contentCtl = WelcomeWindowContentViewController()
    split = SplitViewController()
    split.addSplitViewItem(NSSplitViewItem(viewController: contentCtl))
    split.addSplitViewItem(NSSplitViewItem(sidebarWithViewController: WelcomeWindowConnectionsViewController()))
    contentViewController = split

    newConnectionObserver = NotificationCenter.default.addObserver(
      forName: .NewConnectionRequested,
      object: nil,
      queue: .main) { _ in
        contentCtl.newConnection()
    }
  }
}

// MARK: - NSWindowDelegate
extension WelcomeWindowController: NSWindowDelegate {
  func windowWillClose(_ notification: Notification) {
    if let observer = newConnectionObserver {
      NotificationCenter.default.removeObserver(observer)
      newConnectionObserver = nil
    }
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
    ofDividerAt dividerIndex: Int) -> NSRect {

      return .zero
  }
}

fileprivate class SplitView: NSSplitView {
  override var dividerThickness: CGFloat {
    0
  }
}
