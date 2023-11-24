import Cocoa

class JumpToLineWindowController: NSWindowController {
  @IBOutlet weak var textField: NSTextField!

  weak var delegate: JumpToLineWindowDelegate?

  private let numberFormatter = {
    let formatter = NumberFormatter()
    formatter.allowsFloats = false
    formatter.numberStyle = .none
    return formatter
  }()

  convenience init() {
    self.init(windowNibName: "JumpToLineWindowController")
  }

  override func windowDidLoad() {
    super.windowDidLoad()

    textField.delegate = self
    textField.focusRingType = .none
    textField.formatter = numberFormatter

    window?.delegate = self
    window?.center()
  }
}

// MARK: - JumpToLineWindowDelegate
protocol JumpToLineWindowDelegate: AnyObject {
  func willJumpToLine(_ line: Int) -> Bool
}

// MARK: - NSTextFieldDelegate
extension JumpToLineWindowController: NSTextFieldDelegate {
  func control(_ control: NSControl, textView: NSTextView, doCommandBy commandSelector: Selector) -> Bool {
    if commandSelector == #selector(cancelOperation(_:)) {
      WindowManager.shared.closeJumpToLineWindow()
      return true
    } else if commandSelector == #selector(insertNewline(_:)) {
      guard let delegate else { return false }
      if delegate.willJumpToLine(textField.integerValue) {
        WindowManager.shared.closeJumpToLineWindow()
        return true
      }
    }
    return false
  }
}

// MARK: - NSWindowDelegate
extension JumpToLineWindowController: NSWindowDelegate {
  func windowDidResignKey(_ notification: Notification) {
    WindowManager.shared.closeJumpToLineWindow()
  }
}
