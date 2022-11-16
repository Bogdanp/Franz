import Cocoa
import Foundation
import SwiftUI

struct CodeEditor: View {
  var body: some View {
    CodeField(code: "return {}")
  }
}

fileprivate func rgb(_ r: CGFloat, _ g: CGFloat, _ b: CGFloat) -> NSColor {
  return NSColor.init(srgbRed: r/255.0, green: g/255.0, blue: b/255.0, alpha: 1.0)
}

// MARK: CodeFieldController
class CodeFieldController: NSViewController {
  lazy var textStorage = NSTextStorage()
  lazy var layoutMgr = NSLayoutManager()
  lazy var textContainer = NSTextContainer()
  lazy var textView = NSTextView(frame: .zero, textContainer: textContainer)
  lazy var scrollView = NSScrollView()

  var timer: Timer?

  let font    = NSFont.monospacedSystemFont(ofSize: 14, weight: .regular)
  let comment = rgb(0x4A, 0x56, 0x60)
  let keyword = rgb(0x6C, 0x37, 0xAA)
  let string  = rgb(0xC5, 0x1B, 0x17)
  let number  = rgb(0x1C, 0x04, 0xCE)

  override func loadView() {
    view = scrollView

    let contentSize = scrollView.contentSize
    textStorage.addLayoutManager(layoutMgr)
    textContainer.containerSize = NSSize(width: contentSize.width, height: CGFloat.greatestFiniteMagnitude)
    textContainer.widthTracksTextView = true
    layoutMgr.addTextContainer(textContainer)

    textView.autoresizingMask = [.width, .height]
    textView.delegate = self
    textView.font = font
    textView.focusRingType = .none
    textView.isHorizontallyResizable = false
    textView.isVerticallyResizable = true
    textView.minSize = NSSize(width: 0, height: 0)
    textView.maxSize = NSSize(width: CGFloat.greatestFiniteMagnitude, height: CGFloat.greatestFiniteMagnitude)
    textView.setFrameSize(contentSize)

    scrollView.borderType = .noBorder
    scrollView.hasVerticalScroller = true
    scrollView.documentView = textView
  }

  override func viewDidLoad() {
    super.viewDidLoad()
    scheduleHighlight()
  }

  func configure(withCode code: String) {
    textStorage.setAttributedString(NSAttributedString(string: code))
  }

  private func scheduleHighlight() {
    timer?.invalidate()
    timer = Timer.scheduledTimer(withTimeInterval: 0.25, repeats: false) { [weak self] _ in
      self?.highlight()
    }
  }

  private func highlight() {
    let code = textStorage.string
    guard let tokens = Error.wait(Backend.shared.lexLua(code)) else {
      return
    }
    textStorage.beginEditing()
    let all = NSRange(location: 0, length: code.lengthOfBytes(using: .utf8))
    textStorage.removeAttribute(.foregroundColor, range: all)
    textStorage.removeAttribute(.font, range: all)
    textStorage.addAttribute(.font, value: font, range: all)
    for token in tokens {
      switch token.type {
      case .comment:
        textStorage.addAttribute(.foregroundColor, value: comment, range: token.span.range)
      case .keyword:
        textStorage.addAttribute(.foregroundColor, value: keyword, range: token.span.range)
      case .string:
        textStorage.addAttribute(.foregroundColor, value: string, range: token.span.range)
      case .number:
        textStorage.addAttribute(.foregroundColor, value: number, range: token.span.range)
      default:
        ()
      }
    }
    textStorage.endEditing()
  }
}

// MARK: - NSTextViewDelegate
extension CodeFieldController: NSTextViewDelegate {
  func textDidChange(_ notification: Notification) {
    scheduleHighlight()
  }
}

// MARK: - NSViewControllerRepresentable
struct CodeField: NSViewControllerRepresentable {
  typealias NSViewControllerType = CodeFieldController

  var code: String

  func makeNSViewController(context: Context) -> CodeFieldController {
    let ctl = CodeFieldController()
    ctl.configure(withCode: code)
    return ctl
  }

  func updateNSViewController(_ nsViewController: CodeFieldController, context: Context) {
  }
}
