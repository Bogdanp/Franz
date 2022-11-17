import Cocoa
import Dispatch
import Foundation
import OSLog
import SwiftUI

fileprivate let logger = Logger(
  subsystem: "io.defn.NoiseBackend",
  category: "Editor"
)

// MARK: EditorViewController
class EditorViewController: NSViewController {
  lazy var textStorage = NSTextStorage()
  lazy var layoutMgr = NSLayoutManager()
  lazy var textContainer = NSTextContainer()
  lazy var textView = EditorTextView(frame: .zero, textContainer: textContainer)
  lazy var scrollView = NSScrollView()

  var timer: Timer?
  var observation: NSKeyValueObservation?
  fileprivate var theme: Theme!

  override func loadView() {
    theme = darkModeOn() ? DarkTheme() : LightTheme()
    view = scrollView

    let contentSize = scrollView.contentSize
    textStorage.addLayoutManager(layoutMgr)
    textContainer.containerSize = NSSize(width: contentSize.width, height: CGFloat.greatestFiniteMagnitude)
    textContainer.widthTracksTextView = true
    layoutMgr.addTextContainer(textContainer)

    textView.allowsUndo = true
    textView.autoresizingMask = [.width, .height]
    textView.delegate = self
    textView.font = theme.font
    textView.focusRingType = .none
    textView.isAutomaticDashSubstitutionEnabled = false
    textView.isAutomaticQuoteSubstitutionEnabled = false
    textView.isAutomaticTextReplacementEnabled = false
    textView.isHorizontallyResizable = false
    textView.isVerticallyResizable = true
    textView.minSize = NSSize(width: 0, height: 0)
    textView.maxSize = NSSize(width: CGFloat.greatestFiniteMagnitude, height: CGFloat.greatestFiniteMagnitude)
    textView.setFrameSize(contentSize)

    scrollView.borderType = .noBorder
    scrollView.hasVerticalScroller = true
    scrollView.documentView = textView

    observation = NSApplication.shared.observe(\.effectiveAppearance) { [weak self] app, _ in
      app.effectiveAppearance.performAsCurrentDrawingAppearance {
        self?.theme = darkModeOn() ? DarkTheme() : LightTheme()
        DispatchQueue.main.async {
          self?.highlight()
        }
      }
    }
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
    timer = Timer.scheduledTimer(withTimeInterval: 1.0/30, repeats: false) { [weak self] _ in
      DispatchQueue.main.async {
        self?.highlight()
      }
    }
  }

  private func highlight() {
    let code = textStorage.string
    guard let tokens = Error.wait(Backend.shared.lexLua(code)) else {
      return
    }
    textView.backgroundColor = theme.background
    textView.textColor = theme.foreground
    textStorage.beginEditing()
    let all = NSRange(location: 0, length: textStorage.length)
    textStorage.removeAttribute(.foregroundColor, range: all)
    textStorage.removeAttribute(.font, range: all)
    textStorage.addAttribute(.foregroundColor, value: theme.foreground, range: all)
    textStorage.addAttribute(.font, value: theme.font, range: all)
    for token in tokens {
      let span = token.span.range
      if span.location + span.length > textStorage.length {
        logger.warning("token len=\(self.textStorage.length), range=\(span) skipped")
        continue
      }
      var color: NSColor!
      switch token.type {
      case .comment:
        color = theme.comment
      case .keyword:
        color = theme.keyword
      case .string:
        color = theme.string
      case .number:
        color = theme.number
      default:
        continue
      }
      textStorage.addAttribute(.foregroundColor, value: color!, range: span)
    }
    textStorage.endEditing()
  }
}

// MARK: - NSTextViewDelegate
extension EditorViewController: NSTextViewDelegate {
  func textDidChange(_ notification: Notification) {
    scheduleHighlight()
  }
}

// MARK: - EditorTextView
class EditorTextView: NSTextView {
  override func keyDown(with event: NSEvent) {
    if event.keyCode == 36 { // RET
      newlineAndIndent()
      return
    } else if event.keyCode == 48 { // TAB
      if event.modifierFlags.contains(.shift) {
        dedent()
        return
      }
      indent()
      return
    } else if event.charactersIgnoringModifiers == "\"" {
      insertPair(
        withStartingChar: "\"",
        andEndingChar: "\"",
        andCurrentChar: event.charactersIgnoringModifiers!.first!
      )
      return
    } else if event.charactersIgnoringModifiers == "'" {
      insertPair(
        withStartingChar: "'",
        andEndingChar: "'",
        andCurrentChar: event.charactersIgnoringModifiers!.first!
      )
      return
    } else if event.charactersIgnoringModifiers == "{" || event.charactersIgnoringModifiers == "}" {
      insertPair(
        withStartingChar: "{",
        andEndingChar: "}",
        andCurrentChar: event.charactersIgnoringModifiers!.first!
      )
      return
    }
    super.keyDown(with: event)
    maybeDedent()
  }

  private func attributedString(_ str: String) -> NSAttributedString {
    return NSAttributedString(string: str, attributes: [.font: font as Any])
  }

  private func indent() {
    textStorage?.insert(attributedString("  "), at: point)
    didChangeText()
  }

  private func dedent() {
    guard let textStorage else { return }
    let bol = saveExcursion { _ in
      moveToBeginningOfLine(self)
      return point
    }
    if string(at: NSRange(location: bol, length: 2)) == "  " {
      textStorage.deleteCharacters(in: NSRange(location: bol, length: 2))
      didChangeText()
    }
  }

  private func maybeDedent() {
    let shouldDedent = saveExcursion { pos in
      moveToBeginningOfLine(self)
      let bol = point
      let range = NSRange(location: bol, length: pos-bol)
      guard let modifier = match(regexp: "^( *)(elseif|else|end)", in: range) else {
        return false
      }

      moveUp(self)
      moveToBeginningOfLine(self)
      let pbol = point
      if pbol == bol {
        return false
      }
      moveToEndOfLine(self)
      let peol = point
      let prange = NSRange(location: pbol, length: peol-pbol)
      guard let pline = match(regexp: "^( *)", in: prange) else {
        return false
      }

      return pline.range(at: 1).length <= modifier.range(at: 1).length
    }
    if shouldDedent {
      dedent()
    }
  }

  private func newlineAndIndent() {
    guard let textStorage else { return }
    let indent = saveExcursion { _ in
      moveToBeginningOfLine(self)
      let bol = point
      moveToEndOfLine(self)
      let eol = point
      let range = NSRange(location: bol, length: eol-bol)
      guard let modifier = match(regexp: "^( *)(do|function|if|elseif|else|end)", in: range) else {
        guard let sibling = match(regexp: "^( *)", in: range) else { return 0 }
        return sibling.range(at: 1).length
      }

      if let range = Range(modifier.range(at: 2), in: textStorage.string),
         textStorage.string[range] == "end" {
        return modifier.range(at: 1).length
      }

      return modifier.range(at: 1).length + 2
    }

    insertNewline(nil)
    if indent > 0 {
      let str = attributedString(String(repeating: " ", count: indent))
      textStorage.insert(str, at: point)
    }
    didChangeText()
  }

  private func insertPair(withStartingChar s: Character, andEndingChar e: Character, andCurrentChar c: Character) {
    guard let textStorage else { return }
    let point = self.point
    if character(atPoint: point) == c {
      move(pointTo: point+1)
      return
    }
    textStorage.insert(attributedString(String([s, e])), at: point)
    move(pointTo: point+1)
    didChangeText()
  }
}

// MARK: - EditorTextView+Combinators
extension EditorTextView {
  private var point: Int {
    selectedRange().location
  }

  private func move(pointTo loc: Int) {
    setSelectedRange(NSRange(location: loc, length: 0))
  }

  private func saveExcursion<Res>(_ block: (Int) -> Res) -> Res {
    let point = self.point
    let dest = block(point)
    self.move(pointTo: point)
    return dest
  }

  private func character(atPoint point: Int) -> Character? {
    guard let str = textStorage?.string, point >= 0, point < str.count else { return nil }
    return str[str.index(str.startIndex, offsetBy: point)]
  }

  private func string(at range: NSRange) -> String? {
    guard let str = textStorage?.string else { return nil }
    guard let range = Range(range, in: str) else { return nil }
    return String(str[range])
  }

  private func lookingAt(regexp re: String, in range: NSRange) -> Bool {
    return match(regexp: re, in: range) != nil
  }

  private func match(regexp re: String, in range: NSRange) -> NSTextCheckingResult? {
    guard let str = textStorage?.string else { return nil }
    do {
      let re = try NSRegularExpression(pattern: re)
      return re.firstMatch(in: str, range: range)
    } catch {
      logger.error("invalid re. during match: \(error)")
      return nil
    }
  }
}

// MARK: - NSViewControllerRepresentable
struct Editor: NSViewControllerRepresentable {
  typealias NSViewControllerType = EditorViewController

  var code: String

  func makeNSViewController(context: Context) -> EditorViewController {
    let ctl = EditorViewController()
    ctl.configure(withCode: code)
    return ctl
  }

  func updateNSViewController(_ nsViewController: EditorViewController, context: Context) {
  }
}

// MARK: - Theme
protocol Theme {
  var font: NSFont { get }
  var background: NSColor { get }
  var foreground: NSColor { get }
  var comment: NSColor { get }
  var keyword: NSColor { get }
  var string: NSColor { get }
  var number: NSColor { get }
}

// MARK: - LightTheme
fileprivate class LightTheme: Theme {
  var font       = NSFont.monospacedSystemFont(ofSize: 14, weight: .regular)
  var background = rgb(0xFF, 0xFF, 0xFF)
  var foreground = rgb(0x1D, 0x1D, 0x1D)
  var comment    = rgb(0x4A, 0x56, 0x60)
  var keyword    = rgb(0x6C, 0x37, 0xAA)
  var string     = rgb(0xC5, 0x1B, 0x17)
  var number     = rgb(0x1C, 0x04, 0xCE)
}

// MARK: - DarkTheme
fileprivate class DarkTheme: Theme {
  var font       = NSFont.monospacedSystemFont(ofSize: 14, weight: .regular)
  var background = rgb(0x1F, 0x20, 0x24)
  var foreground = rgb(0xD8, 0xD8, 0xD9)
  var comment    = rgb(0x92, 0xA2, 0xB1)
  var keyword    = rgb(0xFD, 0x5F, 0xA3)
  var string     = rgb(0xFC, 0x6A, 0x5E)
  var number     = rgb(0xD1, 0xC0, 0x6A)
}

fileprivate func rgb(_ r: CGFloat, _ g: CGFloat, _ b: CGFloat) -> NSColor {
  return NSColor.init(srgbRed: r/255.0, green: g/255.0, blue: b/255.0, alpha: 1.0)
}

fileprivate func darkModeOn() -> Bool {
  return NSAppearance.currentDrawing().bestMatch(from: [.aqua, .darkAqua]) == .darkAqua
}
