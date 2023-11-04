import SwiftUI

struct Textarea: NSViewRepresentable {
  typealias NSViewType = NSScrollView

  let text: String
  let textColor: NSColor

  init(_ text: String, textColor color: NSColor = .textColor) {
    self.text = text
    self.textColor = color
  }

  func makeNSView(context: NSViewRepresentableContext<Textarea>) -> NSViewType {
    let scrollView = NSTextView.scrollableTextView()
    scrollView.borderType = .noBorder
    scrollView.hasVerticalScroller = true
    scrollView.hasHorizontalScroller = true
    scrollView.autoresizingMask = [.width, .height]

    let textView = NSTextView()
    textView.font = .monospacedSystemFont(ofSize: 12, weight: .regular)
    textView.string = self.text
    textView.textColor = self.textColor
    scrollView.addSubview(textView)
    scrollView.documentView = textView

    return scrollView
  }

  func updateNSView(_ nsView: NSViewType, context: NSViewRepresentableContext<Textarea>) {
    nsView.backgroundColor = .textBackgroundColor
    nsView.borderType = .noBorder
    nsView.hasVerticalScroller = true
    nsView.hasHorizontalScroller = true
    nsView.autoresizingMask = [.width, .height]

    let contentSize = nsView.contentSize
    if let textView = nsView.documentView as? NSTextView {
      textView.backgroundColor = .textBackgroundColor
      textView.isEditable = false
      textView.isVerticallyResizable = true
      textView.isHorizontallyResizable = true
      textView.autoresizingMask = [.height, .width]
      textView.textContainer?.size = NSSize(width: contentSize.width, height: .greatestFiniteMagnitude)
      textView.textContainer?.widthTracksTextView = true
      textView.string = text
    }
  }
}
