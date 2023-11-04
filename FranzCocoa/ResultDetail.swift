import Charts
import SwiftUI

struct ResultDetail: View {
  let res: ApplyResult

  var body: some View {
    VSplitView {
      if let reduced = res.reduced {
        switch reduced {
        case .text(let s):
          TextResult(text: s)
        case .number(let n):
          TextResult(text: String(format: "%f", n))
        case .barChart(let xlabel, let xs, let ylabel, let ys):
          Chart(pairs(xs, ys), id: \.x) { p in
            BarMark(
              x: .value(xlabel, p.x),
              y: .value(ylabel, p.y)
            )
          }
          .padding(.all, 20)
          .frame(minWidth: 640, minHeight: 320)
          .background()
        case .lineChart(let xlabel, let xs, let ylabel, let ys):
          Chart(pairs(xs, ys), id: \.x) { p in
            LineMark(
              x: .value(xlabel, p.x),
              y: .value(ylabel, p.y)
            )
          }
          .padding(.all, 20)
          .frame(minWidth: 640, minHeight: 320)
          .background()
        default:
          Text("Renderer Missing")
            .font(.title)
            .padding(.all, 20)
        }
      }
      if res.output.count > 0 {
        Console(String(data: res.output, encoding: .utf8) ?? "")
          .frame(minWidth: 400, minHeight: 120, maxHeight: 240)
      }
    }
  }

  private struct Pair<K> {
    let x: K
    let y: Float64
  }

  private func pairs<K>(_ xs: [K], _ ys: [Float64]) -> [Pair<K>] {
    return Array(zip(xs, ys).map { (x, y) in Pair(x: x, y: y) })
  }
}

fileprivate struct TextResult: View {
  let text: String

  var body: some View {
    Text(text)
      .font(.title)
      .padding(.all, 20)
      .frame(minWidth: 400, minHeight: 200)
  }
}

fileprivate struct Console: NSViewRepresentable {
  typealias NSViewType = NSScrollView

  let output: String

  init(_ output: String) {
    self.output = output
  }

  func makeNSView(context: NSViewRepresentableContext<Console>) -> NSViewType {
    let scrollView = NSTextView.scrollableTextView()
    scrollView.borderType = .noBorder
    scrollView.hasVerticalScroller = true
    scrollView.hasHorizontalScroller = true
    scrollView.autoresizingMask = [.width, .height]

    let textView = NSTextView()
    textView.font = .monospacedSystemFont(ofSize: 12, weight: .regular)
    textView.textColor = .secondaryLabelColor
    textView.string = output
    scrollView.addSubview(textView)
    scrollView.documentView = textView

    return scrollView
  }

  func updateNSView(_ nsView: NSViewType, context: NSViewRepresentableContext<Console>) {
    nsView.backgroundColor = .textBackgroundColor
    nsView.borderType = .noBorder
    nsView.hasVerticalScroller = true
    nsView.hasHorizontalScroller = true
    nsView.autoresizingMask = [.width, .height]

    print("update")
    let contentSize = nsView.contentSize
    if let textView = nsView.documentView as? NSTextView {
      textView.backgroundColor = .textBackgroundColor
      textView.isEditable = false
      textView.isVerticallyResizable = true
      textView.isHorizontallyResizable = true
      textView.autoresizingMask = [.height, .width]
      textView.textContainer?.size = NSSize(width: contentSize.width, height: .greatestFiniteMagnitude)
      textView.textContainer?.widthTracksTextView = true
      textView.string = output
    }
  }
}
