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
        Textarea(String(data: res.output, encoding: .utf8) ?? "", textColor: .secondaryLabelColor)
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
