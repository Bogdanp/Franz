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
          Chart(pairs(xs, ys)) { p in
            BarMark(
              x: .value(xlabel, p.x),
              y: .value(ylabel, p.y)
            )
          }
          .padding(.all, 20)
          .frame(minWidth: 640, minHeight: 320)
        case .lineChart(let xlabel, let xs, let ylabel, let ys):
          Chart(pairs(xs, ys)) { p in
            LineMark(
              x: .value(xlabel, p.x),
              y: .value(ylabel, p.y)
            )
          }
          .padding(.all, 20)
          .frame(minWidth: 640, minHeight: 320)
        case .scatterChart(let xlabel, let xs, let ylabel, let ys):
          Chart(pairs(xs, ys)) { p in
            PointMark(
              x: .value(xlabel, p.x),
              y: .value(ylabel, p.y)
            )
          }
          .chartXScale(domain: [xs.min() ?? 0, xs.max() ?? 0])
          .padding(.all, 20)
          .frame(minWidth: 640, minHeight: 320)
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
    .background()
  }

  private struct Pair<K>: Identifiable {
    let id: Int
    let x: K
    let y: Float64
  }

  private func pairs<K>(_ xs: [K], _ ys: [Float64]) -> [Pair<K>] {
    var pairs = [Pair<K>]()
    for (i, (x, y)) in zip(xs, ys).enumerated() {
      pairs.append(.init(id: i, x: x, y: y))
    }
    return pairs
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
