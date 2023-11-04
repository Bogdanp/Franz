import Charts
import SwiftUI

struct ResultDetail: View {
  let res: ApplyResult

  var body: some View {
    VSplitView {
      if let reduced = res.reduced {
        switch reduced {
        case .chart(let chart):
          ChartResult(chart)
        case .number(let n):
          TextResult(text: String(format: "%f", n))
        case .table:
          Text("table") // FIXME
        case .text(let s):
          TextResult(text: s)
        }
      }
      if res.output.count > 0 {
        Textarea(String(data: res.output, encoding: .utf8) ?? "", textColor: .secondaryLabelColor)
          .frame(minWidth: 400, minHeight: 120, maxHeight: 240)
      }
    }
    .background()
  }
}

fileprivate struct ChartResult: View {
  let chart: Chart

  init(_ chart: Chart) {
    self.chart = chart
  }

  var body: some View {
    AnyView(chartView())
      .padding(.all, 20)
      .frame(minWidth: 640, minHeight: 320)
  }

  private func chartView() -> any View {
    let view = Charts.Chart(pairs(chart.xs, chart.ys)) { p in
      switch chart.style {
      case .bar:
        p.barMark(xLabel: chart.xLabel, yLabel: chart.yLabel)
      case .line:
        p.lineMark(xLabel: chart.xLabel, yLabel: chart.yLabel)
      case .scatter:
        p.pointMark(xLabel: chart.xLabel, yLabel: chart.yLabel)
      }
    }
    switch (chart.xScale, chart.yScale) {
    case (.none, .none):
      return view
    case (.numerical(let lo, let hi, _), .none):
      return view.chartXScale(domain: lo...hi, type: .linear)
    case (.none, .numerical(let lo, let hi, _)):
      return view.chartYScale(domain: lo...hi, type: .linear)
    case (.numerical(let xlo, let xhi, _), .numerical(let ylo, let yhi, _)):
      return view
        .chartXScale(domain: xlo...xhi, type: .linear)
        .chartYScale(domain: ylo...yhi, type: .linear)
    }
  }

  private struct Pair: Identifiable {
    let id: Int
    let x: ChartValue
    let y: ChartValue

    func barMark(xLabel: String, yLabel: String) -> BarMark {
      switch (x, y) {
      case (.categorical(let xcat), .categorical(let ycat)):
        return BarMark(x: .value(xLabel, xcat), y: .value(yLabel, ycat))
      case (.categorical(let xcat), .numerical(let y)):
        return BarMark(x: .value(xLabel, xcat), y: .value(yLabel, y))
      case (.numerical(let x), .categorical(let ycat)):
        return BarMark(x: .value(xLabel, x), y: .value(yLabel, ycat))
      case (.numerical(let x), .numerical(let y)):
        return BarMark(x: .value(xLabel, x), y: .value(yLabel, y))
      }
    }

    func lineMark(xLabel: String, yLabel: String) -> LineMark {
      switch (x, y) {
      case (.categorical(let xcat), .categorical(let ycat)):
        return LineMark(x: .value(xLabel, xcat), y: .value(yLabel, ycat))
      case (.categorical(let xcat), .numerical(let y)):
        return LineMark(x: .value(xLabel, xcat), y: .value(yLabel, y))
      case (.numerical(let x), .categorical(let ycat)):
        return LineMark(x: .value(xLabel, x), y: .value(yLabel, ycat))
      case (.numerical(let x), .numerical(let y)):
        return LineMark(x: .value(xLabel, x), y: .value(yLabel, y))
      }
    }

    func pointMark(xLabel: String, yLabel: String) -> PointMark {
      switch (x, y) {
      case (.categorical(let xcat), .categorical(let ycat)):
        return PointMark(x: .value(xLabel, xcat), y: .value(yLabel, ycat))
      case (.categorical(let xcat), .numerical(let y)):
        return PointMark(x: .value(xLabel, xcat), y: .value(yLabel, y))
      case (.numerical(let x), .categorical(let ycat)):
        return PointMark(x: .value(xLabel, x), y: .value(yLabel, ycat))
      case (.numerical(let x), .numerical(let y)):
        return PointMark(x: .value(xLabel, x), y: .value(yLabel, y))
      }
    }
  }

  private func pairs(_ xs: [ChartValue], _ ys: [ChartValue]) -> [Pair] {
    var pairs = [Pair]()
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
