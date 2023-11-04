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
        case .table(let cols, let rows):
          TableResult(cols, rows)
            .frame(minWidth: 400, minHeight: 200)
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

fileprivate struct TableResult: NSViewRepresentable {
  typealias NSViewType = NSScrollView

  let cols: [String]
  let rows: [TableRow]

  init(_ cols: [String], _ rows: [TableRow]) {
    self.cols = cols
    self.rows = rows
  }

  class Coordinator: NSObject, NSTableViewDataSource, NSTableViewDelegate {
    var rows: [TableRow]

    init(_ rows: [TableRow]) {
      self.rows = rows
    }

    func numberOfRows(in tableView: NSTableView) -> Int {
      return self.rows.count
    }

    func tableView(_ tableView: NSTableView, objectValueFor tableColumn: NSTableColumn?, row: Int) -> Any? {
      guard let tableColumn else { return nil }
      guard let col = Int(tableColumn.identifier.rawValue, radix: 10) else { return nil }
      guard row < rows.count else { return nil }
      let theRow = rows[row]
      guard col < theRow.columns.count else { return nil }
      return theRow.columns[col]
    }

    func tableView(_ tableView: NSTableView, heightOfRow row: Int) -> CGFloat {
      return 18
    }

    func tableView(_ tableView: NSTableView, shouldEdit tableColumn: NSTableColumn?, row: Int) -> Bool {
      return false
    }
  }

  func makeCoordinator() -> Coordinator {
    return Coordinator(rows)
  }

  func makeNSView(context: Context) -> NSScrollView {
    let scrollView = NSScrollView()
    let tableView = NSTableView()
    tableView.dataSource = context.coordinator
    tableView.delegate = context.coordinator
    tableView.style = .plain
    scrollView.documentView = tableView
    scrollView.hasVerticalScroller = true
    return scrollView
  }

  func updateNSView(_ nsView: NSScrollView, context: Context) {
    guard let tableView = nsView.documentView as? NSTableView else { return }
    context.coordinator.rows = rows
    for column in tableView.tableColumns {
      tableView.removeTableColumn(column)
    }
    for (i, col) in cols.enumerated() {
      let nsCol = NSTableColumn(identifier: NSUserInterfaceItemIdentifier(String(format: "%d", i)))
      nsCol.title = col
      nsCol.width = 120
      tableView.addTableColumn(nsCol)
    }
    tableView.reloadData()
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
