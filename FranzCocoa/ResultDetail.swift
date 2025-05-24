import Charts
import NoiseSerde
import SwiftUI

struct ResultDetail: View {
  let res: ApplyResult

  var body: some View {
    VSplitView {
      if let reduced = res.reduced {
        Result(reduced)
      }
      if res.output.count > 0 {
        Textarea(String(data: res.output, encoding: .utf8) ?? "", textColor: .secondaryLabelColor)
          .frame(minWidth: 400, minHeight: 120, maxHeight: 240)
      }
    }
    .background()
    .frame(minWidth: 400, minHeight: 240)
  }
}

fileprivate struct Result: View {
  let r: ReduceResult

  init(_ r: ReduceResult) {
    self.r = r
  }

  var body: some View {
    switch r {
    case .chart(let chart):
      ChartResult(chart)
    case .number(let n):
      TextResult(text: String(format: "%f", n))
    case .stack(let s):
      StackResult(s)
    case .table(let cols, let rows):
      TableResult(cols, rows)
        .frame(minWidth: 400, minHeight: 200)
    case .text(let s):
      TextResult(text: s)
    }
  }
}

fileprivate struct ChartResult: View {
  let chart: Chart

  init(_ chart: Chart) {
    self.chart = chart
  }

  var body: some View {
    GeometryReader { reader in
      AnyView(chartView())
        .contextMenu {
          Button {
            export(frame: reader.frame(in: .local))
          } label: {
            Label("Export...", systemImage: "square.and.arrow.down")
          }
        }
    }
    .padding(.all, 20)
    .frame(minWidth: 640, minHeight: 320)
  }

  private func chartView() -> any View {
    let view = Charts.Chart(pairs) { p in
      switch chart.style {
      case .area:
        try? p.areaMark(
          xLabel: chart.xLabel,
          yLabel: chart.yLabel
        )
      case .bar:
        try? p.barMark(
          xLabel: chart.xLabel,
          yLabel: chart.yLabel
        )
      case .candlestick(let width):
        try? p.candlestickMark(
          xLabel: chart.xLabel,
          yLabel: chart.yLabel,
          width: width
        )
      case .line:
        try? p.lineMark(
          xLabel: chart.xLabel,
          yLabel: chart.yLabel
        )
      case .scatter:
        try? p.pointMark(
          xLabel: chart.xLabel,
          yLabel: chart.yLabel
        )
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

  @MainActor
  private func export(frame: CGRect) {
    guard let data = ImageRenderer(content: AnyView(
      chartView()
        .padding(.all, 20)
        .background()
    ).frame(
      width: frame.width * 2,
      height: frame.height * 2
    )).nsImage?.tiffRepresentation else {
      return
    }

    let dialog = NSSavePanel()
    dialog.allowedContentTypes = [.init(filenameExtension: "tiff")!]
    dialog.isExtensionHidden = false
    switch dialog.runModal() {
    case .OK:
      guard let url = dialog.url else { return }
      try? data.write(to: url)
    default:
      return
    }
  }

  enum ChartError: Swift.Error {
    case badMarks
  }

  private struct CandlestickMark<X: Plottable, Y: Plottable>: ChartContent {
    let x: PlottableValue<X>
    let o: PlottableValue<Y>
    let h: PlottableValue<Y>
    let l: PlottableValue<Y>
    let c: PlottableValue<Y>
    let w: UVarint?

    var body: some ChartContent {
      RectangleMark(x: x, yStart: l, yEnd: h, width: 1)
      RectangleMark(x: x, yStart: o, yEnd: c, width: MarkDimension(integerLiteral: Int(w ?? 14)))
    }
  }

  private struct Pair: Identifiable {
    let id: Int
    let x: ChartValue
    let y: ChartValue

    func areaMark(xLabel: String, yLabel: String) throws -> AreaMark {
      switch (x, y) {
      case (.categorical(let xcat), .numerical(let y)):
        return AreaMark(x: .value(xLabel, xcat), y: .value(yLabel, y))
      case (.timestamp(let ts), .numerical(let y)):
        return AreaMark(
          x: .value(xLabel, Date(timeIntervalSince1970: Double(ts))),
          y: .value(yLabel, y)
        )
      default:
        throw ChartError.badMarks
      }
    }

    func barMark(xLabel: String, yLabel: String) throws -> BarMark {
      switch (x, y) {
      case (.categorical(let xcat), .categorical(let ycat)):
        return BarMark(x: .value(xLabel, xcat), y: .value(yLabel, ycat))
      case (.categorical(let xcat), .numerical(let y)):
        return BarMark(x: .value(xLabel, xcat), y: .value(yLabel, y))
      case (.numerical(let x), .categorical(let ycat)):
        return BarMark(x: .value(xLabel, x), y: .value(yLabel, ycat))
      case (.numerical(let x), .numerical(let y)):
        return BarMark(x: .value(xLabel, x), y: .value(yLabel, y))
      case (.timestamp(let ts), .numerical(let y)):
        return BarMark(
          x: .value(xLabel, Date(timeIntervalSince1970: Double(ts))),
          y: .value(yLabel, y)
        )
      default:
        throw ChartError.badMarks
      }
    }

    func candlestickMark(
      xLabel: String,
      yLabel: String,
      width: UVarint?
    ) throws -> some ChartContent {
      switch (x, y) {
      case (.timestamp(let ts), .candlestick(let o, let h, let l, let c)):
        return CandlestickMark(
          x: .value("Date", Date(timeIntervalSince1970: Double(ts))),
          o: .value("Open", o),
          h: .value("High", h),
          l: .value("Low", l),
          c: .value("Close", c),
          w: width
        )
        .foregroundStyle(o > c ? .red : .green)
      default:
        throw ChartError.badMarks
      }
    }

    func lineMark(xLabel: String, yLabel: String) throws -> LineMark {
      switch (x, y) {
      case (.categorical(let xcat), .categorical(let ycat)):
        return LineMark(x: .value(xLabel, xcat), y: .value(yLabel, ycat))
      case (.categorical(let xcat), .numerical(let y)):
        return LineMark(x: .value(xLabel, xcat), y: .value(yLabel, y))
      case (.numerical(let x), .categorical(let ycat)):
        return LineMark(x: .value(xLabel, x), y: .value(yLabel, ycat))
      case (.numerical(let x), .numerical(let y)):
        return LineMark(x: .value(xLabel, x), y: .value(yLabel, y))
      case (.timestamp(let ts), .numerical(let y)):
        return LineMark(
          x: .value(xLabel, Date(timeIntervalSince1970: Double(ts))),
          y: .value(yLabel, y)
        )
      default:
        throw ChartError.badMarks
      }
    }

    func pointMark(xLabel: String, yLabel: String) throws -> PointMark {
      switch (x, y) {
      case (.categorical(let xcat), .categorical(let ycat)):
        return PointMark(x: .value(xLabel, xcat), y: .value(yLabel, ycat))
      case (.categorical(let xcat), .numerical(let y)):
        return PointMark(x: .value(xLabel, xcat), y: .value(yLabel, y))
      case (.numerical(let x), .categorical(let ycat)):
        return PointMark(x: .value(xLabel, x), y: .value(yLabel, ycat))
      case (.numerical(let x), .numerical(let y)):
        return PointMark(x: .value(xLabel, x), y: .value(yLabel, y))
      case (.timestamp(let ts), .numerical(let y)):
        return PointMark(
          x: .value(xLabel, Date(timeIntervalSince1970: Double(ts))),
          y: .value(yLabel, y)
        )
      default:
        throw ChartError.badMarks
      }
    }
  }

  private var pairs: [Pair] {
    var pairs = [Pair]()
    for (i, p) in chart.pairs.enumerated() {
      pairs.append(.init(id: i, x: p.x, y: p.y))
    }
    return pairs
  }
}

fileprivate struct StackResult: View {
  let stack: Stack

  init(_ stack: Stack) {
    self.stack = stack
  }

  var body: some View {
    if stack.direction == "horizontal" {
      HStack {
        ForEach(children) { c in
          Result(c.data)
        }
      }
    } else {
      VStack {
        ForEach(children) { c in
          Result(c.data)
        }
      }
    }
  }

  private struct Child: Identifiable {
    let id: Int
    let data: ReduceResult
  }

  private var children: [Child] {
    var children = [Child]()
    for (i, c) in stack.children.enumerated() {
      children.append(.init(id: i, data: c))
    }
    return children
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
