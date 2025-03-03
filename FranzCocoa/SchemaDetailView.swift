import NoiseSerde
import SwiftUI

struct SchemaDetailView: View {
  var id: UVarint
  @State var schema: Schema
  weak var delegate: WorkspaceDetailDelegate?

  @State private var currentTab = TabState.shared.get(.schemaDetail) as? Tab ?? Tab.info

  private var type: String {
    guard let type = schema.type else { return "Unknown "}
    switch type {
    case .avro:
      return "AVRO"
    case .json:
      return "JSON"
    case .protobuf:
      return "Protobuf"
    }
  }

  @State private var code = ""

  var body: some View {
    VStack(alignment: .leading) {
      HStack(alignment: .top) {
        VStack(alignment: .leading) {
          Text(schema.name)
            .font(.title)
            .textSelection(.enabled)
          Text("Schema")
            .font(.subheadline)
            .foregroundColor(.secondary)

          Tabs(
            autosaveId: .schemaDetail,
            items: [
              .init(id: .info, symbol: "info.circle.fill", shortcut: .init("1")),
              .init(id: .schema, symbol: "doc.plaintext.fill", shortcut: .init("2")),
            ],
            selection: $currentTab
          ) { item in
            switch item.id {
            case .info:
              Infos {
                Info(
                  label: "Type",
                  description: type
                )
                Info(
                  label: "Latest Version",
                  description: schema.version.map { $0.description } ?? "",
                  divider: false
                )
              }
            case .schema:
              VStack(spacing: 0) {
                Editor(
                  code: $code,
                  language: schema.type == .protobuf ? .protobuf : .json,
                  border: .bezelBorder
                )
                SchemaEditorToolbar {
                  switch $0 {
                  case .save:
                    updateSchema()
                  case .check:
                    checkSchema()
                  case .reset:
                    fetchDetails()
                  }
                }
              }
            }
          }
        }
        Spacer()
      }
      Spacer()
    }
    .padding()
    .onAppear {
      fetchDetails()
    }
  }

  private func updateSchema() {
    guard let delegate else { return }
    guard let type = schema.type else { return }
    let status = delegate.makeStatusProc()
    status("Updating Schema")
    let typeSym = {
      switch type {
      case .avro:
        return "avro"
      case .json:
        return "json"
      case .protobuf:
        return "protobuf"
      }
    }()
    Backend.shared.createSchema(
      named: schema.name,
      ofType: typeSym,
      withSchema: code,
      inWorkspace: id
    ).onComplete {
      status("Ready")
      fetchDetails()
    }
  }

  private func checkSchema() {
    guard let delegate else { return }
    let status = delegate.makeStatusProc()
    status("Checking Schema")
    Backend.shared.checkSchema(
      named: schema.name,
      withUpdatedSchema: code,
      inWorkspace: id
    ).onComplete { ok in
      status(ok ? "Schema Compatible" : "Schema Not Compatible")
    }
  }

  private func fetchDetails() {
    guard let delegate else { return }
    let status = delegate.makeStatusProc()
    status("Fetching Schema")
    Backend.shared.getSchema(
      named: schema.name,
      inWorkspace: id
    ).onComplete { schema in
      self.schema = schema
      self.code = format(schema: schema)
      status("Ready")
    }
  }

  private func format(schema: Schema) -> String {
    guard let type = schema.type,
          let code = schema.schema else {
      return ""
    }
    switch type {
    case .avro, .json:
      return Error.wait(Backend.shared.ppJson(code: code)) ?? ""
    case .protobuf:
      return code
    }
  }
}

fileprivate enum Tab {
  case info
  case schema
}
