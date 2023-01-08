import NoiseSerde
import SwiftUI

struct SchemaDetailView: View {
  var id: UVarint
  @State var schema: Schema
  weak var delegate: WorkspaceDetailDelegate?

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

  private var code: String? {
    guard let type = schema.type,
          let code = schema.schema else {
      return nil
    }
    switch type {
    case .avro, .json:
      return Error.wait(Backend.shared.ppJson(code))
    case .protobuf:
      return code
    }
  }

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

          if let code {
            Spacer().frame(height: 15)
            Text("Schema")
              .font(.headline)
            Editor(
              code: code,
              language: .json,
              isEditable: false
            )
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

  private func fetchDetails() {
    guard let delegate else { return }
    let status = delegate.makeStatusProc()
    status("Fetching Schema")
    Backend.shared.getSchema(
      named: schema.name,
      inWorkspace: id
    ).onComplete { schema in
      self.schema = schema
      status("Ready")
    }
  }
}
