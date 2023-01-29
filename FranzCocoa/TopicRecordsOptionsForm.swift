import NoiseSerde
import SwiftUI
import UniformTypeIdentifiers

enum ContentType: Codable {
  case binary
  case json
  case text

  var utType: UTType {
    switch self {
    case .binary:
      return .data
    case .json:
      return .json
    case .text:
      return .plainText
    }
  }

  var dataFormat: DataFormat {
    switch self {
    case .binary:
      return .binary
    case .json:
      return .json
    case .text:
      return .text
    }
  }
}

enum SortDirection: Codable {
  case asc
  case desc
}

final class TopicRecordsOptions: ObservableObject, Codable {
  @Published var keyFormat = ContentType.binary
  @Published var valueFormat = ContentType.binary
  @Published var sortDirection = SortDirection.desc
  @Published var maxMBScaled = 0.0
  @Published var keepMBScaled = 1.0

  var maxBytes: UVarint {
    Self.descale(maxMBScaled)*1024*1024
  }

  var keepBytes: UVarint {
    Self.descale(keepMBScaled)*1024*1024
  }

  struct Data: Codable {
    let keyFormat: ContentType
    let valueFormat: ContentType
    let sortDirection: SortDirection
    let maxMBScaled: Double
    let keepMBScaled: Double
  }

  init() {

  }

  init(from decoder: Decoder) throws {
    let data = try Data(from: decoder)
    self.keyFormat = data.keyFormat
    self.valueFormat = data.valueFormat
    self.sortDirection = data.sortDirection
    self.maxMBScaled = data.maxMBScaled
    self.keepMBScaled = data.keepMBScaled
  }

  func encode(to encoder: Encoder) throws {
    let data = Data(
      keyFormat: keyFormat,
      valueFormat: valueFormat,
      sortDirection: sortDirection,
      maxMBScaled: maxMBScaled,
      keepMBScaled: keepMBScaled
    )
    try data.encode(to: encoder)
  }

  static func descale(_ mbScaled: Double) -> UVarint {
    return UVarint(pow(2, mbScaled))
  }
}

struct TopicRecordsOptionsForm: View {
  @ObservedObject var model: TopicRecordsOptions

  let saveAction: () -> Void
  let jumpAction: (() -> Void)?

  init(model: TopicRecordsOptions, saveAction: @escaping () -> Void) {
    self.model = model
    self.saveAction = saveAction
    self.jumpAction = nil
  }

  init(model: TopicRecordsOptions, saveAction: @escaping () -> Void, jumpAction: @escaping () -> Void) {
    self.model = model
    self.saveAction = saveAction
    self.jumpAction = jumpAction
  }

  var bytesFmt: ByteCountFormatter = {
    let fmt = ByteCountFormatter()
    fmt.countStyle = .memory
    return fmt
  }()
  var formattedMaxMB: String {
    bytesFmt.string(fromByteCount: Int64(model.maxBytes))
  }
  var formattedKeepMB: String {
    bytesFmt.string(fromByteCount: Int64(model.keepBytes))
  }

  var body: some View {
    Form {
      Picker("Key Format:", selection: $model.keyFormat) {
        Text("Binary").tag(ContentType.binary)
        Text("JSON").tag(ContentType.json)
        Text("Text").tag(ContentType.text)
      }
      Picker("Value Format:", selection: $model.valueFormat) {
        Text("Binary").tag(ContentType.binary)
        Text("JSON").tag(ContentType.json)
        Text("Text").tag(ContentType.text)
      }
      Picker("Sort:", selection: $model.sortDirection) {
        Text("Ascending").tag(SortDirection.asc)
        Text("Descending").tag(SortDirection.desc)
      }
      VStack(alignment: .trailing, spacing: 0) {
        Slider(value: $model.maxMBScaled, in: 0...7, step: 1) {
          Text("Request Size:")
        }
        HStack(spacing: 2) {
          Text(formattedMaxMB)
            .font(.system(size: 10).monospacedDigit())
            .foregroundColor(.secondary)
          Image(systemName: "info.circle")
            .font(.system(size: 10))
            .foregroundColor(.secondary)
            .help("The maximum amount of data that will be retrieved per partition.")
        }
      }
      VStack(alignment: .trailing, spacing: 0) {
        Slider(value: $model.keepMBScaled, in: 0...10, step: 1) {
          Text("Buffer Size:")
        }
        HStack(spacing: 2) {
          Text(formattedKeepMB)
            .font(.system(size: 10).monospacedDigit())
            .foregroundColor(.secondary)
          Image(systemName: "info.circle")
            .font(.system(size: 10))
            .foregroundColor(.secondary)
            .help("The maximum amount of data that will be buffered in memory.")
        }
      }
      HStack {
        Button("Save") {
          saveAction()
        }
        .buttonStyle(.borderedProminent)
        .keyboardShortcut(.defaultAction)

        if let jumpAction {
          Button("Jump...") {
            jumpAction()
          }
        }
      }
    }
    .padding()
  }
}
