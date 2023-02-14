import SwiftUI

enum SidebarTopicStat: Codable {
  case none
  case partitionCount
  case minLag
  case maxLag
  case sumLag
}

enum SidebarGroupStat: Codable {
  case none
  case minLag
  case maxLag
  case sumLag
}

final class SidebarOptions: ObservableObject, Codable {
  @Published var topicStat = SidebarTopicStat.partitionCount
  @Published var groupStat = SidebarGroupStat.sumLag

  struct Data: Codable {
    let topicStat: SidebarTopicStat
    let groupStat: SidebarGroupStat
  }

  init() {}

  init(from decoder: Decoder) throws {
    let data = try Data(from: decoder)
    self.topicStat = data.topicStat
    self.groupStat = data.groupStat
  }

  func encode(to encoder: Encoder) throws {
    let data = Data(topicStat: topicStat, groupStat: groupStat)
    try data.encode(to: encoder)
  }
}

struct SidebarOptionsForm: View {
  @ObservedObject var model: SidebarOptions

  let saveAction: () -> Void

  var body: some View {
    Form {
      Picker("Topic Stat:", selection: $model.topicStat) {
        Text("Off").tag(SidebarTopicStat.none)
        Text("Partition Count").tag(SidebarTopicStat.partitionCount)
        Text("Min Partition Lag").tag(SidebarTopicStat.minLag)
        Text("Max Partition Lag").tag(SidebarTopicStat.maxLag)
        Text("Sum Partition Lag").tag(SidebarTopicStat.sumLag)
      }
      Picker("Group Stat:", selection: $model.groupStat) {
        Text("Off").tag(SidebarGroupStat.none)
        Text("Min Lag").tag(SidebarGroupStat.minLag)
        Text("Max Lag").tag(SidebarGroupStat.maxLag)
        Text("Sum Lag").tag(SidebarGroupStat.sumLag)
      }
      Button("Save") {
        saveAction()
      }
      .buttonStyle(.borderedProminent)
      .keyboardShortcut(.defaultAction)
    }
    .padding()
  }
}
