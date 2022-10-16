import NoiseSerde
import SwiftUI

struct WorkspaceTopicDetailView: View {
  var id: UVarint
  var topic: Topic

  @State private var configs = [ResourceConfig]()

  var body: some View {
    VStack(alignment: .leading) {
      HStack(alignment: .top) {
        VStack(alignment: .leading) {
          Text(topic.name).font(.title)
          Text("Topic").font(.subheadline).foregroundColor(.secondary)

          Spacer().frame(height: 15)
          Text("Information").font(.headline)
          Spacer().frame(height: 10)
          Info(label: "Partitions", description: String(topic.partitions.count))
          Info(label: "Internal", description: topic.isInternal ? "yes" : "no", divider: false)

          ResourceConfigTable(configs: configs)
        }
        Spacer()
      }
      Spacer()
    }
    .padding()
    .onAppear {
      Backend.shared.getResourceConfigs(
        forResourceNamed: topic.name,
        resourceType: "topic",
        inWorkspace: id
      ).onComplete { configs in
        self.configs = configs
      }
    }
  }
}

struct ResourceConfigTable: View {
  var configs: [ResourceConfig]

  @State private var selected = Set<String>()

  var body: some View {
    Table(configs, selection: $selected) {
      TableColumn("Config", value: \.name)
      TableColumn("Value", value: \.nonnullValue)
    }
  }
}
