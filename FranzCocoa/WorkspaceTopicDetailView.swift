import NoiseSerde
import SwiftUI

struct WorkspaceTopicDetailView: View {
  var id: UVarint
  var topic: Topic

  @State private var configsLoading = true
  @State private var configs = [ResourceConfig]()
  @State private var selectedConfigs = Set<String>()

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

          if !configsLoading {
            Table(configs, selection: $selectedConfigs) {
              TableColumn("Config", value: \.name)
              TableColumn("Value", value: \.nonnullValue)
            }
          }
        }
        Spacer()
      }
      Spacer()
    }
    .padding()
    .onAppear {
      self.configsLoading = true
      Backend.shared.getResourceConfigs(
        withId: id,
        resourceType: "topic",
        resourceName: topic.name
      ).onComplete { configs in
        self.configs = configs
        self.configsLoading = false
      }
    }
  }
}
