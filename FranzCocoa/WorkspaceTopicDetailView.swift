import NoiseSerde
import SwiftUI

struct WorkspaceTopicDetailView: View {
  var id: UVarint
  var topic: Topic

  @State private var configs = [ResourceConfig]()
  @State private var currentTab = Tab.info

  var body: some View {
    VStack(alignment: .leading) {
      HStack(alignment: .top) {
        VStack(alignment: .leading) {
          Text(topic.name).font(.title)
          Text("Topic").font(.subheadline).foregroundColor(.secondary)

          Tabs(
            items: [
              TabItem(label: .info, symbol: "info.circle.fill"),
              TabItem(label: .messages, symbol: "archivebox.fill"),
              TabItem(label: .groups, symbol: "circle.grid.3x3.fill"),
              TabItem(label: .config, symbol: "gearshape.fill"),
            ],
            selection: $currentTab
          ) { item in
            switch item.label {
            case .info:
              Infos {
                Info(label: "Partitions", description: String(topic.partitions.count))
                Info(label: "Internal", description: topic.isInternal ? "yes" : "no", divider: false)
              }
            case .messages:
              Text("messages")
            case .groups:
              Text("groups")
            case .config:
              ResourceConfigTable(configs: $configs)
            }
          }
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

fileprivate enum Tab {
  case info
  case messages
  case groups
  case config
}
