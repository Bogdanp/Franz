import NoiseSerde
import SwiftUI

struct WorkspaceTopicDetailView: View {
  var id: UVarint
  var topic: Topic

  @State private var configs = [ResourceConfig]()
  @State private var currentTab: Tab? = Tab.info

  var body: some View {
    VStack(alignment: .leading) {
      HStack(alignment: .top) {
        VStack(alignment: .leading) {
          Text(topic.name).font(.title)
          Text("Topic").font(.subheadline).foregroundColor(.secondary)

          Infos {
            Info(label: "Partitions", description: String(topic.partitions.count))
            Info(label: "Internal", description: topic.isInternal ? "yes" : "no", divider: false)
          }

          Tabs(
            tabs: [
              TabItem(
                label: .info,
                symbol: "info.circle.fill"
              ),
              TabItem(
                label: .groups,
                symbol: "circle.grid.3x3.fill"
              ),
            ],
            selection: $currentTab
          ) { item in
            switch item.label {
            case .info:
              ResourceConfigTable(configs: $configs)
            case .groups:
              Text("groups")
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
  case groups
}
