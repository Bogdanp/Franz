import NoiseSerde
import SwiftUI

struct WorkspaceTopicDetailView: View {
  var id: UVarint
  var topic: Topic
  weak var delegate: WorkspaceDetailDelegate?

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
              .init(id: .info, symbol: "info.circle.fill"),
              .init(id: .messages, symbol: "archivebox.fill"),
              .init(id: .groups, symbol: "circle.grid.3x3.fill"),
              .init(id: .config, symbol: "gearshape.fill"),
            ],
            selection: $currentTab
          ) { item in
            switch item.id {
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
                .onAppear {
                  fetchConfigs()
                }
            }
          }
        }
        Spacer()
      }
      Spacer()
    }
    .padding()
  }

  private func fetchConfigs() {
    guard let delegate else { return }
    let cookie = delegate.makeStatusCookie()
    delegate.request(status: "Fetching configs...", withCookie: cookie)
    Backend.shared.getResourceConfigs(
      forResourceNamed: topic.name,
      resourceType: "topic",
      inWorkspace: id
    ).onComplete { configs in
      self.configs = configs
      delegate.request(status: "Ready", withCookie: cookie)
    }
  }
}

fileprivate enum Tab {
  case info
  case messages
  case groups
  case config
}
