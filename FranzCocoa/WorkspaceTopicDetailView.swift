import NoiseSerde
import SwiftUI

struct WorkspaceTopicDetailView: View {
  var id: UVarint
  var topic: Topic
  weak var delegate: WorkspaceDetailDelegate?

  @State private var configs = [ResourceConfig]()
  @State private var currentTab = TabState.shared.get(.topicDetail) as? Tab ?? Tab.info

  var body: some View {
    VStack(alignment: .leading) {
      HStack(alignment: .top) {
        VStack(alignment: .leading) {
          Text(topic.name)
            .font(.title)
            .textSelection(.enabled)
          Text("Topic")
            .font(.subheadline)
            .foregroundColor(.secondary)

          Tabs(
            autosaveId: .topicDetail,
            items: [
              .init(id: .info, symbol: "info.circle.fill", shortcut: .init("1")),
              .init(id: .messages, symbol: "archivebox.fill", shortcut: .init("2")),
              .init(id: .config, symbol: "gearshape.fill", shortcut: .init("3")),
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
    let status = delegate.makeStatusProc()
    status("Fetching configs...")
    Backend.shared.getResourceConfigs(
      forResourceNamed: topic.name,
      resourceType: "topic",
      inWorkspace: id
    ).onComplete { configs in
      self.configs = configs
      status("Ready")
    }
  }
}

fileprivate enum Tab {
  case info
  case messages
  case config
}
