import NoiseSerde
import SwiftUI

struct WorkspaceBrokerDetailView: View {
  var id: UVarint
  var broker: Broker
  weak var delegate: WorkspaceDetailDelegate?

  @State private var configs = [ResourceConfig]()
  @State private var currentTab = Tab.info

  var body: some View {
    VStack(alignment: .leading) {
      HStack(alignment: .top) {
        VStack(alignment: .leading) {
          Text(broker.host).font(.title)
          Text("Broker").font(.subheadline).foregroundColor(.secondary)

          Tabs(
            items: [
              .init(id: .info, symbol: "info.circle.fill", shortcut: .init("1")),
              .init(id: .config, symbol: "gearshape.fill", shortcut: .init("2")),
            ],
            selection: $currentTab
          ) { item in
            switch item.id {
            case .info:
              Infos {
                Info(label: "Address", description: broker.address)
                Info(label: "Node ID", description: "\(broker.id)")
                Info(label: "Controller", description: broker.isController ? "yes" : "no", divider: broker.rack != nil)
                if let rack = broker.rack {
                  Info(label: "Rack", description: "\(rack)", divider: false)
                }
              }
            case .config:
              ResourceConfigTable(configs: $configs)
                .onAppear {
                  fetchConfig()
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

  private func fetchConfig() {
    guard let delegate else { return }
    let status = delegate.makeStatusProc()
    status("Fetching configs...")
    Backend.shared.getResourceConfigs(
      forResourceNamed: String(broker.id),
      resourceType: "broker",
      inWorkspace: id
    ).onComplete { configs in
      self.configs = configs
      status("Ready")
    }
  }
}

fileprivate enum Tab {
  case info
  case config
}
