import NoiseSerde
import SwiftUI

struct BrokerDetailView: View {
  var id: UVarint
  var broker: Broker
  weak var delegate: WorkspaceDetailDelegate?

  @State private var configs = [ResourceConfig]()
  @State private var currentTab = TabState.shared.get(.brokerDetail) as? Tab ?? Tab.info

  var body: some View {
    VStack(alignment: .leading) {
      HStack(alignment: .top) {
        VStack(alignment: .leading) {
          Text(broker.host)
            .font(.title)
            .textSelection(.enabled)
          Text("Broker")
            .font(.subheadline)
            .foregroundColor(.secondary)

          Tabs(
            autosaveId: .brokerDetail,
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
              ResourceConfigTable(configs: $configs) { action in
                switch action {
                case .commit(let entries):
                  updateConfig(entries: entries)
                case .clear:
                  ()
                }
                fetchConfig()
              }.onAppear {
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
    status("Fetching Configs")
    Backend.shared.getResourceConfigs(
      forResourceNamed: String(broker.id),
      resourceType: "broker",
      inWorkspace: id
    ).onComplete { configs in
      self.configs = configs
      status("Ready")
    }
  }

  private func updateConfig(entries: [ResourceConfigEntry]) {
    guard let delegate else { return }
    let status = delegate.makeStatusProc()
    status("Updating Configs")
    var updates = [String: String]()
    for e in entries {
      updates[e.name] = e.value
    }
    Backend.shared.updateResourceConfigs(
      updates,
      forResourceNamed: String(broker.id),
      andResourceType: "broker",
      inWorkspace: id
    ).onComplete { _ in
      status("Ready")
    }
  }
}

fileprivate enum Tab {
  case info
  case config
}
