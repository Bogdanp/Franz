import NoiseSerde
import SwiftUI

struct WorkspaceBrokerDetailView: View {
  var id: UVarint
  var broker: Broker

  @State private var configs = [ResourceConfig]()
  @State private var selectedConfigs = Set<String>()

  var body: some View {
    VStack(alignment: .leading) {
      HStack(alignment: .top) {
        VStack(alignment: .leading) {
          Text(broker.host).font(.title)
          Text("Broker").font(.subheadline).foregroundColor(.secondary)

          Infos {
            Info(label: "Address", description: broker.address)
            Info(label: "Node ID", description: "\(broker.id)")
            Info(label: "Controller", description: broker.isController ? "yes" : "no", divider: broker.rack != nil)
            if let rack = broker.rack {
              Info(label: "Rack", description: "\(rack)", divider: false)
            }
          }

          Table(configs, selection: $selectedConfigs) {
            TableColumn("Config", value: \.name)
            TableColumn("Value", value: \.nonnullValue)
          }
        }
        Spacer()
      }
      Spacer()
    }
    .padding()
    .onAppear {
      Backend.shared.getResourceConfigs(
        forResourceNamed: String(broker.id),
        resourceType: "broker",
        inWorkspace: id
      ).onComplete { configs in
        self.configs = configs
      }
    }
  }
}

// MARK: - Infos
struct Infos<Entries: View>: View {
  @ViewBuilder var content: () -> Entries

  var body: some View {
    VStack(alignment: .leading) {
      Spacer().frame(height: 15)
      Text("Information").font(.headline)
      Spacer().frame(height: 10)
      VStack(alignment: .leading, spacing: 3) {
        content()
      }
    }
  }
}

// MARK: - Info
struct Info: View {
  var label: String
  var description: String
  var divider = true

  var body: some View {
    VStack(alignment: .leading, spacing: 3) {
      HStack(alignment: .lastTextBaseline) {
        Text(label)
          .font(.system(size: 11, weight: .regular))
          .foregroundColor(.secondary)
          .frame(width: 90, alignment: .leading)
        Text(description)
          .font(.system(size: 11, weight: .semibold))
          .textSelection(.enabled)
      }
      if divider {
        Divider()
      }
    }
  }
}
