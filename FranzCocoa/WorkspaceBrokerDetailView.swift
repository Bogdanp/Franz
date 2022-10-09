import NoiseSerde
import SwiftUI

struct WorkspaceBrokerDetailView: View {
  var id: UVarint
  var broker: Broker

  @State private var configsLoading = true
  @State private var configs = [ResourceConfig]()
  @State private var selectedConfigs = Set<String>()

  var body: some View {
    VStack(alignment: .leading) {
      HStack(alignment: .top) {
        VStack(alignment: .leading) {
          Text(broker.host).font(.title)
          Text("Broker").font(.subheadline).foregroundColor(.secondary)

          Spacer().frame(height: 15)
          Text("Information").font(.headline)
          Spacer().frame(height: 10)
          Info(label: "Address", description: broker.address)
          Info(label: "Node ID", description: "\(broker.id)")
          Info(label: "Controller", description: broker.isController ? "yes" : "no", divider: broker.rack != nil)
          if let rack = broker.rack {
            Info(label: "Rack", description: "\(rack)", divider: false)
          }

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
        resourceType: "cluster",
        resourceName: "1"
      ).onComplete { configs in
        self.configs = configs
        self.configsLoading = false
      }
    }
  }
}

struct Info: View {
  var label: String
  var description: String
  var divider = true

  var body: some View {
    VStack(alignment: .leading) {
      HStack(alignment: .lastTextBaseline) {
        Text(label)
          .font(.system(size: 11, weight: .semibold))
          .foregroundColor(.secondary)
          .frame(width: 65, alignment: .leading)
        Text(description)
          .fontWeight(.semibold)
      }
      if divider {
        Divider()
      }
    }
  }
}
