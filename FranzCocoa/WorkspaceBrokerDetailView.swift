import SwiftUI

struct WorkspaceBrokerDetailView: View {
  var broker: Broker

  var body: some View {
    VStack(alignment: .leading) {
      HStack(alignment: .top) {
        VStack(alignment: .leading) {
          Text(broker.address).font(.title)
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

          Spacer()
        }
        Spacer()
      }
      Spacer()
    }
    .padding()
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
