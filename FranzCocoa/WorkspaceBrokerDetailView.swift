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
          if let rack = broker.rack {
            Info(label: "Rack", description: "\(rack)")
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

  var body: some View {
    VStack(alignment: .leading) {
      HStack(alignment: .top) {
        Text(label)
          .foregroundColor(.secondary)
          .fontWeight(.semibold)
          .frame(width: 80, alignment: .leading)
        Text(description)
      }
      Divider()
    }
  }
}
