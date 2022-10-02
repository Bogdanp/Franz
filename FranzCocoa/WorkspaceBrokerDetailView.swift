import SwiftUI

struct WorkspaceBrokerDetailView: View {
  var broker: Broker

  var body: some View {
    VStack(alignment: .leading) {
      HStack(alignment: .top) {
        Text("Broker \(broker.host):\(String(broker.port))")
          .font(.title)
        Spacer()
      }
      Spacer()
    }
    .padding()
  }
}
