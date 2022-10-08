import SwiftUI

struct WorkspaceTopicDetailView: View {
  var topic: Topic

  var body: some View {
    VStack(alignment: .leading) {
      HStack(alignment: .top) {
        VStack(alignment: .leading) {
          Text(topic.name).font(.title)
          Text("Topic").font(.subheadline).foregroundColor(.secondary)

          Spacer().frame(height: 15)
          Text("Information").font(.headline)
          Spacer().frame(height: 10)
          Info(label: "Partitions", description: String(topic.partitions.count))
          Info(label: "Internal", description: topic.isInternal ? "yes" : "no", divider: false)

          Spacer()
        }
        Spacer()
      }
      Spacer()
    }
    .padding()
  }
}
