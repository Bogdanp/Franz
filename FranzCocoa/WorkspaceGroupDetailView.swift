import SwiftUI

struct WorkspaceGroupDetailView: View {
  var group: Group

  var body: some View {
    VStack(alignment: .leading) {
      HStack(alignment: .top) {
        VStack(alignment: .leading) {
          Text(group.id).font(.title)
          Text("Group").font(.subheadline).foregroundColor(.secondary)

          Spacer()
        }
        Spacer()
      }
      Spacer()
    }
    .padding()
  }
}
