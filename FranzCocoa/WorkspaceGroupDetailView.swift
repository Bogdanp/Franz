import NoiseSerde
import SwiftUI

struct WorkspaceGroupDetailView: View {
  var id: UVarint
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
    .onAppear {
      Backend.shared.fetchOffsets(forGroupNamed: group.id, inWorkspace: id).onComplete { offsets in
        print("offsets=\(offsets)")
      }
    }
  }
}
