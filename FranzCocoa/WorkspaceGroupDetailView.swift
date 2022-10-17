import NoiseSerde
import SwiftUI

struct WorkspaceGroupDetailView: View {
  var id: UVarint
  var group: Group
  var delegate: WorkspaceDetailDelegate?

  @State var offsets: GroupOffsets?

  var body: some View {
    VStack(alignment: .leading) {
      HStack(alignment: .top) {
        VStack(alignment: .leading) {
          Text(group.id).font(.title)
          Text("Group").font(.subheadline).foregroundColor(.secondary)

          if let offsets {
            Text("Topics").font(.headline)
            GroupOffsetsTable(offsets: offsets)
          }
          Spacer()
        }
        Spacer()
      }
      Spacer()
    }
    .padding()
    .onAppear {
      guard let delegate else { return }
      let cookie = delegate.makeStatusCookie()
      delegate.request(status: "Fetching offsets...", withCookie: cookie)
      Backend.shared.fetchOffsets(forGroupNamed: group.id, inWorkspace: id).onComplete { offsets in
        delegate.request(status: "Ready", withCookie: cookie)
        self.offsets = offsets
      }
    }
  }
}
