import NoiseSerde
import SwiftUI

struct WorkspaceGroupDetailView: View {
  var id: UVarint
  var group: Group
  var delegate: WorkspaceDetailDelegate?

  @State var offsets: GroupOffsets?

  var totalLag: String? {
    get {
      guard let offsets else { return nil }
      var total = Varint(0)
      for t in offsets.topics {
        for p in t.partitions {
          total += p.lag
        }
      }
      return NumberFormatter.localizedString(from: NSNumber(value: total), number: .decimal)
    }
  }

  var body: some View {
    VStack(alignment: .leading) {
      HStack(alignment: .top) {
        VStack(alignment: .leading) {
          HStack(alignment: .top) {
            VStack(alignment: .leading) {
              Text(group.id)
                .font(.title)
                .truncationMode(.tail)
              Text("Group")
                .font(.subheadline)
                .foregroundColor(.secondary)
            }

            if let totalLag {
              Spacer()
              VStack(alignment: .trailing) {
                Text("Messages behind:")
                  .font(.subheadline)
                  .foregroundColor(.secondary)
                Text(totalLag)
                  .font(.title)
                  .fontWeight(.semibold)
                  .monospacedDigit()
              }
            }
          }

          if let offsets {
            Spacer().frame(height: 15)
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
