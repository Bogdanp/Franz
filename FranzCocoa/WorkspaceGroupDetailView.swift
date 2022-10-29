import NoiseSerde
import SwiftUI

struct WorkspaceGroupDetailView: View {
  var id: UVarint
  var group: Group
  weak var delegate: WorkspaceDetailDelegate?

  @State var offsetsLoading = false
  @State var offsets: GroupOffsets?

  var totalLag: String? {
    guard let offsets else { return nil }
    var total = Varint(0)
    for t in offsets.topics {
      for p in t.partitions {
        total += p.lag
      }
    }
    return NumberFormatter.localizedString(from: NSNumber(value: total), number: .decimal)
  }

  let timer = Timer.publish(
    every: Double(Defaults.shared.reloadIntervalMs)/1000.0,
    on: .main,
    in: .common
  ).autoconnect()

  var body: some View {
    VStack(alignment: .leading) {
      HStack(alignment: .top) {
        VStack(alignment: .leading) {
          HStack(alignment: .top) {
            VStack(alignment: .leading) {
              Text(group.id)
                .font(.title)
                .truncationMode(.tail)
                .textSelection(.enabled)
              Text("Group")
                .font(.subheadline)
                .foregroundColor(.secondary)
              OffsetStatePill(state: offsets?.state ?? "loading")
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

          Spacer().frame(height: 15)
          GroupOffsetsTable(id: id, offsets: $offsets) {
            fetchOffsets()
          } openAction: { topic in
            delegate?.request(topicNamed: topic)
          }
          Spacer()
        }
        Spacer()
      }
      Spacer()
    }
    .padding()
    .onAppear {
      fetchOffsets()
    }
    .onReceive(timer) { _ in
      fetchOffsets()
    }
  }

  private func fetchOffsets() {
    guard let delegate else { return }
    guard !offsetsLoading else { return }
    offsetsLoading = true
    let status = delegate.makeStatusProc()
    status("Fetching offsets...")
    Backend.shared.fetchOffsets(forGroupNamed: group.id, inWorkspace: id).onComplete { offsets in
      self.offsets = offsets
      self.offsetsLoading = false
      status("Ready")
    }
  }
}

fileprivate struct OffsetStatePill: View {
  let state: Symbol

  var foregroundColor: Color {
    Color.white
  }

  var accentColor: Color {
    if state == "stable" {
      return Color.green
    } else if state == "dead" {
      return Color.red
    }
    return Color.secondary
  }

  var body: some View {
    Text("\(state.uppercased())")
      .font(.system(size: 9, weight: .semibold))
      .foregroundColor(foregroundColor)
      .padding([.leading, .trailing], 5)
      .padding([.top, .bottom], 2)
      .background(accentColor)
      .clipShape(RoundedRectangle(cornerRadius: 5))
  }
}
