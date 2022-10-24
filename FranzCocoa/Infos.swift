import Foundation
import SwiftUI

struct Infos<Entries: View>: View {
  @ViewBuilder var content: () -> Entries

  var body: some View {
    VStack(alignment: .leading) {
      Spacer().frame(height: 15)
      Text("Information").font(.headline)
      Spacer().frame(height: 10)
      VStack(alignment: .leading, spacing: 3) {
        content()
      }
    }
  }
}

// MARK: - Info
struct Info: View {
  var label: String
  var description: String
  var divider = true

  var body: some View {
    VStack(alignment: .leading, spacing: 3) {
      HStack(alignment: .lastTextBaseline) {
        Text(label)
          .font(.system(size: 11, weight: .regular))
          .foregroundColor(.secondary)
          .frame(width: 90, alignment: .leading)
        Text(description)
          .font(.system(size: 11, weight: .semibold))
          .textSelection(.enabled)
      }
      if divider {
        Divider()
      }
    }
  }
}
