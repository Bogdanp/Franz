import Foundation
import SwiftUI

struct Tabs<Content: View, Label>: View where Label: Hashable {
  var items: [TabItem<Label>]
  @Binding var selection: Label
  @ViewBuilder var content: (TabItem<Label>) -> Content

  var body: some View {
    VStack {
      HStack {
        ForEach(items, id: \.label) { item in
          Button {
            selection = item.label
          } label: {
            Image(nsImage: .init(systemSymbolName: item.symbol, accessibilityDescription: nil)!)
          }
          .buttonStyle(.borderless)
          .tint(selection == item.label ? .accentColor : nil)
        }
      }
      Divider()
      if let item = items.first(where: { $0.label == selection }) {
        content(item)
      }
    }
  }
}

struct TabItem<Label: Hashable> {
  let label: Label
  let symbol: String
}
