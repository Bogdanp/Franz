import Foundation
import SwiftUI

struct Tabs<Content: View, ID: Hashable>: View {
  var items: [TabItem<ID>]
  @Binding var selection: ID
  @ViewBuilder var content: (TabItem<ID>) -> Content

  var body: some View {
    VStack {
      HStack(spacing: 10) {
        ForEach(items, id: \.id) { item in
          Button {
            selection = item.id
          } label: {
            Image(nsImage: .init(systemSymbolName: item.symbol, accessibilityDescription: nil)!)
          }
          .keyboardShortcut(item.shortcut)
          .buttonStyle(.borderless)
          .tint(selection == item.id ? .accentColor : nil)
        }
      }
      Divider()
      if let item = items.first(where: { $0.id == selection }) {
        content(item)
      }
    }
  }
}

struct TabItem<ID: Hashable> {
  let id: ID
  let symbol: String
  let shortcut: KeyboardShortcut?

  init(id: ID, symbol: String, shortcut: KeyboardShortcut? = nil) {
    self.id = id
    self.symbol = symbol
    self.shortcut = shortcut
  }
}
