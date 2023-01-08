import Foundation
import SwiftUI

struct Tabs<Content: View, ID: Hashable>: View {
  var autosaveId: TabAutosaveId?
  var items: [TabItem<ID>]
  @Binding var selection: ID
  @ViewBuilder var content: (TabItem<ID>) -> Content

  var body: some View {
    VStack {
      HStack(spacing: 10) {
        ForEach(items, id: \.id) { item in
          Button {
            withAnimation(.none) {
              selection = item.id
              if let autosaveId {
                TabState.shared.put(autosaveId, state: item.id)
              }
            }
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

// MARK: - TabItem
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

// MARK: - TabState
class TabState {
  static var shared = TabState()

  private var lastState = [TabAutosaveId: Any]()

  func get(_ id: TabAutosaveId) -> Any? {
    assert(Thread.isMainThread)
    return lastState[id]
  }

  func put(_ id: TabAutosaveId, state: Any) {
    assert(Thread.isMainThread)
    lastState[id] = state
  }
}

// MARK: - TabAutosaveId
enum TabAutosaveId {
  case brokerDetail
  case topicDetail
}
