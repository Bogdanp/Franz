import AppKit
import Foundation

class MainMenu {
  static let shared = MainMenu()

  func find(itemByPath path: [NSUserInterfaceItemIdentifier]) -> NSMenuItem? {
    guard var items = NSApplication.shared.mainMenu?.items else {
      return nil
    }
    guard var head = path.first else {
      return nil
    }
    var tail = path.dropFirst()
  top:
    while true {
      for item in items {
        print("item=\(item.identifier)")
        guard let id = item.identifier else {
          continue
        }
        if id == head {
          if tail.isEmpty {
            return item
          } else {
            head = tail.first!
            tail = tail.dropFirst()
            if let submenu = item.submenu {
              items = submenu.items
            }
            continue top
          }
        }
      }
      return nil
    }
  }
}
