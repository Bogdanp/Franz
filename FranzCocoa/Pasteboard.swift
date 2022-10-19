import AppKit
import Foundation

class Pasteboard {
  static func put(_ string: String) {
    NSPasteboard.general.clearContents()
    NSPasteboard.general.setString(string, forType: .string)
  }
}
