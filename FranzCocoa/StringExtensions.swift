import AppKit

extension String {
  static var backspaceKeyEquivalent: String {
    String(Character(UnicodeScalar(NSBackspaceCharacter)!))
  }
}
