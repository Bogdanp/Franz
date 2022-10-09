import AppKit

extension String {
  static var backspaceKeyEquivalent: String {
    get {
      String(Character(UnicodeScalar(NSBackspaceCharacter)!))
    }
  }
}
