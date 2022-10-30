import AppKit

extension String {
  static func utf8(from data: Data, withMaxLength maxLength: Int) -> String? {
    if data.count < maxLength {
      return String(data: data, encoding: .utf8)
    }
    for i in (0..<3) {
      if var string = String(data: data[..<(maxLength + i)], encoding: .utf8) {
        string += "…"
        return string
      }
    }
    return nil
  }

  static var backspaceKeyEquivalent: String {
    String(Character(UnicodeScalar(NSBackspaceCharacter)!))
  }
}
