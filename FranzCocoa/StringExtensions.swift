import AppKit

extension String {
  static func text(from data: Data, withMaxLength maxLength: Int) -> String? {
    var string: String?
    if data.count < maxLength {
      string = String(data: data, encoding: .utf8)
    } else {
      for i in (0..<3) {
        string = String(data: data[..<(maxLength + i)], encoding: .utf8)
        if var str = string {
          str += "…"
          break
        }
      }
    }
    if let str = string, !str.hasControlCharacters() {
      return str
    }
    return nil
  }

  private func hasControlCharacters() -> Bool {
    return unicodeScalars.first { scalar in
      let cat = scalar.properties.generalCategory
      return cat == .control || cat == .privateUse
    } != nil
  }

  static var backspaceKeyEquivalent: String {
    String(Character(UnicodeScalar(NSBackspaceCharacter)!))
  }
}
