import AppKit
import Foundation
import Security

enum KeychainGetResult {
  case aborted
  case success(String)
  case error(OSStatus)
}

enum KeychainUpsertResult {
  case inserted
  case updated
  case error(OSStatus)
}

enum KeychainDeleteResult {
  case success
  case error(OSStatus)
}

class Keychain {
  static let shared = Keychain()

  func get(passwordWithId id: String) -> KeychainGetResult {
    switch lookup(passwordWithId: id) {
    case .success(let password):
      return .success(password)
    case .error(let status):
      return .error(status)
    case .notFound, .badData:
      let passwordField = NSSecureTextField(frame: NSRect(x: 0, y: 0, width: 250, height: 22))
      let alert = NSAlert()
      alert.messageText = "Password Required"
      alert.informativeText = "A password is required to access this cluster."
      alert.accessoryView = passwordField
      alert.window.initialFirstResponder = passwordField
      alert.addButton(withTitle: "OK")
      alert.addButton(withTitle: "Cancel")
      switch alert.runModal() {
      case .alertFirstButtonReturn:
        let password = passwordField.stringValue
        _ = upsert(password: password, withId: id)
        return .success(password)
      default:
        return .aborted
      }
    }
  }

  func upsert(password: String, withId id: String) -> KeychainUpsertResult {
    switch lookup(passwordWithId: id) {
    case .badData:
      return .error(-1)
    case .error(let status):
      return .error(status)
    case .success:
      switch delete(passwordWithId: id) {
      case .error(let status):
        return .error(status)
      case .success:
        switch insert(password: password, withId: id) {
        case .error(let status):
          return .error(status)
        case .success:
          return .updated
        }
      }
    case .notFound:
      switch insert(password: password, withId: id) {
      case .error(let status):
        return .error(status)
      case .success:
        return .inserted
      }
    }
  }

  func delete(passwordWithId id: String) -> KeychainDeleteResult {
    let query = [
      kSecClass: kSecClassGenericPassword,
      kSecAttrLabel: "Franz",
      kSecAttrAccount: id,
    ] as [String: Any]
    let res = SecItemDelete(query as CFDictionary)
    switch res {
    case errSecSuccess:
      return .success
    default:
      return .error(res)
    }
  }

  private func lookup(passwordWithId id: String) -> KeychainLookupResult {
    let query = [
      kSecClass: kSecClassGenericPassword,
      kSecAttrLabel: "Franz",
      kSecAttrAccount: id,
      kSecMatchLimit: kSecMatchLimitOne,
      kSecReturnData: true,
    ] as [String: Any]
    var ref: CFTypeRef?
    let res = SecItemCopyMatching(query as CFDictionary, &ref)
    switch res {
    case errSecItemNotFound:
      return .notFound
    case errSecSuccess:
      guard let data = ref as? Data else {
        return .badData
      }
      guard let password = String(data: data, encoding: .utf8) else {
        return .badData
      }

      return .success(password)
    default:
      return .error(res)
    }
  }

  private func insert(password: String, withId id: String) -> KeychainResult {
    let attrs = [
      kSecClass: kSecClassGenericPassword,
      kSecAttrLabel: "Franz",
      kSecAttrAccount: id,
      kSecValueData: password,
    ] as [String: Any]
    let res = SecItemAdd(attrs as CFDictionary, nil)
    switch res {
    case errSecSuccess:
      return .success
    default:
      return .error(res)
    }
  }
}

fileprivate enum KeychainLookupResult {
  case badData
  case notFound
  case success(String)
  case error(OSStatus)
}

fileprivate enum KeychainResult {
  case success
  case error(OSStatus)
}
