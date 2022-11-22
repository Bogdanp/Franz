import AppKit
import Foundation
import OSLog

fileprivate let logger = Logger(
  subsystem: "io.defn.Franz",
  category: "Defaults"
)

class Defaults {
  static var shared = Defaults()

  @BoolDefault(key: "checkForUpdates") var checkForUpdates = true
  @CodableDefault(key: "updateInterval") var updateInterval = UpdateInterval.everyFourHours
  @IntDefault(key: "reloadIntervalMs") var reloadIntervalMs = 10000

  @propertyWrapper struct CodableDefault<T: Codable> {
    var key: String
    var def: T!
    var wrappedValue: T {
      get {
        return Defaults.shared.get(codable: key) ?? def
      }
      set {
        let key = self.key
        do {
          try Defaults.shared.set(codable: newValue, forKey: key)
        } catch {
          logger.error("failed to set codable value for \(key): \(error)")
        }
      }
    }

    init(wrappedValue: T, key: String) {
      self.key = key
      self.def = wrappedValue
    }
  }

  @propertyWrapper struct BoolDefault {
    var key: String
    var def: Bool!
    var wrappedValue: Bool {
      get {
        if Defaults.shared.get(string: key) != nil {
          return Defaults.shared.get(bool: key)
        }
        return def
      }
      set {
        Defaults.shared.set(bool: newValue, forKey: key)
      }
    }

    init(wrappedValue: Bool, key: String) {
      self.key = key
      self.def = wrappedValue
    }
  }

  @propertyWrapper struct IntDefault {
    var key: String
    var def: Int!
    var wrappedValue: Int {
      get {
        let n = Defaults.shared.get(integer: key)
        return n > 0 ? n : def
      }
      set {
        Defaults.shared.set(integer: newValue, forKey: key)
      }
    }

    init(wrappedValue: Int, key: String) {
      self.key = key
      self.def = wrappedValue
    }
  }

  func get(bool k: String) -> Bool {
    UserDefaults.standard.bool(forKey: k)
  }

  func get(integer k: String) -> Int {
    UserDefaults.standard.integer(forKey: k)
  }

  func get(string k: String) -> String? {
    UserDefaults.standard.string(forKey: k)
  }

  func get<V: Decodable>(codable k: String) -> V? {
    do {
      guard let data = UserDefaults.standard.data(forKey: k) else { return nil }
      return try JSONDecoder().decode(V.self, from: data)
    } catch {
      return nil
    }
  }

  func set(bool v: Bool, forKey k: String) {
    UserDefaults.standard.set(v, forKey: k)
  }

  func set(integer v: Int, forKey k: String) {
    UserDefaults.standard.set(v, forKey: k)
  }

  func set<V: Encodable>(codable v: V, forKey k: String) throws {
    let data = try JSONEncoder().encode(v)
    UserDefaults.standard.set(data, forKey: k)
  }
}
