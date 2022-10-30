import AppKit
import Foundation

class Defaults {
  static var shared = Defaults()

  @IntDefault(key: "reloadIntervalMs") var reloadIntervalMs: Int = 10000

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

  func get(integer k: String) -> Int {
    UserDefaults.standard.integer(forKey: qualify(k))
  }

  func get<V: Decodable>(codable k: String) -> V? {
    do {
      guard let data = UserDefaults.standard.data(forKey: qualify(k)) else { return nil }
      return try JSONDecoder().decode(V.self, from: data)
    } catch {
      return nil
    }
  }

  func set(integer v: Int, forKey k: String) {
    UserDefaults.standard.set(v, forKey: qualify(k))
  }

  func set<V: Encodable>(codable v: V, forKey k: String) throws {
    let data = try JSONEncoder().encode(v)
    UserDefaults.standard.set(data, forKey: qualify(k))
  }

  private func qualify(_ k: String) -> String {
    return "io.defn.Franz.\(k)"
  }
}
