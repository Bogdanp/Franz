import Foundation
import NoiseBackend
import NoiseSerde

#if arch(x86_64)
let ARCH="x86_64"
#elseif arch(arm64)
let ARCH="arm64"
#endif

// MARK: - Backend
extension Backend {
  static let shared = Backend(
    withZo: Bundle.main.url(forResource: "resources/core-\(ARCH)", withExtension: "zo")!,
    andMod: "main",
    andProc: "main"
  )

  /// Deletes a connection and any related system-level resources (eg. Keychain passwords).
  func deleteConnectionAndSystemResources(_ c: ConnectionDetails) -> Future<String, Void> {
    return deleteConnection(c).andThen { _ in
      if let id = c.passwordId {
        Keychain.shared.delete(passwordWithId: id)
      }
      return FutureUtil.resolved(with: ())
        .mapError({ _ in preconditionFailure("unreachable") })
    }
  }
}

// MARK: - Broker
extension Broker {
  var address: String {
    "\(host):\(String(port))"
  }
}

// MARK: - ConnectionDetails
extension ConnectionDetails {
  var bootstrapAddress: String {
    "\(bootstrapHost):\(bootstrapPort)"
  }
}

// MARK: - GroupPartitionOffset
extension GroupPartitionOffset {
  var lag: Varint {
    highWatermark - offset
  }
}

// MARK: - IteratorOffset
extension IteratorOffset: Hashable {
  public func hash(into hasher: inout Hasher) {
    switch self {
    case .earliest:
      hasher.combine(-2)
    case .latest:
      hasher.combine(-1)
    case .timestamp(let timestamp):
      hasher.combine(0)
      hasher.combine(timestamp)
    case .exact(let offset):
      hasher.combine(1)
      hasher.combine(offset)
    }
  }

  public static func == (lhs: IteratorOffset, rhs: IteratorOffset) -> Bool {
    switch (lhs, rhs) {
    case (.earliest, .earliest):
      return true
    case (.latest, .latest):
      return true
    case (.timestamp(let lhsTimestamp), .timestamp(let rhsTimestamp)):
      return lhsTimestamp == rhsTimestamp
    case (.exact(let lhsOffset), .exact(let rhsOffset)):
      return lhsOffset == rhsOffset
    default:
      return false
    }
  }
}

// MARK: - IteratorRecord
extension IteratorRecord: Comparable {
  var size: UVarint {
    UVarint(key?.count ?? 0) +
    UVarint(value?.count ?? 0)
  }

  public static func == (lhs: IteratorRecord, rhs: IteratorRecord) -> Bool {
    return lhs.partitionId == rhs.partitionId && lhs.offset == rhs.offset
  }

  public static func > (lhs: IteratorRecord, rhs: IteratorRecord) -> Bool {
    if lhs.timestamp == rhs.timestamp {
      if lhs.offset == rhs.offset {
        return lhs.partitionId > rhs.partitionId
      }
      return lhs.offset > rhs.offset
    }
    return lhs.timestamp > rhs.timestamp
  }

  public static func < (lhs: IteratorRecord, rhs: IteratorRecord) -> Bool {
    if lhs.timestamp == rhs.timestamp {
      if lhs.offset == rhs.offset {
        return lhs.partitionId < rhs.partitionId
      }
      return lhs.offset < rhs.offset
    }
    return lhs.timestamp < rhs.timestamp
  }
}

// MARK: - ResourceConfig
extension ResourceConfig: Identifiable {
  public var id: String {
    name
  }

  var nonnullValue: String {
    value ?? ""
  }
}

// MARK: - TokenSpan
extension TokenSpan {
  var range: NSRange {
    NSRange(location: Int(pos), length: Int(len))
  }
}
