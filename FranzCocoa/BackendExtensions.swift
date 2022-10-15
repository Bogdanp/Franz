import Foundation

#if arch(x86_64)
let ARCH="x86_64"
#elseif arch(arm64)
let ARCH="arm64"
#endif

extension Backend {
  static let shared = Backend(
    withZo: Bundle.main.url(forResource: "resources/core-\(ARCH)", withExtension: "zo")!,
    andMod: "main",
    andProc: "main"
  )
}

extension Broker {
  var address: String {
    get {
      return "\(host):\(String(port))"
    }
  }
}


extension ConnectionDetails {
  var bootstrapAddress: String {
    get {
      "\(bootstrapHost):\(bootstrapPort)"
    }
  }
}


extension ResourceConfig: Identifiable {
  public var id: String {
    get {
      name
    }
  }

  public var nonnullValue: String {
    get {
      value ?? ""
    }
  }
}
