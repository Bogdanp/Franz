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

extension ConnectionDetails {
  func detailsString() -> String {
    return "\(bootstrapHost):\(bootstrapPort)"
  }
}
