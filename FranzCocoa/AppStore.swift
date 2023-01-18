import Foundation

class AppStore {
  public static func isAppStoreInstall() -> Bool {
    if let url = Bundle.main.appStoreReceiptURL {
      return FileManager.default.fileExists(atPath: url.absoluteString)
    }
    return false
  }
}
