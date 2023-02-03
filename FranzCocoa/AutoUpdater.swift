import AppKit
import Dispatch
import Foundation
import OSLog
import NoiseSerde

fileprivate let logger = Logger(
  subsystem: "io.defn.Franz",
  category: "AutoUpdater"
)

enum UpdateResult {
  case ok
  case error(String)
}

fileprivate enum ReleaseDownloadResult {
  case ok(URL)
  case error(String)
}

class AutoUpdater {
  private let currentVersion: String = {
    guard let info = Bundle.main.infoDictionary else { return "" }
    let version = info["CFBundleShortVersionString"] as? String ?? ""
    let build = info["CFBundleVersion"] as? String ?? ""
    let pad = String(repeating: "0", count: 4-build.count)
    return "\(version).\(pad)\(build)"
  }()
  private let session = URLSession(configuration: .ephemeral)

  static let shared = AutoUpdater()

  func start(
    withInterval intervalMs: Int?,
    checkingImmediately immediate: Bool = true,
    andCompletionHandler handler: @escaping (String, Release) -> Void
  ) {
    Error.wait(Backend.shared.installCallback(announceUpdate: { changelog, release in
      DispatchQueue.main.async {
        handler(changelog, release)
      }
    }))
    Error.wait(Backend.shared.startAutoUpdater(
      withFrequency: intervalMs.map { UVarint($0) },
      andArch: ARCH,
      andVersion: currentVersion
    ))
    if immediate {
      Error.wait(Backend.shared.checkForUpdates())
    }
  }

  func stop() {
    Error.wait(Backend.shared.stopAutoUpdater())
  }

  func performUpdate(toRelease release: Release, withCompletionHandler handler: @escaping (UpdateResult) -> Void) {
    fetchRelease(release) { res in
      switch res {
      case .ok(let path):
        let downloadsURL = FileManager.default.urls(for: .downloadsDirectory, in: .userDomainMask)[0]
        let targetURL = downloadsURL.appendingPathComponent("Franz \(release.version).dmg")
        do {
          try FileManager.default.copyItem(at: path, to: targetURL)
        } catch {
          logger.error("failed to copy update to downloads folder: \(error)")
        }

        NSWorkspace.shared.open(targetURL)
        handler(.ok)
      case .error(let message):
        handler(.error(message))
      }
    }
  }

  func checkForUpdates(
    rejectionHandler reject: @escaping () -> Void = {  },
    completionHandler complete: @escaping (String, Release) -> Void
  ) {
    Backend.shared.checkForUpdates().onComplete { update in
      if let update {
        DispatchQueue.main.async {
          complete(update.changelog, update.release)
        }
      } else {
        DispatchQueue.main.async {
          reject()
        }
      }
    }
  }

  private func fetchRelease(
    _ release: Release,
    withCompletionHandler handler: @escaping (ReleaseDownloadResult) -> Void
  ) {
    let url = URL(string: release.macURL)!
    let task = session.downloadTask(with: url) { fileURL, response, error in
      if let error {
        logger.error("failed to download release: \(error)")
        handler(.error("""
        We were unable to retrieve the updated files.  \
        Please check your connection and try again later.
        """))
        return
      }

      guard let httpResponse = response as? HTTPURLResponse, httpResponse.statusCode == 200 else {
        logger.error("invalid response from server: \(String(describing: response))")
        handler(.error("An unexpected error occurred.  Please try again later."))
        return
      }

      guard let fileURL else {
        logger.error("release failed to download")
        handler(.error("An unexpected error occurred.  Please try again later."))
        return
      }

      handler(.ok(fileURL))
    }
    task.resume()
  }
}
