import AppKit
import Foundation
import OSLog

fileprivate let versionsServiceURL = "https://franz.defn.io/versions/"

fileprivate let logger = Logger(
  subsystem: "io.defn.Franz",
  category: "AutoUpdater"
)

struct Release: Codable {
  let arch: String
  let version: String
  let macURL: URL
}

enum UpdateResult {
  case ok
  case error(String)
}

fileprivate enum ReleaseDownloadResult {
  case ok(URL)
  case error(String)
}

/// Checks the given service URL for new versions of the current
/// application.  If newer versions (by string comparison) are found,
/// then an action is fired.  The application can then notify the user
/// that updates are available and ask them if they want to perform
/// the update.
///
/// The service must expose two files under its root URL:
///   * `changelog.txt` -- containing plain text describing all of the
///   changes between versions and
///   * `versions.json` -- containing a JSON array with `{arch,
///   version, macURL}` objects inside it.
class AutoUpdater {
  private let serviceURL = URL(string: versionsServiceURL)!
  private let currentVersion: String = {
    guard let info = Bundle.main.infoDictionary else { return "" }
    let version = info["CFBundleShortVersionString"] as? String ?? ""
    let build = info["CFBundleVersion"] as? String ?? ""
    let pad = String(repeating: "0", count: 4-build.count)
    return "\(version).\(pad)\(build)"
  }()
  private let session = URLSession(configuration: .ephemeral)
  private let decoder = JSONDecoder()

  private var timer: Timer?

  func start(withInterval interval: Double, andCompletionHandler handler: @escaping (String, Release) -> Void) {
    stop()
    checkForUpdates(completionHandler: handler)
    timer = Timer.scheduledTimer(withTimeInterval: interval, repeats: true) { [weak self] _ in
      self?.checkForUpdates(completionHandler: handler)
    }
  }

  func stop() {
    timer?.invalidate()
  }

  func resetCaches(completionHandler handler: @escaping () -> Void) {
    session.reset {
      handler()
    }
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
    fetchChangelog { [weak self] changelog in
      self?.fetchReleasesJSON { releases in
        let latest = releases.sorted { a, b in
          a.version > b.version
        }.first

        if let latest, latest.version != self?.currentVersion {
          RunLoop.main.schedule {
            complete(changelog, latest)
          }
        } else {
          RunLoop.main.schedule {
            reject()
          }
        }
      }
    }
  }

  private func fetchChangelog(withCompletionHandler handler: @escaping (String) -> Void) {
    let changelogURL = serviceURL.appendingPathComponent("changelog.txt")
    let task = session.dataTask(with: changelogURL) { data, response, error in
      if let error {
        logger.error("failed to check for updates: \(error)")
        return
      }

      guard let httpResponse = response as? HTTPURLResponse, httpResponse.statusCode == 200,
            let mimeType = httpResponse.mimeType, mimeType == "text/plain" else {
        logger.error("invalid response from server: \(String(describing: response))")
        return
      }

      guard let data else {
        logger.error("empty changelog data from server")
        return
      }

      handler(String(decoding: data, as: UTF8.self))
    }
    task.resume()
  }

  private func fetchReleasesJSON(withCompletionHandler handler: @escaping ([Release]) -> Void) {
    let versionsURL = serviceURL.appendingPathComponent("versions.json")
    let task = session.dataTask(with: versionsURL) { data, response, error in
      if let error = error {
        logger.error("failed to check for updates: \(error)")
        return
      }

      guard let httpResponse = response as? HTTPURLResponse, httpResponse.statusCode == 200,
            let mimeType = httpResponse.mimeType, mimeType == "application/json" else {
        logger.error("invalid response from server: \(String(describing: response))")
        return
      }

      guard let data else {
        logger.error("didn't receive any data from the server")
        return
      }

      do {
        handler(try self.decoder.decode([Release].self, from: data).filter({ release in
          release.arch == ARCH
        }))
      } catch {
        logger.error("failed to parse versions JSON: \(error)")
      }
    }
    task.resume()
  }

  private func fetchRelease(
    _ release: Release,
    withCompletionHandler handler: @escaping (ReleaseDownloadResult) -> Void
  ) {
    let task = session.downloadTask(with: release.macURL) { fileURL, response, error in
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
