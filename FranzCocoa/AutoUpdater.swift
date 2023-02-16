import AppKit
import Dispatch
import Foundation
import NoiseSerde

enum UpdateResult {
  case ok
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
      Backend.shared.checkForUpdates().onComplete { update in
        guard let update else { return }
        DispatchQueue.main.async {
          handler(update.changelog, update.release)
        }
      }
    }
  }

  func stop() {
    Error.wait(Backend.shared.stopAutoUpdater())
  }

  func performUpdate(
    toRelease release: Release,
    withCompletionHandler handler: @escaping (UpdateResult) -> Void
  ) {
    Backend.shared.fetchRelease(release: release).onComplete { path in
      let downloadsURL = FileManager.default.urls(for: .downloadsDirectory, in: .userDomainMask)[0]
      let srcURL = URL(fileURLWithPath: path)
      let dstURL = downloadsURL.appendingPathComponent("Franz \(release.version).dmg")
      do {
        try FileManager.default.copyItem(at: srcURL, to: dstURL)
      } catch {
        handler(.error("Failed to copy release to downloads folder."))
        return
      }

      NSWorkspace.shared.open(dstURL)
      handler(.ok)
    }
  }

  func checkForUpdates(
    rejectionHandler reject: @escaping () -> Void = {  },
    completionHandler complete: @escaping (String, Release) -> Void
  ) {
    Backend.shared.checkForUpdates().onComplete { update in
      if let update {
        complete(update.changelog, update.release)
      } else {
        reject()
      }
    }
  }
}
