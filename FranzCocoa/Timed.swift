import Dispatch
import Foundation
import os

fileprivate let logger = Logger(
  subsystem: Bundle.main.bundleIdentifier!,
  category: "Timed"
)

class Timed {
  static func block<T>(named name: String = "anon", _ proc: () -> T) -> T {
    let t0 = DispatchTime.now()
    let res = proc()
    let dt = DispatchTime.now().uptimeNanoseconds - t0.uptimeNanoseconds
    logger.debug("block '\(name)' took: \(dt/1000/1000)ms")
    return res
  }
}
