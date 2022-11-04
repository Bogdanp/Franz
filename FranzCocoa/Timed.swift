import Dispatch
import Foundation

class Timed {
  static func block<T>(named: String = "anon", _ proc: () -> T) -> T {
    let t0 = DispatchTime.now()
    let res = proc()
    let dt = DispatchTime.now().uptimeNanoseconds - t0.uptimeNanoseconds
    logger.debug("Timed.block named:'\(named)' took: \(dt/1000/1000)ms")
    return res
  }
}
