import Foundation

extension DispatchQueue {
  static var uiBackground = DispatchQueue(
    label: "io.defn.Franz.UIBackground",
    qos: .background
  )
}
