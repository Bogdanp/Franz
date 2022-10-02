import Cocoa
import SwiftUI

class WorkspaceDetailViewController: NSViewController {
  override func viewDidLoad() {
    super.viewDidLoad()
  }

  func show(entry: Any, withKind kind: SidebarEntryKind) {
    switch kind {
    case .broker:
      print("broker=\(entry as! Broker)")
      let ctl = NSHostingController(rootView: WorkspaceBrokerDetailView(broker: entry as! Broker))
      children = [ctl]
      ctl.view.frame = view.bounds
      for v in view.subviews {
        v.removeFromSuperview()
      }
      view.addSubview(ctl.view)
    default:
      ()
    }
  }
}
