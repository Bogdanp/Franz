import Cocoa
import SwiftUI

class WorkspaceDetailViewController: NSViewController {
  @IBOutlet weak var selectLabel: NSTextField!

  private var contentView: NSView?
  private var contentViewConstraints: [NSLayoutConstraint]?

  override func viewDidLoad() {
    super.viewDidLoad()
  }

  func show(entry: Any, withKind kind: SidebarEntryKind) {
    switch kind {
    case .broker:
      let hostingCtl = NSHostingController(rootView: WorkspaceBrokerDetailView(broker: entry as! Broker))
      display(controller: hostingCtl)
    case .topic:
      let hostingCtl = NSHostingController(rootView: WorkspaceTopicDetailView(topic: entry as! Topic))
      display(controller: hostingCtl)
    case .consumerGroup:
      let hostingCtl = NSHostingController(rootView: WorkspaceGroupDetailView(group: entry as! Group))
      display(controller: hostingCtl)
    default:
      clear()
    }
  }

  private func display(controller: NSViewController) {
    selectLabel.isHidden = true
    contentView?.removeFromSuperviewWithoutNeedingDisplay()
    if let contentViewConstraints {
      NSLayoutConstraint.deactivate(contentViewConstraints)
    }
    children = [controller]
    contentView = controller.view
    contentView?.translatesAutoresizingMaskIntoConstraints = false
    view.addSubview(controller.view)
    contentViewConstraints = fullSizeConstraints(forSubview: controller.view)
    NSLayoutConstraint.activate(contentViewConstraints!)
  }

  private func clear() {
    contentView?.removeFromSuperview()
    if let contentViewConstraints {
      NSLayoutConstraint.deactivate(contentViewConstraints)
    }
    selectLabel.isHidden = false
  }
}


// - MARK: NSViewController
extension NSViewController {
  func fullSizeConstraints(forSubview subview: NSView) -> [NSLayoutConstraint] {
    let leading = NSLayoutConstraint(
      item: subview,
      attribute: .leading,
      relatedBy: .equal,
      toItem: self.view,
      attribute: .leading,
      multiplier: 1.0,
      constant: 0.0
    )
    let trailing = NSLayoutConstraint(
      item: subview,
      attribute: .trailing,
      relatedBy: .equal,
      toItem: self.view,
      attribute: .trailing,
      multiplier: 1.0,
      constant: 0.0
    )
    let top = NSLayoutConstraint(
      item: subview,
      attribute: .top,
      relatedBy: .equal,
      toItem: self.view,
      attribute: .top,
      multiplier: 1.0,
      constant: 0.0
    )
    let bottom = NSLayoutConstraint(
      item: subview,
      attribute: .bottom,
      relatedBy: .equal,
      toItem: self.view,
      attribute: .bottom,
      multiplier: 1.0,
      constant: 0.0
    )
    return [leading, trailing, top, bottom]
  }
}
