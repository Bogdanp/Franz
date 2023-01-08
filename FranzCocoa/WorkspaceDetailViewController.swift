import Cocoa
import NoiseSerde
import SwiftUI

class WorkspaceDetailViewController: NSViewController {
  private var id: UVarint!

  weak var delegate: WorkspaceDetailDelegate?

  @IBOutlet weak var selectLabel: NSTextField!

  private var contentView: NSView?
  private var contentViewConstraints: [NSLayoutConstraint]?

  override func viewDidLoad() {
    super.viewDidLoad()
  }

  func configure(withId id: UVarint) {
    self.id = id
  }

  func show(entry: Any, withKind kind: SidebarEntryKind) {
    delegate?.clearStatusCookie()
    switch kind {
    case .broker:
      guard let broker = entry as? Broker else { return }
      let hostingCtl = NSHostingController(
        rootView: BrokerDetailView(
          id: id,
          broker: broker,
          delegate: delegate))
      display(controller: hostingCtl)
    case .topic:
      guard let topic = entry as? Topic else { return }
      let hostingCtl = NSHostingController(
        rootView: TopicDetailView(
          id: id,
          topic: topic,
          delegate: delegate))
      display(controller: hostingCtl)
    case .consumerGroup:
      guard let group = entry as? Group else { return }
      let hostingCtl = NSHostingController(
        rootView: GroupDetailView(
          id: id,
          group: group,
          delegate: delegate))
      display(controller: hostingCtl)
    case .schema:
      guard let schema = entry as? Schema else { return }
      let hostingCtl = NSHostingController(
        rootView: SchemaDetailView(
          id: id,
          schema: schema,
          delegate: delegate))
      display(controller: hostingCtl)
    default:
      clear()
    }
  }

  func clear() {
    selectLabel.isHidden = false
    contentView?.removeFromSuperview()
    if let contentViewConstraints {
      NSLayoutConstraint.deactivate(contentViewConstraints)
    }
    children = []
    contentView = nil
    contentViewConstraints = nil
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
}

// MARK: - WorkspaceDetailDelegate
protocol WorkspaceDetailDelegate: AnyObject {
  func getConnectionName() -> String
  func clearStatusCookie()
  func makeStatusProc() -> ((String) -> Void)
  func request(topicNamed topic: String)
  func request(groupNamed group: String)
}

// MARK: - NSViewController
extension NSViewController {
  func fullSizeConstraints(
    forSubview subview: NSView,
    relativeTo view: NSView? = nil
  ) -> [NSLayoutConstraint] {
    let leading = NSLayoutConstraint(
      item: subview,
      attribute: .leading,
      relatedBy: .equal,
      toItem: view ?? self.view,
      attribute: .leading,
      multiplier: 1.0,
      constant: 0.0
    )
    let trailing = NSLayoutConstraint(
      item: subview,
      attribute: .trailing,
      relatedBy: .equal,
      toItem: view ?? self.view,
      attribute: .trailing,
      multiplier: 1.0,
      constant: 0.0
    )
    let top = NSLayoutConstraint(
      item: subview,
      attribute: .top,
      relatedBy: .equal,
      toItem: view ?? self.view,
      attribute: .top,
      multiplier: 1.0,
      constant: 0.0
    )
    let bottom = NSLayoutConstraint(
      item: subview,
      attribute: .bottom,
      relatedBy: .equal,
      toItem: view ?? self.view,
      attribute: .bottom,
      multiplier: 1.0,
      constant: 0.0
    )
    return [leading, trailing, top, bottom]
  }
}
