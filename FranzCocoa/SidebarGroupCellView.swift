import Cocoa

class SidebarGroupCellView: NSView {
  @IBOutlet weak var textField: NSTextField!
  @IBOutlet weak var toggleCollapseButton: NSButton!

  private var trackingArea: NSTrackingArea?

  private var _collapsed = false
  var collapsed: Bool {
    get {
      _collapsed
    }
    set {
      _collapsed = newValue
      reset()
    }
  }

  private var _tag = -1
  override var tag: Int {
    get {
      _tag
    }
    set {
      _tag = newValue
    }
  }
  var delegate: SidebarGroupCellViewDelegate?

  override func updateTrackingAreas() {
    super.updateTrackingAreas()
    if let trackingArea {
      removeTrackingArea(trackingArea)
      self.trackingArea = nil
    }

    trackingArea = NSTrackingArea(
      rect: self.bounds,
      options: [.mouseEnteredAndExited, .activeAlways],
      owner: self,
      userInfo: nil
    )
    addTrackingArea(trackingArea!)
  }

  override func mouseEntered(with event: NSEvent) {
    super.mouseEntered(with: event)
    toggleCollapseButton.isHidden = false
  }

  override func mouseExited(with event: NSEvent) {
    super.mouseExited(with: event)
    toggleCollapseButton.isHidden = true
  }

  private func reset() {
    if collapsed {
      toggleCollapseButton.image = NSImage(
        systemSymbolName: "chevron.right",
        accessibilityDescription: "Show Group Items"
      )
    } else {
      toggleCollapseButton.image = NSImage(
        systemSymbolName: "chevron.down",
        accessibilityDescription: "Hide Group Items"
      )
    }
  }

  @IBAction func didPushToggleCollapseButton(_ sender: Any) {
    collapsed.toggle()
    reset()
    delegate?.sidebarGroupCellView(withTag: tag, collapsed: collapsed)
  }
}

// MARK: - SidebarGroupCellViewDelegate
protocol SidebarGroupCellViewDelegate: AnyObject {
  func sidebarGroupCellView(withTag tag: Int, collapsed: Bool)
}
