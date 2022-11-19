import Cocoa
import NoiseSerde

class ScriptWindowController: NSWindowController {
  @IBOutlet weak var toolbar: NSToolbar!

  private var id: UVarint!
  private var topic: String!
  private var active = false

  lazy var editorCtl = EditorViewController()

  weak var delegate: ScriptWindowControllerDelegate?

  convenience init(withId id: UVarint, andTopic topic: String) {
    self.init(windowNibName: "ScriptWindowController")
    self.id = id
    self.topic = topic
    if let script = Error.wait(Backend.shared.getScript(forTopic: topic, inWorkspace: id)) {
      editorCtl.configure(code: script)
    }
  }

  override func windowDidLoad() {
    super.windowDidLoad()

    editorCtl.delegate = self
    contentViewController = editorCtl

    guard let window, let id, let topic else { return }
    window.title = "\(topic) – script.lua"
    window.setFrameAutosaveName("Franz:Script:\(id):\(topic)")
    if !window.setFrameUsingName(window.frameAutosaveName) {
      window.setFrame(NSRect(x: 0, y: 0, width: 800, height: 600), display: true)
      window.center()
    }
  }

  private func activate() {
    let toolbarItem = toolbar.items.first(where: { $0.itemIdentifier == .toggleActive })
    if let activated = delegate?.scriptWindow(willActivate: editorCtl.code), activated {
      toolbarItem?.image = bolt(accented: true)
      active = true
    }
  }

  private func deactivate() {
    let toolbarItem = toolbar.items.first(where: { $0.itemIdentifier == .toggleActive })
    delegate?.scriptWindowWillDeactivate()
    toolbarItem?.image = bolt()
    active = false
  }
}

// MARK: - ScriptWindowControllerDelegate
protocol ScriptWindowControllerDelegate: AnyObject {
  func scriptWindow(willActivate script: String) -> Bool
  func scriptWindowWillClose()
  func scriptWindowWillDeactivate()
}

// MARK: - EditorViewControllerDelegate
extension ScriptWindowController: EditorViewControllerDelegate {
  func codeDidChange(_ sender: EditorViewController) {
    if active {
      deactivate()
    }
  }
}

// MARK: - NSToolbarDelegate
extension ScriptWindowController: NSToolbarDelegate {
  private func bolt(accented: Bool = false) -> NSImage {
    var conf = NSImage.SymbolConfiguration(pointSize: 18, weight: .regular)
    if accented {
      conf = conf.applying(.init(paletteColors: [.controlAccentColor]))
    }
    return NSImage(systemSymbolName: "bolt.fill", accessibilityDescription: "Toggle Active")!
      .withSymbolConfiguration(conf)!
  }

  func toolbar(
    _ toolbar: NSToolbar,
    itemForItemIdentifier itemIdentifier: NSToolbarItem.Identifier,
    willBeInsertedIntoToolbar flag: Bool) -> NSToolbarItem? {

      switch itemIdentifier {
      case .toggleActive:
        let item = NSToolbarItem(itemIdentifier: itemIdentifier)
        item.image = bolt()
        item.label = "Toggle Active"
        item.action = #selector(didPressToggleButton(_:))
        return item
      default:
        return nil
      }
  }

  func toolbarAllowedItemIdentifiers(_ toolbar: NSToolbar) -> [NSToolbarItem.Identifier] {
    return [.toggleActive]
  }

  func toolbarDefaultItemIdentifiers(_ toolbar: NSToolbar) -> [NSToolbarItem.Identifier] {
    return [.flexibleSpace, .toggleActive]
  }

  @objc func didPressToggleButton(_ sender: NSToolbarItem) {
    if active {
      deactivate()
    } else {
      activate()
    }
  }
}

// MARK: - NSToolbarItem.Identifier
extension NSToolbarItem.Identifier {
  static let toggleActive = Self("toggleActive")
}

// MARK: - NSWindowDelegate
extension ScriptWindowController: NSWindowDelegate {
  func windowWillClose(_ notification: Notification) {
    deactivate()
    delegate?.scriptWindowWillClose()
  }
}
