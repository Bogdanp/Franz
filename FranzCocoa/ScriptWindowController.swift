import Cocoa
import NoiseSerde

class ScriptWindowController: NSWindowController {
  private var id: UVarint!
  private var topic: String!
  private var active = false
  private var url: URL?

  @IBOutlet weak var toolbar: NSToolbar!

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
    window.setFrameAutosaveName("Franz:Script:\(id):\(topic)")
    if !window.setFrameUsingName(window.frameAutosaveName) {
      window.setFrame(NSRect(x: 0, y: 0, width: 800, height: 600), display: true)
      window.center()
    }
    resetTitle()
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

  private func resetTitle() {
    guard let topic else { return }
    let filename = url?.lastPathComponent ?? "Untitled.lua"
    window?.title = "\(topic) - \(filename)"
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
    window?.isDocumentEdited = true
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

  func windowDidBecomeKey(_ notification: Notification) {
    let items: [[NSUserInterfaceItemIdentifier]: Selector] = [
      [.FileMenuItem, .OpenMenuItem]: #selector(didPressOpenMenuItem(_:)),
      [.FileMenuItem, .SaveMenuItem]: #selector(didPressSaveMenuItem(_:)),
      [.FileMenuItem, .SaveAsMenuItem]: #selector(didPressSaveAsMenuItem(_:)),
    ]
    for (path, selector) in items {
      guard let item = MainMenu.shared.find(itemByPath: path) else { continue }
      item.action = selector
      item.target = self
    }
    resetSaveItems()
  }

  func windowDidResignKey(_ notification: Notification) {
    let paths: [[NSUserInterfaceItemIdentifier]] = [
      [.FileMenuItem, .OpenMenuItem],
      [.FileMenuItem, .SaveMenuItem],
      [.FileMenuItem, .SaveAsMenuItem],
      [.FileMenuItem, .RevertMenuItem],
    ]
    for path in paths {
      guard let item = MainMenu.shared.find(itemByPath: path) else { continue }
      item.action = nil
      item.target = nil
    }

    if let item = MainMenu.shared.find(itemByPath: [.FileMenuItem, .SaveMenuItem]) {
      item.title = "Save..."
    }
  }

  @objc func didPressOpenMenuItem(_ sender: NSMenuItem) {
    let dialog = NSOpenPanel()
    dialog.allowedContentTypes = [.init(filenameExtension: "lua")!]
    dialog.canChooseFiles = true
    switch dialog.runModal() {
    case .OK:
      guard let url = dialog.url else { return }
      self.url = url
      self.open(url)
      self.resetTitle()
      self.resetSaveItems()
    default:
      return
    }
  }

  @objc func didPressSaveMenuItem(_ sender: NSMenuItem) {
    if let url {
      save(to: url)
    } else {
      saveAs()
    }
  }

  @objc func didPressSaveAsMenuItem(_ sender: NSMenuItem) {
    saveAs()
  }

  @objc func didPressRevertMenuItem(_ sender: NSMenuItem) {
    if let url {
      open(url)
    }
  }

  private func open(_ url: URL) {
    assert(Thread.isMainThread)
    do {
      guard let code = String(data: try Data(contentsOf: url), encoding: .utf8) else { return }
      editorCtl.configure(code: code)
      window?.isDocumentEdited = false
    } catch {
      Error.alert(withError: error)
    }
  }

  private func save(to url: URL) {
    assert(Thread.isMainThread)
    do {
      try editorCtl.code.data(using: .utf8)?.write(to: url, options: .atomic)
      window?.isDocumentEdited = false
    } catch {
      Error.alert(withError: error)
    }
  }

  private func saveAs() {
    let dialog = NSSavePanel()
    dialog.allowedContentTypes = [.init(filenameExtension: "lua")!]
    dialog.isExtensionHidden = false
    switch dialog.runModal() {
    case .OK:
      guard let url = dialog.url else { return }
      self.url = url
      self.save(to: url)
      self.resetTitle()
      self.resetSaveItems()
    default:
      return
    }
  }

  private func resetSaveItems() {
    if let item = MainMenu.shared.find(itemByPath: [.FileMenuItem, .SaveMenuItem]) {
      item.title = url == nil ? "Save..." : "Save"
    }
    if let item = MainMenu.shared.find(itemByPath: [.FileMenuItem, .RevertMenuItem]) {
      if url != nil {
        item.action = #selector(didPressRevertMenuItem(_:))
        item.target = self
      }
    }
  }
}
