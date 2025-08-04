import Cocoa
import NoiseSerde

class ScriptWindowController: NSWindowController {
  private var id: UVarint!
  private var topic: String!
  private var active = false
  private var url: URL?

  @IBOutlet weak var toolbar: NSToolbar!

  lazy var editorCtl = EditorViewController()
  weak var delegate: ScriptWindowDelegate?

  private weak var tableAppearedObserver: AnyObject?
  private weak var tableDisappearedObserver: AnyObject?

  deinit {
    for obs in [tableAppearedObserver, tableDisappearedObserver] where obs != nil {
      NotificationCenter.default.removeObserver(obs!)
    }
  }

  convenience init(withId id: UVarint, andTopic topic: String) {
    self.init(windowNibName: "ScriptWindowController")
    self.id = id
    self.topic = topic
    if let script = Error.wait(Backend.shared.getScript(forTopic: topic, inWorkspace: id)) {
      editorCtl.configure(code: script, language: .lua)
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

    tableAppearedObserver = NotificationCenter.default.addObserver(
      forName: .TopicRecordsTableAppeared,
      object: nil,
      queue: .main
    ) { [weak self] notification in
      guard let self else { return }
      guard let ob = notification.object as? ScriptWindowDelegate else { return }
      guard notification.userInfo?["id"] as? UVarint == id else { return }
      guard notification.userInfo?["topic"] as? String == topic else { return }
      self.delegate = ob
      self.toolbar.validateVisibleItems()
    }

    tableDisappearedObserver = NotificationCenter.default.addObserver(
      forName: .TopicRecordsTableDisappeared,
      object: nil,
      queue: .main
    ) { [weak self] notification in
      guard let self else { return }
      guard let ob = notification.object as? ScriptWindowDelegate else { return }
      guard notification.userInfo?["id"] as? UVarint == id else { return }
      guard notification.userInfo?["topic"] as? String == topic else { return }
      self.delegate = ob
      self.deactivate()
      self.toolbar.validateVisibleItems()
    }
  }

  private var toggleToolbarItem: NSToolbarItem? {
    toolbar.items.first(where: { $0.itemIdentifier == .toggleActive })
  }

  private func activate() {
    if let activated = delegate?.scriptWindow(willActivate: editorCtl.code), activated {
      active = true
    }
  }

  private func deactivate() {
    delegate?.scriptWindowWillDeactivate()
    active = false
  }

  private func resetTitle() {
    guard let topic else { return }
    let filename = url?.lastPathComponent ?? "Untitled.lua"
    window?.title = "\(topic) - \(filename)"
  }

  private func open(_ url: URL) {
    assert(Thread.isMainThread)
    do {
      guard let code = String(data: try Data(contentsOf: url), encoding: .utf8) else { return }
      editorCtl.configure(code: code, language: .lua)
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
    default:
      return
    }
  }

  @objc func openDocument(_ sender: Any) {
    let dialog = NSOpenPanel()
    dialog.allowedContentTypes = [.init(filenameExtension: "lua")!]
    dialog.canChooseFiles = true
    switch dialog.runModal() {
    case .OK:
      guard let url = dialog.url else { return }
      self.url = url
      self.open(url)
      self.resetTitle()
    default:
      return
    }
  }

  @objc func saveDocument(_ sender: Any) {
    if let url {
      save(to: url)
    } else {
      saveAs()
    }
  }

  @objc func saveDocumentAs(_ sender: Any) {
    saveAs()
  }

  @objc func revertDocumentToSaved(_ sender: Any) {
    if let url {
      open(url)
    }
  }

  @objc func toggleScript(_ sender: Any) {
    if active {
      deactivate()
    } else {
      activate()
    }
  }

  @objc func applyScript(_ sender: Any) {
    if active {
      return
    }
    delegate?.scriptWindow(willApply: editorCtl.code)
  }
}

// MARK: - ScriptWindowControllerDelegate
protocol ScriptWindowDelegate: AnyObject {
  func scriptWindow(willActivate script: String) -> Bool
  func scriptWindow(willApply script: String)
  func scriptWindowWillClose()
  func scriptWindowWillDeactivate()
}

// MARK: - EditorViewControllerDelegate
extension ScriptWindowController: EditorViewControllerDelegate {
  func codeDidChange(_ sender: EditorViewController) {
    if active {
      deactivate()
      toolbar.validateVisibleItems()
    }
    window?.isDocumentEdited = true
  }
}

// MARK: - NSMenuItemValidation
extension ScriptWindowController: NSMenuItemValidation {
  func validateMenuItem(_ menuItem: NSMenuItem) -> Bool {
    if menuItem.action == #selector(saveDocument(_:)) {
      menuItem.title = url == nil ? "Save..." : "Save"
    }
    if menuItem.action == #selector(revertDocumentToSaved(_:)) {
      return url != nil && (window?.isDocumentEdited ?? false)
    }
    if menuItem.action == #selector(toggleScript(_:)) {
      menuItem.title = active ? "Deactivate" : "Activate"
      return delegate != nil
    }
    if menuItem.action == #selector(applyScript(_:)) {
      return !active && delegate != nil
    }
    return true
  }
}

// MARK: - NSToolbarDelegate
extension ScriptWindowController: NSToolbarDelegate {
  private func makeSymbolConfig() -> NSImage.SymbolConfiguration {
    return NSImage.SymbolConfiguration(pointSize: 14, weight: .regular)
  }

  private func bolt(accented: Bool = false) -> NSImage {
    var conf = makeSymbolConfig()
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
      case .apply:
        let item = NSToolbarItem(itemIdentifier: itemIdentifier)
        item.image = NSImage(systemSymbolName: "play.fill", accessibilityDescription: "Apply")!
          .withSymbolConfiguration(makeSymbolConfig())
        item.label = "Apply"
        item.action = #selector(applyScript(_:))
        return item
      case .toggleActive:
        let item = NSToolbarItem(itemIdentifier: itemIdentifier)
        item.image = bolt()
        item.label = "Toggle Active"
        item.action = #selector(toggleScript(_:))
        return item
      default:
        return nil
      }
  }

  func toolbarAllowedItemIdentifiers(_ toolbar: NSToolbar) -> [NSToolbarItem.Identifier] {
    return [.apply, .toggleActive]
  }

  func toolbarDefaultItemIdentifiers(_ toolbar: NSToolbar) -> [NSToolbarItem.Identifier] {
    return [.apply, .toggleActive]
  }
}

// MARK: - NSToolbarItemValidation
extension ScriptWindowController: NSToolbarItemValidation {
  func validateToolbarItem(_ item: NSToolbarItem) -> Bool {
    if item.action == #selector(applyScript(_:)) {
      return !active && delegate != nil
    }
    if item.action == #selector(toggleScript(_:)) {
      item.image = bolt(accented: active)
      return delegate != nil
    }
    return true
  }
}

// MARK: - NSToolbarItem.Identifier
extension NSToolbarItem.Identifier {
  static let apply = Self("apply")
  static let toggleActive = Self("toggleActive")
}

// MARK: - NSWindowDelegate
extension ScriptWindowController: NSWindowDelegate {
  func windowWillClose(_ notification: Notification) {
    deactivate()
    delegate?.scriptWindowWillClose()
  }
}
