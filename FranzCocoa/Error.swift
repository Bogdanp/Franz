import AppKit
import Foundation
import NoiseBackend

class Error {
  static func block(_ proc: @escaping () throws -> Void) {
    do {
      try proc()
    } catch {
      alert(withError: error)
    }
  }

  @discardableResult
  static func alert(
    withError err: Any,
    andMessageText messageText: String = "Error",
    andInformativeText informativeText: String = "Franz encountered an unexpected error.",
    andHandler handler: @escaping (NSAlert) -> Void = { _ in }
  ) -> NSApplication.ModalResponse {
    var str = "\(err)"
    if let err = err as? LocalizedError {
      str = err.localizedDescription
    }
    let textField = NSTextField()
    textField.isEditable = false
    textField.isSelectable = true
    textField.stringValue = str
    textField.frame = NSRect(x: 0, y: 0, width: 400, height: 200)
    textField.font = .monospacedSystemFont(ofSize: 12, weight: .regular)
    let alert = NSAlert()
    alert.alertStyle = .critical
    alert.messageText = messageText
    alert.informativeText = informativeText
    alert.accessoryView = textField
    handler(alert)
    return alert.runModal()
  }

  @discardableResult
  static func wait<Err, Res>(_ fut: Future<Err, Res?>) -> Res? {
    do {
      return try fut.wait()
    } catch {
      alert(withError: error)
      return nil
    }
  }

  @discardableResult
  static func wait<Err, Res>(_ fut: Future<Err, Res>) -> Res? {
    do {
      return try fut.wait()
    } catch {
      alert(withError: error)
      return nil
    }
  }
}
