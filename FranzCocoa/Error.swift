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

  static func alert(withError err: Any) {
    let textField = NSTextField()
    textField.isEditable = true
    textField.isSelectable = true
    textField.stringValue = "\(err)"
    textField.frame = NSRect(x: 0, y: 0, width: 400, height: 200)
    textField.font = .monospacedSystemFont(ofSize: 12, weight: .regular)
    let alert = NSAlert()
    alert.alertStyle = .critical
    alert.messageText = "Error"
    alert.informativeText = "Franz encountered an unexpected error."
    alert.accessoryView = textField
    alert.runModal()
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
