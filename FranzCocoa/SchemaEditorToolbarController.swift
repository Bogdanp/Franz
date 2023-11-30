import Cocoa
import SwiftUI

class SchemaEditorToolbarController: NSViewController {
  weak var delegate: SchemaEditorToolbarDelegate?

  override func viewDidLoad() {
    super.viewDidLoad()
  }

  @IBAction func didPressSegmentedControl(_ sender: NSSegmentedControl) {
    switch sender.selectedTag() {
    case 0: delegate?.didRequestSave(self)
    case 1: delegate?.didRequestCheck(self)
    case 2: delegate?.didRequestReset(self)
    default:
      ()
    }
  }
}

protocol SchemaEditorToolbarDelegate: AnyObject {
  func didRequestSave(_ sender: AnyObject)
  func didRequestCheck(_ sender: AnyObject)
  func didRequestReset(_ sender: AnyObject)
}

enum SchemaEditorToolbarAction {
  case save
  case check
  case reset
}

struct SchemaEditorToolbar: NSViewControllerRepresentable {
  typealias NSViewControllerType = SchemaEditorToolbarController

  let action: (SchemaEditorToolbarAction) -> Void

  class Coordinator: SchemaEditorToolbarDelegate {
    let action: (SchemaEditorToolbarAction) -> Void

    init(_ action: @escaping (SchemaEditorToolbarAction) -> Void) {
      self.action = action
    }

    func didRequestSave(_ sender: AnyObject) {
      action(.save)
    }

    func didRequestCheck(_ sender: AnyObject) {
      action(.check)
    }

    func didRequestReset(_ sender: AnyObject) {
      action(.reset)
    }
  }

  func makeCoordinator() -> Coordinator {
    return Coordinator(action)
  }

  func makeNSViewController(context: Context) -> SchemaEditorToolbarController {
    let ctl = SchemaEditorToolbarController()
    ctl.delegate = context.coordinator
    return ctl
  }

  func updateNSViewController(_ nsViewController: SchemaEditorToolbarController, context: Context) {
  }
}
