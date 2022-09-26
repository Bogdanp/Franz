import Cocoa

class SidebarEntryCellView: NSTableCellView {
  @IBOutlet weak var extendedTextField: NSTextField!
  @IBOutlet weak var countField: NSTextField!

  func configure(withText text: String) {
    extendedTextField.stringValue = text
    extendedTextField.isHidden = false
    textField?.isHidden = true
    countField.isHidden = true
  }

  func configure(withText text: String, andCount count: String) {
    textField?.stringValue = text
    textField?.isHidden = false
    countField.stringValue = count
    countField.isHidden = false
    extendedTextField.isHidden = true
  }
}
