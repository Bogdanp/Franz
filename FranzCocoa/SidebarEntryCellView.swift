import Cocoa

class SidebarEntryCellView: NSTableCellView {
  @IBOutlet weak var extendedTextField: NSTextField!
  @IBOutlet weak var partitionsField: NSTextField!

  func configure(withText text: String) {
    extendedTextField.stringValue = text
    extendedTextField.isHidden = false
    textField?.isHidden = true
    partitionsField.isHidden = true
  }

  func configure(withText text: String, andCount count: String) {
    textField?.stringValue = text
    textField?.isHidden = false
    partitionsField.stringValue = count
    partitionsField.isHidden = false
    extendedTextField.isHidden = true
  }
}
