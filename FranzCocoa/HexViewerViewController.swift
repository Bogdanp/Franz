import Cocoa

class HexViewerViewController: NSViewController {
  private var data: Data!

  @IBOutlet weak var tableView: NSTableView!

  override func viewDidLoad() {
    super.viewDidLoad()

    tableView.dataSource = self
    tableView.delegate = self
  }

  func configure(withData data: Data) {
    self.data = data
  }
}

// MARK: - NSTableViewDataSource
extension HexViewerViewController: NSTableViewDataSource {
  func numberOfRows(in tableView: NSTableView) -> Int {
    guard let data else { return 0 }
    return data.count/8+1
  }
}

// MARK: - NSTableViewDelegate
extension HexViewerViewController: NSTableViewDelegate {
  func tableView(_ tableView: NSTableView, viewFor tableColumn: NSTableColumn?, row: Int) -> NSView? {
    guard let column = tableColumn,
          let view = tableView.makeView(withIdentifier: column.identifier, owner: nil) as? NSTableCellView else {
      return nil
    }

    var value = ""
    var offset = -1
    switch column.identifier {
    case NSUserInterfaceItemIdentifier("HexLocation"):
      value = String(format: "0x%09x", row*8)
    case NSUserInterfaceItemIdentifier("HexCol0"):
      offset = 0
    case NSUserInterfaceItemIdentifier("HexCol1"):
      offset = 1
    case NSUserInterfaceItemIdentifier("HexCol2"):
      offset = 2
    case NSUserInterfaceItemIdentifier("HexCol3"):
      offset = 3
    case NSUserInterfaceItemIdentifier("HexCol4"):
      offset = 4
    case NSUserInterfaceItemIdentifier("HexCol5"):
      offset = 5
    case NSUserInterfaceItemIdentifier("HexCol6"):
      offset = 6
    case NSUserInterfaceItemIdentifier("HexCol7"):
      offset = 7
    default:
      ()
    }
    if offset >= 0 {
      let index = row*8+offset
      if index < data.count {
        value = String(format: "%02x", data[index])
      }
    }

    view.textField?.font = .monospacedSystemFont(ofSize: 11, weight: .regular)
    view.textField?.stringValue = value
    return view
  }
}
