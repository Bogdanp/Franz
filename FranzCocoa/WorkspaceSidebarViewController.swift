import Cocoa
import NoiseSerde

class WorkspaceSidebarViewController: NSViewController {
  private var id: UVarint!
  private var topics = [String]()

  @IBOutlet weak var tableView: NSTableView!

  convenience init(withClientID id: UVarint) {
    self.init()
    self.id = id
    self.topics = Backend.shared.listTopics(id).wait()
  }

  override func viewDidLoad() {
    super.viewDidLoad()

    tableView.delegate = self
    tableView.dataSource = self
    tableView.reloadData()
  }
}

// MARK: -NSTableViewDelegate
extension WorkspaceSidebarViewController: NSTableViewDelegate {
  func tableView(_ tableView: NSTableView, viewFor tableColumn: NSTableColumn?, row: Int) -> NSView? {
    guard let view = tableView.makeView(withIdentifier: NSUserInterfaceItemIdentifier("Topics"), owner: nil) as? NSTableCellView else {
      return nil
    }

    let topic = topics[row]
    view.textField?.stringValue = topic
    view.imageView?.image = NSImage(systemSymbolName: "tray.full", accessibilityDescription: "Topic")?
      .withSymbolConfiguration(.init(pointSize: 14, weight: .light))
    return view
  }
}

// MARK: -NSTableViewDataSource
extension WorkspaceSidebarViewController: NSTableViewDataSource {

  func numberOfRows(in tableView: NSTableView) -> Int {
    return topics.count
  }

  func tableView(_ tableView: NSTableView, objectValueFor tableColumn: NSTableColumn?, row: Int) -> Any? {
    return topics[row]
  }
}
