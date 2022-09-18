import Cocoa
import NoiseSerde

class WorkspaceSidebarViewController: NSViewController {
  private var topics = [Topic]()

  @IBOutlet weak var tableView: NSTableView!

  override func viewDidLoad() {
    super.viewDidLoad()

    tableView.register(.init(nibNamed: "TopicTableCellView", bundle: nil), forIdentifier: .topics)
    tableView.delegate = self
    tableView.dataSource = self
    tableView.reloadData()
  }

  func configure(withTopics topics: [Topic]) {
    self.topics = topics
    self.tableView.reloadData()
  }
}

// MARK: -NSTableViewDelegate
extension WorkspaceSidebarViewController: NSTableViewDelegate {
  func tableView(_ tableView: NSTableView, viewFor tableColumn: NSTableColumn?, row: Int) -> NSView? {
    guard let view = tableView.makeView(withIdentifier: .topics, owner: nil) as? TopicTableCellView else {
      print("not found")
      return nil
    }

    let topic = topics[row]
    view.textField?.stringValue = topic.name
    view.partitionsField.stringValue = "\(topic.partitions)"
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

// MARK: -NSUserInterfaceItemIdentifier
extension NSUserInterfaceItemIdentifier {
  static let topics = NSUserInterfaceItemIdentifier("Topics")
}
