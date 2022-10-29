import Cocoa
import NoiseSerde
import SwiftUI

class TopicRecordsTableViewController: NSViewController {
  @IBOutlet weak var tableView: NSTableView!
  @IBOutlet weak var segmentedControl: NSSegmentedControl!

  weak var delegate: WorkspaceDetailDelegate?

  private var id: UVarint!
  private var topic: String!
  private var iteratorId: UVarint?

  private var records = [IteratorRecord]()

  override func viewDidLoad() {
    super.viewDidLoad()

    tableView.dataSource = self
    tableView.delegate = self

    segmentedControl.target = self
    segmentedControl.action = #selector(didPressSegmentedControl(_:))
  }

  func configure(withId id: UVarint, andTopic topic: String) {
    self.id = id
    self.topic = topic
    self.iteratorId = Error.wait(Backend.shared.openIterator(forTopic: topic, andOffset: .earliest, inWorkspace: id))
    self.getRecords()
  }

  private func getRecords(_ proc: @escaping ([IteratorRecord]) -> Void = { _ in }) {
    guard let delegate else { return }
    guard let iteratorId else { return }

    let status = delegate.makeStatusProc()
    status("Fetching records...")
    Backend.shared.getRecords(iteratorId).onComplete { records in
      if !records.isEmpty {
        self.records = records
      }
      self.tableView.reloadData()
      status("Ready")
      proc(records)
    }
  }

  @objc func didPressSegmentedControl(_ sender: NSSegmentedControl) {
    if sender.selectedSegment == 1 {
      sender.isEnabled = false
      getRecords { records in
        if records.isEmpty {
          let bounds = sender.relativeBounds(forSegment: 1)
          let popover = NSPopover()
          popover.behavior = .transient
          popover.contentSize = NSSize(width: 200, height: 50)
          popover.contentViewController = NSHostingController(rootView: Text("No more records.").padding())
          popover.show(relativeTo: bounds, of: sender, preferredEdge: .minY)
        }
        sender.isEnabled = true
      }
    }
  }
}

// MARK: - NSSegmentedControl
extension NSSegmentedControl {
  func relativeBounds(forSegment index: Int) -> NSRect {
    let w = bounds.width / CGFloat(segmentCount)
    var r = bounds
    r.size.width = w
    r.origin.x += w * CGFloat(index)
    return r
  }
}

// MARK: - NSTableViewDataSource
extension TopicRecordsTableViewController: NSTableViewDataSource {
  func numberOfRows(in tableView: NSTableView) -> Int {
    return records.count
  }
}

// MARK: - NSTableViewDelegate
extension TopicRecordsTableViewController: NSTableViewDelegate {
  func tableView(_ tableView: NSTableView, viewFor tableColumn: NSTableColumn?, row: Int) -> NSView? {
    guard let id = tableColumn?.identifier else { return nil }
    guard let view = tableView.makeView(withIdentifier: id, owner: self) as? NSTableCellView else { return nil }
    guard let textField = view.textField else { return nil }
    let record = records[row]
    textField.font = .monospacedSystemFont(ofSize: 12, weight: .regular)
    textField.textColor = tableView.selectedRow == row ? .selectedControlTextColor : .controlTextColor
    if id == .TopicRecordsPartitionId {
      textField.stringValue = "\(record.partitionId)"
    } else if id == .TopicRecordsOffset {
      textField.stringValue = "\(record.offset)"
    } else if id == .TopicRecordsKey {
      setTextFieldData(textField, data: record.key)
    } else if id == .TopicRecordsValue {
      setTextFieldData(textField, data: record.value)
    }
    return view
  }

  private func setTextFieldData(_ textField: NSTextField, data: Data?) {
    if let data {
      if let string = String(data: data, encoding: .utf8) {
        textField.stringValue = string
        return
      }
      textField.stringValue = "BINARY DATA"
      textField.textColor = .secondaryLabelColor
    } else {
      textField.stringValue = "NULL"
      textField.textColor = .secondaryLabelColor
    }
  }
}

// MARK: - TopicRecordsTable
struct TopicRecordsTable: NSViewControllerRepresentable {
  typealias NSViewControllerType = TopicRecordsTableViewController

  let id: UVarint
  let topic: String
  weak var delegate: WorkspaceDetailDelegate?

  func makeNSViewController(context: Context) -> TopicRecordsTableViewController {
    let ctl = TopicRecordsTableViewController()
    ctl.delegate = delegate
    ctl.configure(withId: id, andTopic: topic)
    return ctl
  }

  func updateNSViewController(_ nsViewController: TopicRecordsTableViewController, context: Context) {
  }
}
