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

  private func getRecords(_ proc: @escaping () -> Void = {}) {
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
      proc()
    }
  }

  @objc func didPressSegmentedControl(_ sender: NSSegmentedControl) {
    if sender.selectedSegment == 1 {
      sender.isEnabled = false
      getRecords {
        sender.isEnabled = true
      }
    }
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
    if id == .TopicRecordsPartitionId {
      textField.stringValue = "\(record.partitionId)"
      textField.font = .monospacedDigitSystemFont(ofSize: 12, weight: .regular)
    } else if id == .TopicRecordsOffset {
      textField.stringValue = "\(record.offset)"
      textField.font = .monospacedDigitSystemFont(ofSize: 12, weight: .regular)
    } else if id == .TopicRecordsKey {
      if let data = record.key {
        textField.stringValue = String(data: data, encoding: .utf8) ?? data.base64EncodedString()
      } else {
        textField.stringValue = ""
      }
      textField.font = .systemFont(ofSize: 12)
    } else if id == .TopicRecordsValue {
      if let data = record.value {
        textField.stringValue = String(data: data, encoding: .utf8) ?? data.base64EncodedString()
      } else {
        textField.stringValue = ""
      }
      textField.font = .systemFont(ofSize: 12)
    }
    return view
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
