import Cocoa
import NoiseSerde

class PublishRecordFormViewController: NSViewController {
  private var metadata: Metadata?
  private var topic: Topic?
  private var partition: UVarint?

  @IBOutlet weak var topicButton: NSPopUpButton!
  @IBOutlet weak var partitionButton: NSPopUpButton!
  @IBOutlet weak var keyField: NSTextField!
  @IBOutlet weak var valueField: NSTextField!
  @IBOutlet weak var cancelButton: NSButton!
  @IBOutlet weak var publishButton: NSButton!

  weak var delegate: PublishRecordFormDelegate?

  override func viewDidLoad() {
    super.viewDidLoad()

    topicButton.target = self
    topicButton.action = #selector(didSelectTopic(_:))
    topicButton.removeAllItems()

    partitionButton.target = self
    partitionButton.action = #selector(didSelectPartition(_:))
    partitionButton.removeAllItems()

    self.reset()
  }

  func configure(withMetadata meta: Metadata, andTopic topic: Topic?) {
    self.metadata = meta
    self.topic = topic
    if let topic {
      self.partition = topic.partitions[0].id
    }
  }

  private func reset() {
    topicButton.isEnabled = false
    partitionButton.isEnabled = false
    keyField.isEnabled = false
    valueField.isEnabled = false
    publishButton.isEnabled = false

    guard let metadata else { return }
    topicButton.removeAllItems()
    topicButton.addItem(withTitle: "")
    for (i, t) in metadata.topics.enumerated() {
      topicButton.addItem(withTitle: t.name)
      if t.name == topic?.name {
        topicButton.selectItem(at: i+1)
      }
    }
    topicButton.isEnabled = true

    guard let topic else { return }
    partitionButton.removeAllItems()
    for (i, p) in topic.partitions.enumerated() {
      partitionButton.addItem(withTitle: "\(p.id)")
      if p.id == partition {
        partitionButton.selectItem(at: i)
      }
    }
    partitionButton.isEnabled = true

    keyField.isEnabled = true
    valueField.isEnabled = true
    publishButton.isEnabled = true
  }

  @objc func didSelectTopic(_ sender: NSPopUpButton) {
    guard let metadata else { return }
    guard let name = sender.selectedItem?.title else { return }
    topic = metadata.topics.first(where: { $0.name == name })
    partition = topic?.partitions[0].id
    reset()
  }

  @objc func didSelectPartition(_ sender: NSPopUpButton) {
    guard let pid = UVarint(sender.selectedItem?.title ?? "") else { return }
    partition = pid
  }

  @IBAction func didPressCancelButton(_ sender: Any) {
    delegate?.didCancelPublishRecordForm(self)
  }

  @IBAction func didPressPublishButton(_ sender: Any) {
    guard let topic else {
      topicButton.becomeFirstResponder()
      return
    }
    guard let partition else {
      partitionButton.becomeFirstResponder()
      return
    }
    delegate?.didSubmitPublishRecordForm(
      self,
      withTopic: topic,
      partitionId: partition,
      key: keyField.stringValue,
      andValue: valueField.stringValue)
  }
}

// MARK: - PublishRecordFormDelegate
protocol PublishRecordFormDelegate: AnyObject {
  func didCancelPublishRecordForm(_ sender: PublishRecordFormViewController)
  func didSubmitPublishRecordForm(
    _ sender: PublishRecordFormViewController,
    withTopic topic: Topic, partitionId pid: UVarint, key: String, andValue value: String)
}
