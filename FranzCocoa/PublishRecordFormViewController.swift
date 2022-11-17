import Cocoa
import NoiseSerde

class PublishRecordFormViewController: NSViewController {
  private var metadata: Metadata?
  private var topic: Topic?
  private var partition: UVarint?

  @IBOutlet weak var topicButton: NSPopUpButton!
  @IBOutlet weak var partitionButton: NSPopUpButton!
  @IBOutlet weak var keyField: NSTextField!
  @IBOutlet weak var nullKeyButton: NSButton!
  @IBOutlet weak var valueField: NSTextField!
  @IBOutlet weak var nullValueButton: NSButton!
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

    keyField.font = .monospacedSystemFont(ofSize: 12, weight: .regular)
    valueField.font = .monospacedSystemFont(ofSize: 12, weight: .regular)

    self.reset()
  }

  override func viewDidAppear() {
    view.window?.setFrame(NSRect(x: 0, y: 0, width: 460, height: 292), display: true)
    view.window?.styleMask.remove(.resizable)
    view.window?.styleMask.update(with: .fullSizeContentView)
  }

  func configure(withMetadata meta: Metadata, andTopic topic: Topic?) {
    self.metadata = meta
    if let topic {
      self.topic = topic
    } else if meta.topics.count > 0 {
      self.topic = meta.topics[0]
    }
    self.partition = self.topic?.partitions[0].id
  }

  private func reset() {
    topicButton.isEnabled = false
    partitionButton.isEnabled = false
    keyField.isEnabled = false
    valueField.isEnabled = false
    publishButton.isEnabled = false

    guard let metadata else { return }
    topicButton.removeAllItems()
    for (i, t) in metadata.topics.enumerated() {
      topicButton.addItem(withTitle: t.name)
      if t.name == topic?.name {
        topicButton.selectItem(at: i)
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

    keyField.isEnabled = nullKeyButton.state == .on
    valueField.isEnabled = nullValueButton.state == .on
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

  @IBAction func didToggleNullKeyButton(_ sender: NSButton) {
    reset()
    if sender.state == .on {
      keyField.becomeFirstResponder()
    }
  }

  @IBAction func didToggleNullValueButton(_ sender: NSButton) {
    reset()
    if sender.state == .on {
      valueField.becomeFirstResponder()
    }
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
      key: nullKeyButton.state == .on ? keyField.stringValue : nil,
      andValue: nullValueButton.state == .on ? valueField.stringValue : nil)
  }
}

// MARK: - PublishRecordFormDelegate
protocol PublishRecordFormDelegate: AnyObject {
  func didCancelPublishRecordForm(_ sender: PublishRecordFormViewController)
  func didSubmitPublishRecordForm(
    _ sender: PublishRecordFormViewController,
    withTopic topic: Topic,
    partitionId pid: UVarint,
    key: String?,
    andValue value: String?)
}