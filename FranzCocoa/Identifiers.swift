import AppKit
import Foundation

extension NSUserInterfaceItemIdentifier {
  static let FileMenuItem = Self("FileMenuItem")
  static let ConnectionMenuItem = Self("ConnectionMenuItem")
  static let ReloadMetadataMenuItem = Self("ReloadMetadataMenuItem")
  static let TopicMenuItem = Self("TopicMenuItem")
  static let NewTopicMenuItem = Self("NewTopicMenuItem")

  static let GroupOffsetsTopic = Self("GroupOffsets.Topic")
  static let GroupOffsetsOffset = Self("GroupOffsets.Offset")
  static let GroupOffsetsLag = Self("GroupOffsets.Lag")
  static let GroupOffsetsConsumerId = Self("GroupOffsets.ConsumerId")
  static let GroupOffsetsHost = Self("GroupOffsets.Host")
  static let GroupOffsetsClientId = Self("GroupOffsets.ClientId")
}
