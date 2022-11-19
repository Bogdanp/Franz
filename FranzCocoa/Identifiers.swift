import AppKit
import Foundation

extension NSUserInterfaceItemIdentifier {
  static let FileMenuItem = Self("FileMenuItem")
  static let OpenMenuItem = Self("OpenMenuItem")
  static let SaveMenuItem = Self("SaveMenuItem")
  static let SaveAsMenuItem = Self("SaveAsMenuItem")
  static let RevertMenuItem = Self("RevertMenuItem")
  static let ConnectionMenuItem = Self("ConnectionMenuItem")
  static let ReloadMetadataMenuItem = Self("ReloadMetadataMenuItem")
  static let TopicMenuItem = Self("TopicMenuItem")
  static let NewTopicMenuItem = Self("NewTopicMenuItem")
  static let PublishMenuItem = Self("PublishMenuItem")

  static let GroupOffsetsTopic = Self("GroupOffsets.Topic")
  static let GroupOffsetsOffset = Self("GroupOffsets.Offset")
  static let GroupOffsetsLag = Self("GroupOffsets.Lag")
  static let GroupOffsetsConsumerId = Self("GroupOffsets.ConsumerId")
  static let GroupOffsetsHost = Self("GroupOffsets.Host")
  static let GroupOffsetsClientId = Self("GroupOffsets.ClientId")

  static let ResourceConfigConfig = Self("ResourceConfig.Config")
  static let ResourceConfigValue = Self("ResourceConfig.Value")

  static let TopicRecordsPartitionId = Self("TopicRecords.PartitionId")
  static let TopicRecordsOffset = Self("TopicRecords.Offset")
  static let TopicRecordsTimestamp = Self("TopicRecords.Timestamp")
  static let TopicRecordsKey = Self("TopicRecords.Key")
  static let TopicRecordsValue = Self("TopicRecords.Value")
}
