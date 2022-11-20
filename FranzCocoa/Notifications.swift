import Foundation

// MARK: - Notification
extension Notification.Name {
  static let NewConnectionRequested = Self("Franz.NewConnectionRequested")
  static let EditorTextChanged = Self("Franz.EditorTextChanged")
  static let TopicRecordsTableAppeared = Self("Franz.TopicRecordsTableAppeared")
  static let TopicRecordsTableDisappeared = Self("Franz.TopicRecordsTableDisappeared")
}
