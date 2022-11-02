import XCTest

final class FranzCocoaUITests: XCTestCase {
  override func setUpWithError() throws {
    continueAfterFailure = false
  }

  func testExample() throws {
    let app = XCUIApplication()
    app.launch()
  }

  func testLaunchPerformance() throws {
    measure(metrics: [XCTApplicationLaunchMetric()]) {
      XCUIApplication().launch()
    }
  }
}
