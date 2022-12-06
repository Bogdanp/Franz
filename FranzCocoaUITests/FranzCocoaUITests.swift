import XCTest

final class FranzCocoaUITests: XCTestCase {
  override func setUpWithError() throws {
    continueAfterFailure = false
  }

  func testLaunch() throws {
    makeUIApp().launch()
  }

  func testCreateConnection() throws {
    let app = makeUIApp()
    app.launch()

    let window = app.windows["Welcome to Franz"]
    window.buttons["New Connection..."].click()
    window.sheets.textFields["Connection Name"].typeText("Local")
    window.sheets.checkBoxes["Enable SSL"].click()
    window.sheets.buttons["Connect"].click()
    app.windows["Local : 127.0.0.1:9092"].typeKey("w", modifierFlags: .command)
    app.menuBarItems["Window"].menuItems["Welcome to Franz"].click()
    XCTAssertTrue(app.windows["Welcome to Franz"].staticTexts["Local"].exists)
    app.windows["Welcome to Franz"].typeKey("q", modifierFlags: .command)
  }
}

func makeUIApp() -> XCUIApplication {
  let app = XCUIApplication()
  app.launchEnvironment["FRANZ_TEMP_DATABASE"] = "x"
  return app
}
