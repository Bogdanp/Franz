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
    window.typeKey("q", modifierFlags: .command)
  }

  func testCreateDeleteConnection() throws {
    let app = makeUIApp()
    app.launch()

    let window = app.windows["Welcome to Franz"]
    window.buttons["New Connection..."].click()

    let sheet = window.sheets
    sheet.textFields["Connection Name"].typeText("Bad\t127.0.0.1\t9099")
    sheet.checkBoxes["Enable SSL"].click()
    sheet.buttons["Connect"].click()

    let dialog = app.dialogs["alert"]
    dialog.buttons["OK"].click()
    app.windows["Bad : 127.0.0.1:9099"].buttons[XCUIIdentifierCloseWindow].click()

    app.menuBarItems["Window"].menuItems["Welcome to Franz"].click()
    window.tables["Connections Table"]
      .children(matching: .tableRow)
      .element(boundBy: 0)
      .cells.containing(.image, identifier: "Server")
      .element.rightClick()
    window.tables["Connections Table"].menuItems["Delete"].click()
    dialog.buttons["Delete"].click()

    XCTAssertTrue(window.staticTexts["No Recent Connections"].isHittable)
    window.typeKey("q", modifierFlags:.command)
  }
}

func makeUIApp() -> XCUIApplication {
  let app = XCUIApplication()
  app.launchEnvironment["FRANZ_TEMP_DATABASE"] = "x"
  return app
}
