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
    try createLocalConnection(app)

    app.windows["Local — 127.0.0.1:9092"].typeKey("w", modifierFlags: .command)
    app.menuBarItems["Window"].menuItems["Welcome to Franz"].click()
    XCTAssertTrue(app.windows["Welcome to Franz"].staticTexts["Local"].exists)
    app.windows["Welcome to Franz"].typeKey("q", modifierFlags: .command)
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
    app.windows["Bad — 127.0.0.1:9099"].buttons[XCUIIdentifierCloseWindow].click()

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

  func testExpiredTrial() throws {
    let keyData = try Data(contentsOf: Bundle(for: type(of: self)).url(
      forResource: "fixtures/expired-trial-key",
      withExtension: "txt"
    )!)
    let key = String(data: keyData, encoding: .utf8)!.trimmingCharacters(in: .whitespacesAndNewlines)
    let app = makeUIApp(withDatabasePath: fixture("2022-12-06-expired-trial"))
    app.launch()
    try createLocalConnection(app)

    let licenseWindow = app.windows["License"]
    let licenseField = licenseWindow.groups.children(matching: .textField).element
    licenseField.click()
    licenseField.typeText(key)
    licenseWindow.buttons["Activate License"].click()
    XCTAssertTrue(licenseWindow.staticTexts["Full Version Activated"].isHittable)
    licenseWindow.typeKey("q", modifierFlags:.command)
  }

  func testMechanismSelector() throws {
    let app = makeUIApp()
    app.launch()

    let window = app.windows["Welcome to Franz"]
    window.buttons["New Connection..."].click()

    let sheet = window.sheets.firstMatch
    let nameField = sheet.textFields["Connection Name"]
    nameField.click()
    nameField.typeText("Example")
    let usernameField = sheet.textFields["root"]
    usernameField.click()
    usernameField.typeText("root")
    let passwordField = sheet.secureTextFields.firstMatch
    passwordField.click()
    passwordField.typeText("hunter2")

    sheet.popUpButtons["PLAIN"].click()
    sheet.menuItems["SCRAM-SHA-256"].click()
    sheet.popUpButtons["SCRAM-SHA-256"].click()
    sheet.menuItems["SCRAM-SHA-512"].click()
    sheet.popUpButtons["SCRAM-SHA-512"].click()
    sheet.menuItems["AWS-MSK-IAM"].click()

    let regionField = sheet.textFields["us-east-1"]
    regionField.click()
    regionField.typeText("us-east-1")
    let accessKeyField = sheet.textFields["AKEYEXAMPLE"]
    accessKeyField.click()
    accessKeyField.typeText("example")
    let secretKeyField = sheet.secureTextFields.firstMatch
    secretKeyField.click()
    secretKeyField.typeText("hunter2")
    sheet.buttons["Cancel"].click()
    window.typeKey("q", modifierFlags: .command)
  }

  private func fixture(_ path: String, ofType ext: String = "sqlite3") -> String {
    return Bundle(for: type(of: self)).path(forResource: "fixtures/\(path)", ofType: ext)!
  }

  private func createLocalConnection(_ app: XCUIApplication) throws {
    let window = app.windows["Welcome to Franz"]
    window.buttons["New Connection..."].click()
    window.sheets.textFields["Connection Name"].typeText("Local")
    window.sheets.checkBoxes["Enable SSL"].click()
    window.sheets.buttons["Connect"].click()
  }
}

func makeUIApp(withDatabasePath path: String = "x") -> XCUIApplication {
  let app = XCUIApplication()
  app.launchEnvironment["FRANZ_DATABASE_PATH"] = path
  return app
}
