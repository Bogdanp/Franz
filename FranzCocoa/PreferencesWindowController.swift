import Cocoa
import NoiseSerde
import OSLog
import SwiftUI

fileprivate let logger = Logger(
  subsystem: "io.defn.Franz",
  category: "Preferences"
)

enum PreferencesTab {
  case general
  case license
  case updates
}

class PreferencesWindowController: NSWindowController {
  private lazy var tabController = PreferencesTabViewController()

  convenience init() {
    self.init(windowNibName: "PreferencesWindowController")
  }

  override func windowDidLoad() {
    super.windowDidLoad()

    let generalItem = NSTabViewItem()
    generalItem.image = .init(systemSymbolName: "gearshape", accessibilityDescription: "General Tab")
    generalItem.label = "General"
    generalItem.viewController = NSHostingController(rootView: GeneralView())

    let licenseItem = NSTabViewItem()
    licenseItem.image = .init(systemSymbolName: "checkmark.seal", accessibilityDescription: "License Tab")
    licenseItem.label = "License"
    licenseItem.viewController = NSHostingController(rootView: LicenseView())

    let updatesItem = NSTabViewItem()
    updatesItem.image = .init(systemSymbolName: "arrow.clockwise", accessibilityDescription: "Updates Tab")
    updatesItem.label = "Updates"
    updatesItem.viewController = NSHostingController(rootView: UpdatesView())

    tabController.delegate = self
    tabController.tabStyle = .toolbar
    tabController.addTabViewItem(generalItem)
    tabController.addTabViewItem(licenseItem)
    tabController.addTabViewItem(updatesItem)

    window?.contentViewController = tabController
    window?.setContentSize(.init(width: 500, height: 150))
    window?.center()
  }

  func display(tab: PreferencesTab) {
    var index: Int!
    switch tab {
    case .general:
      index = 0
    case .license:
      index = 1
    case .updates:
      index = 2
    }
    tabController.selectedTabViewItemIndex = index
  }
}

// MARK: - PreferencesTabViewDelegate
extension PreferencesWindowController: PreferencesTabViewDelegate {
  func preferencesTabView(didSelectItem item: NSTabViewItem) {
    guard let window else { return }
    window.title = item.label
    switch item.label {
    case "General":
      window.setContentSize(.init(width: 500, height: 170))
    case "License":
      window.setContentSize(.init(width: 500, height: 250))
    case "Updates":
      window.setContentSize(.init(width: 500, height: 150))
    default:
      ()
    }
  }
}

// MARK: - PreferencesTabViewController
protocol PreferencesTabViewDelegate: AnyObject {
  func preferencesTabView(didSelectItem item: NSTabViewItem)
}

fileprivate class PreferencesTabViewController: NSTabViewController {
  var delegate: PreferencesTabViewDelegate?

  override func tabView(_ tabView: NSTabView, didSelect tabViewItem: NSTabViewItem?) {
    super.tabView(tabView, didSelect: tabViewItem)
    guard let item = tabViewItem else { return }
    delegate?.preferencesTabView(didSelectItem: item)
  }
}

// MARK: - GeneralView
fileprivate struct GeneralView: View {
  @State var reloadIvl = Double(Defaults.shared.reloadIntervalMs)/1000.0

  var body: some View {
    Form {
      VStack {
        Slider(
          value: $reloadIvl,
          in: 1...30,
          step: 1
        ) {
          Text("Reload Interval:")
        } onEditingChanged: { _ in
          Defaults.shared.reloadIntervalMs = Int(reloadIvl)*1000
        }
        Text(reloadIvl > 1 ? "Every \(Int(reloadIvl)) seconds." : "Every second.")
      }
      Spacer()
    }
    .padding()
  }
}

// MARK: - LicenseView
fileprivate struct LicenseView: View {
  @State var activatedLicense = Error.wait(Backend.shared.getLicense()) ?? ""
  @State var license = ""
  let (trialActive, trialDeadline): (Bool, String) = {
    if let deadline = Error.wait(Backend.shared.getTrialDeadline()) {
      let now = UVarint(NSDate().timeIntervalSince1970)
      let date = Date(timeIntervalSince1970: TimeInterval(Int(deadline)))
      return (deadline > now, DateFormatter.localizedString(
        from: date,
        dateStyle: .long,
        timeStyle: .none
      ))
    }
    return (false, "")
  }()

  var body: some View {
    if activatedLicense != "" {
      VStack(alignment: .leading) {
        Text("Full Version Activated")
          .font(.largeTitle)
        Text("Thank you for supporting Franz development by purchasing a license.")
      }
      .padding()
    } else if trialActive {
      VStack(alignment: .leading) {
        Text("Trial Active")
          .font(.largeTitle)
        Text("""
        Your trial ends on \(trialDeadline). Please [purchase a license]\
        (https://franz.defn.io) to support Franz development and to continue \
        using the software past that date.
        """)
        Form {
          TextField("License Key:", text: $license)
            .onSubmit {
              activate()
            }
          Button("Activate License") {
            activate()
          }
          .keyboardShortcut(.return)
          .disabled(license == "")
        }
      }
      .padding()
    } else {
      VStack(alignment: .leading) {
        Text("Trial Expired")
          .font(.largeTitle)
        Text("""
        Your trial has ended.  Please [purchase a license](https://franz.defn.io) \
        to continue using the software.
        """)
        Form {
          TextField("License Key:", text: $license)
            .onSubmit {
              activate()
            }
          Button("Activate License") {
            activate()
          }
          .keyboardShortcut(.return)
          .disabled(license == "")
        }
      }
      .padding()
    }
  }

  private func activate() {
    if let activated = Error.wait(Backend.shared.activateLicense(license)), activated {
      activatedLicense = license
    }
  }
}

// MARK: - UpdatesView
fileprivate struct UpdatesView: View {
  @State var checkForUpdates = Defaults.shared.checkForUpdates
  @State var interval = Defaults.shared.updateInterval

  var body: some View {
    Form {
      Toggle("Automatically check for updates", isOn: $checkForUpdates)
        .onChange(of: checkForUpdates) { check in
          Defaults.shared.checkForUpdates = check
          self.reset()
        }
      Picker("Check interval:", selection: $interval) {
        Text("Every hour").tag(UpdateInterval.everyHour)
        Text("Every four hours").tag(UpdateInterval.everyFourHours)
        Text("Every day").tag(UpdateInterval.everyDay)
      }
      .onChange(of: interval) { ivl in
        Defaults.shared.updateInterval = ivl
        self.reset()
      }
      Spacer()
    }
    .padding()
  }

  private func reset() {
    if !checkForUpdates {
      AutoUpdater.shared.stop()
      return
    }

    AutoUpdater.shared.start(
      withInterval: Double(interval.seconds),
      checkingImmediately: false
    ) { changes, version in
      WindowManager.shared.showUpdatesWindow(withChangelog: changes, andRelease: version)
    }
  }
}

enum UpdateInterval: Hashable, Codable {
  case everyHour
  case everyFourHours
  case everyDay

  var seconds: Int {
    switch self {
    case .everyHour:
      return 3600
    case .everyFourHours:
      return 4 * 3600
    case .everyDay:
      return 24 * 3600
    }
  }
}
