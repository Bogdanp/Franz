import Cocoa
import NoiseSerde
import SwiftUI

enum PreferencesTab {
  case general
  case license
}

class PreferencesWindowController: NSWindowController {
  private lazy var tabController = PreferencesTabViewController()

  convenience init() {
    self.init(windowNibName: "PreferencesWindowController")
  }

  override func windowDidLoad() {
    super.windowDidLoad()

    let generalItem = NSTabViewItem()
    generalItem.image = .init(systemSymbolName: "gearshape", accessibilityDescription: nil)
    generalItem.label = "General"
    generalItem.viewController = NSHostingController(rootView: GeneralView())

    let licenseItem = NSTabViewItem()
    licenseItem.image = .init(systemSymbolName: "checkmark.seal", accessibilityDescription: nil)
    licenseItem.label = "License"
    licenseItem.viewController = NSHostingController(rootView: LicenseView())

    tabController.delegate = self
    tabController.tabStyle = .toolbar
    tabController.addTabViewItem(generalItem)
    tabController.addTabViewItem(licenseItem)

    window?.contentViewController = tabController
    window?.setContentSize(.init(width: 600, height: 150))
    window?.center()
  }

  func display(tab: PreferencesTab) {
    switch tab {
    case .general:
      tabController.selectedTabViewItemIndex = 0
    case .license:
      tabController.selectedTabViewItemIndex = 1
    }
  }
}

// MARK: - PreferencesTabViewDelegate
extension PreferencesWindowController: PreferencesTabViewDelegate {
  func preferencesTabView(didSelectItem item: NSTabViewItem) {
    guard let window else { return }
    window.title = item.label
    // FIXME: There must be a better approach.
    switch item.label {
    case "General":
      window.setContentSize(.init(width: 600, height: 170))
    case "License":
      window.setContentSize(.init(width: 600, height: 250))
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
        Your trial ends on \(trialDeadline). Please purchase a license to \
        support Franz development and to continue using the software past \
        that date.
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
        Your trial has ended.  Please [Purchase a License](https://franz.defn.io) \
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
