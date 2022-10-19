import Cocoa
import SwiftUI

class PreferencesWindowController: NSWindowController {
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
    licenseItem.image = .init(systemSymbolName: "person.badge.key", accessibilityDescription: nil)
    licenseItem.label = "License"
    licenseItem.viewController = NSHostingController(rootView: LicenseView())

    let tabController = PreferencesTabViewController()
    tabController.delegate = self
    tabController.tabStyle = .toolbar
    tabController.addTabViewItem(generalItem)
    tabController.addTabViewItem(licenseItem)

    window?.contentViewController = tabController
  }
}

// MARK: -PreferencesTabViewDelegate
extension PreferencesWindowController: PreferencesTabViewDelegate {
  func preferencesTabView(didSelectItem item: NSTabViewItem) {
    window?.title = item.label
  }
}

// MARK: -PreferencesTabViewController
protocol PreferencesTabViewDelegate {
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

// MARK: -GeneralView
fileprivate struct GeneralView: View {
  @State var reloadIvl = Double(Defaults.shared.reloadIntervalMs)/1000.0

  var body: some View {
    Form {
      Slider(
        value: $reloadIvl,
        in: 1...10,
        step: 1
      ) {
        Text("Reload Interval:")
      } minimumValueLabel: {
        Text("")
          .font(.caption2)
      } maximumValueLabel: {
        Text("10 seconds")
          .font(.caption2)
      } onEditingChanged: { _ in
        Defaults.shared.reloadIntervalMs = Int(reloadIvl)*1000
      }
      Spacer()
    }
    .frame(width: 400)
    .padding()
  }
}

// MARK: -LicenseView
fileprivate struct LicenseView: View {
  var body: some View {
    Text("License")
  }
}
