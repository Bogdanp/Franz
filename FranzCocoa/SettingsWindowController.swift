import Cocoa
import NoiseSerde
import OSLog
import SwiftUI

fileprivate let logger = Logger(
  subsystem: "io.defn.Franz",
  category: "Settings"
)

enum SettingsTab: Int {
  case general = 0
  case connections = 1
  case license = 2
  case updates = 3
}

class SettingsWindowController: NSWindowController {
  private lazy var tabController = SettingsTabViewController()

  convenience init() {
    self.init(windowNibName: "SettingsWindowController")
  }

  override func windowDidLoad() {
    super.windowDidLoad()

    let generalItem = NSTabViewItem()
    generalItem.image = .init(systemSymbolName: "gearshape", accessibilityDescription: "General Tab")
    generalItem.label = "General"
    generalItem.viewController = NSHostingController(rootView: GeneralView())

    let connectionsItem = NSTabViewItem()
    connectionsItem.image = .init(systemSymbolName: "network", accessibilityDescription: "Connections Tab")
    connectionsItem.label = "Connections"
    connectionsItem.viewController = NSHostingController(rootView: ConnectionsView())

#if !MAC_APP_STORE_BUILD
    let licenseItem = NSTabViewItem()
    licenseItem.image = .init(systemSymbolName: "checkmark.seal", accessibilityDescription: "License Tab")
    licenseItem.label = "License"
    licenseItem.viewController = NSHostingController(rootView: LicenseView())

    let updatesItem = NSTabViewItem()
    updatesItem.image = .init(systemSymbolName: "arrow.clockwise", accessibilityDescription: "Updates Tab")
    updatesItem.label = "Updates"
    updatesItem.viewController = NSHostingController(rootView: UpdatesView())
#endif

    tabController.delegate = self
    tabController.tabStyle = .toolbar
    tabController.addTabViewItem(generalItem)
    tabController.addTabViewItem(connectionsItem)
#if !MAC_APP_STORE_BUILD
    tabController.addTabViewItem(licenseItem)
    tabController.addTabViewItem(updatesItem)
#endif

    window?.contentViewController = tabController
    window?.setContentSize(.init(width: 500, height: 150))
    window?.center()
  }

  func display(tab: SettingsTab) {
    tabController.selectedTabViewItemIndex = tab.rawValue
  }
}

// MARK: - SettingsTabViewDelegate
extension SettingsWindowController: SettingsTabViewDelegate {
  func settingsTabView(didSelectItem item: NSTabViewItem) {
    let index = tabController.tabView.indexOfTabViewItem(item)
    guard let window, let tab = SettingsTab(rawValue: index) else {
      return
    }

    var size: NSSize!
    switch tab {
    case .general:
      size = .init(width: 500, height: 170)
    case .connections:
      size = .init(width: 500, height: 400)
    case .license:
      size = .init(width: 500, height: 250)
    case .updates:
      size = .init(width: 500, height: 150)
    }

    window.title = item.label
    window.setContentSize(size)
  }
}

// MARK: - SettingsTabViewController
protocol SettingsTabViewDelegate: AnyObject {
  func settingsTabView(didSelectItem item: NSTabViewItem)
}

fileprivate class SettingsTabViewController: NSTabViewController {
  var delegate: SettingsTabViewDelegate?

  override func tabView(_ tabView: NSTabView, didSelect tabViewItem: NSTabViewItem?) {
    super.tabView(tabView, didSelect: tabViewItem)
    guard let item = tabViewItem else { return }
    delegate?.settingsTabView(didSelectItem: item)
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

// MARK: - ConnectionsView
fileprivate struct ConnectionsView: View {
  @State private var connections = Error.wait(Backend.shared.getConnections()) ?? []
  @State private var selectedId: UVarint??
  @State private var sidebarOptionsKey: String?
  @State private var sidebarOptions: SidebarOptions?
  @State private var topicRecordsOptionsKey: String?
  @State private var topicRecordsOptions: TopicRecordsOptions?

  var body: some View {
    HStack(alignment: .top) {
      Table(connections, selection: $selectedId) {
        TableColumn("Connection", value: \.name)
      }
      .tableStyle(.bordered)
      .frame(width: 200)
      .onChange(of: selectedId) { id in
        guard let connection = connections.first(where: { $0.id == id }) else {
          sidebarOptionsKey = nil
          sidebarOptions = nil
          topicRecordsOptionsKey = nil
          topicRecordsOptions = nil
          return
        }

        let optionsRoot = "Franz:\(connection.name)"

        sidebarOptionsKey = "\(optionsRoot):SidebarOptions"
        sidebarOptions = Defaults.shared.get(codable: sidebarOptionsKey!) ?? SidebarOptions()
        topicRecordsOptionsKey = "\(optionsRoot):TopicRecordsOptions"
        topicRecordsOptions = Defaults.shared.get(codable: topicRecordsOptionsKey!) ?? TopicRecordsOptions()
      }
      VStack {
        if let sidebarOptionsKey, let sidebarOptions,
           let topicRecordsOptionsKey, let topicRecordsOptions {
          TabView {
            VStack {
              SidebarOptionsForm(model: sidebarOptions) {
                try? Defaults.shared.set(codable: sidebarOptions, forKey: sidebarOptionsKey)
              }
              Spacer()
            }
            .tabItem {
              Label("Sidebar", systemImage: "sidebar.left")
            }

            VStack {
              TopicRecordsOptionsForm(model: topicRecordsOptions) {
                try? Defaults.shared.set(codable: topicRecordsOptions, forKey: topicRecordsOptionsKey)
              }
              Spacer()
            }
            .tabItem {
              Label("Iterators", systemImage: "gearshape")
            }
          }
        } else {
          VStack {
            Spacer()
            Text("Please select a connection.")
            Spacer()
          }
        }
      }
      .frame(maxWidth: .greatestFiniteMagnitude)
    }
    .padding()
  }
}

// MARK: - LicenseView
fileprivate struct LicenseView: View {
  @State var isLoading = false
  @State var activatedLicense = ""
  @State var trialActive = false
  @State var trialDeadline: String = ""
  @State var license = ""

  var body: some View {
    VStack(alignment: .leading) {
      if isLoading {
        ProgressView()
      } else if activatedLicense != "" {
        Text("Full Version Activated")
          .font(.largeTitle)
        Text("Thank you for supporting Franz development by purchasing a license.")
      } else if trialActive {
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
      } else {
        Text("Trial Expired")
          .font(.largeTitle)
        Text("""
        Your trial has ended.  Please [purchase a license](https://franz.defn.io) \
        to continue using Franz.
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
    }
    .padding()
    .task {
      do {
        isLoading = true
        defer { isLoading = false }
        activatedLicense = try await Backend.shared.getLicense() ?? ""
        let deadline = try await Backend.shared.getTrialDeadline()
        let now = UVarint(NSDate().timeIntervalSince1970)
        let date = Date(timeIntervalSince1970: TimeInterval(Int(deadline)))
        trialActive = deadline > now
        trialDeadline = DateFormatter.localizedString(
          from: date,
          dateStyle: .long,
          timeStyle: .none
        )
      } catch {
        Error.alert(withError: error)
      }
    }
  }

  private func activate() {
    if let activated = Error.wait(Backend.shared.activateLicense(license)), activated {
      activatedLicense = Error.wait(Backend.shared.getLicense()) ?? ""
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
    AutoUpdater.shared.stop()
    AutoUpdater.shared.start(
      withInterval: checkForUpdates ? interval.seconds * 1000 : nil,
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
