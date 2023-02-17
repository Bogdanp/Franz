import Foundation
import SwiftUI

class UpdatesWindowController: NSWindowController {
  override func showWindow(_ sender: Any?) {
    guard window == nil else {
      window?.makeKeyAndOrderFront(self)
      return
    }
    window = NSWindow(
      contentRect: NSRect(x: 0, y: 0, width: 640, height: 600),
      styleMask: [.closable, .titled],
      backing: .buffered,
      defer: false
    )
    window?.title = "Software Update"
    window?.center()
    window?.makeKeyAndOrderFront(self)
    super.showWindow(sender)
  }

  func configure(withChangelog changelog: String, andRelease release: Release) {
    window?.contentView = NSHostingView(rootView: UpdatesView(changelog: changelog, release: release))
  }
}

fileprivate class UpdatesStore: ObservableObject {
  @Published var updating = false

  func performUpdate(toRelease release: Release) {
    if updating {
      return
    }

    updating = true
    AutoUpdater.shared.performUpdate(toRelease: release) { res in
      RunLoop.main.schedule {
        defer {
          self.updating = false
        }

        switch res {
        case .ok(let url):
          NSWorkspace.shared.open(url)
          NSApp.terminate(nil)
        case .error(let message):
          Error.alert(withError: message)
        }
      }
    }
  }
}

fileprivate struct UpdatesView: View {
  let changelog: String
  let release: Release

  @ObservedObject var store = UpdatesStore()

  var body: some View {
    HStack(alignment: .top, spacing: 16) {
      Image(nsImage: NSImage(named: "NSApplicationIcon")!)
        .resizable()
        .frame(width: 32, height: 32, alignment: .leading)

      VStack(alignment: .leading, spacing: 6) {
        Text("A new version of Franz is available!").bold()
        Text("Version \(release.version) is now available. Would you like to install it now?")
        Text("Release notes:").bold()
        Changelog(changelog)
          .frame(width: nil, height: 150, alignment: .top)
        HStack {
          Spacer()

          Button(action: {
            WindowManager.shared.closeUpdatesWindow()
          }, label: {
            Text("Remind me Later")
          })
          .buttonStyle(BorderedButtonStyle())
          .disabled(store.updating)

          Button(action: {
            self.store.performUpdate(toRelease: self.release)
          }, label: {
            if store.updating {
              ProgressIndicator()
              Text("Downloading Update...")
            } else {
              Text("Download Update")
            }
          })
          .buttonStyle(BorderedButtonStyle())
          .disabled(store.updating)
        }
      }
    }
    .padding(16)
  }
}

fileprivate struct Changelog: NSViewRepresentable {
  typealias NSViewType = NSScrollView

  let changelog: String

  init(_ changelog: String) {
    self.changelog = changelog
  }

  func makeNSView(context: NSViewRepresentableContext<Changelog>) -> NSViewType {
    let scrollView = NSTextView.scrollableTextView()
    scrollView.borderType = .noBorder
    scrollView.hasVerticalScroller = true
    scrollView.hasHorizontalScroller = true
    scrollView.autoresizingMask = [.width, .height]

    let textView = NSTextView()
    textView.font = .monospacedSystemFont(ofSize: 12, weight: .regular)
    scrollView.addSubview(textView)
    scrollView.documentView = textView

    return scrollView
  }

  func updateNSView(_ nsView: NSViewType, context: NSViewRepresentableContext<Changelog>) {
    nsView.backgroundColor = .white
    nsView.borderType = .lineBorder
    nsView.hasVerticalScroller = true
    nsView.hasHorizontalScroller = true
    nsView.autoresizingMask = [.width, .height]

    let contentSize = nsView.contentSize
    if let textView = nsView.documentView as? NSTextView {
      textView.backgroundColor = .white
      textView.isEditable = false
      textView.isVerticallyResizable = true
      textView.isHorizontallyResizable = true
      textView.autoresizingMask = [.height, .width]
      textView.textContainer?.size = NSSize(width: contentSize.width, height: .greatestFiniteMagnitude)
      textView.textContainer?.widthTracksTextView = true
      textView.string = changelog
    }
  }
}

fileprivate struct ProgressIndicator: NSViewRepresentable {
  typealias NSViewType = NSProgressIndicator

  func makeNSView(context: NSViewRepresentableContext<ProgressIndicator>) -> NSViewType {
    let indicator = NSProgressIndicator()
    indicator.controlSize = .small
    indicator.isIndeterminate = true
    indicator.style = .spinning
    indicator.translatesAutoresizingMaskIntoConstraints = false
    indicator.startAnimation(self)
    return indicator
  }

  func updateNSView(_ nsView: NSViewType, context: NSViewRepresentableContext<ProgressIndicator>) {

  }
}
