import NoiseSerde
import SwiftUI

struct TopicDetailView: View {
  var id: UVarint
  var topic: Topic
  weak var delegate: WorkspaceDetailDelegate?

  enum Tab {
    case info
    case messages
    case groups
    case config
  }

  @State private var groups = [Group]()
  @State private var groupsLoading = false
  @State private var configs = [ResourceConfig]()
  @State private var currentTab = TabState.shared.get(.topicDetail) as? Tab ?? Tab.info

  private var replicas: Int {
    topic.partitions.reduce(0) { n, p in
      n + p.replicaNodeIds.count
    }
  }
  private var inSyncReplicas: Int {
    topic.partitions.reduce(0) { n, p in
      n + p.inSyncReplicaNodeIds.count
    }
  }

  var body: some View {
    VStack(alignment: .leading) {
      HStack(alignment: .top) {
        VStack(alignment: .leading) {
          Text(topic.name)
            .font(.title)
            .textSelection(.enabled)
          Text("Topic")
            .font(.subheadline)
            .foregroundColor(.secondary)

          Tabs(
            autosaveId: .topicDetail,
            items: [
              .init(id: .info, symbol: "info.circle.fill", shortcut: .init("1")),
              .init(id: .messages, symbol: "archivebox.fill", shortcut: .init("2")),
              .init(id: .groups, symbol: "circle.grid.3x3.fill", shortcut: .init("3")),
              .init(id: .config, symbol: "gearshape.fill", shortcut: .init("4")),
            ],
            selection: $currentTab
          ) { item in
            switch item.id {
            case .info:
              VStack(alignment: .leading) {
                Infos {
                  Info(label: "Partitions", description: String(topic.partitions.count))
                  Info(label: "Replicas", description: String(replicas))
                  Info(label: "In-sync Replicas", description: String(inSyncReplicas))
                  Info(label: "Internal", description: topic.isInternal ? "yes" : "no", divider: false)
                }

                Spacer().frame(height: 15)
                Text("Partitions").font(.headline)
                Table(topic.partitions) {
                  TableColumn("Partition ID") { part in
                    Text(String(part.id))
                  }
                  TableColumn("Leader ID") { part in
                    Text(String(part.leaderId))
                  }
                  TableColumn("Replica IDs") { part in
                    Text(part.replicaNodeIds.map { String($0) }.joined(separator: ", "))
                  }
                  TableColumn("In-sync IDs") { part in
                    Text(part.inSyncReplicaNodeIds.map { String($0) }.joined(separator: ", "))
                  }
                }
                .tableStyle(BorderedTableStyle())
              }

            case .messages:
              TopicRecordsTable(
                id: id,
                topic: topic.name,
                delegate: delegate
              )
            case .groups:
              ConsumerGroups(
                id: id,
                topic: topic,
                delegate: delegate
              )
            case .config:
              ResourceConfigTable(configs: $configs) { action in
                switch action {
                case .edit(let entry):
                  updateConfigEntry(named: entry.name, withValue: entry.value)
                case .delete(let entry):
                  deleteConfigEntry(named: entry.name)
                }
                fetchConfigs()
              }.onAppear {
                fetchConfigs()
              }
            }
          }
        }
        Spacer()
      }
      Spacer()
    }
    .padding()
  }

  private func fetchConfigs() {
    guard let delegate else { return }
    let status = delegate.makeStatusProc()
    status("Fetching Configs")
    Backend.shared.getResourceConfigs(
      forResourceNamed: topic.name,
      resourceType: "topic",
      inWorkspace: id
    ).onComplete { configs in
      self.configs = configs
      status("Ready")
    }
  }

  private func updateConfigEntry(named name: String, withValue value: String) {
    guard let delegate else { return }
    let status = delegate.makeStatusProc()
    status("Updating Configs")
    Backend.shared.updateResourceConfigs(
      [name: value],
      forResourceNamed: topic.name,
      andResourceType: "topic",
      inWorkspace: id
    ).onComplete { _ in
      status("Ready")
    }
  }

  private func deleteConfigEntry(named name: String) {
    guard let delegate else { return }
    let status = delegate.makeStatusProc()
    status("Deleting Config")
    Backend.shared.updateResourceConfigs(
      [name: ""],
      forResourceNamed: topic.name,
      andResourceType: "topic",
      inWorkspace: id
    ).onComplete { _ in
      status("Ready")
    }
  }
}

// MARK: - ConsumerGroups
fileprivate struct ConsumerGroups: View {
  let id: UVarint
  let topic: Topic
  weak var delegate: WorkspaceDetailDelegate?

  private struct Group: Identifiable {
    let id: Int
    let name: String
  }

  @State private var groups = [Group]()
  @State private var groupsLoading = false

  var body: some View {
    HStack(alignment: .top) {
      VStack(alignment: .leading) {
        Spacer().frame(height: 15)
        Text("Consumer Groups")
          .font(.headline)
        Spacer().frame(height: 10)

        if groupsLoading {
          ProgressView()
            .scaleEffect(0.5, anchor: .topLeading)
        } else if groups.count == 0 {
          Text("This topic has no active consumer groups.")
        } else {
          ForEach(groups) { group in
            HStack {
              Text(group.name)
              Spacer()
              Button(action: {
                delegate?.request(groupNamed: group.name)
              }, label: {
                Image(systemName: "arrow.triangle.turn.up.right.circle.fill")
                  .renderingMode(.template)
                  .tint(.secondary)
              })
              .buttonStyle(.plain)
            }
            Divider()
          }
        }

        Spacer()
      }

      Spacer()
    }
    .onAppear {
      fetchGroups()
    }
  }

  private func fetchGroups() {
    guard let delegate else { return }
    let status = delegate.makeStatusProc()
    status("Finding Groups")
    groupsLoading = true
    Backend.shared.findTopicGroups(
      forTopic: topic.name,
      inWorkspace: id
    ).onComplete { groups in
      self.groups = groups.enumerated().map { Group(id: $0.0, name: $0.1) }
      self.groupsLoading = false
      status("Ready")
    }
  }
}
