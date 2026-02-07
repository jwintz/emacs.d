// PiChatView.swift - Inspector chat view for Pi coding agent
// Copyright (C) 2025
// Target: macOS 26 Tahoe
//
// Ported from Pi Island's RPCChatView with appearance-aware colors.
// Features: SF Mono typography, role indicator dots, collapsible thinking,
// show more/less, tool result views with syntax highlighting, command output,
// slash command completion, @file completion, token/cost display.

import MarkdownUI
import OSLog
import SwiftUI

private let logger = Logger(subsystem: "com.hyalo", category: "PiChat")

/// SF Mono font helper — uses the system monospaced font (SF Mono on macOS)
private func sfMono(size: CGFloat, weight: Font.Weight = .regular) -> Font {
    .system(size: size, weight: weight, design: .monospaced)
}

// MARK: - PiChatView

@available(macOS 26.0, *)
struct PiChatView: View {
    @State private var rpcClient: PiRPCClient?
    @State private var messages: [RPCMessage] = []
    @State private var inputText = ""
    @State private var isStreaming = false
    @State private var streamingText = ""
    @State private var streamingThinking = ""
    @State private var currentTool: RPCToolExecution?
    @State private var commandOutput: String?
    @State private var selectedModel: RPCModel?
    @State private var availableModels: [RPCModel] = []
    @State private var availableCommands: [SlashCommandInfo] = []
    @State private var sessionStats: SessionStats?
    @State private var phase: RPCPhase = .disconnected

    // Completion state
    @State private var showCommandCompletion = false
    @State private var showFileCompletion = false
    @State private var selectedCommandIndex = 0
    @State private var selectedFileIndex = 0
    @State private var fileCompletions: [FileCompletionInfo] = []
    @State private var fileCompletionQuery = ""
    @FocusState private var isInputFocused: Bool

    // Scroll tracking for streaming
    @State private var isAtBottom = true
    @State private var scrollThrottleTask: Task<Void, Never>?

    /// Working directory for file completions
    @State private var workingDirectory: String = FileManager.default.homeDirectoryForCurrentUser.path

    /// Displayable messages in reverse order (for inverted scroll)
    private var displayMessages: [RPCMessage] {
        messages.filter(\.isDisplayable).reversed()
    }

    /// Whether a divider is needed before the message at `index` in `displayMessages`
    private func needsDivider(at index: Int) -> Bool {
        let msgs = displayMessages
        guard index > 0 else { return false }
        let prev = msgs[index - 1].role
        let curr = msgs[index].role
        // Divider between user<->assistant transitions, not around tool messages
        return prev != curr && curr != .tool && prev != .tool
    }

    /// Filtered commands based on current input
    private var filteredCommands: [SlashCommandInfo] {
        guard inputText.hasPrefix("/") else { return [] }
        let query = String(inputText.dropFirst()).lowercased()
        if query.isEmpty { return availableCommands }
        return availableCommands.filter {
            $0.displayName.lowercased().contains(query)
        }
    }

    var body: some View {
        VStack(spacing: 0) {
            // Messages — inverted scroll (newest at bottom)
            ScrollViewReader { proxy in
                ScrollView(.vertical) {
                    LazyVStack(alignment: .leading, spacing: 6) {
                        Color.clear.frame(height: 1).id("bottom")

                        if let output = commandOutput {
                            CommandOutputView(text: output)
                                .scaleEffect(x: 1, y: -1)
                        }

                        if let tool = currentTool {
                            ToolExecutionView(tool: tool)
                                .scaleEffect(x: 1, y: -1)
                        }

                        if !streamingText.isEmpty {
                            StreamingMessageView(text: streamingText)
                                .scaleEffect(x: 1, y: -1)
                        }

                        if !streamingThinking.isEmpty {
                            ThinkingMessageView(text: streamingThinking)
                                .scaleEffect(x: 1, y: -1)
                        }

                        ForEach(Array(displayMessages.enumerated()), id: \.element.id) { index, message in
                            // Divider on role transitions (user<->assistant)
                            if index > 0, needsDivider(at: index) {
                                Divider()
                                    .padding(.horizontal, 4)
                                    .scaleEffect(x: 1, y: -1)
                            }
                            MessageRow(message: message)
                                .scaleEffect(x: 1, y: -1)
                        }
                    }
                    .padding(.vertical, 12)
                    .padding(.horizontal, 8)
                }
                .scaleEffect(x: 1, y: -1)
                .onScrollGeometryChange(for: Bool.self) { geometry in
                    // Inverted scroll: "at bottom" means content offset is near the top
                    geometry.contentOffset.y <= 20
                } action: { _, newValue in
                    isAtBottom = newValue
                }
                .overlay(alignment: .bottom) {
                    if !isAtBottom && isStreaming {
                        Button {
                            isAtBottom = true
                            proxy.scrollTo("bottom", anchor: .bottom)
                        } label: {
                            HStack(spacing: 4) {
                                Image(systemName: "arrow.down")
                                    .font(.system(size: 9, weight: .medium))
                                Text("Scroll to bottom")
                                    .font(sfMono(size: 10, weight: .medium))
                            }
                            .foregroundStyle(.secondary)
                            .padding(.horizontal, 10)
                            .padding(.vertical, 5)
                            .background(.ultraThickMaterial)
                            .clipShape(Capsule())
                        }
                        .buttonStyle(.plain)
                        .padding(.bottom, 8)
                        .transition(.opacity)
                    }
                }
                .onAppear {
                    DispatchQueue.main.asyncAfter(deadline: .now() + 0.05) {
                        proxy.scrollTo("bottom", anchor: .bottom)
                    }
                }
                .onChange(of: messages.count) { _, _ in
                    if isAtBottom {
                        proxy.scrollTo("bottom", anchor: .bottom)
                    }
                }
                .onChange(of: streamingText) { _, _ in
                    if isAtBottom {
                        scrollToBottomThrottled(proxy: proxy)
                    }
                }
                .onChange(of: streamingThinking) { _, _ in
                    if isAtBottom {
                        scrollToBottomThrottled(proxy: proxy)
                    }
                }
                .onChange(of: commandOutput) { _, _ in
                    if isAtBottom {
                        proxy.scrollTo("bottom", anchor: .bottom)
                    }
                }
            }
            .frame(maxWidth: .infinity, maxHeight: .infinity)

            // Completion popovers + input bar + footer
            VStack(spacing: 0) {
                if showCommandCompletion && !filteredCommands.isEmpty {
                    commandCompletionView
                }
                if showFileCompletion && !fileCompletions.isEmpty {
                    fileCompletionView
                }
                inputBar
                chatFooter
            }
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity)
        .onChange(of: inputText) { _, newValue in
            let shouldShowCommand = newValue.hasPrefix("/") && !filteredCommands.isEmpty
            showCommandCompletion = shouldShowCommand
            selectedCommandIndex = 0
            updateFileCompletion(for: newValue)
        }
        .onAppear { startPiAgent() }
        .onDisappear { stopPiAgent() }
    }

    // MARK: - (Header removed — pi-mono style: no header, footer-centric)

    // MARK: - Command Completion

    private var commandCompletionView: some View {
        ScrollView(.vertical) {
            VStack(alignment: .leading, spacing: 2) {
                ForEach(
                    Array(filteredCommands.prefix(8).enumerated()),
                    id: \.element.id
                ) { index, cmd in
                    HStack(spacing: 8) {
                        Text("/" + cmd.displayName)
                            .font(sfMono(size: 11, weight: .medium))
                            .foregroundStyle(.primary)
                        if let desc = cmd.description {
                            Text(desc)
                                .font(sfMono(size: 10))
                                .foregroundStyle(.tertiary)
                                .lineLimit(1)
                        }
                        Spacer()
                    }
                    .padding(.horizontal, 10)
                    .padding(.vertical, 6)
                    .background(index == selectedCommandIndex
                        ? Color.accentColor.opacity(0.2)
                        : Color.clear)
                    .clipShape(RoundedRectangle(cornerRadius: 4))
                    .onTapGesture { selectCommand(cmd) }
                }
            }
            .padding(.vertical, 4)
        }
        .scrollIndicators(.hidden)
        .frame(maxHeight: 200)
        .glassEffect(.regular, in: .rect(cornerRadius: 8))
        .padding(.horizontal, 12)
        .padding(.bottom, 4)
    }

    // MARK: - File Completion

    private var fileCompletionView: some View {
        ScrollView(.vertical) {
            VStack(alignment: .leading, spacing: 2) {
                ForEach(
                    Array(fileCompletions.prefix(10).enumerated()),
                    id: \.element.id
                ) { index, file in
                    HStack(spacing: 8) {
                        Image(systemName: file.iconName)
                            .font(.system(size: 10))
                            .foregroundStyle(file.isDirectory ? .orange : .secondary)
                            .frame(width: 14)
                        Text(file.displayName)
                            .font(sfMono(size: 11, weight: .medium))
                            .foregroundStyle(.primary)
                            .lineLimit(1)
                        Spacer()
                    }
                    .padding(.horizontal, 10)
                    .padding(.vertical, 6)
                    .background(index == selectedFileIndex
                        ? Color.accentColor.opacity(0.2)
                        : Color.clear)
                    .clipShape(RoundedRectangle(cornerRadius: 4))
                    .onTapGesture { selectFile(file) }
                }
            }
            .padding(.vertical, 4)
        }
        .scrollIndicators(.hidden)
        .frame(maxHeight: 250)
        .glassEffect(.regular, in: .rect(cornerRadius: 8))
        .padding(.horizontal, 12)
        .padding(.bottom, 4)
    }

    // MARK: - Input Bar

    /// Handle keyboard navigation for completion popovers.
    /// Attached to the input bar container so arrow keys are intercepted
    /// before NSTextField consumes them for cursor movement.
    private func handleKeyPress(_ press: KeyPress) -> KeyPress.Result {
        switch press.key {
        case .upArrow:
            if showCommandCompletion {
                selectedCommandIndex = max(0, selectedCommandIndex - 1)
                return .handled
            }
            if showFileCompletion {
                selectedFileIndex = max(0, selectedFileIndex - 1)
                return .handled
            }
        case .downArrow:
            if showCommandCompletion {
                selectedCommandIndex = min(
                    filteredCommands.count - 1, selectedCommandIndex + 1)
                return .handled
            }
            if showFileCompletion {
                selectedFileIndex = min(
                    fileCompletions.count - 1, selectedFileIndex + 1)
                return .handled
            }
        case .tab:
            if showCommandCompletion && !filteredCommands.isEmpty {
                selectCommand(filteredCommands[selectedCommandIndex])
                return .handled
            }
            if showFileCompletion && !fileCompletions.isEmpty {
                selectFile(fileCompletions[selectedFileIndex])
                return .handled
            }
        case .escape:
            if showCommandCompletion {
                showCommandCompletion = false
                return .handled
            }
            if showFileCompletion {
                showFileCompletion = false
                return .handled
            }
        default:
            break
        }
        return .ignored
    }

    private var inputBar: some View {
        HStack(spacing: 8) {
            TextField("Message...", text: $inputText)
                .textFieldStyle(.plain)
                .font(sfMono(size: 12))
                .padding(.horizontal, 10)
                .padding(.vertical, 8)
                .focused($isInputFocused)
                .onSubmit {
                    if showCommandCompletion && !filteredCommands.isEmpty {
                        selectCommand(filteredCommands[selectedCommandIndex])
                    } else if showFileCompletion && !fileCompletions.isEmpty {
                        selectFile(fileCompletions[selectedFileIndex])
                    } else {
                        sendMessage()
                    }
                }
                .disabled(phase == .disconnected)

            if isStreaming {
                Button(action: abortMessage) {
                    Image(systemName: "stop.circle.fill")
                        .font(.system(size: 20))
                        .foregroundStyle(.red)
                }
                .buttonStyle(.plain)
            } else {
                Button(action: sendMessage) {
                    Image(systemName: "arrow.up.circle.fill")
                        .font(.system(size: 20))
                        .foregroundStyle(
                            canSend ? Color.accentColor : Color.secondary.opacity(0.4))
                }
                .buttonStyle(.plain)
                .disabled(!canSend)
            }
        }
        .onKeyPress(phases: .down, action: handleKeyPress)
        .padding(.horizontal, 12)
        .padding(.vertical, 8)
        .glassEffect(.regular.interactive(), in: .rect(cornerRadius: 12))
    }

    // MARK: - Footer

    /// Abbreviated working directory (~ for home)
    private var abbreviatedWorkDir: String {
        let home = FileManager.default.homeDirectoryForCurrentUser.path
        if workingDirectory.hasPrefix(home) {
            return "~" + workingDirectory.dropFirst(home.count)
        }
        return workingDirectory
    }

    private var chatFooter: some View {
        VStack(alignment: .leading, spacing: 1) {
            // Line 1: Working directory
            Text(abbreviatedWorkDir)
                .lineLimit(1)
                .truncationMode(.middle)

            // Line 2: Token stats + cost
            HStack(spacing: 6) {
                if let stats = sessionStats {
                    HStack(spacing: 3) {
                        Text("↑")
                        Text(formatTokenCount(stats.tokens?.input))
                        Text("↓")
                        Text(formatTokenCount(stats.tokens?.output))
                        if let cache = stats.tokens?.cacheRead, cache > 0 {
                            Text("R")
                            Text(formatTokenCount(cache))
                        }
                    }
                    Spacer()
                    Text(stats.formattedCost)
                        .foregroundStyle(.green.opacity(0.8))
                } else {
                    Spacer()
                }
            }

            // Line 3: Model + phase indicator
            HStack(spacing: 6) {
                Text(selectedModel?.displayName ?? "no model")

                Spacer()

                HStack(spacing: 4) {
                    Circle()
                        .fill(phaseColor)
                        .frame(width: 5, height: 5)
                    Text(phase.displayText)
                }
            }
        }
        .font(sfMono(size: 9))
        .foregroundStyle(.tertiary)
        .padding(.horizontal, 12)
        .padding(.vertical, 6)
    }

    private func formatTokenCount(_ count: Int?) -> String {
        guard let count, count > 0 else { return "0" }
        if count >= 1_000_000 { return String(format: "%.1fM", Double(count) / 1_000_000) }
        if count >= 1_000 { return String(format: "%.1fK", Double(count) / 1_000) }
        return "\(count)"
    }

    /// Throttle scroll-to-bottom during streaming to avoid jank.
    /// Coalesces rapid updates into a single scroll per 50ms interval.
    private func scrollToBottomThrottled(proxy: ScrollViewProxy) {
        guard scrollThrottleTask == nil else { return }
        scrollThrottleTask = Task { @MainActor in
            try? await Task.sleep(for: .milliseconds(50))
            proxy.scrollTo("bottom", anchor: .bottom)
            scrollThrottleTask = nil
        }
    }

    private var canSend: Bool {
        !inputText.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty
            && phase != .disconnected
            && !isStreaming
    }

    private var phaseColor: Color {
        switch phase {
        case .disconnected: return .gray
        case .starting: return .orange
        case .idle: return .green
        case .thinking: return .blue
        case .executing: return .cyan
        case .error: return .red
        }
    }

    // MARK: - Completion Actions

    private func selectCommand(_ command: SlashCommandInfo) {
        inputText = "/" + command.name + " "
        showCommandCompletion = false
    }

    private func selectFile(_ file: FileCompletionInfo) {
        guard let atRange = findCurrentAtReference() else { return }
        let before = String(inputText[inputText.startIndex..<atRange.lowerBound])
        let after = String(inputText[atRange.upperBound...])

        if file.isDirectory {
            inputText = before + "@" + file.path + "/" + after
            updateFileCompletion(for: inputText)
        } else {
            inputText = before + "@" + file.path + " " + after
            showFileCompletion = false
        }
    }

    private func findCurrentAtReference() -> Range<String.Index>? {
        guard let atIndex = inputText.lastIndex(of: "@") else { return nil }
        return atIndex..<inputText.endIndex
    }

    private func updateFileCompletion(for text: String) {
        guard let atIndex = text.lastIndex(of: "@") else {
            showFileCompletion = false
            return
        }

        let afterAt = String(text[text.index(after: atIndex)...])
        if afterAt.contains(" ") && !afterAt.hasSuffix("/") {
            showFileCompletion = false
            return
        }

        let query = afterAt.trimmingCharacters(in: .whitespaces)
        fileCompletionQuery = query
        selectedFileIndex = 0

        // Capture locals for the detached task
        let cwd = workingDirectory

        Task.detached {
            let completions = await Self.getFileCompletions(
                query: query, cwd: cwd)
            await MainActor.run {
                fileCompletions = completions
                showFileCompletion = !completions.isEmpty
            }
        }
    }

    private static func getFileCompletions(
        query: String, cwd: String
    ) async -> [FileCompletionInfo] {
        let queryPath = query.isEmpty ? "." : query

        var searchDir: String
        var filterPrefix: String

        if queryPath.hasSuffix("/") {
            searchDir = (cwd as NSString).appendingPathComponent(queryPath)
            filterPrefix = ""
        } else if queryPath.contains("/") {
            let dirPart = (queryPath as NSString).deletingLastPathComponent
            searchDir = (cwd as NSString).appendingPathComponent(dirPart)
            filterPrefix = (queryPath as NSString).lastPathComponent.lowercased()
        } else {
            searchDir = cwd
            filterPrefix = queryPath.lowercased()
        }

        let fm = FileManager.default
        guard let contents = try? fm.contentsOfDirectory(atPath: searchDir) else {
            return []
        }

        let basePath = queryPath.contains("/")
            ? (queryPath as NSString).deletingLastPathComponent
            : ""

        var results: [FileCompletionInfo] = []
        for item in contents {
            if item.hasPrefix(".") && !filterPrefix.hasPrefix(".") { continue }
            if !filterPrefix.isEmpty && !item.lowercased().hasPrefix(filterPrefix) {
                continue
            }

            let fullPath = (searchDir as NSString).appendingPathComponent(item)
            var isDir: ObjCBool = false
            fm.fileExists(atPath: fullPath, isDirectory: &isDir)
            let relativePath = basePath.isEmpty
                ? item
                : (basePath as NSString).appendingPathComponent(item)

            results.append(FileCompletionInfo(
                path: relativePath, fullPath: fullPath,
                isDirectory: isDir.boolValue))
        }

        results.sort { a, b in
            if a.isDirectory != b.isDirectory { return a.isDirectory }
            return a.path.localizedCaseInsensitiveCompare(b.path) == .orderedAscending
        }
        return Array(results.prefix(50))
    }

    // MARK: - Actions

    private func selectModel(_ model: RPCModel) {
        Task.detached {
            do {
                try await rpcClient?.setModel(
                    provider: model.provider, modelId: model.id)
                await MainActor.run { selectedModel = model }
            } catch {
                logger.error("Failed to set model: \(error)")
            }
        }
    }

    private func sendMessage() {
        let text = inputText.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !text.isEmpty else { return }

        let userMessage = RPCMessage(
            id: UUID().uuidString, role: .user,
            content: text, timestamp: Date())
        messages.append(userMessage)
        inputText = ""
        commandOutput = nil
        showCommandCompletion = false
        showFileCompletion = false

        Task.detached {
            do {
                try await rpcClient?.prompt(text)
            } catch {
                logger.error("Failed to send: \(error)")
            }
        }
    }

    private func abortMessage() {
        Task.detached { try? await rpcClient?.abort() }
    }

    // MARK: - Pi Agent Lifecycle

    private func startPiAgent() {
        // Use Task.detached to avoid blocking MainActor during sendAndWait polling
        Task.detached {
            do {
                let client = PiRPCClient()

                await client.setCallbacks(
                    onAgentStart: {
                        isStreaming = true
                        phase = .thinking
                        streamingText = ""
                        streamingThinking = ""
                        commandOutput = nil
                    },
                    onAgentEnd: { _ in
                        isStreaming = false
                        phase = .idle
                        if !streamingText.isEmpty {
                            let msg = RPCMessage(
                                id: UUID().uuidString, role: .assistant,
                                content: streamingText, timestamp: Date())
                            messages.append(msg)
                            streamingText = ""
                        }
                        streamingThinking = ""
                        currentTool = nil
                        // Refresh stats after each agent turn
                        Task.detached {
                            try? await client.getSessionStats()
                        }
                    },
                    onMessageUpdate: { _, delta in
                        handleMessageDelta(delta)
                    },
                    onToolExecutionStart: { id, name, args in
                        phase = .executing
                        let convertedArgs = args.mapValues { AnyCodable($0) }
                        currentTool = RPCToolExecution(
                            id: id, name: name,
                            args: convertedArgs, status: .running)
                    },
                    onToolExecutionUpdate: { id, _, partial in
                        if currentTool?.id == id {
                            currentTool?.partialOutput = partial?.stringValue
                        }
                    },
                    onToolExecutionEnd: { id, name, result, isError in
                        if let tool = currentTool, tool.id == id {
                            let resultString = result?.stringValue
                                ?? (result != nil
                                    ? String(describing: result!.value) : nil)
                            let toolMsg = RPCMessage(
                                id: id, role: .tool,
                                toolName: name, toolResult: resultString,
                                toolStatus: isError ? .error : .success,
                                timestamp: Date())
                            messages.append(toolMsg)
                            currentTool = nil
                        }
                        phase = .thinking
                    },
                    onStateChanged: { state in
                        if let model = state.model {
                            selectedModel = model
                        }
                        if let sf = state.sessionFile {
                            // Session file is at <project>/.pi/sessions/<id>.json
                            // Go up 3 levels to get the project root
                            var dir = sf as NSString
                            for _ in 0..<3 {
                                dir = dir.deletingLastPathComponent as NSString
                            }
                            let projectDir = dir as String
                            if !projectDir.isEmpty
                                && FileManager.default.fileExists(atPath: projectDir)
                            {
                                workingDirectory = projectDir
                            }
                        }
                    },
                    onError: { error in
                        phase = .error(error)
                        isStreaming = false
                    },
                    onProcessTerminated: {
                        phase = .disconnected
                        isStreaming = false
                    },
                    onCommandOutput: { text in
                        commandOutput = text
                    },
                    onSessionStats: { stats in
                        sessionStats = stats
                    }
                )

                await MainActor.run {
                    self.rpcClient = client
                    phase = .starting
                }

                try await client.start()
                try await client.getState()
                await MainActor.run { phase = .idle }

                // Fetch models and commands in parallel, non-blocking
                async let modelsResult = client.getAvailableModels()
                async let commandsResult = client.getCommands()
                async let statsResult: Void = client.getSessionStats()

                let models = try await modelsResult
                let commands = try await commandsResult
                _ = try? await statsResult

                await MainActor.run {
                    availableModels = models
                    availableCommands = commands
                }

            } catch {
                logger.error("Failed to start Pi agent: \(error)")
                await MainActor.run {
                    phase = .error(error.localizedDescription)
                }
            }
        }
    }

    @MainActor
    private func handleMessageDelta(_ delta: AssistantMessageEvent) {
        switch delta.type {
        case "text_delta":
            if let text = delta.delta { streamingText += text }
        case "thinking_delta":
            if let text = delta.delta { streamingThinking += text }
        case "toolcall_start":
            phase = .executing
        default:
            break
        }
    }

    private func stopPiAgent() {
        Task.detached { await rpcClient?.stop() }
    }
}

// MARK: - File Completion Info

@available(macOS 26.0, *)
struct FileCompletionInfo: Identifiable, Equatable {
    let id = UUID()
    let path: String
    let fullPath: String
    let isDirectory: Bool

    var displayName: String {
        isDirectory ? path + "/" : path
    }

    var iconName: String {
        if isDirectory { return "folder" }
        let ext = (path as NSString).pathExtension.lowercased()
        switch ext {
        case "swift": return "swift"
        case "ts", "tsx", "js", "jsx": return "doc.text"
        case "json": return "curlybraces"
        case "md", "markdown": return "doc.richtext"
        case "png", "jpg", "jpeg", "gif", "webp": return "photo"
        case "yml", "yaml": return "list.bullet"
        default: return "doc"
        }
    }
}

// MARK: - Message Row

@available(macOS 26.0, *)
private struct MessageRow: View {
    let message: RPCMessage

    var body: some View {
        switch message.role {
        case .user:
            UserMessageView(text: message.content ?? "")
        case .assistant:
            AssistantBubble(text: message.content ?? "")
        case .tool:
            ToolRow(message: message)
        }
    }
}

// MARK: - User Message (no bubble — right-aligned plain text)

@available(macOS 26.0, *)
private struct UserMessageView: View {
    let text: String

    var body: some View {
        HStack(alignment: .top, spacing: 6) {
            Spacer(minLength: 40)
            Text(text)
                .font(sfMono(size: 11))
                .foregroundStyle(.primary)
                .padding(.horizontal, 10)
                .padding(.vertical, 6)
                .background(Color.accentColor.opacity(0.15))
                .clipShape(RoundedRectangle(cornerRadius: 8))
            Circle()
                .fill(Color.accentColor)
                .frame(width: 6, height: 6)
                .padding(.top, 5)
        }
    }
}

// MARK: - Assistant Bubble

@available(macOS 26.0, *)
private struct AssistantBubble: View {
    let text: String
    @State private var isExpanded = false

    private var isLong: Bool {
        text.count > 500 || text.components(separatedBy: "\n").count > 10
    }

    var body: some View {
        VStack(alignment: .leading, spacing: 4) {
            HStack(alignment: .top, spacing: 6) {
                Circle()
                    .fill(Color.secondary.opacity(0.6))
                    .frame(width: 6, height: 6)
                    .padding(.top, 5)

                VStack(alignment: .leading, spacing: 0) {
                    Markdown(isExpanded || !isLong ? text : truncatedText)
                        .markdownTheme(.hyaloInspector)
                        .textSelection(.enabled)
                }

                Spacer(minLength: 40)
            }

            if isLong {
                Button(action: { withAnimation { isExpanded.toggle() } }) {
                    HStack(spacing: 4) {
                        Text(isExpanded ? "Show less" : "Show more")
                            .font(sfMono(size: 10))
                        Image(systemName: isExpanded ? "chevron.up" : "chevron.down")
                            .font(.system(size: 8))
                    }
                    .foregroundStyle(Color.accentColor)
                }
                .buttonStyle(.plain)
                .padding(.leading, 12)
            }
        }
    }

    private var truncatedText: String {
        let lines = text.components(separatedBy: "\n")
        let maxLines = 8
        guard lines.count > maxLines else { return text }
        var result = Array(lines.prefix(maxLines))
        var inCodeBlock = false
        for line in result {
            if line.hasPrefix("```") { inCodeBlock.toggle() }
        }
        if inCodeBlock { result.append("```") }
        result.append("...")
        return result.joined(separator: "\n")
    }
}

// MARK: - Tool Row

@available(macOS 26.0, *)
private struct ToolRow: View {
    let message: RPCMessage
    @State private var isExpanded = false

    private var statusColor: Color {
        switch message.toolStatus {
        case .running: return .blue
        case .success: return .green
        case .error: return .red
        case nil: return .gray
        }
    }

    private var hasResult: Bool {
        message.toolResult != nil && message.toolStatus != .running
    }

    private var isBash: Bool {
        message.toolName?.lowercased() == "bash"
    }

    var body: some View {
        VStack(alignment: .leading, spacing: 4) {
            HStack(spacing: 6) {
                Circle()
                    .fill(statusColor)
                    .frame(width: 6, height: 6)

                if isBash {
                    // Terminal-style: $ command
                    Text("$")
                        .font(sfMono(size: 10, weight: .bold))
                        .foregroundStyle(.green)
                    Text(message.toolArgsPreview)
                        .font(sfMono(size: 10, weight: .medium))
                        .foregroundStyle(.primary.opacity(0.85))
                        .lineLimit(1)
                } else {
                    Text(message.toolName ?? "tool")
                        .font(sfMono(size: 10, weight: .medium))
                        .foregroundStyle(.secondary)
                    Text(message.toolArgsPreview)
                        .font(sfMono(size: 10))
                        .foregroundStyle(.tertiary)
                        .lineLimit(1)
                }

                Spacer()

                if hasResult {
                    Image(systemName: "chevron.right")
                        .font(.system(size: 8, weight: .medium))
                        .foregroundStyle(.tertiary)
                        .rotationEffect(.degrees(isExpanded ? 90 : 0))
                }
            }
            .contentShape(Rectangle())
            .onTapGesture {
                if hasResult {
                    withAnimation(.spring(response: 0.25, dampingFraction: 0.8)) {
                        isExpanded.toggle()
                    }
                }
            }

            if isExpanded, let result = message.toolResult {
                ToolResultView(result: result, toolName: message.toolName ?? "")
            }
        }
        .padding(.vertical, 2)
    }
}

// MARK: - Tool Result View

@available(macOS 26.0, *)
private struct ToolResultView: View {
    let result: String
    let toolName: String
    @State private var isFullyExpanded = false
    @Environment(\.colorScheme) private var colorScheme

    private var lines: [String] { result.components(separatedBy: "\n") }
    private var collapsedLineCount: Int { isBash ? 5 : 12 }
    private var isBash: Bool { toolName.lowercased() == "bash" }
    private var displayLines: [String] {
        if isFullyExpanded { return lines }
        // Bash: show last N lines (tail); others: show first N lines (head)
        if isBash && lines.count > collapsedLineCount {
            return Array(lines.suffix(collapsedLineCount))
        }
        return Array(lines.prefix(collapsedLineCount))
    }
    /// Line number offset for display (Bash tail starts from end)
    private var lineNumberOffset: Int {
        if isBash && !isFullyExpanded && lines.count > collapsedLineCount {
            return lines.count - collapsedLineCount
        }
        return 0
    }

    private var language: String {
        switch toolName.lowercased() {
        case "read": return detectLanguageFromContent()
        case "bash": return "bash"
        default: return "text"
        }
    }

    private var codeBackground: Color {
        colorScheme == .dark
            ? Color(nsColor: .textBackgroundColor).opacity(0.4)
            : Color(nsColor: .textBackgroundColor).opacity(0.8)
    }

    var body: some View {
        VStack(alignment: .leading, spacing: 0) {
            HStack {
                Text(language)
                    .font(sfMono(size: 9, weight: .medium))
                    .foregroundStyle(.tertiary)
                Spacer()
                Button(action: copyResult) {
                    Image(systemName: "doc.on.doc")
                        .font(.system(size: 9))
                        .foregroundStyle(.tertiary)
                }
                .buttonStyle(.plain)
            }
            .padding(.horizontal, 8)
            .padding(.vertical, 4)
            .background(Color(nsColor: .separatorColor).opacity(0.3))

            ScrollView(.horizontal) {
                HStack(alignment: .top, spacing: 0) {
                    VStack(alignment: .trailing, spacing: 0) {
                        ForEach(
                            Array(displayLines.enumerated()), id: \.offset
                        ) { index, _ in
                            Text("\(index + 1 + lineNumberOffset)")
                                .font(sfMono(size: 9))
                                .foregroundStyle(.tertiary)
                                .frame(height: 15)
                        }
                    }
                    .padding(.trailing, 8)
                    .padding(.leading, 8)

                    Rectangle()
                        .fill(Color(nsColor: .separatorColor).opacity(0.5))
                        .frame(width: 1)

                    VStack(alignment: .leading, spacing: 0) {
                        ForEach(
                            Array(displayLines.enumerated()), id: \.offset
                        ) { _, line in
                            Text(line.isEmpty ? " " : line)
                                .font(sfMono(size: 10))
                                .foregroundStyle(
                                    isErrorLine(line)
                                        ? Color.red : .primary.opacity(0.85))
                                .frame(height: 15, alignment: .leading)
                        }
                    }
                    .padding(.horizontal, 8)
                }
                .padding(.vertical, 6)
            }
            .scrollIndicators(.hidden)

            if lines.count > collapsedLineCount {
                Button(action: { withAnimation { isFullyExpanded.toggle() } }) {
                    HStack(spacing: 4) {
                        Text(
                            isFullyExpanded
                                ? "Show less"
                                : "Show all \(lines.count) lines")
                            .font(sfMono(size: 9))
                        Image(
                            systemName: isFullyExpanded
                                ? "chevron.up" : "chevron.down"
                        )
                        .font(.system(size: 8))
                    }
                    .foregroundStyle(Color.accentColor)
                    .padding(.vertical, 4)
                    .frame(maxWidth: .infinity)
                }
                .buttonStyle(.plain)
                .background(Color(nsColor: .separatorColor).opacity(0.15))
            }
        }
        .background(codeBackground)
        .clipShape(RoundedRectangle(cornerRadius: 6))
        .overlay(
            RoundedRectangle(cornerRadius: 6)
                .stroke(
                    Color(nsColor: .separatorColor).opacity(0.5), lineWidth: 1)
        )
    }

    private func copyResult() {
        NSPasteboard.general.clearContents()
        NSPasteboard.general.setString(result, forType: .string)
    }

    private func isErrorLine(_ line: String) -> Bool {
        let l = line.lowercased()
        return l.hasPrefix("error:") || l.hasPrefix("fatal:")
            || l.hasPrefix("warning:") || l.contains("traceback")
    }

    private func detectLanguageFromContent() -> String {
        let first = lines.first ?? ""
        let content = result.lowercased()
        if first.trimmingCharacters(in: .whitespaces).hasPrefix("{")
            || first.trimmingCharacters(in: .whitespaces).hasPrefix("[")
        { return "json" }
        if content.contains("import ") && content.contains("func ") {
            return "swift"
        }
        if content.contains("def ") { return "python" }
        return "text"
    }
}

// MARK: - Streaming Message View

@available(macOS 26.0, *)
private struct StreamingMessageView: View {
    let text: String

    var body: some View {
        HStack(alignment: .top, spacing: 6) {
            Circle()
                .fill(Color.accentColor)
                .frame(width: 6, height: 6)
                .padding(.top, 5)
                .opacity(0.8)

            Markdown(text)
                .markdownTheme(.hyaloInspector)
                .textSelection(.enabled)

            Rectangle()
                .fill(Color.accentColor)
                .frame(width: 2, height: 14)
                .padding(.top, 2)

            Spacer(minLength: 40)
        }
    }
}

// MARK: - Thinking Message View

@available(macOS 26.0, *)
private struct ThinkingMessageView: View {
    let text: String
    @State private var isExpanded = false

    var body: some View {
        VStack(alignment: .leading, spacing: 4) {
            Button(action: { withAnimation { isExpanded.toggle() } }) {
                HStack(spacing: 6) {
                    Image(systemName: "brain")
                        .font(.system(size: 10))
                        .foregroundStyle(.purple)
                    Text("Thinking...")
                        .font(sfMono(size: 10, weight: .medium))
                        .foregroundStyle(.purple)
                    Spacer()
                    Image(
                        systemName: isExpanded ? "chevron.up" : "chevron.down"
                    )
                    .font(.system(size: 8))
                    .foregroundStyle(.tertiary)
                }
            }
            .buttonStyle(.plain)

            if isExpanded {
                Text(text)
                    .font(sfMono(size: 10))
                    .foregroundStyle(.secondary)
                    .padding(8)
                    .frame(maxWidth: .infinity, alignment: .leading)
                    .background(Color.purple.opacity(0.08))
                    .clipShape(RoundedRectangle(cornerRadius: 6))
            }
        }
        .padding(.horizontal, 8)
        .padding(.vertical, 6)
        .background(Color.purple.opacity(0.04))
        .clipShape(RoundedRectangle(cornerRadius: 8))
    }
}

// MARK: - Tool Execution View (live)

@available(macOS 26.0, *)
private struct ToolExecutionView: View {
    let tool: RPCToolExecution

    var body: some View {
        VStack(alignment: .leading, spacing: 4) {
            HStack(spacing: 6) {
                Circle()
                    .fill(Color.green)
                    .frame(width: 6, height: 6)

                Text(tool.name)
                    .font(sfMono(size: 10, weight: .medium))
                    .foregroundStyle(.secondary)

                Text(toolPreview)
                    .font(sfMono(size: 10))
                    .foregroundStyle(.tertiary)
                    .lineLimit(1)

                Spacer()

                ProgressView()
                    .scaleEffect(0.5)
            }

            if let partial = tool.partialOutput, !partial.isEmpty {
                Text(partial.suffix(200))
                    .font(sfMono(size: 9))
                    .foregroundStyle(.tertiary)
                    .lineLimit(3)
                    .padding(.leading, 12)
            }
        }
        .padding(.horizontal, 8)
        .padding(.vertical, 6)
    }

    private var toolPreview: String {
        if let path = tool.args["path"]?.stringValue {
            return URL(fileURLWithPath: path).lastPathComponent
        }
        if let command = tool.args["command"]?.stringValue {
            return String(command.prefix(50))
        }
        return ""
    }
}

// MARK: - Command Output View

@available(macOS 26.0, *)
private struct CommandOutputView: View {
    let text: String

    var body: some View {
        HStack(alignment: .top, spacing: 6) {
            Image(systemName: "terminal")
                .font(.system(size: 12))
                .foregroundStyle(.green)
                .padding(.top, 3)

            Text(text)
                .font(sfMono(size: 12))
                .foregroundStyle(.green)
                .textSelection(.enabled)

            Spacer(minLength: 20)
        }
        .padding(.vertical, 12)
        .padding(.horizontal, 12)
        .frame(maxWidth: .infinity, alignment: .leading)
        .background(Color.green.opacity(0.06))
        .clipShape(RoundedRectangle(cornerRadius: 8))
        .overlay(
            RoundedRectangle(cornerRadius: 8)
                .stroke(Color.green.opacity(0.2), lineWidth: 1)
        )
    }
}
