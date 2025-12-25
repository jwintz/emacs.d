// NavigationSidebar.swift - NavigationSplitView layout for Hyalo
// Copyright (C) 2025

import AppKit
import SwiftUI

// MARK: - Data Models

/// Represents a buffer in the Open Editors section
struct SidebarBuffer: Identifiable, Hashable {
    let id: String
    let name: String
    let path: String?
    let isModified: Bool
    let isCurrent: Bool
    let icon: String  // Nerd font icon character from treemacs
}

/// Represents a file in the Explorer section
struct SidebarFile: Identifiable, Hashable {
    let id: String
    let name: String
    let path: String
    let isDirectory: Bool
    let isExpanded: Bool
    let depth: Int
    let icon: String  // Nerd font icon character
}

// MARK: - Sidebar SwiftUI Views

/// Row for a single buffer in Open Editors
struct BufferRow: View {
    let buffer: SidebarBuffer
    var onSelect: ((SidebarBuffer) -> Void)?
    var onClose: ((SidebarBuffer) -> Void)?

    @State private var isHovering = false

    var body: some View {
        HStack(spacing: 6) {
            // Nerd font icon (requires nerd font installed)
            Text(buffer.icon)
                .font(.custom("Symbols Nerd Font Mono", size: 14))
                .foregroundStyle(.secondary)

            Text(buffer.name)
                .font(.system(size: 12, weight: buffer.isCurrent ? .semibold : .regular))
                .foregroundStyle(buffer.isCurrent ? .primary : .secondary)
                .lineLimit(1)

            if buffer.isModified {
                Circle()
                    .fill(.orange)
                    .frame(width: 6, height: 6)
            }

            Spacer()

            if isHovering {
                Button { onClose?(buffer) } label: {
                    Image(systemName: "xmark")
                        .font(.system(size: 10, weight: .medium))
                        .foregroundStyle(.secondary)
                }
                .buttonStyle(.plain)
            }
        }
        .padding(.horizontal, 8)
        .padding(.vertical, 4)
        .background(buffer.isCurrent ? Color.accentColor.opacity(0.15) : Color.clear)
        .clipShape(RoundedRectangle(cornerRadius: 6))
        .contentShape(Rectangle())
        .onHover { isHovering = $0 }
        .onTapGesture { onSelect?(buffer) }
    }
}

/// Row for a file in Explorer
struct FileRow: View {
    let file: SidebarFile
    var onSelect: ((SidebarFile) -> Void)?

    var body: some View {
        HStack(spacing: 4) {
            // Indentation
            if file.depth > 0 {
                Spacer().frame(width: CGFloat(file.depth) * 12)
            }

            // Expand indicator for directories
            if file.isDirectory {
                Image(systemName: file.isExpanded ? "chevron.down" : "chevron.right")
                    .font(.system(size: 9))
                    .foregroundStyle(.tertiary)
                    .frame(width: 10)
            } else {
                Spacer().frame(width: 10)
            }

            // Nerd font icon
            Text(file.icon)
                .font(.custom("Symbols Nerd Font Mono", size: 14))
                .foregroundStyle(.secondary)

            Text(file.name)
                .font(.system(size: 12))
                .foregroundStyle(file.isDirectory ? .primary : .secondary)
                .lineLimit(1)

            Spacer()
        }
        .padding(.horizontal, 8)
        .padding(.vertical, 2)
        .contentShape(Rectangle())
        .onTapGesture { onSelect?(file) }
    }
}

/// The sidebar content
struct SidebarContentView: View {
    @Binding var buffers: [SidebarBuffer]
    @Binding var files: [SidebarFile]
    @Binding var projectName: String

    var onBufferSelect: ((SidebarBuffer) -> Void)?
    var onBufferClose: ((SidebarBuffer) -> Void)?
    var onFileSelect: ((SidebarFile) -> Void)?

    var body: some View {
        List {
            // Open Editors section
            Section {
                if buffers.isEmpty {
                    Text("No open files")
                        .font(.system(size: 11))
                        .foregroundStyle(.tertiary)
                } else {
                    ForEach(buffers) { buffer in
                        BufferRow(
                            buffer: buffer,
                            onSelect: onBufferSelect,
                            onClose: onBufferClose
                        )
                        .listRowInsets(EdgeInsets(top: 1, leading: 4, bottom: 1, trailing: 4))
                        .listRowBackground(Color.clear)
                    }
                }
            } header: {
                Label("OPEN EDITORS", systemImage: "doc.on.doc")
                    .font(.system(size: 11, weight: .semibold))
            }

            // Explorer section (from treemacs data)
            Section {
                if files.isEmpty {
                    Text("No project open")
                        .font(.system(size: 11))
                        .foregroundStyle(.tertiary)
                } else {
                    ForEach(files) { file in
                        FileRow(file: file, onSelect: onFileSelect)
                            .listRowInsets(EdgeInsets(top: 1, leading: 4, bottom: 1, trailing: 4))
                            .listRowBackground(Color.clear)
                    }
                }
            } header: {
                Label("EXPLORER", systemImage: "folder")
                    .font(.system(size: 11, weight: .semibold))
            }
        }
        .listStyle(.sidebar)
    }
}

/// Mode-line toolbar view - fills available width with proper LHS/RHS parsing
@available(macOS 15.0, *)
struct ModeLineToolbarView: View {
    let content: String
    
    /// Parse a string into LHS and RHS by splitting on 3+ consecutive spaces
    private func parseSegments(_ input: String) -> (lhs: String, rhs: String) {
        let content = input.trimmingCharacters(in: .whitespaces)
        if let range = content.range(of: "   +", options: .regularExpression) {
            let lhs = String(content[..<range.lowerBound]).trimmingCharacters(in: .whitespaces)
            let rhs = String(content[range.upperBound...]).trimmingCharacters(in: .whitespaces)
            return (lhs, rhs)
        }
        return (content, "")
    }

    var body: some View {
        let segments = parseSegments(content)
        HStack(spacing: 0) {
            ModeLineTextView(text: segments.lhs, fontSize: 11)
                .lineLimit(1)
            Spacer(minLength: 8)
            if !segments.rhs.isEmpty {
                ModeLineTextView(text: segments.rhs, fontSize: 11)
                    .lineLimit(1)
            }
        }
        .frame(maxWidth: .infinity)
    }
}

/// Wrapper to embed Emacs NSView in SwiftUI
/// The view should have no round corners and expand to fill available space
struct EmacsContentView: NSViewRepresentable {
    let emacsView: NSView

    func makeNSView(context: Context) -> NSView {
        // Ensure the Emacs view has no corner radius
        emacsView.wantsLayer = true
        emacsView.layer?.cornerRadius = 0
        emacsView.layer?.masksToBounds = false
        return emacsView
    }

    func updateNSView(_ nsView: NSView, context: Context) {
        // Ensure corner radius stays at 0
        nsView.layer?.cornerRadius = 0
    }
}

/// The main NavigationSplitView layout
@available(macOS 15.0, *)
struct HyaloNavigationLayout: View {
    @Binding var buffers: [SidebarBuffer]
    @Binding var files: [SidebarFile]
    @Binding var projectName: String
    @Binding var modeLine: String
    let emacsView: NSView

    var onBufferSelect: ((SidebarBuffer) -> Void)?
    var onBufferClose: ((SidebarBuffer) -> Void)?
    var onFileSelect: ((SidebarFile) -> Void)?

    @State private var columnVisibility: NavigationSplitViewVisibility = .all

    var body: some View {
        NavigationSplitView(columnVisibility: $columnVisibility) {
            SidebarContentView(
                buffers: $buffers,
                files: $files,
                projectName: $projectName,
                onBufferSelect: onBufferSelect,
                onBufferClose: onBufferClose,
                onFileSelect: onFileSelect
            )
            .navigationSplitViewColumnWidth(min: 200, ideal: 280, max: 400)
            .navigationTitle(projectName)
        } detail: {
            EmacsContentView(emacsView: emacsView)
                .ignoresSafeArea(.all, edges: .top)  // Expand under toolbar
        }
        .navigationSplitViewStyle(.balanced)
        .toolbar {
            ToolbarItem(placement: .principal) {
                ModeLineToolbarView(content: modeLine)
            }
        }
    }
}

// MARK: - Controller

@available(macOS 15.0, *)
final class NavigationSidebarController: NSObject {
    private weak var window: NSWindow?
    private var originalContentView: NSView?
    private var originalCornerRadius: CGFloat = 0
    private var hostingView: NSHostingView<AnyView>?
    private var vibrancyView: NSVisualEffectView?
    
    /// Current background color (Emacs default face + alpha)
    private var backgroundColor: NSColor = .windowBackgroundColor
    private var backgroundAlpha: CGFloat = 1.0

    // State
    private var buffers: [SidebarBuffer] = []
    private var files: [SidebarFile] = []
    private var projectName: String = ""
    private var modeLine: String = ""

    /// Callbacks
    var onBufferSelect: ((String) -> Void)?
    var onBufferClose: ((String) -> Void)?
    var onFileSelect: ((String) -> Void)?

    private(set) var isVisible: Bool = false

    init(window: NSWindow) {
        self.window = window
        super.init()
    }

    func show() {
        guard !isVisible, let window = window, let emacsView = window.contentView else { return }

        originalContentView = emacsView
        
        // Save and remove corner radius from Emacs view (keep all overlays visible!)
        if let layer = emacsView.layer {
            originalCornerRadius = layer.cornerRadius
            layer.cornerRadius = 0
            layer.masksToBounds = false
        }
        
        // Only remove corner radius from blur subviews, do NOT hide any overlays
        for subview in emacsView.subviews {
            if let effectView = subview as? NSVisualEffectView {
                effectView.layer?.cornerRadius = 0
                effectView.layer?.masksToBounds = false
            }
        }
        
        emacsView.removeFromSuperview()

        // Create vibrancy base view
        let vibrancy = NSVisualEffectView(frame: emacsView.frame)
        vibrancy.blendingMode = .behindWindow
        vibrancy.material = .hudWindow
        vibrancy.state = .active
        vibrancy.autoresizingMask = [.width, .height]
        vibrancy.wantsLayer = true
        
        // Set background color on the vibrancy view itself (not layer)
        // This colors the vibrancy effect to match Emacs theme
        if backgroundAlpha < 1.0 {
            vibrancy.layer?.backgroundColor = backgroundColor.withAlphaComponent(backgroundAlpha).cgColor
        } else {
            // For opaque backgrounds, set the view's fill color
            vibrancy.layer?.backgroundColor = backgroundColor.cgColor
        }
        
        self.vibrancyView = vibrancy

        // Create layout
        let layout = createLayout(emacsView: emacsView)
        let hosting = NSHostingView(rootView: AnyView(layout))
        hosting.frame = vibrancy.bounds
        hosting.autoresizingMask = [.width, .height]

        // Layer: vibrancyView (base) -> hosting (on top)
        vibrancy.addSubview(hosting)
        window.contentView = vibrancy

        hostingView = hosting
        isVisible = true

        // Show native traffic lights AFTER setting content view
        showTrafficLights(window)

        print("[Hyalo] NavigationSplitView shown with vibrancy base")
    }





    func hide() {
        guard isVisible, let window = window, let original = originalContentView else { return }

        original.removeFromSuperview()
        original.frame = hostingView?.superview?.frame ?? window.frame
        original.autoresizingMask = [.width, .height]
        
        // Restore corner radius
        if let layer = original.layer {
            layer.cornerRadius = originalCornerRadius
            layer.masksToBounds = true
        }
        
        // Restore corner radius on blur subviews
        for subview in original.subviews {
            if let effectView = subview as? NSVisualEffectView {
                effectView.layer?.cornerRadius = originalCornerRadius
                effectView.layer?.masksToBounds = true
            }
        }
        
        window.contentView = original

        // Hide traffic lights again (Hyalo custom buttons take over)
        hideTrafficLights(window)

        hostingView = nil
        vibrancyView = nil
        originalContentView = nil
        isVisible = false

        print("[Hyalo] NavigationSplitView hidden")
    }

    func toggle() {
        if isVisible { hide() } else { show() }
    }

    // MARK: - Traffic Lights

    private func showTrafficLights(_ window: NSWindow) {
        // Unhide and make fully visible (in case they were faded out)
        if let close = window.standardWindowButton(.closeButton) {
            close.isHidden = false
            close.alphaValue = 1.0
        }
        if let mini = window.standardWindowButton(.miniaturizeButton) {
            mini.isHidden = false
            mini.alphaValue = 1.0
        }
        if let zoom = window.standardWindowButton(.zoomButton) {
            zoom.isHidden = false
            zoom.alphaValue = 1.0
        }
    }

    private func hideTrafficLights(_ window: NSWindow) {
        window.standardWindowButton(.closeButton)?.isHidden = true
        window.standardWindowButton(.miniaturizeButton)?.isHidden = true
        window.standardWindowButton(.zoomButton)?.isHidden = true
    }

    // MARK: - Data Updates

    func updateBuffers(_ newBuffers: [SidebarBuffer]) {
        buffers = newBuffers
        refreshView()
    }

    func updateFiles(_ newFiles: [SidebarFile]) {
        files = newFiles
        refreshView()
    }

    func setProjectName(_ name: String) {
        projectName = name
        refreshView()
    }

    func updateModeLine(_ content: String) {
        modeLine = content
        refreshView()
    }
    
    /// Set the background color from Emacs (color string + alpha)
    func setBackgroundColor(_ colorString: String, alpha: CGFloat) {
        backgroundColor = parseEmacsColor(colorString) ?? .windowBackgroundColor
        backgroundAlpha = alpha
        
        // Apply to vibrancy view if visible
        if let vibrancy = vibrancyView, isVisible {
            vibrancy.layer?.backgroundColor = backgroundColor.withAlphaComponent(alpha).cgColor
        }
    }
    
    /// Parse Emacs color string (e.g., "#282c34" or "white")
    private func parseEmacsColor(_ colorString: String) -> NSColor? {
        let trimmed = colorString.trimmingCharacters(in: .whitespaces)
        
        // Handle hex colors
        if trimmed.hasPrefix("#") {
            let hex = String(trimmed.dropFirst())
            guard hex.count == 6 else { return nil }
            
            var rgb: UInt64 = 0
            Scanner(string: hex).scanHexInt64(&rgb)
            
            return NSColor(
                red: CGFloat((rgb >> 16) & 0xFF) / 255.0,
                green: CGFloat((rgb >> 8) & 0xFF) / 255.0,
                blue: CGFloat(rgb & 0xFF) / 255.0,
                alpha: 1.0
            )
        }
        
        // Handle named colors
        switch trimmed.lowercased() {
        case "white": return .white
        case "black": return .black
        default: return .windowBackgroundColor
        }
    }


    private func createLayout(emacsView: NSView) -> some View {
        let buffersBinding = Binding<[SidebarBuffer]>(
            get: { [weak self] in self?.buffers ?? [] },
            set: { [weak self] in self?.buffers = $0 }
        )
        let filesBinding = Binding<[SidebarFile]>(
            get: { [weak self] in self?.files ?? [] },
            set: { [weak self] in self?.files = $0 }
        )
        let projectBinding = Binding<String>(
            get: { [weak self] in self?.projectName ?? "" },
            set: { [weak self] in self?.projectName = $0 }
        )
        let modeLineBinding = Binding<String>(
            get: { [weak self] in self?.modeLine ?? "" },
            set: { [weak self] in self?.modeLine = $0 }
        )

        return HyaloNavigationLayout(
            buffers: buffersBinding,
            files: filesBinding,
            projectName: projectBinding,
            modeLine: modeLineBinding,
            emacsView: emacsView,
            onBufferSelect: { [weak self] buffer in self?.onBufferSelect?(buffer.name) },
            onBufferClose: { [weak self] buffer in self?.onBufferClose?(buffer.name) },
            onFileSelect: { [weak self] file in self?.onFileSelect?(file.path) }
        )
    }

    private func refreshView() {
        guard isVisible, let emacsView = originalContentView else { return }
        hostingView?.rootView = AnyView(createLayout(emacsView: emacsView))
    }
}

// MARK: - Manager

@available(macOS 15.0, *)
final class NavigationSidebarManager {
    static let shared = NavigationSidebarManager()

    private var controllers: [Int: NavigationSidebarController] = [:]

    var onBufferSelect: ((String) -> Void)?
    var onBufferClose: ((String) -> Void)?
    var onFileSelect: ((String) -> Void)?

    private init() {}

    func getController(for window: NSWindow) -> NavigationSidebarController {
        let key = window.windowNumber
        if let existing = controllers[key] {
            return existing
        }
        let controller = NavigationSidebarController(window: window)
        controller.onBufferSelect = onBufferSelect
        controller.onBufferClose = onBufferClose
        controller.onFileSelect = onFileSelect
        controllers[key] = controller
        return controller
    }

    func showSidebar(for window: NSWindow) {
        getController(for: window).show()
    }

    func hideSidebar(for window: NSWindow) {
        getController(for: window).hide()
    }

    func toggleSidebar(for window: NSWindow) {
        getController(for: window).toggle()
    }

    func isSidebarVisible(for window: NSWindow) -> Bool {
        controllers[window.windowNumber]?.isVisible ?? false
    }

    func updateBuffers(for window: NSWindow, buffers: [SidebarBuffer]) {
        controllers[window.windowNumber]?.updateBuffers(buffers)
    }

    func updateFiles(for window: NSWindow, files: [SidebarFile]) {
        controllers[window.windowNumber]?.updateFiles(files)
    }

    func setProjectName(for window: NSWindow, name: String) {
        controllers[window.windowNumber]?.setProjectName(name)
    }

    func updateModeLine(for window: NSWindow, content: String) {
        controllers[window.windowNumber]?.updateModeLine(content)
    }
    
    func setBackgroundColor(for window: NSWindow, color: String, alpha: CGFloat) {
        controllers[window.windowNumber]?.setBackgroundColor(color, alpha: alpha)
    }
}
