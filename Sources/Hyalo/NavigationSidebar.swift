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
    
    /// Background color from Emacs default face
    var backgroundColor: NSColor
    /// Background alpha from Emacs frame parameter
    var backgroundAlpha: CGFloat

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
        .scrollContentBackground(.hidden)
        .background(Color(nsColor: backgroundColor.withAlphaComponent(backgroundAlpha)))
        .ignoresSafeArea()
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
        GeometryReader { geometry in
            HStack(spacing: 0) {
                Text(segments.lhs)
                    .font(.system(size: 11, design: .monospaced))
                    .lineLimit(1)
                Spacer()
                if !segments.rhs.isEmpty {
                    Text(segments.rhs)
                        .font(.system(size: 11, design: .monospaced))
                        .lineLimit(1)
                }
            }
            .frame(width: geometry.size.width)
        }
        .frame(maxWidth: .infinity)
    }
}

/// Wrapper to embed Emacs NSView in SwiftUI
/// Includes gradient overlay at top and echo area overlay at bottom
struct EmacsContentView: View {
    let emacsView: NSView
    var backgroundColor: NSColor
    var backgroundAlpha: CGFloat
    var echoAreaHeight: CGFloat = 22
    
    var body: some View {
        ZStack {
            // Emacs content
            EmacsNSViewRepresentable(emacsView: emacsView)
            
            // Overlays anchored to top and bottom
            VStack(spacing: 0) {
                // Gradient overlay at top (blends with toolbar)
                LinearGradient(
                    colors: [
                        Color(nsColor: backgroundColor.withAlphaComponent(backgroundAlpha * 0.8)),
                        Color(nsColor: backgroundColor.withAlphaComponent(0))
                    ],
                    startPoint: .top,
                    endPoint: .bottom
                )
                .frame(height: 40)
                .allowsHitTesting(false)
                
                Spacer()
                
                // Echo area overlay at bottom (height matches minibuffer)
                EchoAreaOverlay(backgroundColor: backgroundColor, backgroundAlpha: backgroundAlpha, height: echoAreaHeight)
            }
        }
        // Only extend under top edge (toolbar), not sidebar
        .ignoresSafeArea(.all, edges: .top)
    }
}

/// Echo area overlay at the bottom of the content
struct EchoAreaOverlay: View {
    var backgroundColor: NSColor
    var backgroundAlpha: CGFloat
    var height: CGFloat = 22
    
    var body: some View {
        Rectangle()
            .fill(Color(nsColor: backgroundColor.withAlphaComponent(backgroundAlpha * 0.5)))
            .frame(height: height)
            .allowsHitTesting(false)
    }
}

/// NSViewRepresentable wrapper for Emacs NSView
struct EmacsNSViewRepresentable: NSViewRepresentable {
    let emacsView: NSView

    func makeNSView(context: Context) -> NSView {
        emacsView.wantsLayer = true
        emacsView.layer?.cornerRadius = 0
        emacsView.layer?.masksToBounds = false
        return emacsView
    }

    func updateNSView(_ nsView: NSView, context: Context) {
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
    @Binding var sidebarVisible: Bool
    @Binding var echoAreaHeight: CGFloat
    let emacsView: NSView
    
    /// Background color from Emacs default face
    var backgroundColor: NSColor
    /// Background alpha from Emacs frame parameter
    var backgroundAlpha: CGFloat

    var onBufferSelect: ((SidebarBuffer) -> Void)?
    var onBufferClose: ((SidebarBuffer) -> Void)?
    var onFileSelect: ((SidebarFile) -> Void)?

    @State private var columnVisibility: NavigationSplitViewVisibility = .detailOnly
    
    var body: some View {
        NavigationSplitView(columnVisibility: $columnVisibility) {
            SidebarContentView(
                buffers: $buffers,
                files: $files,
                projectName: $projectName,
                backgroundColor: backgroundColor,
                backgroundAlpha: backgroundAlpha,
                onBufferSelect: onBufferSelect,
                onBufferClose: onBufferClose,
                onFileSelect: onFileSelect
            )
            .navigationSplitViewColumnWidth(min: 200, ideal: 280, max: 400)
        } detail: {
            EmacsContentView(
                emacsView: emacsView,
                backgroundColor: backgroundColor,
                backgroundAlpha: backgroundAlpha,
                echoAreaHeight: echoAreaHeight
            )
        }
        .navigationSplitViewStyle(.balanced)
        .toolbar {
            // Use a custom toolbar layout that expands to fill available space
            ToolbarItem(placement: .principal) {
                ModeLineToolbarView(content: modeLine)
                    .frame(minWidth: 300, idealWidth: 800, maxWidth: .infinity)
            }
        }
        .onChange(of: sidebarVisible) { _, newValue in
            withAnimation {
                columnVisibility = newValue ? .all : .detailOnly
            }
        }
        .onAppear {
            columnVisibility = sidebarVisible ? .all : .detailOnly
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
    
    /// Current background color (Emacs default face + alpha)
    private var backgroundColor: NSColor = .windowBackgroundColor
    private var backgroundAlpha: CGFloat = 1.0

    // State
    private var buffers: [SidebarBuffer] = []
    private var files: [SidebarFile] = []
    private var projectName: String = ""
    private var modeLine: String = ""
    private var echoAreaHeight: CGFloat = 22
    
    /// Whether the sidebar column is visible
    private var sidebarVisible: Bool = false

    /// Callbacks
    var onBufferSelect: ((String) -> Void)?
    var onBufferClose: ((String) -> Void)?
    var onFileSelect: ((String) -> Void)?

    /// Whether the NavigationSplitView is installed
    private(set) var isSetup: Bool = false

    init(window: NSWindow) {
        self.window = window
        super.init()
    }
    
    /// Setup the NavigationSplitView (called at Hyalo initialization)
    /// Sidebar starts collapsed, toolbar is immediately visible
    func setup() {
        guard !isSetup, let window = window, let emacsView = window.contentView else { return }

        originalContentView = emacsView
        sidebarVisible = false
        
        // Save and remove corner radius from Emacs view
        if let layer = emacsView.layer {
            originalCornerRadius = layer.cornerRadius
            layer.cornerRadius = 0
            layer.masksToBounds = false
        }
        
        for subview in emacsView.subviews {
            if let effectView = subview as? NSVisualEffectView {
                effectView.layer?.cornerRadius = 0
                effectView.layer?.masksToBounds = false
            }
        }
        
        emacsView.removeFromSuperview()

        // Create layout with sidebar initially hidden
        let layout = createLayout(emacsView: emacsView)
        let hosting = NSHostingView(rootView: AnyView(layout))
        hosting.frame = emacsView.frame
        hosting.autoresizingMask = [.width, .height]
        
        // Make hosting view layer-backed for proper rendering
        hosting.wantsLayer = true

        window.contentView = hosting
        
        // Configure window for proper NavigationSplitView
        window.styleMask.insert(.fullSizeContentView)
        window.titlebarAppearsTransparent = true
        window.titleVisibility = .hidden
        
        // Configure toolbar - use automatic style for proper Liquid Glass behavior
        if window.toolbar == nil {
            let toolbar = NSToolbar(identifier: "HyaloNavigationToolbar")
            toolbar.displayMode = .iconOnly
            window.toolbar = toolbar
        }
        window.toolbar?.isVisible = true
        window.toolbarStyle = .automatic

        hostingView = hosting
        isSetup = true

        // Show native traffic lights immediately and after a short delay
        // (Emacs or other code may hide them during initialization)
        showTrafficLights(window)
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.5) { [weak self, weak window] in
            guard let window = window else { return }
            self?.showTrafficLights(window)
        }

        print("[Hyalo] NavigationSplitView setup complete (sidebar collapsed)")
    }
    
    /// Teardown the NavigationSplitView
    func teardown() {
        guard isSetup, let window = window, let original = originalContentView else { return }

        original.removeFromSuperview()
        original.frame = hostingView?.superview?.frame ?? window.frame
        original.autoresizingMask = [.width, .height]
        
        if let layer = original.layer {
            layer.cornerRadius = originalCornerRadius
            layer.masksToBounds = true
        }
        
        for subview in original.subviews {
            if let effectView = subview as? NSVisualEffectView {
                effectView.layer?.cornerRadius = originalCornerRadius
                effectView.layer?.masksToBounds = true
            }
        }
        
        window.contentView = original
        window.toolbar?.isVisible = false
        hideTrafficLights(window)

        hostingView = nil
        originalContentView = nil
        isSetup = false
        sidebarVisible = false

        print("[Hyalo] NavigationSplitView teardown complete")
    }

    /// Show the sidebar column
    func showSidebar() {
        guard isSetup else { return }
        sidebarVisible = true
        refreshView()
        print("[Hyalo] Sidebar shown")
    }
    
    /// Hide the sidebar column
    func hideSidebar() {
        guard isSetup else { return }
        sidebarVisible = false
        refreshView()
        print("[Hyalo] Sidebar hidden")
    }
    
    /// Toggle the sidebar column visibility
    func toggleSidebar() {
        if sidebarVisible {
            hideSidebar()
        } else {
            showSidebar()
        }
    }
    
    /// Check if sidebar is currently visible
    var isSidebarVisible: Bool {
        sidebarVisible
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
    
    /// Set the echo area height (minibuffer height)
    func setEchoAreaHeight(_ height: CGFloat) {
        echoAreaHeight = height
        refreshView()
    }
    
    /// Set the background color from Emacs (color string + alpha)
    func setBackgroundColor(_ colorString: String, alpha: CGFloat) {
        backgroundColor = parseEmacsColor(colorString) ?? .windowBackgroundColor
        backgroundAlpha = alpha
        
        // Refresh SwiftUI views to update sidebar and gradient colors
        refreshView()
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
        let sidebarVisibleBinding = Binding<Bool>(
            get: { [weak self] in self?.sidebarVisible ?? false },
            set: { [weak self] in self?.sidebarVisible = $0 }
        )
        let echoAreaHeightBinding = Binding<CGFloat>(
            get: { [weak self] in self?.echoAreaHeight ?? 22 },
            set: { [weak self] in self?.echoAreaHeight = $0 }
        )

        return HyaloNavigationLayout(
            buffers: buffersBinding,
            files: filesBinding,
            projectName: projectBinding,
            modeLine: modeLineBinding,
            sidebarVisible: sidebarVisibleBinding,
            echoAreaHeight: echoAreaHeightBinding,
            emacsView: emacsView,
            backgroundColor: backgroundColor,
            backgroundAlpha: backgroundAlpha,
            onBufferSelect: { [weak self] buffer in self?.onBufferSelect?(buffer.name) },
            onBufferClose: { [weak self] buffer in self?.onBufferClose?(buffer.name) },
            onFileSelect: { [weak self] file in self?.onFileSelect?(file.path) }
        )
    }

    private func refreshView() {
        guard isSetup, let emacsView = originalContentView else { return }
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
    
    /// Setup the NavigationSplitView for a window (called at Hyalo initialization)
    func setup(for window: NSWindow) {
        getController(for: window).setup()
    }
    
    /// Teardown the NavigationSplitView for a window
    func teardown(for window: NSWindow) {
        controllers[window.windowNumber]?.teardown()
    }
    
    /// Check if NavigationSplitView is set up for a window
    func isSetup(for window: NSWindow) -> Bool {
        controllers[window.windowNumber]?.isSetup ?? false
    }

    func showSidebar(for window: NSWindow) {
        getController(for: window).showSidebar()
    }

    func hideSidebar(for window: NSWindow) {
        getController(for: window).hideSidebar()
    }

    func toggleSidebar(for window: NSWindow) {
        getController(for: window).toggleSidebar()
    }

    func isSidebarVisible(for window: NSWindow) -> Bool {
        controllers[window.windowNumber]?.isSidebarVisible ?? false
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
    
    func setEchoAreaHeight(for window: NSWindow, height: CGFloat) {
        controllers[window.windowNumber]?.setEchoAreaHeight(height)
    }
    
    func setBackgroundColor(for window: NSWindow, color: String, alpha: CGFloat) {
        controllers[window.windowNumber]?.setBackgroundColor(color, alpha: alpha)
    }
}
