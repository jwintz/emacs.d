// NavigationSidebar.swift - NavigationSplitView layout for Hyalo with Liquid Glass
// Copyright (C) 2025
// Target: macOS 26 Tahoe with Liquid Glass design

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

/// The sidebar content with Liquid Glass styling
@available(macOS 26.0, *)
struct SidebarContentView: View {
    var state: NavigationSidebarState

    var onBufferSelect: ((SidebarBuffer) -> Void)?
    var onBufferClose: ((SidebarBuffer) -> Void)?
    var onFileSelect: ((SidebarFile) -> Void)?

    var body: some View {
        List {
            // Open Editors section
            Section {
                if state.buffers.isEmpty {
                    Text("No open files")
                        .font(.system(size: 11))
                        .foregroundStyle(.tertiary)
                } else {
                    ForEach(state.buffers) { buffer in
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
                if state.files.isEmpty {
                    Text("No project open")
                        .font(.system(size: 11))
                        .foregroundStyle(.tertiary)
                } else {
                    ForEach(state.files) { file in
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
        // Liquid Glass: NavigationSplitView sidebar automatically gets glass effect
        // We only need to tint if Emacs specifies a background color
        .background {
            Color(nsColor: state.backgroundColor)
                .opacity(Double(state.backgroundAlpha))
                .ignoresSafeArea()
        }
    }
}

/// Mode-line toolbar view with Liquid Glass styling
/// Uses flexible layout to fill entire toolbar width
/// NOTE: For macOS Tahoe, this is placed in the title/subtitle area for full width
@available(macOS 26.0, *)
struct ModeLineToolbarView: View {
    let content: String

    init(content: String) {
        self.content = content
    }

    /// Parse a string into LHS and RHS by splitting on 3+ consecutive spaces
    private func parseSegments(_ input: String) -> (lhs: String, rhs: String) {
        let trimmed = input.trimmingCharacters(in: .whitespaces)
        if let range = trimmed.range(of: "   +", options: .regularExpression) {
            let lhs = String(trimmed[..<range.lowerBound]).trimmingCharacters(in: .whitespaces)
            let rhs = String(trimmed[range.upperBound...]).trimmingCharacters(in: .whitespaces)
            return (lhs, rhs)
        }
        return (trimmed, "")
    }

    var body: some View {
        let segments = parseSegments(content)
        HStack(spacing: 8) {
            Text(segments.lhs.isEmpty ? " " : segments.lhs)
                .font(.system(size: 11, design: .monospaced))
                .foregroundStyle(.primary)
                .lineLimit(1)
                .truncationMode(.tail)

            Spacer(minLength: 0)

            if !segments.rhs.isEmpty {
                Text(segments.rhs)
                    .font(.system(size: 11, design: .monospaced))
                    .foregroundStyle(.secondary)
                    .lineLimit(1)
                    .truncationMode(.head)
            }
        }
        .frame(height: 22)
        .frame(maxWidth: .infinity)
        .padding(.horizontal, 8)
    }
}

/// NSVisualEffectView wrapper for fine-grained blur control
@available(macOS 26.0, *)
struct VibrancyBackgroundView: NSViewRepresentable {
    var material: NSVisualEffectView.Material
    var blendingMode: NSVisualEffectView.BlendingMode
    var isActive: Bool

    func makeNSView(context: Context) -> NSVisualEffectView {
        let view = NSVisualEffectView()
        view.material = material
        view.blendingMode = blendingMode
        view.state = isActive ? .active : .inactive
        view.isEmphasized = true
        view.wantsLayer = true
        return view
    }

    func updateNSView(_ nsView: NSVisualEffectView, context: Context) {
        nsView.material = material
        nsView.blendingMode = blendingMode
        nsView.state = isActive ? .active : .inactive
    }
}

/// Wrapper to embed Emacs NSView in SwiftUI
/// Uses NSVisualEffectView for fine-grained vibrancy/blur control
/// NOTE: With comprehensive alpha-background patch, Emacs renders fully transparent
/// backgrounds. Vibrancy shows through naturally.
@available(macOS 26.0, *)
struct EmacsContentView: View {
    let emacsView: NSView
    var state: NavigationSidebarState

    /// Map vibrancy material to NSVisualEffectView.Material
    /// Ordered from least vibrancy (none) to most vibrancy (ultraThin)
    private var effectMaterial: NSVisualEffectView.Material {
        switch state.vibrancyMaterial {
        case .none: return .windowBackground      // Solid, no vibrancy
        case .ultraThick: return .headerView      // Minimal vibrancy
        case .thick: return .titlebar             // Low vibrancy
        case .regular: return .menu               // Medium vibrancy
        case .thin: return .popover               // High vibrancy
        case .ultraThin: return .hudWindow        // Maximum vibrancy
        }
    }

    var body: some View {
        ZStack {
            // Vibrancy background using NSVisualEffectView for better control
            // This shows through where Emacs renders transparent (default face)
            if state.vibrancyMaterial != .none {
                VibrancyBackgroundView(
                    material: effectMaterial,
                    blendingMode: .behindWindow,
                    isActive: true
                )
            }

            // Tint layer on top of vibrancy to match Emacs theme
            // Alpha controls how much of the theme color shows vs vibrancy
            Color(nsColor: state.backgroundColor)
                .opacity(Double(state.backgroundAlpha))

            // Emacs content on top (renders with fully transparent default background)
            EmacsNSViewRepresentable(emacsView: emacsView)
        }
        // Only extend under top edge (toolbar), NOT the sidebar (leading edge)
        .ignoresSafeArea(.container, edges: .top)
    }
}

// NOTE: EchoAreaOverlay removed per AGENTS.md
// The NavigationSplitView handles all vibrancy uniformly - no bottom overlay needed

/// Custom container view that ensures Emacs keeps keyboard focus
/// and filters out mouse events in the toolbar area
class EmacsContainerView: NSView {
    weak var emacsView: NSView?

    /// Height of the toolbar area to exclude from hit testing
    var toolbarHeight: CGFloat = 52  // Approximate toolbar height

    override var acceptsFirstResponder: Bool { true }

    override func becomeFirstResponder() -> Bool {
        // Forward first responder to Emacs view
        if let emacs = emacsView {
            return window?.makeFirstResponder(emacs) ?? false
        }
        return false
    }

    override func viewDidMoveToWindow() {
        super.viewDidMoveToWindow()
        // Ensure Emacs view gets focus when added to window
        DispatchQueue.main.async { [weak self] in
            self?.restoreEmacsFirstResponder()
        }
    }

    func restoreEmacsFirstResponder() {
        guard let window = window, let emacs = emacsView else { return }
        if window.firstResponder != emacs {
            window.makeFirstResponder(emacs)
        }
    }

    override func hitTest(_ point: NSPoint) -> NSView? {
        // Don't accept hits in the toolbar area (top of the view)
        // This prevents mouse clicks in toolbar from being forwarded to Emacs
        if point.y > bounds.height - toolbarHeight {
            return nil
        }
        return super.hitTest(point)
    }

    override func mouseDown(with event: NSEvent) {
        // Check if click is in toolbar area
        let location = convert(event.locationInWindow, from: nil)
        if location.y > bounds.height - toolbarHeight {
            return // Don't forward to Emacs
        }
        super.mouseDown(with: event)
    }
}

/// NSViewRepresentable wrapper for Emacs NSView with focus management
struct EmacsNSViewRepresentable: NSViewRepresentable {
    let emacsView: NSView

    func makeNSView(context: Context) -> EmacsContainerView {
        let container = EmacsContainerView()
        container.emacsView = emacsView
        container.wantsLayer = true
        container.layer?.cornerRadius = 0
        container.layer?.masksToBounds = false

        emacsView.wantsLayer = true
        emacsView.layer?.cornerRadius = 0
        emacsView.layer?.masksToBounds = false
        emacsView.translatesAutoresizingMaskIntoConstraints = false

        container.addSubview(emacsView)
        NSLayoutConstraint.activate([
            emacsView.leadingAnchor.constraint(equalTo: container.leadingAnchor),
            emacsView.trailingAnchor.constraint(equalTo: container.trailingAnchor),
            emacsView.topAnchor.constraint(equalTo: container.topAnchor),
            emacsView.bottomAnchor.constraint(equalTo: container.bottomAnchor)
        ])

        return container
    }

    func updateNSView(_ nsView: EmacsContainerView, context: Context) {
        nsView.layer?.cornerRadius = 0
        // Restore Emacs first responder on updates
        nsView.restoreEmacsFirstResponder()
    }
}

/// The main NavigationSplitView layout with Liquid Glass styling
@available(macOS 26.0, *)
struct HyaloNavigationLayout: View {
    var state: NavigationSidebarState
    let emacsView: NSView

    var onBufferSelect: ((SidebarBuffer) -> Void)?
    var onBufferClose: ((SidebarBuffer) -> Void)?
    var onFileSelect: ((SidebarFile) -> Void)?

    @State private var columnVisibility: NavigationSplitViewVisibility = .detailOnly

    /// Map vibrancy material to NSVisualEffectView.Material
    /// Ordered from least vibrancy (none) to most vibrancy (ultraThin)
    private var effectMaterialForState: NSVisualEffectView.Material {
        switch state.vibrancyMaterial {
        case .none: return .windowBackground      // Solid, no vibrancy
        case .ultraThick: return .headerView      // Minimal vibrancy
        case .thick: return .titlebar             // Low vibrancy
        case .regular: return .menu               // Medium vibrancy
        case .thin: return .popover               // High vibrancy
        case .ultraThin: return .hudWindow        // Maximum vibrancy
        }
    }

    var body: some View {
        NavigationSplitView(columnVisibility: $columnVisibility) {
            SidebarContentView(
                state: state,
                onBufferSelect: onBufferSelect,
                onBufferClose: onBufferClose,
                onFileSelect: onFileSelect
            )
            .navigationSplitViewColumnWidth(min: 200, ideal: 280, max: 400)
        } detail: {
            EmacsContentView(
                emacsView: emacsView,
                state: state
            )
        }
        .navigationSplitViewStyle(.balanced)
        .toolbar {
            // Mode-line expands to fill available toolbar space
            ToolbarItem(placement: .principal) {
                ModeLineToolbarView(content: state.modeLine)
            }
        }
        .toolbarRole(.editor)
        // Use .large title display mode for more space in toolbar area
        .toolbarTitleDisplayMode(.inline)
        .toolbarBackgroundVisibility(.visible, for: .windowToolbar)
        .navigationTitle("")  // Prevent geometry from showing in title area
        // Liquid Glass: Apply matching vibrancy material to entire NavigationSplitView
        .background {
            ZStack {
                // Base vibrancy using NSVisualEffectView to match EmacsContentView
                VibrancyBackgroundView(
                    material: effectMaterialForState,
                    blendingMode: .behindWindow,
                    isActive: state.vibrancyMaterial != .none
                )
                // Emacs theme color tint
                Color(nsColor: state.backgroundColor)
                    .opacity(Double(state.backgroundAlpha))
            }
            .ignoresSafeArea()
        }
        .onChange(of: state.sidebarVisible) { _, newValue in
            withAnimation {
                columnVisibility = newValue ? .all : .detailOnly
            }
        }
    }
}

// MARK: - Vibrancy Material Types

/// Available vibrancy material styles matching SwiftUI's Material enum
enum VibrancyMaterial: String, CaseIterable {
    case ultraThin = "ultraThin"
    case thin = "thin"
    case regular = "regular"
    case thick = "thick"
    case ultraThick = "ultraThick"
    case none = "none"
}

// MARK: - Observable State for SwiftUI reactivity

@available(macOS 26.0, *)
@Observable
final class NavigationSidebarState {
    var buffers: [SidebarBuffer] = []
    var files: [SidebarFile] = []
    var projectName: String = ""
    var modeLine: String = ""
    var sidebarVisible: Bool = false
    var backgroundColor: NSColor = .windowBackgroundColor
    var backgroundAlpha: CGFloat = 0.5  // Reduced default for more vibrancy
    /// Window appearance mode: "light", "dark", or "auto"
    var windowAppearance: String = "auto"
    /// Vibrancy material style
    var vibrancyMaterial: VibrancyMaterial = .ultraThin
    /// Debug: reference to Emacs view for diagnostics
    weak var debugEmacsView: NSView?
}

// MARK: - Controller

@available(macOS 26.0, *)
final class NavigationSidebarController: NSObject {
    private weak var window: NSWindow?
    private var originalContentView: NSView?
    private var originalCornerRadius: CGFloat = 0
    private var hostingView: NSHostingView<AnyView>?
    private var blurView: NSVisualEffectView?
    private var titleObservation: NSKeyValueObservation?

    /// Observable state for SwiftUI reactivity
    let state = NavigationSidebarState()

    /// Appearance change observer
    private var appearanceObserver: NSObjectProtocol?

    /// Window focus observer to restore Emacs first responder
    private var windowBecameKeyObserver: NSObjectProtocol?
    private var windowBecameMainObserver: NSObjectProtocol?
    private var appDidBecomeActiveObserver: NSObjectProtocol?

    /// Reference to the Emacs view for focus restoration
    private weak var emacsViewRef: NSView?

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
        guard let window = window, let currentContentView = window.contentView else { return }

        // If already setup, check if content view changed (restart-emacs case)
        if isSetup {
            // If current content view is our hosting view, just restore focus
            if currentContentView == hostingView {
                restoreEmacsFocus()
                return
            }
            // Content view changed - need to re-setup
            teardown()
        }

        var emacsView = currentContentView

        // The contentView might be a wrapper NSView - find the actual EmacsView
        // which accepts first responder
        for subview in emacsView.subviews {
            let subClass = String(describing: type(of: subview))
            if subClass.contains("EmacsView") || subview.acceptsFirstResponder {
                // Store the wrapper as originalContentView but use EmacsView for focus
                originalContentView = emacsView
                emacsViewRef = subview
                state.debugEmacsView = subview
                break
            }
        }

        // If no EmacsView found in subviews, use the contentView itself
        if emacsViewRef == nil {
            originalContentView = emacsView
            emacsViewRef = emacsView
        }

        state.sidebarVisible = false

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
        hosting.wantsLayer = true

        // Set hosting view directly as content view (required for SwiftUI toolbar)
        window.contentView = hosting

        // Configure window for proper NavigationSplitView with vibrancy
        window.styleMask.insert(.fullSizeContentView)
        window.titlebarAppearsTransparent = true
        window.titleVisibility = .hidden
        window.isOpaque = false
        window.backgroundColor = .clear

        // Configure toolbar style
        window.toolbarStyle = .unified

        // CRITICAL: Clear and maintain empty window title to prevent Emacs geometry display
        window.title = ""
        window.subtitle = ""
        window.representedURL = nil

        // Add observer to prevent Emacs from changing the title
        titleObservation = window.observe(\.title, options: [.new]) { window, _ in
            if window.title != "" {
                window.title = ""
            }
        }

        hostingView = hosting
        isSetup = true

        // Show native traffic lights immediately and after a short delay
        // (Emacs or other code may hide them during initialization)
        showTrafficLights(window)
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.5) { [weak self, weak window] in
            guard let window = window else { return }
            self?.showTrafficLights(window)
        }

        // Setup appearance change observer
        setupAppearanceObserver()

        // emacsViewRef is already set above to the actual EmacsView

        // Setup window focus observer to restore Emacs first responder
        setupWindowFocusObserver(emacsView: emacsView)

        // Initial focus restore with multiple attempts for robustness on launch
        for delay in [0.0, 0.1, 0.3, 0.5, 1.0] {
            DispatchQueue.main.asyncAfter(deadline: .now() + delay) { [weak self, weak window] in
                guard let self = self, let window = window, let emacs = self.emacsViewRef else { return }
                window.makeFirstResponder(emacs)
            }
        }
    }

    /// Restore Emacs focus - can be called externally
    func restoreEmacsFocus() {
        guard let window = window, let emacs = emacsViewRef else { return }

        // Make window key first
        if !window.isKeyWindow {
            window.makeKeyAndOrderFront(nil)
        }

        // Make Emacs first responder
        window.makeFirstResponder(emacs)
    }

    /// Find the Emacs NSView in the view hierarchy
    private func findEmacsViewInHierarchy(_ view: NSView?) -> NSView? {
        guard let view = view else { return nil }

        let className = String(describing: type(of: view))

        // Check for EmacsView or similar Emacs-related view
        if className.contains("EmacsView") {
            return view
        }

        // Check if this is our container with the emacs view
        if let container = view as? EmacsContainerView, let emacs = container.emacsView {
            return emacs
        }

        // Check for views that accept first responder (likely the real Emacs view)
        if view.acceptsFirstResponder && !className.hasPrefix("NS") && !className.contains("Hosting") && !className.contains("Container") {
            return view
        }

        // Recursively search subviews
        for subview in view.subviews {
            if let found = findEmacsViewInHierarchy(subview) {
                return found
            }
        }

        return nil
    }

    /// Setup observer for window becoming key to restore Emacs focus
    private func setupWindowFocusObserver(emacsView: NSView) {
        // Observer for window becoming key
        windowBecameKeyObserver = NotificationCenter.default.addObserver(
            forName: NSWindow.didBecomeKeyNotification,
            object: window,
            queue: .main
        ) { [weak self] notification in
            guard let self = self else { return }
            DispatchQueue.main.asyncAfter(deadline: .now() + 0.05) {
                self.restoreEmacsFocus()
            }
        }

        // Observer for window becoming main
        windowBecameMainObserver = NotificationCenter.default.addObserver(
            forName: NSWindow.didBecomeMainNotification,
            object: window,
            queue: .main
        ) { [weak self] notification in
            guard let self = self else { return }
            DispatchQueue.main.asyncAfter(deadline: .now() + 0.1) {
                self.restoreEmacsFocus()
            }
        }

        // Observer for application becoming active (handles restart-emacs)
        appDidBecomeActiveObserver = NotificationCenter.default.addObserver(
            forName: NSApplication.didBecomeActiveNotification,
            object: nil,
            queue: .main
        ) { [weak self] _ in
            guard let self = self else { return }
            // Multiple attempts after app activation
            for delay in [0.1, 0.3, 0.5, 1.0] {
                DispatchQueue.main.asyncAfter(deadline: .now() + delay) {
                    self.restoreEmacsFocus()
                }
            }
        }
    }

    /// Setup observer for system appearance changes
    private func setupAppearanceObserver() {
        appearanceObserver = DistributedNotificationCenter.default.addObserver(
            forName: NSNotification.Name("AppleInterfaceThemeChangedNotification"),
            object: nil,
            queue: .main
        ) { [weak self] _ in
            self?.handleAppearanceChange()
        }
    }

    /// Handle system appearance change
    private func handleAppearanceChange() {
        guard let window = window else { return }
        let appearance = NSApp.effectiveAppearance
        let isDark = appearance.bestMatch(from: [.darkAqua, .aqua]) == .darkAqua
        let appearanceStr = isDark ? "dark" : "light"

        // Update window appearance
        if state.windowAppearance == "auto" {
            window.appearance = nil  // Follow system
        }

        // Force SwiftUI to re-render by updating state
        // The actual color update will come from Emacs via setBackgroundColor
        state.windowAppearance = appearanceStr
    }
    
    deinit {
        if let observer = appearanceObserver {
            DistributedNotificationCenter.default.removeObserver(observer)
        }
        if let observer = windowBecameKeyObserver {
            NotificationCenter.default.removeObserver(observer)
        }
        if let observer = windowBecameMainObserver {
            NotificationCenter.default.removeObserver(observer)
        }
        if let observer = appDidBecomeActiveObserver {
            NotificationCenter.default.removeObserver(observer)
        }
    }

    /// Teardown the NavigationSplitView
    func teardown() {
        guard isSetup, let window = window, let original = originalContentView else { return }

        // Clean up appearance observer
        if let observer = appearanceObserver {
            DistributedNotificationCenter.default.removeObserver(observer)
            appearanceObserver = nil
        }

        // Clean up window focus observers
        if let observer = windowBecameKeyObserver {
            NotificationCenter.default.removeObserver(observer)
            windowBecameKeyObserver = nil
        }
        if let observer = windowBecameMainObserver {
            NotificationCenter.default.removeObserver(observer)
            windowBecameMainObserver = nil
        }
        if let observer = appDidBecomeActiveObserver {
            NotificationCenter.default.removeObserver(observer)
            appDidBecomeActiveObserver = nil
        }

        // Clean up title observation
        titleObservation?.invalidate()
        titleObservation = nil

        // Clean up blur view
        blurView?.removeFromSuperview()
        blurView = nil

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
        window.isOpaque = true
        hideTrafficLights(window)

        hostingView = nil
        originalContentView = nil
        isSetup = false
        state.sidebarVisible = false
    }

    /// Show the sidebar column
    func showSidebar() {
        guard isSetup else { return }
        state.sidebarVisible = true
    }

    /// Hide the sidebar column
    func hideSidebar() {
        guard isSetup else { return }
        state.sidebarVisible = false
    }

    /// Toggle the sidebar column visibility
    func toggleSidebar() {
        if state.sidebarVisible {
            hideSidebar()
        } else {
            showSidebar()
        }
    }

    /// Check if sidebar is currently visible
    var isSidebarVisible: Bool {
        state.sidebarVisible
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
        state.buffers = newBuffers
    }

    func updateFiles(_ newFiles: [SidebarFile]) {
        state.files = newFiles
    }

    func setProjectName(_ name: String) {
        state.projectName = name
    }

    func updateModeLine(_ content: String) {
        state.modeLine = content
    }

    /// Set the background color from Emacs (color string + alpha)
    /// NOTE: Does NOT sync to AppearanceSettings.shared to avoid overwriting
    /// user's panel adjustments. Panel syncs from controller when opened.
    func setBackgroundColor(_ colorString: String, alpha: CGFloat) {
        let newColor = parseEmacsColor(colorString) ?? .windowBackgroundColor
        state.backgroundColor = newColor
        state.backgroundAlpha = alpha
    }

    /// Set the window appearance mode
    /// NOTE: Does NOT sync to AppearanceSettings.shared to avoid overwriting
    /// user's panel adjustments. Panel syncs from controller when opened.
    func setWindowAppearance(_ appearance: String) {
        guard let window = window else { return }
        state.windowAppearance = appearance

        switch appearance {
        case "light":
            window.appearance = NSAppearance(named: .aqua)
        case "dark":
            window.appearance = NSAppearance(named: .darkAqua)
        default:
            window.appearance = nil  // Follow system
        }
    }

    /// Set the vibrancy material style
    /// NOTE: Does NOT sync to AppearanceSettings.shared to avoid overwriting
    /// user's panel adjustments. Panel syncs from controller when opened.
    func setVibrancyMaterial(_ materialName: String) {
        if let material = VibrancyMaterial(rawValue: materialName) {
            state.vibrancyMaterial = material
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
        return HyaloNavigationLayout(
            state: state,
            emacsView: emacsView,
            onBufferSelect: { [weak self] buffer in self?.onBufferSelect?(buffer.name) },
            onBufferClose: { [weak self] buffer in self?.onBufferClose?(buffer.name) },
            onFileSelect: { [weak self] file in self?.onFileSelect?(file.path) }
        )
    }
}

// MARK: - Manager

@available(macOS 26.0, *)
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
    
    func setBackgroundColor(for window: NSWindow, color: String, alpha: CGFloat) {
        controllers[window.windowNumber]?.setBackgroundColor(color, alpha: alpha)
    }

    func setWindowAppearance(for window: NSWindow, appearance: String) {
        controllers[window.windowNumber]?.setWindowAppearance(appearance)
    }

    func setVibrancyMaterial(for window: NSWindow, material: String) {
        controllers[window.windowNumber]?.setVibrancyMaterial(material)
    }
}
