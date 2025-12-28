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

// MARK: - Custom NSToolbar for Modeline

/// Custom toolbar item identifier for the mode-line
private extension NSToolbarItem.Identifier {
    static let modeLineItem = NSToolbarItem.Identifier("com.hyalo.modeline")
    static let flexibleSpace = NSToolbarItem.Identifier.flexibleSpace
    static let sidebarTracking = NSToolbarItem.Identifier.sidebarTrackingSeparator
    static let toggleSidebar = NSToolbarItem.Identifier.toggleSidebar
}

/// Custom view for the modeline toolbar item with proper layout
/// NO background - relies on toolbar's native glass effect
/// Uses autoresizing to fill available space
@available(macOS 26.0, *)
final class ModeLineToolbarItemView: NSView {
    private let lhsLabel = NSTextField(labelWithString: "")
    private let rhsLabel = NSTextField(labelWithString: "")

    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        setupView()
    }

    required init?(coder: NSCoder) {
        super.init(coder: coder)
        setupView()
    }

    private func setupView() {
        // NO background - toolbar provides glass effect
        wantsLayer = true
        layer?.backgroundColor = NSColor.clear.cgColor

        // Use autoresizing to fill container
        autoresizingMask = [.width, .height]

        // Configure LHS label - use autoresizing, not constraints
        lhsLabel.font = NSFont.monospacedSystemFont(ofSize: 11, weight: .regular)
        lhsLabel.textColor = .labelColor
        lhsLabel.backgroundColor = .clear
        lhsLabel.drawsBackground = false
        lhsLabel.alignment = .left
        lhsLabel.lineBreakMode = .byTruncatingTail
        lhsLabel.autoresizingMask = [.maxXMargin]

        // Configure RHS label
        rhsLabel.font = NSFont.monospacedSystemFont(ofSize: 11, weight: .regular)
        rhsLabel.textColor = .secondaryLabelColor
        rhsLabel.backgroundColor = .clear
        rhsLabel.drawsBackground = false
        rhsLabel.alignment = .right
        rhsLabel.lineBreakMode = .byTruncatingHead
        rhsLabel.autoresizingMask = [.minXMargin]

        addSubview(lhsLabel)
        addSubview(rhsLabel)

    }

    func updateContent(lhs: String, rhs: String) {
        lhsLabel.stringValue = lhs
        rhsLabel.stringValue = rhs
        layoutLabels()
    }

    private func layoutLabels() {
        let margin: CGFloat = 8
        let spacing: CGFloat = 16
        let height = bounds.height

        // Size labels to fit content
        lhsLabel.sizeToFit()
        rhsLabel.sizeToFit()

        // Position LHS at left
        let lhsY = (height - lhsLabel.frame.height) / 2
        lhsLabel.frame = NSRect(x: margin, y: lhsY, width: lhsLabel.frame.width, height: lhsLabel.frame.height)

        // Position RHS at right
        let rhsY = (height - rhsLabel.frame.height) / 2
        let rhsX = bounds.width - margin - rhsLabel.frame.width
        rhsLabel.frame = NSRect(x: rhsX, y: rhsY, width: rhsLabel.frame.width, height: rhsLabel.frame.height)

        // If labels overlap, truncate LHS
        let lhsRight = lhsLabel.frame.maxX + spacing
        if lhsRight > rhsLabel.frame.minX {
            let newLhsWidth = max(0, rhsLabel.frame.minX - spacing - margin)
            lhsLabel.frame.size.width = newLhsWidth
        }
    }

    override func layout() {
        super.layout()
        layoutLabels()
    }

    override func resizeSubviews(withOldSize oldSize: NSSize) {
        super.resizeSubviews(withOldSize: oldSize)
        layoutLabels()
    }

    // Don't override intrinsicContentSize - let toolbar manage sizing
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
/// Uses full-width layout that spans the entire toolbar area
/// NOTE: For macOS Tahoe, uses glassEffect for Liquid Glass appearance
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
            // Left padding for traffic lights area (approximately 78px)
            Spacer()
                .frame(width: 78)

            Text(segments.lhs.isEmpty ? " " : segments.lhs)
                .font(.system(size: 11, design: .monospaced))
                .foregroundStyle(.primary)
                .lineLimit(1)
                .truncationMode(.tail)

            Spacer(minLength: 20)

            if !segments.rhs.isEmpty {
                Text(segments.rhs)
                    .font(.system(size: 11, design: .monospaced))
                    .foregroundStyle(.secondary)
                    .lineLimit(1)
                    .truncationMode(.head)
            }

            // Right margin
            Spacer()
                .frame(width: 12)
        }
        .frame(maxWidth: .infinity)
        .frame(height: 28)
        .onAppear {
        }
    }
}

// MARK: - Titlebar Accessory View for Toolbar Integration

/// NSTitlebarAccessoryViewController that hosts the modeline SwiftUI view
/// This places the modeline directly in the toolbar area alongside traffic lights
@available(macOS 26.0, *)
final class ModeLineTitlebarAccessoryController: NSTitlebarAccessoryViewController {
    private var hostingView: NSHostingView<AnyView>?
    private var modeLineContent: String = ""
    private var isContentVisible: Bool = true

    override func loadView() {
        // Create the hosting view with the modeline content
        let swiftUIView = ModeLineTitlebarView(content: modeLineContent)
        let hosting = NSHostingView(rootView: AnyView(swiftUIView))
        hosting.translatesAutoresizingMaskIntoConstraints = false
        self.view = hosting
        self.hostingView = hosting
    }

    override func viewDidLoad() {
        super.viewDidLoad()
        // Position at the bottom of the titlebar (in the toolbar area)
        self.layoutAttribute = .bottom
        // Allow full width
        self.fullScreenMinHeight = 28
    }

    func updateModeLine(_ content: String) {
        modeLineContent = content
        let swiftUIView = ModeLineTitlebarView(content: content)
        hostingView?.rootView = AnyView(swiftUIView)
    }

    func setVisible(_ visible: Bool) {
        isContentVisible = visible
        view.isHidden = !visible
    }
}

/// Controller for modeline overlay with NSGlassEffectView positioned in toolbar area
/// Uses NSGlassEffectView.cornerRadius for proper Liquid Glass appearance
/// Frame is calculated relative to window's titlebar/toolbar area
@available(macOS 26.0, *)
final class ModeLineGlassOverlayController {
    private var glassView: NSGlassEffectView?
    private var modeLineView: ModeLineToolbarItemView?
    private var modeLineContent: String = ""
    private var frameObserver: NSObjectProtocol?
    private var animationTimer: Timer?
    private var animationEndTime: Date?
    weak var window: NSWindow?
    
    /// Metrics for toolbar glass bar (matching SwiftUI defaults)
    /// Height matches toolbar button height, radius is half for pill/capsule shape
    private let glassHeight: CGFloat = 34     // Match toolbar button height
    private let glassCornerRadius: CGFloat = 17  // Half of height for pill shape
    private let verticalCenterOffset: CGFloat = 0  // Center in toolbar area
    private let leadingPadding: CGFloat = 8   // Padding from left edge
    private let trailingPadding: CGFloat = 8  // Padding from right edge
    
    /// Stored geometry for calculations (updated on each call)
    private var sidebarToggleWidth: CGFloat = 116  // traffic lights (69) + toggle (47) 
    private var inspectorToggleWidth: CGFloat = 47 // toggle button width
    private var inspectorVisible: Bool = false
    
    /// Animation update callback (called by timer during animations)
    var onAnimationUpdate: (() -> Void)?
    
    /// Setup the modeline overlay in the window
    func setup(for window: NSWindow) {
        self.window = window
        
        // Create modeline content view
        let modeView = ModeLineToolbarItemView(frame: .zero)
        
        // Create glass effect view with proper properties
        let glass = NSGlassEffectView()
        glass.contentView = modeView
        glass.cornerRadius = glassCornerRadius  // Use native NSGlassEffectView.cornerRadius
        glass.wantsLayer = true
        glass.autoresizingMask = []  // We manage position manually
        
        // Store references
        self.glassView = glass
        self.modeLineView = modeView
        
        // Add to window - find the toolbar view or titlebar container
        if let toolbarView = findToolbarView(in: window) {
            toolbarView.addSubview(glass)
        } else if let contentSuperview = window.contentView?.superview {
            // Fallback: add to theme frame
            contentSuperview.addSubview(glass)
        }
        
        // Observe window frame changes to update geometry
        frameObserver = NotificationCenter.default.addObserver(
            forName: NSWindow.didResizeNotification,
            object: window,
            queue: .main
        ) { [weak self] _ in
            self?.updateLayout()
        }
        
        // Also observe parent view frame changes for real-time animation updates
        if let glass = glassView, let parentView = glass.superview {
            parentView.postsFrameChangedNotifications = true
            parentFrameObserver = NotificationCenter.default.addObserver(
                forName: NSView.frameDidChangeNotification,
                object: parentView,
                queue: .main
            ) { [weak self] _ in
                self?.updateLayout()
            }
        }
        
        // Initial layout
        updateLayout()
    }
    
    private var parentFrameObserver: NSObjectProtocol?
    
    /// Find the toolbar view in the window hierarchy
    private func findToolbarView(in window: NSWindow) -> NSView? {
        // The toolbar lives inside NSTitlebarContainerView
        guard let contentSuperview = window.contentView?.superview else { return nil }
        
        func findToolbarContainer(in view: NSView) -> NSView? {
            let className = String(describing: type(of: view))
            if className.contains("NSTitlebarContainerView") || className.contains("NSToolbarView") {
                return view
            }
            for subview in view.subviews {
                if let found = findToolbarContainer(in: subview) {
                    return found
                }
            }
            return nil
        }
        
        return findToolbarContainer(in: contentSuperview)
    }
    
    /// Find the rightmost edge of the sidebar toggle button
    /// Returns the X coordinate where the modeline should start (after sidebar toggle)
    private func findSidebarToggleMaxX() -> CGFloat {
        guard let window = window,
              let glass = glassView,
              let parentView = glass.superview else { return 120 }  // Fallback
        
        // Find ALL toolbar items and identify the leftmost one
        // The sidebar toggle is typically the leftmost toolbar item
        var allToolbarItems: [(view: NSView, frame: NSRect)] = []
        
        func findToolbarItems(in view: NSView) {
            let className = String(describing: type(of: view))
            if className.contains("NSToolbarItemViewer") {
                // Convert to parentView coordinates (same as glass view)
                let frame = view.convert(view.bounds, to: parentView)
                allToolbarItems.append((view, frame))
            }
            for subview in view.subviews {
                findToolbarItems(in: subview)
            }
        }
        
        if let contentSuperview = window.contentView?.superview {
            findToolbarItems(in: contentSuperview)
        }
        
        // Filter out separators (width < 30pt) and sort by minX to find leftmost button
        let buttons = allToolbarItems.filter { $0.frame.width >= 30 }
        let sortedButtons = buttons.sorted { $0.frame.minX < $1.frame.minX }
        
        // Debug: log all toolbar items
        for (index, item) in allToolbarItems.enumerated() {
            let isSeparator = item.frame.width < 30 ? " (separator)" : ""
        }
        
        // The leftmost button (not separator) is the sidebar toggle
        if let first = sortedButtons.first {
            return first.frame.maxX
        }
        
        return 120
    }
    
    /// Find the leftmost edge of the inspector toggle button
    /// Returns the X coordinate where the modeline should end (before inspector toggle)
    private func findInspectorToggleMinX() -> CGFloat {
        guard let window = window else { return 0 }
        
        // Find the rightmost toolbar item (inspector toggle)
        var rightmostToolbarItem: NSView?
        var rightmostX: CGFloat = 0
        
        func findToolbarItems(in view: NSView) {
            let className = String(describing: type(of: view))
            if className.contains("NSToolbarItemViewer") {
                let frame = view.convert(view.bounds, to: nil)
                if frame.maxX > rightmostX {
                    rightmostX = frame.maxX
                    rightmostToolbarItem = view
                }
            }
            for subview in view.subviews {
                findToolbarItems(in: subview)
            }
        }
        
        if let contentSuperview = window.contentView?.superview {
            findToolbarItems(in: contentSuperview)
        }
        
        if let toolbarItem = rightmostToolbarItem {
            let frame = toolbarItem.convert(toolbarItem.bounds, to: window.contentView?.superview)
            return frame.minX
        }
        
        return window.frame.width - 50
    }
    
    /// Update content and store geometry parameters
    func updateGeometry(
        content: String,
        sidebarToggleWidth: CGFloat,
        contentWidth: CGFloat,
        hasInspector: Bool,
        inspectorToggleWidth: CGFloat
    ) {
        // Store parameters
        modeLineContent = content
        self.sidebarToggleWidth = sidebarToggleWidth
        self.inspectorToggleWidth = inspectorToggleWidth
        self.inspectorVisible = hasInspector
        
        // Update content
        let segments = parseSegments(content)
        modeLineView?.updateContent(lhs: segments.lhs, rhs: segments.rhs)
        
        // Update layout immediately
        updateLayout()
        
        // Start animation timer for smooth updates during sidebar/inspector expansion
        startAnimationTimer()
    }
    
    /// Start a timer that triggers geometry updates during animations
    private func startAnimationTimer() {
        // Stop any existing timer
        animationTimer?.invalidate()
        
        // Animation typically lasts 0.25-0.35 seconds, run for 0.5s to be safe
        animationEndTime = Date().addingTimeInterval(0.5)
        
        // Create timer at 60fps
        animationTimer = Timer.scheduledTimer(withTimeInterval: 1.0/60.0, repeats: true) { [weak self] timer in
            guard let self = self else { 
                timer.invalidate()
                return 
            }
            
            // Check if animation time has elapsed
            if let endTime = self.animationEndTime, Date() > endTime {
                timer.invalidate()
                self.animationTimer = nil
                return
            }
            
            // Request geometry update from controller
            self.onAnimationUpdate?()
        }
    }
    
    /// Calculate and set the glass view frame
    /// Uses stored sidebar/inspector widths when panels are visible,
    /// constant offsets when panels are collapsed
    private func updateLayout() {
        guard let glass = glassView,
              let window = window,
              let parentView = glass.superview else { return }
        
        // Get titlebar height (toolbar is part of titlebar in unified style)
        let titlebarHeight = window.frame.height - window.contentLayoutRect.height
        
        // Calculate X position dynamically:
        // - If sidebar is visible (expanded): use stored sidebarWidth + padding
        //   (toggle button is inside sidebar, so we just need spacing from sidebar edge)
        // - If sidebar is collapsed: dynamically find sidebar toggle button's right edge
        let expandedSidebarPadding: CGFloat = 18  // padding after sidebar edge when expanded
        
        let x: CGFloat
        if sidebarToggleWidth > 120 {  
            // Sidebar is expanded, sidebarToggleWidth is the full sidebar width
            // Toggle button is now inside the sidebar, so X = sidebarWidth + padding
            x = sidebarToggleWidth + expandedSidebarPadding
        } else {
            // Sidebar is collapsed, find the actual toggle button position dynamically
            let sidebarMaxX = findSidebarToggleMaxX()
            x = sidebarMaxX + leadingPadding
        }
        
        // Calculate Y position: center in toolbar area
        let parentHeight = parentView.bounds.height
        let toolbarCenterY = parentHeight - (titlebarHeight / 2)
        let y = toolbarCenterY - (glassHeight / 2) + verticalCenterOffset
        
        // Calculate right edge dynamically:
        // - If inspector is visible (expanded): subtract inspector panel width from parent width
        // - If inspector is collapsed: dynamically find inspector toggle button's left edge
        let rightLimit: CGFloat
        if inspectorVisible {
            // Inspector is expanded, inspectorToggleWidth is the full inspector panel width
            rightLimit = parentView.bounds.width - inspectorToggleWidth - trailingPadding
        } else {
            // Inspector is collapsed, find the actual toggle button position dynamically
            let inspectorMinX = findInspectorToggleMinX()
            rightLimit = inspectorMinX - trailingPadding
        }
        
        let width = max(100, rightLimit - x)  // Minimum width of 100
        
        // Set frame
        glass.frame = NSRect(x: x, y: y, width: width, height: glassHeight)
        
        // Ensure modeline view fills glass
        modeLineView?.frame = glass.bounds
    }
    
    private func parseSegments(_ input: String) -> (lhs: String, rhs: String) {
        let trimmed = input.trimmingCharacters(in: .whitespaces)
        if let range = trimmed.range(of: "   +", options: .regularExpression) {
            let lhs = String(trimmed[..<range.lowerBound]).trimmingCharacters(in: .whitespaces)
            let rhs = String(trimmed[range.upperBound...]).trimmingCharacters(in: .whitespaces)
            return (lhs, rhs)
        }
        return (trimmed, "")
    }
    
    deinit {
        animationTimer?.invalidate()
        if let observer = frameObserver {
            NotificationCenter.default.removeObserver(observer)
        }
    }
}

/// SwiftUI view for the modeline displayed in the titlebar accessory
/// Designed to sit alongside traffic lights and sidebar toggle
@available(macOS 26.0, *)
struct ModeLineTitlebarView: View {
    let content: String

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
            // Left side content
            Text(segments.lhs.isEmpty ? " " : segments.lhs)
                .font(.system(size: 11, design: .monospaced))
                .foregroundStyle(.primary)
                .lineLimit(1)
                .truncationMode(.tail)

            Spacer(minLength: 20)

            // Right side content
            if !segments.rhs.isEmpty {
                Text(segments.rhs)
                    .font(.system(size: 11, design: .monospaced))
                    .foregroundStyle(.secondary)
                    .lineLimit(1)
                    .truncationMode(.head)
            }
        }
        .padding(.horizontal, 12)
        .padding(.vertical, 4)
        .frame(maxWidth: .infinity)
        .frame(height: 28)
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

/// Placeholder view for the detail column (right panel)
/// Will eventually host child frames for agent-shell, buffer list, etc.
@available(macOS 26.0, *)
struct DetailPlaceholderView: View {
    var state: NavigationSidebarState
    
    /// Map vibrancy material to NSVisualEffectView.Material
    private var effectMaterial: NSVisualEffectView.Material {
        switch state.vibrancyMaterial {
        case .none: return .windowBackground
        case .ultraThick: return .headerView
        case .thick: return .titlebar
        case .regular: return .menu
        case .thin: return .popover
        case .ultraThin: return .hudWindow
        }
    }
    
    var body: some View {
        ZStack {
            // Vibrancy background matching the rest of the UI
            if state.vibrancyMaterial != .none {
                VibrancyBackgroundView(
                    material: effectMaterial,
                    blendingMode: .behindWindow,
                    isActive: true
                )
            }
            
            // Tint layer
            Color(nsColor: state.backgroundColor)
                .opacity(Double(state.backgroundAlpha))
            
            // Placeholder content
            VStack(spacing: 16) {
                Image(systemName: "sidebar.trailing")
                    .font(.system(size: 48, weight: .ultraLight))
                    .foregroundStyle(.tertiary)
                
                Text("Detail Panel")
                    .font(.headline)
                    .foregroundStyle(.secondary)
                
                Text("Future home of agent-shell,\nbuffer list, and other tools")
                    .font(.caption)
                    .foregroundStyle(.tertiary)
                    .multilineTextAlignment(.center)
            }
        }
        .ignoresSafeArea(.container, edges: .top)
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
    var onGeometryUpdate: (() -> Void)?  // Called when sidebar/inspector visibility changes

    @State private var columnVisibility: NavigationSplitViewVisibility = .detailOnly
    
    /// Binding for inspector (right panel) visibility
    private var inspectorVisibleBinding: Binding<Bool> {
        Binding(
            get: { state.detailVisible },
            set: { newValue in
                state.detailVisible = newValue
            }
        )
    }

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

    /// Parse modeline into LHS and RHS segments
    private func parseSegments(_ input: String) -> (lhs: String, rhs: String) {
        let trimmed = input.trimmingCharacters(in: .whitespaces)
        if let range = trimmed.range(of: "   +", options: .regularExpression) {
            let lhs = String(trimmed[..<range.lowerBound]).trimmingCharacters(in: .whitespaces)
            let rhs = String(trimmed[range.upperBound...]).trimmingCharacters(in: .whitespaces)
            return (lhs, rhs)
        }
        return (trimmed, "")
    }
    
    /// Log toolbar metrics for debugging layout calculations
    private func logToolbarMetrics() {
        guard let window = NSApp.keyWindow else {
            return
        }
        
        
        // Traffic light buttons (close, minimize, zoom)
        var trafficLightWidth: CGFloat = 0
        var trafficLightDetails: [String] = []
        
        if let closeButton = window.standardWindowButton(.closeButton) {
            let frame = closeButton.frame
            trafficLightDetails.append("close: x=\(frame.origin.x), w=\(frame.width)")
            trafficLightWidth = max(trafficLightWidth, frame.maxX)
        }
        if let miniButton = window.standardWindowButton(.miniaturizeButton) {
            let frame = miniButton.frame
            trafficLightDetails.append("minimize: x=\(frame.origin.x), w=\(frame.width)")
            trafficLightWidth = max(trafficLightWidth, frame.maxX)
        }
        if let zoomButton = window.standardWindowButton(.zoomButton) {
            let frame = zoomButton.frame
            trafficLightDetails.append("zoom: x=\(frame.origin.x), w=\(frame.width)")
            trafficLightWidth = max(trafficLightWidth, frame.maxX)
        }
        
        for detail in trafficLightDetails {
        }
        
        // Toolbar and its items
        if let toolbar = window.toolbar {
            
            // Find toolbar view in the view hierarchy
            if let contentView = window.contentView {
                findToolbarItems(in: window, contentView: contentView)
            }
        } else {
        }
        
        // Sidebar toggle button - it's typically part of the toolbar
        // We search for NSSplitViewDividerView or sidebar toggle in the hierarchy
        if let contentView = window.contentView {
            findSidebarToggle(in: contentView)
        }
        
        // Detail toggle button is our custom SwiftUI button
        // Its frame is within the sidebar column
        
        // Standard toolbar item spacing
        
    }
    
    /// Find toolbar items in the view hierarchy and log their frames
    private func findToolbarItems(in window: NSWindow, contentView: NSView) {
        // Get the titlebar container view
        if let titlebarView = window.standardWindowButton(.closeButton)?.superview?.superview {
            
            // Look for toolbar items
            enumerateViews(titlebarView, depth: 0) { view, depth in
                let viewType = String(describing: type(of: view))
                let _ = String(repeating: "  ", count: depth)
                
                if viewType.contains("ToolbarItem") || viewType.contains("Button") || viewType.contains("Sidebar") {
                }
            }
        }
    }
    
    /// Find the sidebar toggle button
    private func findSidebarToggle(in view: NSView) {
        enumerateViews(view, depth: 0) { subview, _ in
            let viewType = String(describing: type(of: subview))
            
            // Look for sidebar toggle related views
            if viewType.contains("SidebarToggle") || viewType.contains("NSSplitViewItemViewControllerWrapperView") {
            }
            
            // NSButton with sidebar.left image might be the toggle
            if let button = subview as? NSButton {
                if let image = button.image, image.name()?.contains("sidebar") == true {
                }
            }
        }
        
        // The sidebar toggle from NavigationSplitView is typically ~28pt wide
    }
    
    /// Enumerate views recursively
    private func enumerateViews(_ view: NSView, depth: Int, handler: (NSView, Int) -> Void) {
        handler(view, depth)
        for subview in view.subviews {
            enumerateViews(subview, depth: depth + 1, handler: handler)
        }
    }
    
    var body: some View {
        // NOTE: Modeline is now rendered by NSToolbar via ModeLineToolbarController
        // with NSGlassEffectView for Liquid Glass effect

        GeometryReader { geometry in
            // 2-column NavigationSplitView with inspector for symmetric toolbar behavior
            // sidebar | content (Emacs + inspector)
            // This provides: sidebar toggle + tracking separator on left
            //                inspector toggle + tracking separator on right
            NavigationSplitView(columnVisibility: $columnVisibility) {
                // Sidebar column (left)
                SidebarContentView(
                    state: state,
                    onBufferSelect: onBufferSelect,
                    onBufferClose: onBufferClose,
                    onFileSelect: onFileSelect
                )
                .navigationSplitViewColumnWidth(min: 200, ideal: 280, max: 400)
                .background {
                    // Track sidebar width for modeline calculation
                    GeometryReader { sidebarGeometry in
                        Color.clear
                            .onAppear {
                                state.sidebarWidth = sidebarGeometry.size.width
                            }
                            .onChange(of: sidebarGeometry.size.width) { _, newWidth in
                                state.sidebarWidth = newWidth
                            }
                    }
                }
            } detail: {
                // Content/Detail column - Emacs with inspector
                EmacsContentView(
                    emacsView: emacsView,
                    state: state
                )
                .background {
                    // Track content column width for modeline calculation
                    GeometryReader { contentGeometry in
                        Color.clear
                            .onAppear {
                                state.contentWidth = contentGeometry.size.width
                            }
                            .onChange(of: contentGeometry.size.width) { _, newWidth in
                                state.contentWidth = newWidth
                            }
                    }
                }
                // Inspector (right panel) - symmetric to sidebar
                // System automatically adds inspector toggle button and tracking separator
                .inspector(isPresented: inspectorVisibleBinding) {
                    DetailPlaceholderView(state: state)
                        .inspectorColumnWidth(min: 200, ideal: 300, max: 500)
                        .background {
                            // Track inspector width
                            GeometryReader { inspectorGeometry in
                                Color.clear
                                    .onAppear {
                                        state.detailWidth = inspectorGeometry.size.width
                                    }
                                    .onChange(of: inspectorGeometry.size.width) { _, newWidth in
                                        state.detailWidth = newWidth
                                    }
                            }
                        }
                }
            }
            .navigationSplitViewStyle(.balanced)
            // Let the system handle toolbar background for Safari-like blur effect
            // Content will scroll behind the toolbar with vibrancy
            .toolbarBackgroundVisibility(.visible, for: .windowToolbar)
            .toolbarTitleDisplayMode(.inline)
            // SwiftUI toolbar for inspector toggle only
            // Modeline is rendered as separate NSGlassEffectView overlay by NavigationSidebarController
            .toolbar {
                // Inspector toggle button - symmetric to sidebar toggle
                ToolbarItem(placement: .primaryAction) {
                    Button {
                        withAnimation(.easeInOut(duration: 0.25)) {
                            state.detailVisible.toggle()
                        }
                    } label: {
                        Image(systemName: state.detailVisible ? "sidebar.trailing" : "sidebar.trailing")
                            .symbolVariant(state.detailVisible ? .none : .none)
                    }
                    .help(state.detailVisible ? "Hide Inspector" : "Show Inspector")
                }
            }
            .background {
                ZStack {
                    VibrancyBackgroundView(
                        material: effectMaterialForState,
                        blendingMode: .behindWindow,
                        isActive: state.vibrancyMaterial != .none
                    )
                    Color(nsColor: state.backgroundColor)
                        .opacity(Double(state.backgroundAlpha))
                }
                .ignoresSafeArea()
            }
            .onChange(of: state.sidebarVisible) { _, newValue in
                withAnimation {
                    columnVisibility = newValue ? .all : .detailOnly
                }
                // Trigger geometry update after animation completes
                DispatchQueue.main.asyncAfter(deadline: .now() + 0.3) {
                    onGeometryUpdate?()
                }
            }
            .onChange(of: columnVisibility) { _, newValue in
                // Sync state.sidebarVisible when NavigationSplitView's built-in toggle is used
                let isSidebarNowVisible = (newValue == .all || newValue == .doubleColumn)
                if state.sidebarVisible != isSidebarNowVisible {
                    state.sidebarVisible = isSidebarNowVisible
                }
                // Trigger geometry update after animation completes
                DispatchQueue.main.asyncAfter(deadline: .now() + 0.3) {
                    onGeometryUpdate?()
                }
            }
            .onChange(of: state.detailVisible) { _, newValue in
                // Trigger geometry update after animation completes
                DispatchQueue.main.asyncAfter(deadline: .now() + 0.3) {
                    onGeometryUpdate?()
                }
            }
            .onChange(of: geometry.size) { _, newSize in
                state.toolbarWidth = newSize.width
                onGeometryUpdate?()
            }
            .onAppear {
                state.toolbarWidth = geometry.size.width
                
                // Compute and log toolbar metrics
                logToolbarMetrics()
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
    var detailVisible: Bool = false  // Detail column (right panel) visibility
    var backgroundColor: NSColor = .windowBackgroundColor
    var backgroundAlpha: CGFloat = 0.5  // Reduced default for more vibrancy
    /// Window appearance mode: "light", "dark", or "auto"
    var windowAppearance: String = "auto"
    /// Vibrancy material style
    var vibrancyMaterial: VibrancyMaterial = .ultraThin
    /// Whether decorations (toolbar and traffic lights) are visible
    var decorationsVisible: Bool = true
    /// Available toolbar width (calculated from window geometry)
    var toolbarWidth: CGFloat = 600
    /// Current sidebar width (when visible)
    var sidebarWidth: CGFloat = 280
    /// Current detail panel width (when visible)
    var detailWidth: CGFloat = 300
    /// Current content column width (center column with Emacs)
    var contentWidth: CGFloat = 400
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

    /// Glass overlay controller for modeline in toolbar area
    private var modeLineGlassOverlay: ModeLineGlassOverlayController?

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

        let emacsView = currentContentView

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

        // Configure window for proper NavigationSplitView with glass effect
        window.styleMask.insert(.fullSizeContentView)
        // DON'T set titlebarAppearsTransparent - let system provide glass effect
        // window.titlebarAppearsTransparent = true
        window.titleVisibility = .hidden
        // Keep window opaque - the glass effect provides transparency
        // window.isOpaque = false
        // window.backgroundColor = .clear

        // Configure toolbar style - .unified merges toolbar with titlebar for glass effect
        window.toolbarStyle = .unified


        // CRITICAL: Clear and maintain empty window title to prevent Emacs geometry display
        // NOTE: Do NOT set window.subtitle - SwiftUI's .navigationSubtitle() handles modeline
        window.title = ""
        window.representedURL = nil

        // Add observer to prevent Emacs from changing the title
        titleObservation = window.observe(\.title, options: [.new]) { window, _ in
            if window.title != "" {
                window.title = ""
            }
        }

        hostingView = hosting
        isSetup = true

        // Setup modeline overlay with NSGlassEffectView in toolbar area
        // Uses NSTitlebarAccessoryViewController for proper positioning
        setupModeLineOverlay(for: window)

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
    
    /// Setup the modeline overlay with NSGlassEffectView in the toolbar area
    /// Uses direct frame positioning for precise geometry control
    private func setupModeLineOverlay(for window: NSWindow) {
        let overlay = ModeLineGlassOverlayController()
        overlay.setup(for: window)
        
        // Wire animation callback for smooth updates during sidebar/inspector expansion
        overlay.onAnimationUpdate = { [weak self] in
            self?.updateModeLineGeometry()
        }
        
        modeLineGlassOverlay = overlay
        
        // Initial geometry update with current state
        updateModeLineGeometry()
        
    }
    
    /// Update the modeline overlay geometry based on current state
    private func updateModeLineGeometry() {
        // Calculate leading offset (left edge of modeline)
        // From debug logs: traffic lights end at 69pt, sidebar toggle button at 47pt
        // When sidebar hidden: traffic lights (69pt) + toggle button (47pt) + padding (8pt) = ~124pt
        // When sidebar visible: sidebar width
        let sidebarToggleButtonWidth: CGFloat = 47  // From debug: NSToolbarItemViewer width
        let trafficLightsWidth: CGFloat = 69        // From debug: rightmost edge of zoom button
        let leadingOffset: CGFloat = state.sidebarVisible 
            ? state.sidebarWidth 
            : (trafficLightsWidth + sidebarToggleButtonWidth)
        
        // Calculate trailing offset (right edge of modeline)
        // From debug: inspector toggle button NSToolbarItemViewer is 47pt  
        // When inspector hidden: just the inspector toggle button (~47pt)
        // When inspector visible: inspector panel width (detailWidth)
        let inspectorToggleButtonWidth: CGFloat = 47  // From debug: NSToolbarItemViewer width
        let trailingOffset: CGFloat = state.detailVisible 
            ? state.detailWidth 
            : inspectorToggleButtonWidth
        
        modeLineGlassOverlay?.updateGeometry(
            content: state.modeLine,
            sidebarToggleWidth: leadingOffset,
            contentWidth: state.contentWidth,
            hasInspector: state.detailVisible,
            inspectorToggleWidth: trailingOffset
        )
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

        // Clean up glass overlay
        modeLineGlassOverlay = nil

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

    // MARK: - Detail Panel

    /// Show the detail column (right panel)
    func showDetail() {
        guard isSetup else { return }
        state.detailVisible = true
    }

    /// Hide the detail column
    func hideDetail() {
        guard isSetup else { return }
        state.detailVisible = false
    }

    /// Toggle the detail column visibility
    func toggleDetail() {
        if state.detailVisible {
            hideDetail()
        } else {
            showDetail()
        }
    }

    /// Check if detail is currently visible
    var isDetailVisible: Bool {
        state.detailVisible
    }

    // MARK: - Decorations (Toolbar & Traffic Lights)

    /// Set visibility of decorations (toolbar area and traffic lights)
    func setDecorationsVisible(_ visible: Bool) {
        guard let window = window else {
            return
        }
        state.decorationsVisible = visible
        // Toggle toolbar visibility
        window.toolbar?.isVisible = visible
        if visible {
            showTrafficLights(window)
        } else {
            hideTrafficLights(window)
        }
    }

    /// Toggle visibility of decorations
    func toggleDecorations() {
        setDecorationsVisible(!state.decorationsVisible)
    }

    /// Check if decorations are currently visible
    var areDecorationsVisible: Bool {
        state.decorationsVisible
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
        // Update the modeline overlay with new content and recalculated geometry
        updateModeLineGeometry()
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
            onFileSelect: { [weak self] file in self?.onFileSelect?(file.path) },
            onGeometryUpdate: { [weak self] in self?.updateModeLineGeometry() }
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

    func showDetail(for window: NSWindow) {
        getController(for: window).showDetail()
    }

    func hideDetail(for window: NSWindow) {
        getController(for: window).hideDetail()
    }

    func toggleDetail(for window: NSWindow) {
        getController(for: window).toggleDetail()
    }

    func isDetailVisible(for window: NSWindow) -> Bool {
        controllers[window.windowNumber]?.isDetailVisible ?? false
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

    func setDecorationsVisible(for window: NSWindow, visible: Bool) {
        controllers[window.windowNumber]?.setDecorationsVisible(visible)
    }

    func toggleDecorations(for window: NSWindow) {
        controllers[window.windowNumber]?.toggleDecorations()
    }

    func areDecorationsVisible(for window: NSWindow) -> Bool {
        controllers[window.windowNumber]?.areDecorationsVisible ?? true
    }
}
