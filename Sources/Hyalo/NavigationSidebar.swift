// NavigationSidebar.swift - NavigationSplitView layout for Hyalo with Liquid Glass
// Copyright (C) 2025
// Target: macOS 26 Tahoe with Liquid Glass design

import AppKit
import SwiftUI

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

        // Use nerd font for proper icon rendering
        let font = findModeLineFont(size: 11)

        // Configure LHS label - use autoresizing, not constraints
        lhsLabel.font = font
        lhsLabel.textColor = .labelColor
        lhsLabel.backgroundColor = .clear
        lhsLabel.drawsBackground = false
        lhsLabel.alignment = .left
        lhsLabel.lineBreakMode = .byTruncatingTail
        lhsLabel.autoresizingMask = [.maxXMargin]

        // Configure RHS label
        rhsLabel.font = font
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

/// Section header view for sidebar panels
@available(macOS 26.0, *)
struct SidebarSectionHeader: View {
    let title: String
    let systemImage: String
    var isBusy: Bool = false

    @State private var rotation: Double = 0

    var body: some View {
        let _ = print("[SidebarSectionHeader] Rendering. Title: \(title), Busy: \(isBusy), Rotation: \(rotation)")
        HStack(spacing: 6) {
            Image(systemName: systemImage)
                .font(.system(size: 10, weight: .medium))
                .foregroundStyle(.secondary)
                .symbolEffect(.pulse, isActive: isBusy)
                .rotationEffect(.degrees(rotation))
                .onChange(of: isBusy, initial: true) { _, busy in
                    print("[SidebarSectionHeader] onChange(isBusy): \(busy)")
                    if busy {
                        // Continuous rotation
                        rotation = 0
                        withAnimation(.linear(duration: 2).repeatForever(autoreverses: false)) {
                            rotation = 360
                        }
                    } else {
                        // Reset rotation
                        withAnimation(.default) {
                            rotation = 0
                        }
                    }
                }
            Text(title)
                .font(.system(size: 11, weight: .semibold))
                .foregroundStyle(.secondary)
            Spacer()
        }
        .padding(.horizontal, 12)
        .padding(.vertical, 6)
    }
}

/// Custom NSSplitView wrapper for sidebar that allows divider customization
@available(macOS 26.0, *)
struct StyledSplitView<TopContent: View, BottomContent: View>: NSViewRepresentable {
    let topContent: TopContent
    let bottomContent: BottomContent
    let dividerColor: NSColor
    let dividerThickness: CGFloat

    init(
        dividerColor: NSColor = .separatorColor,
        dividerThickness: CGFloat = 1,
        @ViewBuilder top: () -> TopContent,
        @ViewBuilder bottom: () -> BottomContent
    ) {
        self.dividerColor = dividerColor
        self.dividerThickness = dividerThickness
        self.topContent = top()
        self.bottomContent = bottom()
    }

    func makeNSView(context: Context) -> NSSplitView {
        let splitView = CustomDividerSplitView()
        splitView.isVertical = false  // Vertical stacking (horizontal divider)
        splitView.dividerStyle = .thin
        splitView.customDividerColor = dividerColor
        splitView.customDividerThickness = dividerThickness
        splitView.delegate = context.coordinator

        // Create hosting views for SwiftUI content
        let topHost = NSHostingView(rootView: topContent)
        let bottomHost = NSHostingView(rootView: bottomContent)

        topHost.translatesAutoresizingMaskIntoConstraints = false
        bottomHost.translatesAutoresizingMaskIntoConstraints = false

        splitView.addArrangedSubview(topHost)
        splitView.addArrangedSubview(bottomHost)

        // Set initial proportions
        splitView.setHoldingPriority(.defaultLow, forSubviewAt: 0)
        splitView.setHoldingPriority(.defaultLow, forSubviewAt: 1)

        return splitView
    }

    func updateNSView(_ nsView: NSSplitView, context: Context) {
        if let customSplit = nsView as? CustomDividerSplitView {
            customSplit.customDividerColor = dividerColor
            customSplit.customDividerThickness = dividerThickness
            customSplit.needsDisplay = true
        }
    }

    func makeCoordinator() -> Coordinator {
        Coordinator()
    }

    class Coordinator: NSObject, NSSplitViewDelegate {
        func splitView(_ splitView: NSSplitView, constrainMinCoordinate proposedMinimumPosition: CGFloat, ofSubviewAt dividerIndex: Int) -> CGFloat {
            return 80  // Minimum height for top pane
        }

        func splitView(_ splitView: NSSplitView, constrainMaxCoordinate proposedMaximumPosition: CGFloat, ofSubviewAt dividerIndex: Int) -> CGFloat {
            return splitView.bounds.height - 150  // Minimum height for bottom pane
        }
    }
}

/// Custom NSSplitView subclass with configurable divider appearance
@available(macOS 26.0, *)
final class CustomDividerSplitView: NSSplitView {
    var customDividerColor: NSColor = .separatorColor
    var customDividerThickness: CGFloat = 1

    override var dividerColor: NSColor {
        return customDividerColor
    }

    override var dividerThickness: CGFloat {
        return customDividerThickness
    }
}

/// NSViewRepresentable wrapper for embedded Emacs child-frame NSView
/// Uses EmacsEventForwardingContainer to forward events to the original hidden window
@available(macOS 26.0, *)
struct EmbeddedEmacsView: NSViewRepresentable {
    let embeddedView: NSView
    let originalWindow: NSWindow?
    let slot: String
    var onResize: ((String, CGFloat, CGFloat) -> Void)?

    func makeNSView(context: Context) -> EmacsEventForwardingContainer {
        let container = EmacsEventForwardingContainer()

        if let originalWindow = originalWindow {
            // Use event forwarding to route events back to the original Emacs window
            container.configure(embeddedView: embeddedView, originalWindow: originalWindow)
        } else {
            // Fallback without forwarding (shouldn't happen but handles edge cases)
            container.wantsLayer = true
            container.layer?.backgroundColor = .clear

            embeddedView.translatesAutoresizingMaskIntoConstraints = false
            container.addSubview(embeddedView)

            NSLayoutConstraint.activate([
                embeddedView.leadingAnchor.constraint(equalTo: container.leadingAnchor),
                embeddedView.trailingAnchor.constraint(equalTo: container.trailingAnchor),
                embeddedView.topAnchor.constraint(equalTo: container.topAnchor),
                embeddedView.bottomAnchor.constraint(equalTo: container.bottomAnchor)
            ])
        }

        return container
    }

    func updateNSView(_ nsView: EmacsEventForwardingContainer, context: Context) {
        // Notify Elisp of size changes
        let size = nsView.bounds.size
        if size.width > 0 && size.height > 0 {
            onResize?(slot, size.width, size.height)
        }
    }
}

/// The sidebar content with Liquid Glass styling
/// Shows embedded Emacs views when available, falls back to SwiftUI List
@available(macOS 26.0, *)
struct SidebarContentView: View {
    var state: NavigationSidebarState
    var onResize: ((String, CGFloat, CGFloat) -> Void)?

    /// SwiftUI-side margin for embedded frames (matches Emacs internal-border-width)
    private let embeddedMargin: CGFloat = 12

    var body: some View {
        // If embedded views are available, use custom StyledSplitView with Emacs frames
        if let topView = state.leftTopView, let bottomView = state.leftBottomView {
            StyledSplitView(
              dividerColor: .separatorColor,
                dividerThickness: 1,
                top: {
                    VStack(spacing: 0) {
                        SidebarSectionHeader(title: "OPEN BUFFERS", systemImage: "doc.on.doc")
                        EmbeddedEmacsView(embeddedView: topView, originalWindow: state.leftTopWindow, slot: "left-top", onResize: onResize)
                            .padding(.horizontal, embeddedMargin)
                            .padding(.bottom, embeddedMargin)
                            .frame(maxWidth: .infinity, maxHeight: .infinity)
                    }
                },
                bottom: {
                    VStack(spacing: 0) {
                        SidebarSectionHeader(title: "WORKSPACE", systemImage: "folder")
                        EmbeddedEmacsView(embeddedView: bottomView, originalWindow: state.leftBottomWindow, slot: "left-bottom", onResize: onResize)
                            .padding(.horizontal, embeddedMargin)
                            .padding(.bottom, embeddedMargin)
                            .frame(maxWidth: .infinity, maxHeight: .infinity)
                    }
                }
            )
            .transaction { $0.animation = nil }  // Disable animation to prevent resize flicker
            .background {
                Color(nsColor: state.backgroundColor)
                    .opacity(Double(state.backgroundAlpha))
                    .ignoresSafeArea()
            }
        } else if let topView = state.leftTopView {
            // Only top view available
            VStack(spacing: 0) {
                SidebarSectionHeader(title: "OPEN BUFFERS", systemImage: "doc.on.doc")
                EmbeddedEmacsView(embeddedView: topView, originalWindow: state.leftTopWindow, slot: "left-top", onResize: onResize)
                    .padding(.horizontal, embeddedMargin)
                    .padding(.bottom, embeddedMargin)
                    .frame(maxWidth: .infinity, maxHeight: .infinity)
            }
            .transaction { $0.animation = nil }
            .background {
                Color(nsColor: state.backgroundColor)
                    .opacity(Double(state.backgroundAlpha))
                    .ignoresSafeArea()
            }
        } else if let bottomView = state.leftBottomView {
            // Only bottom view available
            VStack(spacing: 0) {
                SidebarSectionHeader(title: "WORKSPACE", systemImage: "folder")
                EmbeddedEmacsView(embeddedView: bottomView, originalWindow: state.leftBottomWindow, slot: "left-bottom", onResize: onResize)
                    .padding(.horizontal, embeddedMargin)
                    .padding(.bottom, embeddedMargin)
                    .frame(maxWidth: .infinity, maxHeight: .infinity)
            }
            .transaction { $0.animation = nil }
            .background {
                Color(nsColor: state.backgroundColor)
                    .opacity(Double(state.backgroundAlpha))
                    .ignoresSafeArea()
            }
        } else {
            // Empty placeholder when no embedded views
            Color.clear
                .background {
                    Color(nsColor: state.backgroundColor)
                        .opacity(Double(state.backgroundAlpha))
                        .ignoresSafeArea()
                }
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

/// Custom NSView for gradient fade that properly resizes with layout
/// Creates a Safari-like fade effect from toolbar down into content
@available(macOS 26.0, *)
final class GradientFadeView: NSView {
    var fadeColor: NSColor = .windowBackgroundColor {
        didSet { updateGradientColors() }
    }

    /// Target opacity at the top of the gradient (interpolates from 0 to this value)
    var topOpacity: CGFloat = 0.5 {
        didSet { updateGradientColors() }
    }

    private var gradientLayer: CAGradientLayer?

    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        setupGradient()
    }

    required init?(coder: NSCoder) {
        super.init(coder: coder)
        setupGradient()
    }

    private func setupGradient() {
        wantsLayer = true

        let gradient = CAGradientLayer()
        gradient.name = "toolbarFadeGradient"
        // CAGradientLayer uses Core Graphics coordinates: y=0 is BOTTOM, y=1 is TOP
        // We want solid at top fading to transparent at bottom
        gradient.startPoint = CGPoint(x: 0.5, y: 1.0)  // Top (visually)
        gradient.endPoint = CGPoint(x: 0.5, y: 0.0)    // Bottom (visually)
        gradient.locations = [0.0, 0.4, 1.0]
        layer?.addSublayer(gradient)
        gradientLayer = gradient
        updateGradientColors()
    }

    private func updateGradientColors() {
        // Scale the gradient stops by topOpacity (window's background alpha)
        // Base values: 0.85, 0.3, 0.0 - scaled by current window opacity
        let solidColor = fadeColor.withAlphaComponent(0.85 * topOpacity)
        let midColor = fadeColor.withAlphaComponent(0.3 * topOpacity)
        let clearColor = fadeColor.withAlphaComponent(0.0)
        gradientLayer?.colors = [solidColor.cgColor, midColor.cgColor, clearColor.cgColor]
    }

    override func layout() {
        super.layout()
        // Update gradient frame to match view bounds on every layout pass
        gradientLayer?.frame = bounds
    }
}

/// Gradient fade overlay for toolbar area - Safari-like effect
/// This view spans the full window width and fades from solid at top to transparent at bottom
/// Positioned between tint layer and Emacs content for subtle toolbar blending
@available(macOS 26.0, *)
struct ToolbarFadeOverlay: NSViewRepresentable {
    var fadeColor: NSColor
    var topOpacity: CGFloat

    func makeNSView(context: Context) -> GradientFadeView {
        let view = GradientFadeView()
        view.fadeColor = fadeColor
        view.topOpacity = topOpacity
        return view
    }

    func updateNSView(_ nsView: GradientFadeView, context: Context) {
        nsView.fadeColor = fadeColor
        nsView.topOpacity = topOpacity
    }
}

/// Inspector content view for the detail column (right panel)
/// Shows embedded agent-shell when available, otherwise placeholder
@available(macOS 26.0, *)
struct DetailPlaceholderView: View {
    var state: NavigationSidebarState
    var onResize: ((String, CGFloat, CGFloat) -> Void)?

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

            // Show embedded agent-shell or placeholder
            if let rightView = state.rightView {
                VStack(spacing: 0) {
                    SidebarSectionHeader(
                        title: state.inspectorTitle,
                        systemImage: state.inspectorIcon,
                        isBusy: state.inspectorBusy
                    )
                    EmbeddedEmacsView(embeddedView: rightView, originalWindow: state.rightWindow, slot: "right", onResize: onResize)
                        .padding(.horizontal, 12)
                        .padding(.bottom, 12)
                        .frame(maxWidth: .infinity, maxHeight: .infinity)
                }
                  .padding(.top, 12)  // Increased from 6 to 16 for more padding before the header view
            } else {
                // Placeholder content
                VStack(spacing: 16) {
                    Image(systemName: "sparkles")
                        .font(.system(size: 48, weight: .ultraLight))
                        .foregroundStyle(.tertiary)

                    Text("Inspector")
                        .font(.headline)
                        .foregroundStyle(.secondary)

                    Text("Run hyalo-sidebar-right-setup\nto embed agent-shell")
                        .font(.caption)
                        .foregroundStyle(.tertiary)
                        .multilineTextAlignment(.center)
                }
            }
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity)
        .transaction { $0.animation = nil }  // Disable all animation to prevent resize
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

    /// Determine if we're in dark mode based on appearance
    private var isDarkMode: Bool {
        switch state.windowAppearance {
        case "dark":
            return true
        case "light":
            return false
        default:
            // Auto mode - check system appearance
            let appearance = NSApp.effectiveAppearance
            return appearance.bestMatch(from: [.darkAqua, .aqua]) == .darkAqua
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

            // Footer pattern layer - positioned at bottom over echo area
            // Hidden when sidebar is expanded to avoid visual discontinuity
            if !state.sidebarVisible {
                FooterPatternLayer(
                    pattern: state.footerPattern,
                    height: state.footerHeight,
                    tintColor: state.backgroundColor,
                    backgroundAlpha: state.footerBackgroundAlpha,
                    patternAlpha: state.footerPatternAlpha,
                    isDarkMode: isDarkMode
                )
            }

            // Emacs content
            EmacsNSViewRepresentable(emacsView: emacsView)

            // Toolbar fade overlay - Safari-like gradient from toolbar into content
            // ON TOP of Emacs content, but behind NSToolbar (window chrome)
            // Hidden when sidebar is expanded to avoid visual discontinuity
            if !state.sidebarVisible {
                VStack(spacing: 0) {
                    ToolbarFadeOverlay(
                        fadeColor: state.backgroundColor,
                        topOpacity: state.backgroundAlpha
                    )
                    .frame(height: state.toolbarHeight)
                    Spacer()
                }
                .allowsHitTesting(false)
            }
        }
        // Only extend under top edge (toolbar), NOT the sidebar (leading edge)
        .ignoresSafeArea(.container, edges: .top)
    }
}

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

    var onGeometryUpdate: (() -> Void)?  // Called when sidebar/inspector visibility changes
    var onEmbeddedResize: ((String, CGFloat, CGFloat) -> Void)?  // Called when embedded view resizes
    var onSidebarVisibilityChanged: ((Bool, Bool) -> Void)?  // Called when sidebar toggles (visible, needsSetup)
    var onDetailVisibilityChanged: ((Bool, Bool) -> Void)?  // Called when inspector toggles (visible, needsSetup)

    @State private var columnVisibility: NavigationSplitViewVisibility = .detailOnly


    /// Binding for inspector (right panel) visibility
    /// Uses withTransaction to disable animation and prevent window resize flicker
    private var inspectorVisibleBinding: Binding<Bool> {
        Binding(
            get: { state.detailVisible },
            set: { newValue in
                // Disable animation to prevent window resize during inspector show/hide
                var transaction = Transaction()
                transaction.disablesAnimations = true
                withTransaction(transaction) {
                    state.detailVisible = newValue
                }
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
                    onResize: onEmbeddedResize
                )
                .navigationSplitViewColumnWidth(min: 200, ideal: 280, max: 400)
                //.transaction { $0.animation = nil }  // Removed to allow content animations
                .background {
                    // Track sidebar width for modeline calculation
                    GeometryReader { sidebarGeometry in
                        Color.clear
                            .onAppear {
                                state.sidebarWidth = sidebarGeometry.size.width
                            }
                            .onChange(of: sidebarGeometry.size.width) { _, newWidth in
                                state.sidebarWidth = newWidth
                                onGeometryUpdate?()  // Update toolbar when splitter moves
                            }
                    }
                }
            } detail: {
                // Content/Detail column - Emacs with inspector
                EmacsContentView(
                    emacsView: emacsView,
                    state: state
                )
                .frame(maxWidth: .infinity, maxHeight: .infinity)  // Stabilize main content size
                .background {
                    // Track content column width for modeline calculation
                    GeometryReader { contentGeometry in
                        Color.clear
                            .onAppear {
                                state.contentWidth = contentGeometry.size.width
                            }
                            .onChange(of: contentGeometry.size.width) { _, newWidth in
                                state.contentWidth = newWidth
                                onGeometryUpdate?()  // Update toolbar when splitter moves
                            }
                    }
                }
                // Inspector (right panel) - symmetric to sidebar
                // System automatically adds inspector toggle button and tracking separator
                .inspector(isPresented: inspectorVisibleBinding) {
                    DetailPlaceholderView(state: state, onResize: onEmbeddedResize)
                        .inspectorColumnWidth(min: 300, ideal: 400, max: 500)
                        //.transaction { $0.animation = nil }  // Removed to allow content animations
                        .background {
                            // Track inspector width
                            GeometryReader { inspectorGeometry in
                                Color.clear
                                    .onAppear {
                                        state.detailWidth = inspectorGeometry.size.width
                                    }
                                    .onChange(of: inspectorGeometry.size.width) { _, newWidth in
                                        state.detailWidth = newWidth
                                        onGeometryUpdate?()  // Update toolbar when splitter moves
                                    }
                            }
                        }
                }
            }
            .navigationSplitViewStyle(.balanced)
            .transaction { $0.disablesAnimations = true }  // Disable all NavigationSplitView animations
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
                        // Disable animation to prevent window resize flicker
                        var transaction = Transaction()
                        transaction.disablesAnimations = true
                        withTransaction(transaction) {
                            state.detailVisible.toggle()
                        }
                    } label: {
                        Image(systemName: "sparkles")
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
                // Notify Elisp about visibility change - check if setup is needed
                let needsSetup = newValue && (state.leftTopView == nil || state.leftBottomView == nil)
                onSidebarVisibilityChanged?(newValue, needsSetup)
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
                    // Notify Elisp about visibility change - check if setup is needed
                    let needsSetup = isSidebarNowVisible && (state.leftTopView == nil || state.leftBottomView == nil)
                    onSidebarVisibilityChanged?(isSidebarNowVisible, needsSetup)
                }
                // Trigger geometry update after animation completes
                DispatchQueue.main.asyncAfter(deadline: .now() + 0.3) {
                    onGeometryUpdate?()
                }
            }
            .onChange(of: state.detailVisible) { _, newValue in
                // Notify Elisp about visibility change - check if setup is needed
                let needsSetup = newValue && state.rightView == nil
                onDetailVisibilityChanged?(newValue, needsSetup)
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

// MARK: - Footer Pattern Types

/// Available footer patterns from heropatterns.com
enum FooterPattern: String, CaseIterable {
    case none = "none"
    case hideout = "hideout"
    case hexagons = "hexagons"
    case deathStar = "death-star"
    case bathroomFloor = "bathroom-floor"
    case tinyCheckers = "tiny-checkers"
    case plus = "plus"
    case cage = "cage"
    case diagonalStripes = "diagonal-stripes"
    case stripes = "stripes"
    case diagonalLines = "diagonal-lines"
    case polkaDots = "polka-dots"
    case signal = "signal"
    case wallpaper = "wallpaper"

    /// SVG path data and viewBox for each pattern
    var svgData: (path: String, width: CGFloat, height: CGFloat) {
        switch self {
        case .none:
            return ("", 40, 40)
        case .hideout:
            // ViewBox: 0 0 40 40
            return ("M0 38.59l2.83-2.83 1.41 1.41L1.41 40H0v-1.41zM0 1.4l2.83 2.83 1.41-1.41L1.41 0H0v1.41zM38.59 40l-2.83-2.83 1.41-1.41L40 38.59V40h-1.41zM40 1.41l-2.83 2.83-1.41-1.41L38.59 0H40v1.41zM20 18.6l2.83-2.83 1.41 1.41L21.41 20l2.83 2.83-1.41 1.41L20 21.41l-2.83 2.83-1.41-1.41L18.59 20l-2.83-2.83 1.41-1.41L20 18.59z", 40, 40)
        case .hexagons:
            // ViewBox: 0 0 28 49
            return ("M13.99 9.25l13 7.5v15l-13 7.5L1 31.75v-15l12.99-7.5zM3 17.9v12.7l10.99 6.34 11-6.35V17.9l-11-6.34L3 17.9zM0 15l12.98-7.5V0h-2v6.35L0 12.69v2.3zm0 18.5L12.98 41v8h-2v-6.85L0 35.81v-2.3zM15 0v7.5L27.99 15H28v-2.31h-.01L17 6.35V0h-2zm0 49v-8l12.99-7.5H28v2.31h-.01L17 42.15V49h-2z", 28, 49)
        case .deathStar:
            // ViewBox: 0 0 80 105
            return ("M20 10a5 5 0 0 1 10 0v50a5 5 0 0 1-10 0V10zm15 35a5 5 0 0 1 10 0v50a5 5 0 0 1-10 0V45zM20 75a5 5 0 0 1 10 0v20a5 5 0 0 1-10 0V75zm30-65a5 5 0 0 1 10 0v50a5 5 0 0 1-10 0V10zm0 65a5 5 0 0 1 10 0v20a5 5 0 0 1-10 0V75zM35 10a5 5 0 0 1 10 0v20a5 5 0 0 1-10 0V10zM5 45a5 5 0 0 1 10 0v50a5 5 0 0 1-10 0V45zm0-35a5 5 0 0 1 10 0v20a5 5 0 0 1-10 0V10zm60 35a5 5 0 0 1 10 0v50a5 5 0 0 1-10 0V45zm0-35a5 5 0 0 1 10 0v20a5 5 0 0 1-10 0V10z", 80, 105)
        case .bathroomFloor:
            // ViewBox: 0 0 80 80
            return ("M0 40L40 0H20L0 20M40 40L80 0v2L42 40h-2zm4 0L80 4v2L46 40h-2zm4 0l28-28v2L50 40h-2zm4 0l24-24v2L54 40h-2zm4 0l20-20v2L58 40h-2zm4 0l16-16v2L62 40h-2zm4 0l12-12v2L66 40h-2zm4 0l8-8v2l-6 6h-2z", 80, 80)
        case .tinyCheckers:
            // ViewBox: 0 0 8 8
            return ("M0 0h4v4H0V0zm4 4h4v4H4V4z", 8, 8)
        case .plus:
            // ViewBox: 0 0 60 60
            return ("M36 34v-4h-2v4h-4v2h4v4h2v-4h4v-2h-4zm0-30V0h-2v4h-4v2h4v4h2V6h4V4h-4zM6 34v-4H4v4H0v2h4v4h2v-4h4v-2H6zM6 4V0H4v4H0v2h4v4h2V6h4V4H6z", 60, 60)
        case .cage:
            // ViewBox: 0 0 36 36 (simplified version)
            return ("M0 0h36v36H0V0zm2 2v32h32V2H2zm7 7h18v18H9V9zm2 2v14h14V11H11z", 36, 36)
        case .diagonalStripes:
            // ViewBox: 0 0 40 40
            return ("M0 40L40 0H20L0 20M40 40V20L20 40", 40, 40)
        case .stripes:
            // ViewBox: 0 0 40 1
            return ("M0 0h20v1H0z", 40, 1)
        case .diagonalLines:
            // ViewBox: 0 0 6 6
            return ("M5 0h1L0 6V5zM6 5v1H5z", 6, 6)
        case .polkaDots:
            // ViewBox: 0 0 20 20 (uses circles, rendered as filled arcs)
            return ("M3 3m-3 0a3 3 0 1 0 6 0a3 3 0 1 0 -6 0M13 13m-3 0a3 3 0 1 0 6 0a3 3 0 1 0 -6 0", 20, 20)
        case .signal:
            // ViewBox: 0 0 84 48
            return ("M0 0h12v6H0V0zm28 8h12v6H28V8zm14-8h12v6H42V0zm14 0h12v6H56V0zm0 8h12v6H56V8zM42 8h12v6H42V8zm0 16h12v6H42v-6zm14-8h12v6H56v-6zm14 0h12v6H70v-6zm0-16h12v6H70V0zM28 32h12v6H28v-6zM14 16h12v6H14v-6zM0 24h12v6H0v-6zm0 8h12v6H0v-6zm14 0h12v6H14v-6zm14 8h12v6H28v-6zm-14 0h12v6H14v-6zm28 0h12v6H42v-6zm14-8h12v6H56v-6zm0-8h12v6H56v-6zm14 8h12v6H70v-6zm0 8h12v6H70v-6zM14 24h12v6H14v-6zm14-8h12v6H28v-6zM14 8h12v6H14V8zM0 8h12v6H0V8z", 84, 48)
        case .wallpaper:
            // ViewBox: 0 0 84 48 (uses plus signs and bars)
            return ("M78 7V4h-2v3h-3v2h3v3h2V9h3V7h-3zM30 7V4h-2v3h-3v2h3v3h2V9h3V7h-3zM10 0h2v16h-2V0zm6 0h4v16h-4V0zM2 0h4v16H2V0zm50 0h2v16h-2V0zM38 0h2v16h-2V0zm28 0h2v16h-2V0zm-8 0h6v16h-6V0zM42 0h6v16h-6V0z", 84, 16)
        }
    }

    /// Whether pattern uses stroke (lines) vs fill (shapes)
    var usesStroke: Bool {
        switch self {
        case .diagonalLines, .diagonalStripes:
            return false  // These use fill
        default:
            return false
        }
    }
}

// MARK: - Footer Pattern Layer

/// A view that draws a tiled pattern at the bottom of the window
/// positioned to overlay the echo area/minibuffer region
@available(macOS 26.0, *)
struct FooterPatternLayer: View {
    var pattern: FooterPattern
    var height: CGFloat
    var tintColor: NSColor
    var backgroundAlpha: CGFloat  // Alpha for background tint
    var patternAlpha: CGFloat     // Alpha for pattern foreground
    var isDarkMode: Bool

    var body: some View {
        if height > 0 {
            VStack(spacing: 0) {
                Spacer()
                ZStack {
                    // Background tint layer - darker for dark mode, lighter for light mode
                    let bgColor: Color = isDarkMode ? .black : .white
                    bgColor.opacity(Double(backgroundAlpha))

                    // Pattern layer on top (only if pattern is not none)
                    if pattern != .none {
                        PatternTileView(
                            pattern: pattern,
                            tintColor: tintColor,
                            patternAlpha: patternAlpha,
                            isDarkMode: isDarkMode
                        )
                        .frame(maxWidth: .infinity, maxHeight: .infinity)
                    }
                }
                .frame(maxWidth: .infinity)
                .frame(height: height)
            }
            .allowsHitTesting(false)  // Don't intercept mouse events
        } else {
        }
    }
}

/// NSView that renders tiled SVG patterns using Core Graphics
@available(macOS 26.0, *)
struct PatternTileView: NSViewRepresentable {
    var pattern: FooterPattern
    var tintColor: NSColor
    var patternAlpha: CGFloat
    var isDarkMode: Bool

    func makeNSView(context: Context) -> PatternTileNSView {
        let view = PatternTileNSView()
        view.pattern = pattern
        view.tintColor = tintColor
        view.patternAlpha = patternAlpha
        view.isDarkMode = isDarkMode
        return view
    }

    func updateNSView(_ nsView: PatternTileNSView, context: Context) {
        nsView.pattern = pattern
        nsView.tintColor = tintColor
        nsView.patternAlpha = patternAlpha
        nsView.isDarkMode = isDarkMode
        nsView.needsDisplay = true
    }
}

/// NSView subclass that renders the pattern tile
@available(macOS 26.0, *)
final class PatternTileNSView: NSView {
    var pattern: FooterPattern = .hideout
    var tintColor: NSColor = .windowBackgroundColor
    var patternAlpha: CGFloat = 0.15
    var isDarkMode: Bool = false

    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        wantsLayer = true
        layer?.backgroundColor = .clear
    }

    required init?(coder: NSCoder) {
        super.init(coder: coder)
        wantsLayer = true
        layer?.backgroundColor = .clear
    }

    override var isFlipped: Bool { true }  // Use standard top-left origin like SVG

    override func draw(_ dirtyRect: NSRect) {
        guard pattern != .none else { return }

        guard let context = NSGraphicsContext.current?.cgContext else { return }


        // Calculate pattern color:
        // For dark mode: use lighter color on dark background
        // For light mode: use darker color on light background
        let adjustedColor: NSColor
        if isDarkMode {
            adjustedColor = tintColor.blended(withFraction: 0.5, of: .white) ?? .white
        } else {
            adjustedColor = tintColor.blended(withFraction: 0.5, of: .black) ?? .black
        }

        let patternColor = adjustedColor.withAlphaComponent(patternAlpha)

        // Get pattern tile size from SVG data
        let svgData = pattern.svgData
        let tileWidth = svgData.width
        let tileHeight = svgData.height


        // Draw tiled pattern across the entire bounds
        context.saveGState()

        // Set up pattern color
        patternColor.setFill()
        patternColor.setStroke()

        // Tile across the entire view bounds (not just dirtyRect)
        let cols = Int(ceil(bounds.width / tileWidth)) + 1
        let rows = Int(ceil(bounds.height / tileHeight)) + 1


        for row in 0..<rows {
            for col in 0..<cols {
                let x = CGFloat(col) * tileWidth
                let y = CGFloat(row) * tileHeight

                context.saveGState()
                context.translateBy(x: x, y: y)

                // Draw the pattern
                drawPattern(in: context)

                context.restoreGState()
            }
        }

        context.restoreGState()
    }

    private func drawPattern(in context: CGContext) {
        // Parse and draw the SVG path
        let svgData = pattern.svgData
        let pathData = svgData.path
        guard !pathData.isEmpty else { return }

        if let path = parseSVGPath(pathData) {
            context.addPath(path)
            if pattern.usesStroke {
                context.setLineWidth(1.0)
                context.strokePath()
            } else {
                context.fillPath()
            }
        }
    }

    /// Simple SVG path parser for basic path commands (M, L, H, V, Z, A, C)
    private func parseSVGPath(_ data: String) -> CGPath? {
        let path = CGMutablePath()

        // Tokenize the path data
        let tokens = tokenizeSVGPath(data)
        var i = 0
        var currentX: CGFloat = 0
        var currentY: CGFloat = 0
        var startX: CGFloat = 0
        var startY: CGFloat = 0

        while i < tokens.count {
            let token = tokens[i]

            switch token {
            case "M", "m":  // MoveTo
                let relative = token == "m"
                i += 1
                while i < tokens.count && isNumber(tokens[i]) {
                    let x = CGFloat(Double(tokens[i]) ?? 0)
                    let y = CGFloat(Double(tokens[i+1]) ?? 0)
                    i += 2
                    if relative {
                        currentX += x
                        currentY += y
                    } else {
                        currentX = x
                        currentY = y
                    }
                    path.move(to: CGPoint(x: currentX, y: currentY))
                    startX = currentX
                    startY = currentY
                }

            case "L", "l":  // LineTo
                let relative = token == "l"
                i += 1
                while i < tokens.count && isNumber(tokens[i]) {
                    let x = CGFloat(Double(tokens[i]) ?? 0)
                    let y = CGFloat(Double(tokens[i+1]) ?? 0)
                    i += 2
                    if relative {
                        currentX += x
                        currentY += y
                    } else {
                        currentX = x
                        currentY = y
                    }
                    path.addLine(to: CGPoint(x: currentX, y: currentY))
                }

            case "H", "h":  // Horizontal LineTo
                let relative = token == "h"
                i += 1
                while i < tokens.count && isNumber(tokens[i]) {
                    let x = CGFloat(Double(tokens[i]) ?? 0)
                    i += 1
                    if relative {
                        currentX += x
                    } else {
                        currentX = x
                    }
                    path.addLine(to: CGPoint(x: currentX, y: currentY))
                }

            case "V", "v":  // Vertical LineTo
                let relative = token == "v"
                i += 1
                while i < tokens.count && isNumber(tokens[i]) {
                    let y = CGFloat(Double(tokens[i]) ?? 0)
                    i += 1
                    if relative {
                        currentY += y
                    } else {
                        currentY = y
                    }
                    path.addLine(to: CGPoint(x: currentX, y: currentY))
                }

            case "Z", "z":  // ClosePath
                path.closeSubpath()
                currentX = startX
                currentY = startY
                i += 1

            case "A", "a":  // Arc (simplified - draw as line for now)
                let relative = token == "a"
                i += 1
                while i + 6 < tokens.count && isNumber(tokens[i]) {
                    // Skip arc parameters: rx, ry, rotation, large-arc, sweep
                    i += 5
                    let x = CGFloat(Double(tokens[i]) ?? 0)
                    let y = CGFloat(Double(tokens[i+1]) ?? 0)
                    i += 2
                    if relative {
                        currentX += x
                        currentY += y
                    } else {
                        currentX = x
                        currentY = y
                    }
                    // Simplified: just draw line to end point
                    path.addLine(to: CGPoint(x: currentX, y: currentY))
                }

            case "C", "c":  // Cubic bezier
                let relative = token == "c"
                i += 1
                while i + 5 < tokens.count && isNumber(tokens[i]) {
                    var x1 = CGFloat(Double(tokens[i]) ?? 0)
                    var y1 = CGFloat(Double(tokens[i+1]) ?? 0)
                    var x2 = CGFloat(Double(tokens[i+2]) ?? 0)
                    var y2 = CGFloat(Double(tokens[i+3]) ?? 0)
                    var x = CGFloat(Double(tokens[i+4]) ?? 0)
                    var y = CGFloat(Double(tokens[i+5]) ?? 0)
                    i += 6
                    if relative {
                        x1 += currentX; y1 += currentY
                        x2 += currentX; y2 += currentY
                        x += currentX; y += currentY
                    }
                    path.addCurve(to: CGPoint(x: x, y: y),
                                  control1: CGPoint(x: x1, y: y1),
                                  control2: CGPoint(x: x2, y: y2))
                    currentX = x
                    currentY = y
                }

            case "S", "s":  // Smooth cubic bezier
                let relative = token == "s"
                i += 1
                while i + 3 < tokens.count && isNumber(tokens[i]) {
                    var x2 = CGFloat(Double(tokens[i]) ?? 0)
                    var y2 = CGFloat(Double(tokens[i+1]) ?? 0)
                    var x = CGFloat(Double(tokens[i+2]) ?? 0)
                    var y = CGFloat(Double(tokens[i+3]) ?? 0)
                    i += 4
                    if relative {
                        x2 += currentX; y2 += currentY
                        x += currentX; y += currentY
                    }
                    // Use current point as first control point
                    path.addCurve(to: CGPoint(x: x, y: y),
                                  control1: CGPoint(x: currentX, y: currentY),
                                  control2: CGPoint(x: x2, y: y2))
                    currentX = x
                    currentY = y
                }

            default:
                i += 1
            }
        }

        return path
    }

    private func tokenizeSVGPath(_ data: String) -> [String] {
        var tokens: [String] = []
        var current = ""

        for char in data {
            if char.isLetter {
                if !current.isEmpty {
                    tokens.append(current)
                    current = ""
                }
                tokens.append(String(char))
            } else if char == " " || char == "," {
                if !current.isEmpty {
                    tokens.append(current)
                    current = ""
                }
            } else if char == "-" {
                // Negative number - might be start of new number
                if !current.isEmpty {
                    tokens.append(current)
                }
                current = String(char)
            } else {
                current.append(char)
            }
        }
        if !current.isEmpty {
            tokens.append(current)
        }

        return tokens
    }

    private func isNumber(_ str: String) -> Bool {
        return Double(str) != nil
    }
}

// MARK: - Observable State for SwiftUI reactivity

@available(macOS 26.0, *)
@Observable
final class NavigationSidebarState {
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

    // Inspector Header
    var inspectorTitle: String = "AGENT SHELL"
    var inspectorIcon: String = "sparkles"
    var inspectorBusy: Bool = false

    /// Available toolbar width (calculated from window geometry)
    var toolbarWidth: CGFloat = 600
    /// Toolbar/titlebar height (calculated from window geometry)
    var toolbarHeight: CGFloat = 52
    /// Current sidebar width (when visible)
    var sidebarWidth: CGFloat = 280
    /// Current detail panel width (when visible)
    var detailWidth: CGFloat = 300
    /// Current content column width (center column with Emacs)
    var contentWidth: CGFloat = 400
    /// Debug: reference to Emacs view for diagnostics
    weak var debugEmacsView: NSView?

    // MARK: - Footer Pattern

    /// The footer pattern to display over the echo area
    var footerPattern: FooterPattern = .none
    /// Height of the echo area/minibuffer in pixels
    var footerHeight: CGFloat = 0
    /// Alpha for the footer background tint (0.0 to 1.0)
    /// Makes dark themes darker, light themes lighter
    var footerBackgroundAlpha: CGFloat = 0.3
    /// Alpha for the footer pattern foreground (0.0 to 1.0)
    var footerPatternAlpha: CGFloat = 0.15

    // MARK: - Embedded Child-Frame Views

    /// Embedded view for left sidebar top (ibuffer)
    var leftTopView: NSView?
    /// Original window for left-top (for event forwarding)
    weak var leftTopWindow: NSWindow?
    /// Embedded view for left sidebar bottom (dired-sidebar)
    var leftBottomView: NSView?
    /// Original window for left-bottom (for event forwarding)
    weak var leftBottomWindow: NSWindow?
    /// Embedded view for right sidebar (agent-shell)
    var rightView: NSView?
    /// Original window for right (for event forwarding)
    weak var rightWindow: NSWindow?

    /// Track last resize sizes to avoid duplicate notifications
    var debugLastResizeSize: [String: String] = [:]
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

    /// Whether the NavigationSplitView is installed
    private(set) var isSetup: Bool = false

    init(window: NSWindow) {
        self.window = window
        super.init()
    }

    /// Callbacks for visibility changes (visible, needsSetup)
    var onSidebarVisibilityChanged: ((Bool, Bool) -> Void)?
    var onDetailVisibilityChanged: ((Bool, Bool) -> Void)?

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
        // Update toolbar height from window geometry
        if let window = window {
            let toolbarHeight = window.frame.height - window.contentLayoutRect.height
            if toolbarHeight > 0 {
                state.toolbarHeight = toolbarHeight
            }
        }

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

        // Don't steal focus from mini-frames (child windows like minibuffer)
        // Check if any child EmacsWindow is currently the key window
        if let keyWindow = NSApp.keyWindow {
            let keyClassName = String(describing: type(of: keyWindow))
            if keyClassName.contains("EmacsWindow") && keyWindow.parent != nil {
                // A mini-frame has focus, don't steal it
                return
            }
        }

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

    func setInspectorHeader(title: String, icon: String, busy: Bool) {
        print("[NavigationSidebarController] setInspectorHeader. Title: \(title), Busy: \(busy)")
        state.inspectorTitle = title
        state.inspectorIcon = icon
        state.inspectorBusy = busy
    }

    // MARK: - Detail Panel

    /// Show the detail column (right panel)
    func showDetail() {
        guard isSetup else { return }
        // Disable animation to prevent window resize flicker
        var transaction = Transaction()
        transaction.disablesAnimations = true
        withTransaction(transaction) {
            state.detailVisible = true
        }
    }

    /// Hide the detail column
    func hideDetail() {
        guard isSetup else { return }
        // Disable animation to prevent window resize flicker
        var transaction = Transaction()
        transaction.disablesAnimations = true
        withTransaction(transaction) {
            state.detailVisible = false
        }
    }

    /// Toggle the detail column visibility
    func toggleDetail() {
        guard isSetup else { return }
        // Disable animation to prevent window resize flicker
        var transaction = Transaction()
        transaction.disablesAnimations = true
        withTransaction(transaction) {
            state.detailVisible.toggle()
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
    /// IMPORTANT: Uses transaction with disabled animations to prevent
    /// SwiftUI layout recalculation that could cause window resize.
    func setBackgroundColor(_ colorString: String, alpha: CGFloat) {
        let newColor = parseEmacsColor(colorString) ?? .windowBackgroundColor

        // Wrap in transaction to prevent layout recalculation during theme change
        var transaction = Transaction()
        transaction.disablesAnimations = true
        withTransaction(transaction) {
            state.backgroundColor = newColor
            state.backgroundAlpha = alpha
        }
    }

    /// Set the window appearance mode
    /// NOTE: Does NOT sync to AppearanceSettings.shared to avoid overwriting
    /// user's panel adjustments. Panel syncs from controller when opened.
    /// IMPORTANT: Uses transaction with disabled animations to prevent
    /// SwiftUI layout recalculation that could cause window resize.
    func setWindowAppearance(_ appearance: String) {
        guard let window = window else { return }

        // Wrap state update in transaction to prevent layout recalculation
        var transaction = Transaction()
        transaction.disablesAnimations = true
        withTransaction(transaction) {
            state.windowAppearance = appearance
        }

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
    /// IMPORTANT: Uses transaction with disabled animations to prevent
    /// SwiftUI layout recalculation that could cause window resize.
    func setVibrancyMaterial(_ materialName: String) {
        if let material = VibrancyMaterial(rawValue: materialName) {
            // Wrap in transaction to prevent layout recalculation
            var transaction = Transaction()
            transaction.disablesAnimations = true
            withTransaction(transaction) {
                state.vibrancyMaterial = material
            }
        }
    }

    // MARK: - Footer Pattern

    /// Set the footer pattern type
    /// PATTERN is one of the FooterPattern enum values (e.g., "hideout", "hexagons")
    func setFooterPattern(_ patternName: String) {
        if let pattern = FooterPattern(rawValue: patternName) {
            var transaction = Transaction()
            transaction.disablesAnimations = true
            withTransaction(transaction) {
                state.footerPattern = pattern
            }
        } else {
        }
    }

    /// Set the footer height (echo area/minibuffer height in pixels)
    func setFooterHeight(_ height: CGFloat) {
        var transaction = Transaction()
        transaction.disablesAnimations = true
        withTransaction(transaction) {
            state.footerHeight = height
        }
    }

    /// Set the footer background alpha (0.0 to 1.0) - tint layer
    func setFooterBackgroundAlpha(_ alpha: CGFloat) {
        var transaction = Transaction()
        transaction.disablesAnimations = true
        withTransaction(transaction) {
            state.footerBackgroundAlpha = alpha
        }
    }

    /// Set the footer pattern alpha (0.0 to 1.0) - pattern foreground
    func setFooterPatternAlpha(_ alpha: CGFloat) {
        var transaction = Transaction()
        transaction.disablesAnimations = true
        withTransaction(transaction) {
            state.footerPatternAlpha = alpha
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
            onGeometryUpdate: { [weak self] in self?.updateModeLineGeometry() },
            onEmbeddedResize: { [weak self] slot, width, height in
                self?.handleEmbeddedResize(slot: slot, width: width, height: height)
            },
            onSidebarVisibilityChanged: { visible, needsSetup in
                NavigationSidebarManager.shared.notifySidebarVisibilityChanged(visible: visible, needsSetup: needsSetup)
            },
            onDetailVisibilityChanged: { visible, needsSetup in
                NavigationSidebarManager.shared.notifyDetailVisibilityChanged(visible: visible, needsSetup: needsSetup)
            }
        )
    }


    /// Handle resize of embedded child-frame views
    /// Calls Elisp hyalo-sidebar-resize-slot to update frame size
    private func handleEmbeddedResize(slot: String, width: CGFloat, height: CGFloat) {
        // Store last known sizes to avoid duplicate calls
        let key = "resize-\(slot)"
        let newSize = "\(Int(width))x\(Int(height))"
        if state.debugLastResizeSize[key] == newSize { return }
        state.debugLastResizeSize[key] = newSize

        // Post notification for Elisp to handle
        NotificationCenter.default.post(
            name: NSNotification.Name("HyaloEmbeddedResize"),
            object: nil,
            userInfo: ["slot": slot, "width": width, "height": height]
        )
    }
}

// MARK: - Manager

@available(macOS 26.0, *)
final class NavigationSidebarManager {
    static let shared = NavigationSidebarManager()

    private var controllers: [Int: NavigationSidebarController] = [:]

    /// Callback when sidebar visibility changes (called with "left" or nil)
    var onSidebarVisibilityChanged: ((String, Bool) -> Void)?

    /// Callback when detail visibility changes (called with "right" or nil)
    var onDetailVisibilityChanged: ((String, Bool) -> Void)?

    /// Notify about sidebar visibility change
    func notifySidebarVisibilityChanged(visible: Bool, needsSetup: Bool) {
        print("[Hyalo] NavigationSidebarManager: notifySidebarVisibilityChanged(visible: \(visible))")
        // Always notify Lisp so it can sync its internal state (e.g. dired buffer visibility)
        if let callback = onSidebarVisibilityChanged {
            print("[Hyalo] NavigationSidebarManager: invoking callback")
            callback("left", visible)
        } else {
            print("[Hyalo] NavigationSidebarManager: onSidebarVisibilityChanged is nil")
        }
    }

    /// Notify about detail visibility change
    func notifyDetailVisibilityChanged(visible: Bool, needsSetup: Bool) {
        print("[Hyalo] NavigationSidebarManager: notifyDetailVisibilityChanged(visible: \(visible))")
        // Always notify Lisp
        onDetailVisibilityChanged?("right", visible)
    }



    private init() {}


    func getController(for window: NSWindow) -> NavigationSidebarController {
        let key = window.windowNumber
        if let existing = controllers[key] {
            return existing
        }
        let controller = NavigationSidebarController(window: window)
        
        // Connect callbacks
        controller.onSidebarVisibilityChanged = { [weak self] visible, needsSetup in
            self?.notifySidebarVisibilityChanged(visible: visible, needsSetup: needsSetup)
        }
        controller.onDetailVisibilityChanged = { [weak self] visible, needsSetup in
            self?.notifyDetailVisibilityChanged(visible: visible, needsSetup: needsSetup)
        }
        
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

    func setInspectorHeader(for window: NSWindow, title: String, icon: String, busy: Bool) {
        controllers[window.windowNumber]?.setInspectorHeader(title: title, icon: icon, busy: busy)
    }

    func areDecorationsVisible(for window: NSWindow) -> Bool {
        controllers[window.windowNumber]?.areDecorationsVisible ?? true
    }

    // MARK: - Footer Pattern

    func setFooterPattern(for window: NSWindow, pattern: String) {
        controllers[window.windowNumber]?.setFooterPattern(pattern)
    }

    func setFooterHeight(for window: NSWindow, height: CGFloat) {
        controllers[window.windowNumber]?.setFooterHeight(height)
    }

    func setFooterBackgroundAlpha(for window: NSWindow, alpha: CGFloat) {
        controllers[window.windowNumber]?.setFooterBackgroundAlpha(alpha)
    }

    func setFooterPatternAlpha(for window: NSWindow, alpha: CGFloat) {
        controllers[window.windowNumber]?.setFooterPatternAlpha(alpha)
    }

    // MARK: - Embedded View Management

    func setEmbeddedView(view: NSView, slot: String, for window: NSWindow, originalWindow: NSWindow? = nil) {
        guard let controller = controllers[window.windowNumber] else { return }

        switch slot {
        case "left-top":
            controller.state.leftTopView = view
            controller.state.leftTopWindow = originalWindow
        case "left-bottom":
            controller.state.leftBottomView = view
            controller.state.leftBottomWindow = originalWindow
        case "right":
            controller.state.rightView = view
            controller.state.rightWindow = originalWindow
        default:
            break
        }
    }

    func clearEmbeddedView(slot: String, for window: NSWindow) {
        guard let controller = controllers[window.windowNumber] else { return }
        switch slot {
        case "left-top":
            controller.state.leftTopView = nil
            controller.state.leftTopWindow = nil
        case "left-bottom":
            controller.state.leftBottomView = nil
            controller.state.leftBottomWindow = nil
        case "right":
            controller.state.rightView = nil
            controller.state.rightWindow = nil
        default:
            break
        }
    }
}
