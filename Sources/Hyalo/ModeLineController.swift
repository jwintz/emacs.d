// ModeLineController.swift - Mode-line overlay and titlebar accessory controllers
// Copyright (C) 2025
// Target: macOS 26 Tahoe with Liquid Glass design

import AppKit
import SwiftUI

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

// MARK: - Glass Overlay Controller

/// Controller for modeline overlay with NSGlassEffectView positioned in toolbar area
/// Uses NSGlassEffectView.cornerRadius for proper Liquid Glass appearance
/// Frame is calculated relative to window's titlebar/toolbar area
@available(macOS 26.0, *)
final class ModeLineGlassOverlayController {
    var modeLineView: ModeLineToolbarItemView?  // Internal for popover positioning
    private var glassView: NSGlassEffectView?
    private var modeLineContent: String = ""
    private var frameObserver: NSObjectProtocol?
    private var animationTimer: Timer?
    private var animationEndTime: Date?
    private var eventMonitor: Any?
    weak var window: NSWindow?

    /// Metrics for toolbar glass bar (matching SwiftUI defaults)
    /// Height matches toolbar button height, radius is half for pill/capsule shape
    private let glassHeight: CGFloat = 34     // Match toolbar button height
    private let glassCornerRadius: CGFloat = 17  // Half of height for pill shape
    private let verticalCenterOffset: CGFloat = 0  // Center in toolbar area
    private let leadingPadding: CGFloat = 12   // Padding from left edge
    private let trailingPadding: CGFloat = 12  // Padding from right edge

    /// Stored geometry for calculations (updated on each call)
    private var sidebarToggleWidth: CGFloat = 116  // traffic lights (69) + toggle (47)
    private var inspectorToggleWidth: CGFloat = 47 // toggle button width
    private var inspectorVisible: Bool = false

    /// Animation update callback (called by timer during animations)
    var onAnimationUpdate: (() -> Void)?

    /// Callback for mode-line clicks. Parameters: segment ("lhs" or "rhs"), relative position (0.0-1.0)
    var onModeLineClick: ((String, Double) -> Void)?

    /// Callback for specific segment click (includes view for positioning)
    var onSegmentClick: ((ModeLineSegment, NSView) -> Void)?

    /// Update segments
    func updateSegments(_ segments: [ModeLineSegment]) {
        modeLineView?.updateWithSegments(segments)
    }

    private var parentFrameObserver: NSObjectProtocol?
    private var windowMoveObserver: NSObjectProtocol?

    /// Setup the modeline overlay in the window
    func setup(for window: NSWindow) {
        self.window = window

        // Create modeline content view
        let modeView = ModeLineToolbarItemView(frame: .zero)
        modeView.onModeLineClick = { [weak self] segment, position in
            self?.onModeLineClick?(segment, position)
        }
        modeView.onSegmentClick = { [weak self] segment, view in
            self?.onSegmentClick?(segment, view)
        }

        // Create glass effect view
        let glass = NSGlassEffectView()
        glass.cornerRadius = glassCornerRadius
        glass.wantsLayer = true
        glass.autoresizingMask = []

        // Store references
        self.glassView = glass
        self.modeLineView = modeView

        // Ensure layer backing for proper z-ordering
        modeView.wantsLayer = true
        glass.wantsLayer = true

        // Add to window's theme frame (contentView.superview)
        if let themeFrame = window.contentView?.superview {
            // Add glass first (background)
            themeFrame.addSubview(glass)

            // Add content view ON TOP of glass (sibling)
            // This ensures it receives clicks and is not blocked by glass effect internal views
            themeFrame.addSubview(modeView)

            // CRITICAL: Set explicit z-ordering to ensure modeLineView is ALWAYS on top
            // The glass effect is visual only; the content view must receive all events
            glass.layer?.zPosition = 0
            modeView.layer?.zPosition = 1000

            // IMPORTANT: Bring modeView to front of subviews array for correct hit testing
            // (z-position only affects rendering, not hit test order!)
            modeView.removeFromSuperview()
            themeFrame.addSubview(modeView, positioned: .above, relativeTo: nil)

            // Ensure window accepts mouse moved events for tooltips/hover
            window.acceptsMouseMovedEvents = true

            // Add local event monitor to intercept mouse events in toolbar area
            self.setupEventMonitor()
        }

        // Observe window frame changes to update geometry
        frameObserver = NotificationCenter.default.addObserver(
            forName: NSWindow.didResizeNotification,
            object: window,
            queue: .main
        ) { [weak self] _ in
            self?.updateLayout()
        }

        // Also observe window move to fix coordinate computation after repositioning
        windowMoveObserver = NotificationCenter.default.addObserver(
            forName: NSWindow.didMoveNotification,
            object: window,
            queue: .main
        ) { [weak self] _ in
            // Update layout after window move to ensure hit testing works correctly
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

    /// Find the toolbar view in the window hierarchy
    /// Deprecated: We now attach directly to theme frame
    private func findToolbarView(in window: NSWindow) -> NSView? {
        return window.contentView?.superview
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

        // Set frame for glass (background)
        glass.frame = NSRect(x: x, y: y, width: width, height: glassHeight)

        // Set frame for content (on top) - matches glass frame exactly
        modeLineView?.frame = glass.frame

        // Ensure z-ordering is maintained (macOS may reorder subviews)
        if let mode = modeLineView {
            mode.layer?.zPosition = 1000
            glass.layer?.zPosition = 0

            // Re-add to front of subviews array if not already last
            // (hit testing uses subviews order, not z-position)
            if let parent = mode.superview, parent.subviews.last !== mode {
                mode.removeFromSuperview()
                parent.addSubview(mode, positioned: .above, relativeTo: nil)
            }
        }

        // Force layout pass to ensure subviews (stack items) get their frames assigned
        // Otherwise they might have intrinsic size (sizing the stack) but 0 frame
        modeLineView?.layoutSubtreeIfNeeded()
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

    // MARK: - Event Monitoring

    private func setupEventMonitor() {
        // Monitor mouse events to intercept clicks in our view's frame
        // This bypasses the titlebar's event interception
        eventMonitor = NSEvent.addLocalMonitorForEvents(matching: [.leftMouseDown, .leftMouseUp]) { [weak self] event in
            guard let self = self,
                  let modeView = self.modeLineView,
                  let window = self.window,
                  event.window === window else {
                return event
            }

            // Convert to modeLineView coordinates
            let windowLocation = event.locationInWindow
            let viewLocation = modeView.convert(windowLocation, from: nil)

            // Check if click is within our view's bounds
            if modeView.bounds.contains(viewLocation) && event.type == .leftMouseDown {
                // Find the segment view under the click
                for case let segmentView as ModeLineToolbarItemView.SegmentView in modeView.lhsStack.arrangedSubviews + modeView.rhsStack.arrangedSubviews {
                    let segmentLocation = segmentView.convert(windowLocation, from: nil)
                    if segmentView.bounds.contains(segmentLocation) {
                        let segment = segmentView.segment
                        self.onSegmentClick?(segment, segmentView)
                        return nil // Consume the event
                    }
                }
            }

            return event
        }
    }

    private func removeEventMonitor() {
        if let monitor = eventMonitor {
            NSEvent.removeMonitor(monitor)
            eventMonitor = nil
        }
    }

    deinit {
        animationTimer?.invalidate()
        removeEventMonitor()
        if let observer = frameObserver {
            NotificationCenter.default.removeObserver(observer)
        }
        if let observer = windowMoveObserver {
            NotificationCenter.default.removeObserver(observer)
        }
        if let observer = parentFrameObserver {
            NotificationCenter.default.removeObserver(observer)
        }
    }
}
