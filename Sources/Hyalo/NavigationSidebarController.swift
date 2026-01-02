// NavigationSidebarController.swift - Main controller for NavigationSidebar
// Copyright (C) 2025
// Target: macOS 26 Tahoe with Liquid Glass design

import AppKit
import SwiftUI

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

    /// Callback for mode-line clicks. Parameters: segment ("lhs" or "rhs"), relative position (0.0-1.0)
    var onModeLineClick: ((String, Double) -> Void)?

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
        window.titleVisibility = .hidden

        // Configure toolbar style - .unified merges toolbar with titlebar for glass effect
        window.toolbarStyle = .unified

        // CRITICAL: Clear and maintain empty window title to prevent Emacs geometry display
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
        setupModeLineOverlay(for: window)

        // Show native traffic lights immediately and after a short delay
        showTrafficLights(window)
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.5) { [weak self, weak window] in
            guard let window = window else { return }
            self?.showTrafficLights(window)
        }

        // Setup appearance change observer
        setupAppearanceObserver()

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
    private func setupModeLineOverlay(for window: NSWindow) {
        let overlay = ModeLineGlassOverlayController()
        overlay.setup(for: window)

        // Wire animation callback for smooth updates during sidebar/inspector expansion
        overlay.onAnimationUpdate = { [weak self] in
            self?.updateModeLineGeometry()
        }

        // Wire mode-line click callback
        overlay.onModeLineClick = { [weak self] segment, position in
            self?.onModeLineClick?(segment, position)
        }

        // Wire direct segment click (includes view for popover positioning)
        overlay.onSegmentClick = { [weak self] segment, view in
            guard let self = self else { return }

            if let menuItems = segment.menuItems, !menuItems.isEmpty {
                self.showSegmentMenuDirectly(items: menuItems, for: segment, from: view)
            } else if let command = segment.command, !command.isEmpty {
                self.executeEmacsCommand(command)
            }
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
        let sidebarToggleButtonWidth: CGFloat = 47
        let trafficLightsWidth: CGFloat = 69
        let leadingOffset: CGFloat = state.sidebarVisible
            ? state.sidebarWidth
            : (trafficLightsWidth + sidebarToggleButtonWidth)

        // Calculate trailing offset (right edge of modeline)
        let inspectorToggleButtonWidth: CGFloat = 47
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
        if let keyWindow = NSApp.keyWindow {
            let keyClassName = String(describing: type(of: keyWindow))
            if keyClassName.contains("EmacsWindow") && keyWindow.parent != nil {
                return
            }
        }

        if !window.isKeyWindow {
            window.makeKeyAndOrderFront(nil)
        }

        window.makeFirstResponder(emacs)
    }

    /// Find the Emacs NSView in the view hierarchy
    private func findEmacsViewInHierarchy(_ view: NSView?) -> NSView? {
        guard let view = view else { return nil }

        let className = String(describing: type(of: view))

        if className.contains("EmacsView") {
            return view
        }

        if let container = view as? EmacsContainerView, let emacs = container.emacsView {
            return emacs
        }

        if view.acceptsFirstResponder && !className.hasPrefix("NS") && !className.contains("Hosting") && !className.contains("Container") {
            return view
        }

        for subview in view.subviews {
            if let found = findEmacsViewInHierarchy(subview) {
                return found
            }
        }

        return nil
    }

    /// Setup observer for window becoming key to restore Emacs focus
    private func setupWindowFocusObserver(emacsView: NSView) {
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

        appDidBecomeActiveObserver = NotificationCenter.default.addObserver(
            forName: NSApplication.didBecomeActiveNotification,
            object: nil,
            queue: .main
        ) { [weak self] _ in
            guard let self = self else { return }
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

        if state.windowAppearance == "auto" {
            window.appearance = nil
        }

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

        if let observer = appearanceObserver {
            DistributedNotificationCenter.default.removeObserver(observer)
            appearanceObserver = nil
        }

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

        titleObservation?.invalidate()
        titleObservation = nil

        blurView?.removeFromSuperview()
        blurView = nil

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
        var transaction = Transaction()
        transaction.disablesAnimations = true
        withTransaction(transaction) {
            state.detailVisible = true
        }
    }

    /// Hide the detail column
    func hideDetail() {
        guard isSetup else { return }
        var transaction = Transaction()
        transaction.disablesAnimations = true
        withTransaction(transaction) {
            state.detailVisible = false
        }
    }

    /// Toggle the detail column visibility
    func toggleDetail() {
        guard isSetup else { return }
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
        updateModeLineGeometry()
    }

    /// Set the background color from Emacs (color string + alpha)
    func setBackgroundColor(_ colorString: String, alpha: CGFloat) {
        let newColor = parseEmacsColor(colorString) ?? .windowBackgroundColor

        var transaction = Transaction()
        transaction.disablesAnimations = true
        withTransaction(transaction) {
            state.backgroundColor = newColor
            state.backgroundAlpha = alpha
        }
    }

    /// Set the window appearance mode
    func setWindowAppearance(_ appearance: String) {
        guard let window = window else { return }

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
            window.appearance = nil
        }
    }

    /// Set the vibrancy material style
    func setVibrancyMaterial(_ materialName: String) {
        if let material = VibrancyMaterial(rawValue: materialName) {
            var transaction = Transaction()
            transaction.disablesAnimations = true
            withTransaction(transaction) {
                state.vibrancyMaterial = material
            }
        }
    }

    // MARK: - Footer Pattern

    /// Set the footer pattern type
    func setFooterPattern(_ patternName: String) {
        if let pattern = FooterPattern(rawValue: patternName) {
            var transaction = Transaction()
            transaction.disablesAnimations = true
            withTransaction(transaction) {
                state.footerPattern = pattern
            }
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
    private func handleEmbeddedResize(slot: String, width: CGFloat, height: CGFloat) {
        let key = "resize-\(slot)"
        let newSize = "\(Int(width))x\(Int(height))"
        if state.debugLastResizeSize[key] == newSize { return }
        state.debugLastResizeSize[key] = newSize

        NotificationCenter.default.post(
            name: NSNotification.Name("HyaloEmbeddedResize"),
            object: nil,
            userInfo: ["slot": slot, "width": width, "height": height]
        )
    }

    // MARK: - Mode-line Segments

    /// Stored segments from Elisp for click handling
    private var modeLineSegments: [ModeLineSegment] = []

    /// Update mode-line segments from JSON
    func updateModeLineSegments(_ segmentsJson: String) {
        guard let data = segmentsJson.data(using: .utf8) else { return }
        do {
            modeLineSegments = try JSONDecoder().decode([ModeLineSegment].self, from: data)
            modeLineGlassOverlay?.updateSegments(modeLineSegments)
        } catch {
            print("[Hyalo] updateModeLineSegments: decode error: \(error)")
        }
    }

    /// Find segment at relative position within side ("lhs" or "rhs")
    func findSegment(at relativePosition: Double, side: String) -> ModeLineSegment? {
        return modeLineSegments.first { segment in
            segment.side == side &&
            relativePosition >= segment.relStart &&
            relativePosition < segment.relEnd
        }
    }

    /// Show a popup menu from JSON and return the selected command
    func showSegmentMenu(menuJson: String, at screenPoint: NSPoint) -> String? {
        guard let data = menuJson.data(using: .utf8) else { return nil }
        guard let menuData = try? JSONDecoder().decode(ModeLineMenu.self, from: data) else {
            print("[Hyalo] NavigationSidebarController: failed to decode menu")
            return nil
        }

        let menu = NSMenu(title: menuData.title ?? "")
        menu.autoenablesItems = false

        for item in menuData.items {
            if item.separator == true {
                menu.addItem(NSMenuItem.separator())
            } else {
                let menuItem = NSMenuItem(title: item.title, action: nil, keyEquivalent: "")
                menuItem.representedObject = item.command
                menuItem.isEnabled = item.enabled ?? true
                menuItem.state = (item.checked == true) ? .on : .off
                menu.addItem(menuItem)
            }
        }

        var selectedCommand: String? = nil

        for menuItem in menu.items {
            menuItem.target = self
            menuItem.action = #selector(menuItemSelected(_:))
        }

        pendingMenuSelection = nil

        if let window = window {
            let windowPoint = window.convertPoint(fromScreen: screenPoint)
            if let contentView = window.contentView {
                let viewPoint = contentView.convert(windowPoint, from: nil)
                menu.popUp(positioning: nil, at: viewPoint, in: contentView)
            }
        }

        selectedCommand = pendingMenuSelection
        pendingMenuSelection = nil

        return selectedCommand
    }

    private var pendingMenuSelection: String? = nil

    @objc private func menuItemSelected(_ sender: NSMenuItem) {
        pendingMenuSelection = sender.representedObject as? String
        if let command = pendingMenuSelection {
            executeEmacsCommand(command)
        }
    }

    /// Liquid Glass popover for menus
    private var glassMenuPopover: GlassMenuPopover?

    /// The clicked segment view for popover positioning
    private weak var clickedSegmentView: NSView?

    /// Show segment menu with Liquid Glass popover
    private func showSegmentMenuDirectly(items: [ModeLineMenuItem], for segment: ModeLineSegment, from view: NSView?) {
        glassMenuPopover?.close()

        let popover = GlassMenuPopover()
        glassMenuPopover = popover

        guard let anchorView = view ?? modeLineGlassOverlay?.modeLineView else { return }

        popover.showMenu(
            items: items,
            title: segment.text.trimmingCharacters(in: .whitespaces),
            relativeTo: anchorView,
            onSelect: { [weak self] command in
                self?.executeEmacsCommand(command)
            }
        )
    }

    /// Execute an Emacs command by name
    private func executeEmacsCommand(_ command: String) {
        NavigationSidebarManager.shared.executeCommand(command)
    }
}
