// NavigationSidebarManager.swift - Singleton manager for NavigationSidebar controllers
// Copyright (C) 2025
// Target: macOS 26 Tahoe with Liquid Glass design

import AppKit

// MARK: - Manager

@available(macOS 26.0, *)
final class NavigationSidebarManager {
    static let shared = NavigationSidebarManager()

    private var controllers: [Int: NavigationSidebarController] = [:]

    /// Callback when sidebar visibility changes (called with "left" or nil)
    var onSidebarVisibilityChanged: ((String, Bool) -> Void)?

    /// Callback when detail visibility changes (called with "right" or nil)
    var onDetailVisibilityChanged: ((String, Bool) -> Void)?

    /// Callback for mode-line clicks. Parameters: segment ("lhs" or "rhs"), relative position (0.0-1.0)
    var onModeLineClick: ((String, Double) -> Void)?

    /// Callback for executing Emacs commands by name
    var onExecuteCommand: ((String) -> Void)?

    /// Execute a command - calls the onExecuteCommand callback
    func executeCommand(_ command: String) {
        onExecuteCommand?(command)
    }

    /// Execute command asynchronously - escapes the Button action context
    func executeCommandAsync(_ command: String) {
        DispatchQueue.main.async { [weak self] in
            self?.onExecuteCommand?(command)
        }
    }

    // Track last visibility state to prevent duplicate callbacks
    private var lastSidebarVisible: Bool? = nil
    private var lastDetailVisible: Bool? = nil

    /// Notify about sidebar visibility change (deduplicated)
    func notifySidebarVisibilityChanged(visible: Bool, needsSetup: Bool) {
        guard lastSidebarVisible != visible else { return }
        lastSidebarVisible = visible
        onSidebarVisibilityChanged?("left", visible)
    }

    /// Notify about detail visibility change (deduplicated)
    func notifyDetailVisibilityChanged(visible: Bool, needsSetup: Bool) {
        guard lastDetailVisible != visible else { return }
        lastDetailVisible = visible
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
        controller.onModeLineClick = { [weak self] segment, position in
            self?.onModeLineClick?(segment, position)
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

    /// Get the current content area width in pixels
    func getContentWidth(for window: NSWindow) -> CGFloat {
        controllers[window.windowNumber]?.contentWidth ?? 0
    }

    /// Get the current sidebar width in pixels
    func getSidebarWidth(for window: NSWindow) -> CGFloat {
        controllers[window.windowNumber]?.sidebarWidth ?? 0
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

    // MARK: - Mode-line Segments

    /// Update mode-line with structured segment data
    func updateModeLineSegments(for window: NSWindow, segmentsJson: String) {
        guard let controller = controllers[window.windowNumber] else { return }
        controller.updateModeLineSegments(segmentsJson)
    }

    /// Show a popup menu for a mode-line segment
    /// Returns the selected command string, or nil if cancelled
    func showSegmentMenu(for window: NSWindow, menuJson: String, at point: NSPoint) -> String? {
        guard let controller = controllers[window.windowNumber] else { return nil }
        return controller.showSegmentMenu(menuJson: menuJson, at: point)
    }
}
