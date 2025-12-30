// Hyalo - Emacs Dynamic Module for macOS Liquid Glass Effects
// Copyright (C) 2024

import AppKit
import EmacsSwiftModule

/// Helper to find the MAIN Emacs window (not child-frames)
/// Child-frames are positioned off-screen at x < -5000
func findEmacsWindow() -> NSWindow? {
    // Helper to check if window is on-screen (not a child-frame)
    func isOnScreen(_ window: NSWindow) -> Bool {
        return window.frame.origin.x > -5000
    }

    // Check mainWindow first, but only if it's on-screen
    if let window = NSApp.mainWindow, isOnScreen(window) { return window }

    // Check keyWindow, but only if it's on-screen
    if let window = NSApp.keyWindow, isOnScreen(window) { return window }

    // Find any visible, on-screen window
    for window in NSApp.windows where window.isVisible && !window.isMiniaturized && isOnScreen(window) {
        let className = String(describing: type(of: window))
        if className.contains("EmacsWindow") {
            return window
        }
    }

    // Last resort: any on-screen EmacsWindow
    for window in NSApp.windows where isOnScreen(window) {
        let className = String(describing: type(of: window))
        if className.contains("EmacsWindow") {
            return window
        }
    }

    return nil
}


/// Hyalo module provides:
/// - Vibrancy (NSVisualEffectView)
/// - Window styling (transparent titlebar, corners)
/// - Traffic light hover behavior
/// - System appearance detection
///
/// Note: alpha-background is handled by patch.
final class HyaloModule: Module {
    let isGPLCompatible = true
    private let version = "1.0.0"

    func Init(_ env: Environment) throws {

        // MARK: - Info

        try env.defun("hyalo-version") { [version] in version }

        try env.defun("hyalo-corner-radius") {
            Double(HyaloManager.cornerRadius)
        }

        // MARK: - Setup/Teardown

        try env.defun(
            "hyalo-setup",
            with: "Setup Hyalo effects on current frame."
        ) { (env: Environment) throws -> Bool in
            DispatchQueue.main.async {
                guard let window = findEmacsWindow() else { return }
                HyaloManager.shared.setupHyalo(for: window)
            }
            return true
        }

        try env.defun(
            "hyalo-teardown",
            with: "Remove Hyalo effects from current frame."
        ) { (env: Environment) throws -> Bool in
            DispatchQueue.main.async {
                guard let window = findEmacsWindow() else { return }
                HyaloManager.shared.removeHyalo(from: window)
            }
            return true
        }

        // MARK: - Traffic Lights

        try env.defun(
            "hyalo-set-traffic-lights-auto-hide",
            with: "Enable/disable traffic light auto-hide with hover."
        ) { (env: Environment, enabled: Bool) throws -> Bool in
            DispatchQueue.main.async {
                guard let window = findEmacsWindow() else { return }
                HyaloManager.shared.setTrafficLightsAutoHide(enabled, for: window)
            }
            return true
        }

        try env.defun(
            "hyalo-show-traffic-lights",
            with: "Show the traffic light buttons."
        ) { (env: Environment) throws -> Bool in
            DispatchQueue.main.async {
                guard let window = findEmacsWindow() else { return }
                HyaloManager.shared.showTrafficLights(for: window)
            }
            return true
        }

        try env.defun(
            "hyalo-hide-traffic-lights",
            with: "Hide the traffic light buttons."
        ) { (env: Environment) throws -> Bool in
            DispatchQueue.main.async {
                guard let window = findEmacsWindow() else { return }
                HyaloManager.shared.hideTrafficLights(for: window)
            }
            return true
        }

        // MARK: - Appearance

        try env.defun(
            "hyalo-get-system-appearance",
            with: "Get system appearance: \"light\" or \"dark\"."
        ) { () -> String in
            HyaloManager.shared.getSystemAppearance()
        }

        try env.defun(
            "hyalo-set-window-appearance",
            with: "Set window appearance: \"light\", \"dark\", or \"auto\"."
        ) { (env: Environment, appearance: String) throws -> Bool in
            DispatchQueue.main.async {
                guard let window = findEmacsWindow() else { return }
                HyaloManager.shared.setWindowAppearance(appearance, for: window)
            }
            return true
        }

        // MARK: - Header View

        try env.defun(
            "hyalo-update-header",
            with: "Update header view with formatted mode-line string."
        ) { (env: Environment, modeLineString: String) throws -> Bool in
            DispatchQueue.main.async {
                guard let window = findEmacsWindow() else { return }
                HyaloManager.shared.updateHeader(for: window, modeLineString: modeLineString)
            }
            return true
        }

        try env.defun(
            "hyalo-update-header-line",
            with: "Update header-line content. CONTENT is the header-line string."
        ) { (env: Environment, content: String) throws -> Bool in
            DispatchQueue.main.async {
                guard let window = findEmacsWindow() else { return }
                HyaloManager.shared.updateHeaderLine(for: window, content: content)
            }
            return true
        }

        try env.defun(
            "hyalo-set-header-position",
            with: """
            Set header view position.
            TOP: top padding in pixels
            LEFT: left padding in pixels
            RIGHT: right padding in pixels
            """
        ) { (env: Environment, top: Int, left: Int, right: Int) throws -> Bool in
            DispatchQueue.main.async {
                guard let window = findEmacsWindow() else { return }
                HyaloManager.shared.setHeaderPosition(
                    for: window,
                    top: CGFloat(top),
                    left: CGFloat(left),
                    right: CGFloat(right)
                )
            }
            return true
        }

        try env.defun(
            "hyalo-header-height",
            with: "Get the header view height in pixels."
        ) { () -> Int in
            if #available(macOS 26.0, *) {
                return Int(HeaderHostingController.headerHeight)
            }
            return 44
        }

        // MARK: - Native Navigation Sidebar
        
        try env.defun(
            "hyalo-navigation-setup",
            with: """
            Setup the NavigationSplitView with toolbar.
            This replaces the legacy HeaderView approach.
            The sidebar starts collapsed, toolbar is immediately visible.
            """
        ) { (env: Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    guard let window = findEmacsWindow() else { return }
                    NavigationSidebarManager.shared.setup(for: window)
                }
                return true
            }
            return false
        }
        
        try env.defun(
            "hyalo-navigation-teardown",
            with: """
            Teardown the NavigationSplitView.
            """
        ) { (env: Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    guard let window = findEmacsWindow() else { return }
                    NavigationSidebarManager.shared.teardown(for: window)
                }
                return true
            }
            return false
        }

        try env.defun(
            "hyalo-sidebar-show",
            with: """
            Show the native SwiftUI navigation sidebar.
            This embeds Emacs content in a NavigationSplitView with a glass sidebar.
            """
        ) { (env: Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    guard let window = findEmacsWindow() else { return }
                    NavigationSidebarManager.shared.showSidebar(for: window)
                }
                return true
            }
            return false
        }

        try env.defun(
            "hyalo-sidebar-hide",
            with: """
            Hide the native SwiftUI navigation sidebar.
            Restores Emacs to its original layout.
            """
        ) { (env: Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    guard let window = findEmacsWindow() else { return }
                    NavigationSidebarManager.shared.hideSidebar(for: window)
                }
                return true
            }
            return false
        }

        try env.defun(
            "hyalo-sidebar-toggle",
            with: """
            Toggle the native SwiftUI navigation sidebar.
            """
        ) { (env: Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    guard let window = findEmacsWindow() else { return }
                    NavigationSidebarManager.shared.toggleSidebar(for: window)
                }
                return true
            }
            return false
        }

        try env.defun(
            "hyalo-sidebar-visible-p",
            with: """
            Return t if the native sidebar is visible, nil otherwise.
            """
        ) { (env: Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                guard let window = findEmacsWindow() else { return false }
                return NavigationSidebarManager.shared.isSidebarVisible(for: window)
            }
            return false
        }

        try env.defun(
            "hyalo-detail-show",
            with: """
            Show the native SwiftUI detail/inspector panel.
            """
        ) { (env: Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    guard let window = findEmacsWindow() else { return }
                    NavigationSidebarManager.shared.showDetail(for: window)
                }
                return true
            }
            return false
        }

        try env.defun(
            "hyalo-detail-hide",
            with: """
            Hide the native SwiftUI detail/inspector panel.
            """
        ) { (env: Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    guard let window = findEmacsWindow() else { return }
                    NavigationSidebarManager.shared.hideDetail(for: window)
                }
                return true
            }
            return false
        }

        try env.defun(
            "hyalo-detail-toggle",
            with: """
            Toggle the native SwiftUI detail/inspector panel.
            """
        ) { (env: Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    guard let window = findEmacsWindow() else { return }
                    NavigationSidebarManager.shared.toggleDetail(for: window)
                }
                return true
            }
            return false
        }

        try env.defun(
            "hyalo-detail-visible-p",
            with: """
            Return t if the detail/inspector panel is visible, nil otherwise.
            """
        ) { (env: Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                guard let window = findEmacsWindow() else { return false }
                return NavigationSidebarManager.shared.isDetailVisible(for: window)
            }
            return false
        }

        try env.defun(
            "hyalo-panel-needs-setup",
            with: """
            Return which panel needs embedded content setup, or nil if none.
            Returns "right" if inspector is visible but has no embedded content.
            Returns "left" if sidebar is visible but has no embedded content.
            Returns nil if all visible panels have content or are hidden.
            """
        ) { (env: Environment) throws -> String? in
            if #available(macOS 26.0, *) {
                guard let window = findEmacsWindow() else { return nil }
                let controller = NavigationSidebarManager.shared.getController(for: window)

                // Check if detail/inspector needs setup
                if controller.state.detailVisible && controller.state.rightView == nil {
                    return "right"
                }

                // Check if sidebar needs setup
                if controller.state.sidebarVisible &&
                   controller.state.leftTopView == nil &&
                   controller.state.leftBottomView == nil {
                    return "left"
                }

                return nil
            }
            return nil
        }

        // MARK: - Visibility Change Callbacks
        
        try env.defun(
            "hyalo-setup-visibility-callbacks",
            with: """
            Setup callbacks for sidebar/inspector visibility changes.
            This enables Swift to notify Elisp when toolbar buttons toggle panels.
            The callbacks call `hyalo-on-sidebar-visibility-changed' and
            `hyalo-on-detail-visibility-changed' hooks respectively.
            """
        ) { (env: Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                // Open a channel for async callbacks from Swift to Elisp
                let channel = try env.openChannel(name: "hyalo-visibility")
                
                // Create callback for sidebar visibility changes
                let sidebarCallback: (String, Bool) -> Void = channel.hook("hyalo-on-sidebar-visibility-changed")
                
                // Create callback for detail visibility changes  
                let detailCallback: (String, Bool) -> Void = channel.hook("hyalo-on-detail-visibility-changed")
                
                // Set the callbacks on the manager
                DispatchQueue.main.async {
                    NavigationSidebarManager.shared.onSidebarVisibilityChanged = sidebarCallback
                    NavigationSidebarManager.shared.onDetailVisibilityChanged = detailCallback
                }

                return true
            }
            return false
        }


        try env.defun(
            "hyalo-restore-focus",
            with: """
            Restore keyboard focus to the Emacs content view.
            Call this after restart-emacs or when Meta key stops working.
            """
        ) { (env: Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    guard let window = findEmacsWindow() else { return }
                    let controller = NavigationSidebarManager.shared.getController(for: window)
                    controller.restoreEmacsFocus()
                }
                return true
            }
            return false
        }

        try env.defun(
            "hyalo-debug-focus",
            with: """
            Debug keyboard focus state. Returns info about current first responder.
            """
        ) { () -> String in
            if #available(macOS 26.0, *) {
                guard let window = findEmacsWindow() else { return "No window" }
                let firstResponder = window.firstResponder
                let frClass = String(describing: type(of: firstResponder as Any))
                let isKey = window.isKeyWindow
                let isMain = window.isMainWindow

                // Try to find Emacs view
                let controller = NavigationSidebarManager.shared.getController(for: window)
                var emacsInfo = "not found"
                if let emacs = controller.state.debugEmacsView {
                    emacsInfo = String(describing: type(of: emacs))
                }

                return "FirstResponder: \(frClass), isKey: \(isKey), isMain: \(isMain), emacsView: \(emacsInfo)"
            }
            return "macOS 26 required"
        }

        try env.defun(
            "hyalo-sidebar-set-project",
            with: """
            Set the project name displayed in the sidebar header.
            NAME is the project name string.
            """
        ) { (env: Environment, name: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    guard let window = findEmacsWindow() else { return }
                    NavigationSidebarManager.shared.setProjectName(for: window, name: name)
                }
                return true
            }
            return false
        }

        try env.defun(
            "hyalo-set-inspector-header",
            with: """
            Set the inspector header properties.
            TITLE: The title string.
            ICON: The SF Symbols icon name.
            BUSY: Whether to animate the icon (boolean).
            """
        ) { (env: Environment, title: String, icon: String, busy: Bool) throws -> Bool in
            print("[Hyalo Module] hyalo-set-inspector-header called with title: \(title), busy: \(busy)")
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    guard let window = findEmacsWindow() else { return }
                    NavigationSidebarManager.shared.setInspectorHeader(for: window, title: title, icon: icon, busy: busy)
                }
                return true
            }
            return false
        }

        try env.defun(
            "hyalo-sidebar-update-mode-line",
            with: """
            Update the mode-line content in the toolbar.
            CONTENT is the mode-line string to display.
            """
        ) { (env: Environment, content: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    guard let window = findEmacsWindow() else { return }
                    NavigationSidebarManager.shared.updateModeLine(for: window, content: content)
                }
                return true
            }
            return false
        }

        try env.defun(
            "hyalo-sidebar-set-background-color",
            with: """
            Set the sidebar vibrancy background color.
            COLOR is a color string (e.g., "#282c34" or "white").
            ALPHA is the alpha/transparency value (0.0 to 1.0).
            """
        ) { (env: Environment, color: String, alpha: Double) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    guard let window = findEmacsWindow() else { return }
                    NavigationSidebarManager.shared.setBackgroundColor(
                        for: window,
                        color: color,
                        alpha: CGFloat(alpha)
                    )
                }
                return true
            }
            return false
        }

        try env.defun(
            "hyalo-sidebar-set-window-appearance",
            with: """
            Set the window appearance for NavigationSplitView.
            APPEARANCE is "light", "dark", or "auto".
            """
        ) { (env: Environment, appearance: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    guard let window = findEmacsWindow() else { return }
                    NavigationSidebarManager.shared.setWindowAppearance(
                        for: window,
                        appearance: appearance
                    )
                }
                return true
            }
            return false
        }
        
        try env.defun(
            "hyalo-sidebar-set-vibrancy-material",
            with: """
            Set the vibrancy material style for the content area.
            MATERIAL is one of: "ultraThin", "thin", "regular", "thick", "ultraThick", "none".
            Use "ultraThin" for maximum see-through effect, "ultraThick" for minimal.
            Use "none" to disable vibrancy entirely.
            """
        ) { (env: Environment, material: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    guard let window = findEmacsWindow() else { return }
                    NavigationSidebarManager.shared.setVibrancyMaterial(
                        for: window,
                        material: material
                    )
                }
                return true
            }
            return false
        }

        // MARK: - Footer Pattern

        try env.defun(
            "hyalo-set-footer-pattern",
            with: """
            Set the footer pattern for the echo area/minibuffer region.
            PATTERN is one of: "none", "hideout", "hexagons", "death-star",
            "bathroom-floor", "tiny-checkers", "plus", "cage", "diagonal-stripes",
            "stripes", "diagonal-lines", "polka-dots", "signal", "wallpaper".
            The pattern draws a tinted overlay at the bottom of the window.
            """
        ) { (env: Environment, pattern: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    guard let window = findEmacsWindow() else { return }
                    NavigationSidebarManager.shared.setFooterPattern(
                        for: window,
                        pattern: pattern
                    )
                }
                return true
            }
            return false
        }

        try env.defun(
            "hyalo-set-footer-height",
            with: """
            Set the footer height (echo area/minibuffer height) in pixels.
            HEIGHT is the height in pixels. Set to 0 to disable the footer.
            Call this when the echo area size changes (e.g., via hooks/advices).
            """
        ) { (env: Environment, height: Int) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    guard let window = findEmacsWindow() else { return }
                    NavigationSidebarManager.shared.setFooterHeight(
                        for: window,
                        height: CGFloat(height)
                    )
                }
                return true
            }
            return false
        }

        try env.defun(
            "hyalo-set-footer-background-alpha",
            with: """
            Set the footer background tint alpha (transparency).
            ALPHA is a value from 0.0 (invisible) to 1.0 (fully opaque).
            Default is 0.3. Makes dark themes darker, light themes lighter.
            """
        ) { (env: Environment, alpha: Double) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    guard let window = findEmacsWindow() else { return }
                    NavigationSidebarManager.shared.setFooterBackgroundAlpha(
                        for: window,
                        alpha: CGFloat(alpha)
                    )
                }
                return true
            }
            return false
        }

        try env.defun(
            "hyalo-set-footer-pattern-alpha",
            with: """
            Set the footer pattern foreground alpha (transparency).
            ALPHA is a value from 0.0 (invisible) to 1.0 (fully opaque).
            Default is 0.15 for subtle pattern visibility.
            """
        ) { (env: Environment, alpha: Double) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    guard let window = findEmacsWindow() else { return }
                    NavigationSidebarManager.shared.setFooterPatternAlpha(
                        for: window,
                        alpha: CGFloat(alpha)
                    )
                }
                return true
            }
            return false
        }

        // MARK: - Minibuffer

        try env.defun(
            "hyalo-minibuffer-enable",
            with: """
            Enable glass effect for a specific window (minibuffer).
            WINDOW-ID is the string ID of the window (frame-parameter 'window-id).
            Note: window-id doesn't match NSWindow.windowNumber, so we find child frames differently.
            """
        ) { (env: Environment, windowID: String) throws -> Bool in
            DispatchQueue.main.async {
                var emacsWindows: [NSWindow] = []
                for w in NSApp.windows {
                    // Collect EmacsWindow instances
                    let className = String(describing: type(of: w))
                    if className.contains("EmacsWindow") && w.isVisible {
                        emacsWindows.append(w)
                    }
                }
                
                // Find the mini-frame: it's an EmacsWindow WITH a parent (child frame)
                let miniFrame = emacsWindows.first { $0.parent != nil }
                
                // Find the ROOT window - must be a visible window WITHOUT a parent
                // Cannot use keyWindow because mini-frame might be key
                let rootCandidates = NSApp.windows.filter { $0.isVisible && $0.parent == nil }
                let rootWindow = rootCandidates.first { String(describing: type(of: $0)).contains("EmacsWindow") }
                    ?? rootCandidates.first
                
                guard let root = rootWindow else {
                    return
                }
                
                if let window = miniFrame {
                    if #available(macOS 10.14, *) {
                        // Check if glass is already attached (don't resize again)
                        let glassAlreadyAttached = window.contentView?.identifier?.rawValue == "HyaloGlassEffect"
                        
                        // Position the mini-frame RELATIVE TO ROOT WINDOW (screen coordinates)
                        // Width: 75% of root (min 500px), Y position: 25% from top
                        let rootFrame = root.frame
                        let widthRatio: CGFloat = 0.75
                        let yRatio: CGFloat = 0.25
                        let minWidth: CGFloat = 500
                        
                        let newWidth = max(rootFrame.width * widthRatio, minWidth)
                        // Keep existing height - don't accumulate
                        let currentHeight = window.frame.height
                        let newX = rootFrame.origin.x + (rootFrame.width - newWidth) / 2
                        // Position from top of root window (screen coordinates)
                        let newY = rootFrame.origin.y + rootFrame.height * (1 - yRatio) - currentHeight
                        
                        // Set frame position and width (without display to minimize flicker)
                        window.setFrame(NSRect(x: newX, y: newY, width: newWidth, height: currentHeight), display: false, animate: false)
                        
                        // Attach the glass effect (only if not already attached)
                        if !glassAlreadyAttached {
                            GlassEffectView.attach(to: window)
                        }
                        
                        // Make mini-frame modal-like: higher window level prevents interaction with parent
                        window.level = .modalPanel
                        
                        // Activate modal tracking to enforce focus
                        MiniFrameTracker.activate(window)
                        
                        // Ensure focus on the mini-frame
                        window.makeKeyAndOrderFront(nil)
                        NSApp.activate(ignoringOtherApps: true)
                    }
                }
            }
            return true
        }

        try env.defun(
            "hyalo-window-focus",
            with: """
            Make the window with WINDOW-ID key and order front.
            Forcefully activates the application and focuses the specific window.
            """
        ) { (env: Environment, windowID: String) throws -> Bool in
            DispatchQueue.main.async {
                let id = Int(windowID) ?? 0
                if let window = NSApp.window(withWindowNumber: id) {
                    window.makeKeyAndOrderFront(nil)
                    // Also ensure the app is active
                    NSApp.activate(ignoringOtherApps: true)
                }
            }
            return true
        }

        try env.defun(
            "hyalo-minibuffer-deactivate",
            with: """
            Deactivate mini-frame modal tracking.
            Call this when the minibuffer is dismissed to allow normal window focus.
            """
        ) { (env: Environment) throws -> Bool in
            DispatchQueue.main.async {
                MiniFrameTracker.deactivate()
            }
            return true
        }

        try env.defun(
            "hyalo-minibuffer-update-appearance",
            with: """
            Update appearance of mini-frame glass effect.
            Call this when system appearance or theme changes to refresh the glass look.
            """
        ) { (env: Environment) throws -> Bool in
            DispatchQueue.main.async {
                // Find all EmacsWindow child frames (mini-frames)
                for w in NSApp.windows {
                    let className = String(describing: type(of: w))
                    if className.contains("EmacsWindow") && w.parent != nil && w.isVisible {
                        GlassEffectView.updateAppearance(for: w)
                    }
                }
            }
            return true
        }

        // MARK: - Decorations (Toolbar & Traffic Lights)

        try env.defun(
            "hyalo-set-decorations-visible",
            with: """
            Set visibility of decorations (toolbar and traffic lights).
            VISIBLE is t to show, nil to hide.
            """
        ) { (env: Environment, visible: Bool) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    guard let window = findEmacsWindow() else { return }
                    NavigationSidebarManager.shared.setDecorationsVisible(for: window, visible: visible)
                }
                return true
            }
            return false
        }

        try env.defun(
            "hyalo-toggle-decorations",
            with: """
            Toggle visibility of decorations (toolbar and traffic lights).
            """
        ) { (env: Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    guard let window = findEmacsWindow() else { return }
                    NavigationSidebarManager.shared.toggleDecorations(for: window)
                }
                return true
            }
            return false
        }

        try env.defun(
            "hyalo-decorations-visible-p",
            with: """
            Return t if decorations (toolbar and traffic lights) are visible, nil otherwise.
            """
        ) { (env: Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                guard let window = findEmacsWindow() else { return true }
                return NavigationSidebarManager.shared.areDecorationsVisible(for: window)
            }
            return true
        }

        // Note: Treemacs runs inside Emacs (detail area), not managed by Swift sidebar

        // MARK: - Appearance Panel

        try env.defun(
            "hyalo-show-appearance-panel",
            with: """
            Show the Hyalo appearance panel.
            Opens a Liquid Glass popup with sliders for vibrancy, blur, and tint control.
            """
        ) { (env: Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    guard let window = findEmacsWindow() else { return }
                    AppearancePanelController.shared.show(relativeTo: window)
                }
                return true
            }
            return false
        }

        try env.defun(
            "hyalo-toggle-appearance-panel",
            with: """
            Toggle the Hyalo appearance panel visibility.
            """
        ) { (env: Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    guard let window = findEmacsWindow() else { return }
                    AppearancePanelController.shared.toggle(relativeTo: window)
                }
                return true
            }
            return false
        }

        try env.defun(
            "hyalo-hide-appearance-panel",
            with: """
            Hide the Hyalo appearance panel if visible.
            """
        ) { (env: Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    AppearancePanelController.shared.dismiss()
                }
                return true
            }
            return false
        }

        try env.defun(
            "hyalo-appearance-panel-visible-p",
            with: """
            Return t if the appearance panel is currently visible.
            """
        ) { () -> Bool in
            if #available(macOS 26.0, *) {
                return AppearancePanelController.shared.isVisible()
            }
            return false
        }

        try env.defun(
            "hyalo-set-opacity-from-panel",
            with: """
            Set opacity from the appearance panel.
            OPACITY is a value from 0.0 (full vibrancy) to 1.0 (solid theme color).
            """
        ) { (env: Environment, opacity: Double) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    AppearanceSettings.shared.opacity = opacity
                    AppearancePanelController.shared.onSettingsChanged?(AppearanceSettings.shared)
                    AppearancePanelController.shared.refreshPanel()
                }
                return true
            }
            return false
        }

        try env.defun(
            "hyalo-set-vibrancy-material-from-panel",
            with: """
            Set vibrancy material from the appearance panel.
            MATERIAL is one of: "ultraThin", "thin", "regular", "thick", "ultraThick", "none".
            """
        ) { (env: Environment, material: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    if let mat = VibrancyMaterial(rawValue: material) {
                        AppearanceSettings.shared.setVibrancyFromMaterial(mat)
                        AppearancePanelController.shared.onSettingsChanged?(AppearanceSettings.shared)
                        // Refresh panel if visible
                        AppearancePanelController.shared.refreshPanel()
                    }
                }
                return true
            }
            return false
        }

        try env.defun(
            "hyalo-sync-appearance-to-panel",
            with: """
            Sync Emacs appearance settings to the Swift panel.
            MATERIAL is vibrancy material, OPACITY is 0.0-1.0.
            """
        ) { (env: Environment, material: String, opacity: Double) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    if let mat = VibrancyMaterial(rawValue: material) {
                        AppearanceSettings.shared.setVibrancyFromMaterial(mat)
                    }
                    AppearanceSettings.shared.opacity = opacity
                    AppearancePanelController.shared.onSettingsChanged?(AppearanceSettings.shared)
                    // Refresh panel if visible
                    AppearancePanelController.shared.refreshPanel()
                }
                return true
            }
            return false
        }

        try env.defun(
            "hyalo-refresh-appearance-panel",
            with: "Refresh the appearance panel to show current settings."
        ) { (env: Environment) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    AppearancePanelController.shared.refreshPanel()
                }
                return true
            }
            return false
        }

        try env.defun(
            "hyalo-get-panel-appearance-mode",
            with: """
            Get the current appearance mode from the Swift panel.
            Returns "light", "dark", or "auto".
            """
        ) { () -> String in
            if #available(macOS 26.0, *) {
                return AppearanceSettings.shared.appearanceMode.emacsValue
            }
            return "auto"
        }

        try env.defun(
            "hyalo-set-panel-appearance-mode",
            with: """
            Set the appearance mode in the Swift panel.
            MODE is "light", "dark", or "auto".
            This updates the controller state and panel UI.
            """
        ) { (env: Environment, mode: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    // Update controller state (source of truth)
                    if let window = findEmacsWindow() {
                        let controller = NavigationSidebarManager.shared.getController(for: window)
                        controller.state.windowAppearance = mode
                    }
                    // Update panel settings
                    let appearanceMode = AppearanceMode.from(emacsValue: mode)
                    AppearanceSettings.shared.appearanceMode = appearanceMode
                    // Refresh panel to display updated value
                    AppearancePanelController.shared.refreshPanel()
                }
                return true
            }
            return false
        }

        try env.defun(
            "hyalo-get-panel-vibrancy-material",
            with: """
            Get the current vibrancy material from the Swift controller state.
            Returns one of: "none", "ultraThick", "thick", "regular", "thin", "ultraThin".
            """
        ) { () -> String in
            if #available(macOS 26.0, *) {
                // Read from controller state (source of truth), not AppearanceSettings
                if let window = findEmacsWindow() {
                    let controller = NavigationSidebarManager.shared.getController(for: window)
                    return controller.state.vibrancyMaterial.rawValue
                }
            }
            return "ultraThin"
        }

        try env.defun(
            "hyalo-get-panel-opacity",
            with: """
            Get the current opacity from the Swift controller state.
            Returns a value from 0.0 to 1.0.
            """
        ) { () -> Double in
            if #available(macOS 26.0, *) {
                // Read from controller state (source of truth), not AppearanceSettings
                if let window = findEmacsWindow() {
                    let controller = NavigationSidebarManager.shared.getController(for: window)
                    return Double(controller.state.backgroundAlpha)
                }
            }
            return 0.5
        }


        // MARK: - System Integration

        try env.defun(
            "hyalo-reveal-in-finder",
            with: "Reveal files in Finder. FILES is a vector of file paths."
        ) { (env: Environment, files: [String]) throws -> Bool in
            DispatchQueue.main.async {
                NSWorkspace.shared.activateFileViewerSelecting(
                    files.map { URL(fileURLWithPath: $0) }
                )
            }
            return true
        }

        try env.defun(
            "hyalo-share",
            with: "Share files via macOS share sheet. FILES is a vector of file paths."
        ) { (env: Environment, files: [String]) throws -> Bool in
            DispatchQueue.main.async {
                guard let window = findEmacsWindow(),
                      let contentView = window.contentView else { return }
                let urls = files.map { URL(fileURLWithPath: $0) }
                let picker = NSSharingServicePicker(items: urls)
                // Position near window center
                let rect = NSRect(
                    x: contentView.bounds.width / 2,
                    y: contentView.bounds.height / 2,
                    width: 1,
                    height: 1
                )
                picker.show(relativeTo: rect, of: contentView, preferredEdge: .maxY)
            }
            return true
        }

        try env.defun(
            "hyalo-show-emoji-picker",
            with: "Show the macOS emoji picker."
        ) { (env: Environment) throws -> Bool in
            DispatchQueue.main.async {
                NSApp.orderFrontCharacterPalette(nil)
            }
            return true
        }

        // MARK: - Embedded Sidebar

        try env.defun(
            "hyalo-sidebar-register-frame",
            with: """
            Register a child-frame for embedding in the sidebar.
            SLOT is the slot identifier: "left-top", "left-bottom", or "right".
            WINDOW-ID is the frame's window-id (from frame-parameter).
            """
        ) { (env: Environment, slot: String, windowId: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    guard let window = findEmacsWindow() else {
                        return
                    }
                    EmbeddedFrameManager.shared.registerFrame(slot: slot, windowId: windowId, parentWindow: window)
                }
                return true
            }
            return false
        }

        try env.defun(
            "hyalo-sidebar-embed-frames",
            with: """
            Embed registered child-frames for the specified panel.
            PANEL is "left" or "right".
            """
        ) { (env: Environment, panel: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    guard let window = findEmacsWindow() else {
                        return
                    }
                    let result = EmbeddedFrameManager.shared.embedFrames(for: panel, parentWindow: window)
                }
                return true
            }
            return false
        }

        try env.defun(
            "hyalo-sidebar-detach-frames",
            with: """
            Detach embedded frames and return them to their original windows.
            PANEL is "left" or "right".
            """
        ) { (env: Environment, panel: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    EmbeddedFrameManager.shared.detachFrames(for: panel)
                }
                return true
            }
            return false
        }

        try env.defun(
            "hyalo-sidebar-clear-registration",
            with: """
            Clear frame registration for a panel before re-setup.
            PANEL is "left" or "right".
            """
        ) { (env: Environment, panel: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    EmbeddedFrameManager.shared.clearRegistration(for: panel)
                }
                return true
            }
            return false
        }
    }
}

// MARK: - Module Factory

func createModule() -> Module {
    HyaloModule()
}
