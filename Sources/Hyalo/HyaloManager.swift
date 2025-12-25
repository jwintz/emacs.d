// HyaloManager - Singleton manager for window controllers
// Copyright (C) 2024

import AppKit

/// Singleton manager that tracks HyaloController instances per window.
/// Also handles system appearance detection.
final class HyaloManager {
    static let shared = HyaloManager()

    /// Corner radius (delegates to HyaloController)
    static var cornerRadius: CGFloat { HyaloController.cornerRadius }

    /// Controller per window
    private var controllers: [Int: HyaloController] = [:]

    /// Appearance change observer
    private var appearanceObserver: NSObjectProtocol?

    private init() {
        setupAppearanceObserver()
    }

    // MARK: - Controller Management

    private func getOrCreateController(for window: NSWindow) -> HyaloController {
        let windowNumber = window.windowNumber
        if let controller = controllers[windowNumber] {
            return controller
        }
        let controller = HyaloController(window: window)
        controllers[windowNumber] = controller
        return controller
    }

    // MARK: - Full Setup/Teardown

    func setupHyalo(for window: NSWindow) {
        let controller = getOrCreateController(for: window)
        controller.setup()
    }

    func removeHyalo(from window: NSWindow) {
        let windowNumber = window.windowNumber
        controllers[windowNumber]?.teardown()
        controllers.removeValue(forKey: windowNumber)
    }

    // MARK: - Traffic Lights

    func setTrafficLightsAutoHide(_ enabled: Bool, for window: NSWindow) {
        let controller = getOrCreateController(for: window)
        controller.setTrafficLightsAutoHide(enabled)
    }

    func showTrafficLights(for window: NSWindow) {
        controllers[window.windowNumber]?.showTrafficLights()
    }

    func hideTrafficLights(for window: NSWindow) {
        controllers[window.windowNumber]?.hideTrafficLights()
    }

    // MARK: - Header View

    func updateHeader(for window: NSWindow, modeLineString: String) {
        controllers[window.windowNumber]?.updateHeader(modeLineString: modeLineString)
    }

    func updateHeaderLine(for window: NSWindow, content: String) {
        controllers[window.windowNumber]?.updateHeaderLine(content)
    }

    func setHeaderPosition(for window: NSWindow, top: CGFloat, left: CGFloat, right: CGFloat) {
        controllers[window.windowNumber]?.setHeaderPosition(top: top, left: left, right: right)
    }
    
    /// Show or hide the floating header view
    /// Used when NavigationSplitView is active (toolbar replaces floating header)
    func setHeaderHidden(_ hidden: Bool, for window: NSWindow) {
        controllers[window.windowNumber]?.setHeaderHidden(hidden)
    }
    
    /// Show or hide the gradient overlay view
    func setGradientHidden(_ hidden: Bool, for window: NSWindow) {
        controllers[window.windowNumber]?.setGradientHidden(hidden)
    }

    // MARK: - Sidebar

    func setSidebarOffset(_ offset: CGFloat, for window: NSWindow) {
        controllers[window.windowNumber]?.setSidebarOffset(offset)
    }

    func setSidebarOffsetForAll(_ offset: CGFloat) {
        for (_, controller) in controllers {
            controller.setSidebarOffset(offset)
        }
    }

    // NOTE: Sidebar margin is now handled by child frame approach (SidebarFrameManager)

    // MARK: - Background Color and Echo Area

    func setBackgroundColor(_ colorString: String, for window: NSWindow) {
        controllers[window.windowNumber]?.setBackgroundColor(colorString)
    }

    func setEchoAreaHeight(_ height: CGFloat, for window: NSWindow) {
        controllers[window.windowNumber]?.setEchoAreaHeight(height)
    }

    func setEchoAreaTintOpacity(_ opacity: CGFloat, for window: NSWindow) {
        controllers[window.windowNumber]?.setEchoAreaTintOpacity(opacity)
    }

    // MARK: - Apply to ALL Windows

    /// Apply appearance to all managed windows
    func setWindowAppearanceForAll(_ appearance: String) {
        for (_, controller) in controllers {
            if let window = controller.window {
                setWindowAppearance(appearance, for: window)
            }
        }
    }

    /// Apply background color to all managed windows
    func setBackgroundColorForAll(_ colorString: String) {
        for (_, controller) in controllers {
            controller.setBackgroundColor(colorString)
        }
    }

    /// Apply echo area height to all managed windows
    func setEchoAreaHeightForAll(_ height: CGFloat) {
        for (_, controller) in controllers {
            controller.setEchoAreaHeight(height)
        }
    }

    /// Apply echo area tint opacity to all managed windows
    func setEchoAreaTintOpacityForAll(_ opacity: CGFloat) {
        for (_, controller) in controllers {
            controller.setEchoAreaTintOpacity(opacity)
        }
    }

    /// Set echo area dark theme for a single window
    func setEchoAreaDarkTheme(_ isDark: Bool, for window: NSWindow) {
        controllers[window.windowNumber]?.setEchoAreaDarkTheme(isDark)
    }

    /// Apply echo area dark theme to all managed windows
    func setEchoAreaDarkThemeForAll(_ isDark: Bool) {
        for (_, controller) in controllers {
            controller.setEchoAreaDarkTheme(isDark)
        }
    }

    // MARK: - System Appearance

    private func setupAppearanceObserver() {
        appearanceObserver = DistributedNotificationCenter.default.addObserver(
            forName: NSNotification.Name("AppleInterfaceThemeChangedNotification"),
            object: nil,
            queue: .main
        ) { [weak self] _ in
            _ = self?.getSystemAppearance()
        }
    }

    func getSystemAppearance() -> String {
        let appearance = NSApp.effectiveAppearance
        if appearance.bestMatch(from: [.darkAqua, .aqua]) == .darkAqua {
            return "dark"
        }
        return "light"
    }

    func setWindowAppearance(_ appearance: String, for window: NSWindow) {
        switch appearance {
        case "light":
            window.appearance = NSAppearance(named: .aqua)
        case "dark":
            window.appearance = NSAppearance(named: .darkAqua)
        default:
            window.appearance = nil
        }
    }

    deinit {
        if let observer = appearanceObserver {
            DistributedNotificationCenter.default.removeObserver(observer)
        }
    }
}
