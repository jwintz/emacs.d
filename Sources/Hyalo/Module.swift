// Hyalo - Emacs Dynamic Module for macOS Liquid Glass Effects
// Copyright (C) 2024

import AppKit
import EmacsSwiftModule

/// Helper to find an Emacs window
func findEmacsWindow() -> NSWindow? {
    if let window = NSApp.mainWindow { return window }
    if let window = NSApp.keyWindow { return window }
    for window in NSApp.windows where window.isVisible && !window.isMiniaturized {
        return window
    }
    return NSApp.windows.first
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

    // MARK: - JSON Parsing Helpers

    @available(macOS 15.0, *)
    static func parseBuffersJSON(_ json: String) -> [SidebarBuffer]? {
        guard let data = json.data(using: .utf8) else { return nil }
        guard let array = try? JSONSerialization.jsonObject(with: data) as? [[String: Any]] else { return nil }

        return array.compactMap { dict -> SidebarBuffer? in
            guard let name = dict["name"] as? String else { return nil }
            return SidebarBuffer(
                id: dict["id"] as? String ?? name,
                name: name,
                path: dict["path"] as? String,
                isModified: dict["modified"] as? Bool ?? false,
                isCurrent: dict["current"] as? Bool ?? false,
                icon: dict["icon"] as? String ?? ""
            )
        }
    }

    @available(macOS 15.0, *)
    static func parseFilesJSON(_ json: String) -> [SidebarFile]? {
        guard let data = json.data(using: .utf8) else { return nil }
        guard let array = try? JSONSerialization.jsonObject(with: data) as? [[String: Any]] else { return nil }

        return array.compactMap { dict -> SidebarFile? in
            guard let name = dict["name"] as? String,
                  let path = dict["path"] as? String else { return nil }
            return SidebarFile(
                id: dict["id"] as? String ?? path,
                name: name,
                path: path,
                isDirectory: dict["directory"] as? Bool ?? false,
                isExpanded: dict["expanded"] as? Bool ?? false,
                depth: dict["depth"] as? Int ?? 0,
                icon: dict["icon"] as? String ?? ""
            )
        }
    }

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
            if #available(macOS 15.0, *) {
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
            if #available(macOS 15.0, *) {
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
            if #available(macOS 15.0, *) {
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
            if #available(macOS 15.0, *) {
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
            if #available(macOS 15.0, *) {
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
            if #available(macOS 15.0, *) {
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
            if #available(macOS 15.0, *) {
                guard let window = findEmacsWindow() else { return false }
                return NavigationSidebarManager.shared.isSidebarVisible(for: window)
            }
            return false
        }

        try env.defun(
            "hyalo-sidebar-set-project",
            with: """
            Set the project name displayed in the sidebar header.
            NAME is the project name string.
            """
        ) { (env: Environment, name: String) throws -> Bool in
            if #available(macOS 15.0, *) {
                DispatchQueue.main.async {
                    guard let window = findEmacsWindow() else { return }
                    NavigationSidebarManager.shared.setProjectName(for: window, name: name)
                }
                return true
            }
            return false
        }

        // Note: Buffer and file updates use JSON strings for complex data
        // Emacs will encode the data as JSON, Swift will decode it

        try env.defun(
            "hyalo-sidebar-update-buffers-json",
            with: """
            Update the Open Editors section with buffer list.
            JSON-DATA is a JSON string encoding a list of buffer objects with keys:
            id, name, path, modified, current, icon
            """
        ) { (env: Environment, jsonData: String) throws -> Bool in
            if #available(macOS 15.0, *) {
                DispatchQueue.main.async {
                    guard let window = findEmacsWindow() else { return }
                    if let buffers = Self.parseBuffersJSON(jsonData) {
                        NavigationSidebarManager.shared.updateBuffers(for: window, buffers: buffers)
                    }
                }
                return true
            }
            return false
        }

        try env.defun(
            "hyalo-sidebar-update-files-json",
            with: """
            Update the Explorer section with file tree from treemacs.
            JSON-DATA is a JSON string encoding a list of file objects with keys:
            id, name, path, directory, expanded, depth, icon
            """
        ) { (env: Environment, jsonData: String) throws -> Bool in
            if #available(macOS 15.0, *) {
                DispatchQueue.main.async {
                    guard let window = findEmacsWindow() else { return }
                    if let files = Self.parseFilesJSON(jsonData) {
                        NavigationSidebarManager.shared.updateFiles(for: window, files: files)
                    }
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
            if #available(macOS 15.0, *) {
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
            if #available(macOS 15.0, *) {
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
            if #available(macOS 15.0, *) {
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
            "hyalo-sidebar-set-echo-area-height",
            with: """
            Set the echo area overlay height.
            HEIGHT is the minibuffer height in pixels.
            """
        ) { (env: Environment, height: Int) throws -> Bool in
            if #available(macOS 15.0, *) {
                DispatchQueue.main.async {
                    guard let window = findEmacsWindow() else { return }
                    NavigationSidebarManager.shared.setEchoAreaHeight(
                        for: window,
                        height: CGFloat(height)
                    )
                }
                return true
            }
            return false
        }

        // Note: Treemacs runs inside Emacs (detail area), not managed by Swift sidebar

        // MARK: - Background Color and Echo Area

        try env.defun(
            "hyalo-set-background-color",
            with: """
            Set the background color for gradient overlays.
            COLOR is a color string (e.g., "#282c34" or "white").
            This is used to create the Safari-like header blend effect.
            """
        ) { (env: Environment, color: String) throws -> Bool in
            DispatchQueue.main.async {
                guard let window = findEmacsWindow() else { return }
                HyaloManager.shared.setBackgroundColor(color, for: window)
            }
            return true
        }

        try env.defun(
            "hyalo-set-echo-area-height",
            with: """
            Set the echo area overlay height.
            HEIGHT is the height in pixels of the echo area/minibuffer.
            The overlay will include +1 pixel for the window divider.
            """
        ) { (env: Environment, height: Int) throws -> Bool in
            DispatchQueue.main.async {
                guard let window = findEmacsWindow() else { return }
                HyaloManager.shared.setEchoAreaHeight(CGFloat(height), for: window)
            }
            return true
        }

        try env.defun(
            "hyalo-set-echo-area-tint-opacity",
            with: """
            Set the echo area tint opacity.
            OPACITY is a float from 0.0 (invisible) to 1.0 (fully opaque).
            For light themes, this applies a white tint. For dark themes, a black tint.
            Recommended values: 0.1 to 0.3.
            Note: This is independent of frame alpha-background.
            """
        ) { (env: Environment, opacity: Double) throws -> Bool in
            DispatchQueue.main.async {
                guard let window = findEmacsWindow() else { return }
                HyaloManager.shared.setEchoAreaTintOpacity(CGFloat(opacity), for: window)
            }
            return true
        }

        // MARK: - Apply to ALL windows

        try env.defun(
            "hyalo-set-window-appearance-all",
            with: "Set window appearance for ALL managed windows: \"light\", \"dark\", or \"auto\"."
        ) { (env: Environment, appearance: String) throws -> Bool in
            DispatchQueue.main.async {
                HyaloManager.shared.setWindowAppearanceForAll(appearance)
            }
            return true
        }

        try env.defun(
            "hyalo-set-background-color-all",
            with: "Set the background color for ALL managed windows."
        ) { (env: Environment, color: String) throws -> Bool in
            DispatchQueue.main.async {
                HyaloManager.shared.setBackgroundColorForAll(color)
            }
            return true
        }

        try env.defun(
            "hyalo-set-echo-area-height-all",
            with: "Set the echo area overlay height for ALL managed windows."
        ) { (env: Environment, height: Int) throws -> Bool in
            DispatchQueue.main.async {
                HyaloManager.shared.setEchoAreaHeightForAll(CGFloat(height))
            }
            return true
        }

        try env.defun(
            "hyalo-set-echo-area-tint-opacity-all",
            with: "Set the echo area tint opacity for ALL managed windows."
        ) { (env: Environment, opacity: Double) throws -> Bool in
            DispatchQueue.main.async {
                HyaloManager.shared.setEchoAreaTintOpacityForAll(CGFloat(opacity))
            }
            return true
        }

        try env.defun(
            "hyalo-set-echo-area-dark-theme-all",
            with: "Set echo area dark theme mode for ALL managed windows. IS-DARK is t or nil."
        ) { (env: Environment, isDark: Bool) throws -> Bool in
            DispatchQueue.main.async {
                HyaloManager.shared.setEchoAreaDarkThemeForAll(isDark)
            }
            return true
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
    }
}

// MARK: - Module Factory

func createModule() -> Module {
    HyaloModule()
}
