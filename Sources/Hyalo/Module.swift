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

    @available(macOS 26.0, *)
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

    @available(macOS 26.0, *)
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
            if #available(macOS 26.0, *) {
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
            if #available(macOS 26.0, *) {
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
            This updates the panel UI and applies the window appearance.
            """
        ) { (env: Environment, mode: String) throws -> Bool in
            if #available(macOS 26.0, *) {
                DispatchQueue.main.async {
                    let appearanceMode = AppearanceMode.from(emacsValue: mode)
                    AppearanceSettings.shared.appearanceMode = appearanceMode
                    AppearancePanelController.shared.onAppearanceModeChanged?(appearanceMode)
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
    }
}

// MARK: - Module Factory

func createModule() -> Module {
    HyaloModule()
}
