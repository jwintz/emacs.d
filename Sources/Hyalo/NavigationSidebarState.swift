// NavigationSidebarState.swift - Observable state for NavigationSidebar
// Copyright (C) 2025
// Target: macOS 26 Tahoe with Liquid Glass design

import AppKit
import SwiftUI

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
    var inspectorTitle: String = "PI AGENT"
    var inspectorIcon: String = "sparkles"
    var inspectorBusy: Bool = false
    var inspectorSubtitle: String = ""  // Secondary info (e.g., token usage)

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
