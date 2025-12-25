// HyaloController - Window styling and overlay management
// Copyright (C) 2024

import AppKit

/// Corner radius configuration
/// Formula: outerRadius = innerRadius + padding
struct CornerRadiusConfig {
    /// Window (outer) corner radius
    let outer: CGFloat
    /// Header capsule (inner) corner radius
    let inner: CGFloat
    /// Padding between inner and outer
    var padding: CGFloat { outer - inner }

    /// Default configuration for macOS Tahoe
    static let tahoe = CornerRadiusConfig(outer: 12.0, inner: 14.0)

    /// Create from outer radius, deriving inner from padding
    static func fromOuter(_ outer: CGFloat, padding: CGFloat = 6.0) -> CornerRadiusConfig {
        CornerRadiusConfig(outer: outer, inner: max(4, outer - padding))
    }
}

/// Custom view that draws a vertical gradient for header blend effect
/// Positioned at the top of the content area, overlapping text that scrolls under the header.
/// Uses a gradient alpha mask to fade text as it approaches the header.
final class GradientOverlayView: NSView {
    var backgroundColor: NSColor = .windowBackgroundColor {
        didSet { needsDisplay = true }
    }

    var gradientHeight: CGFloat = 50 {
        didSet { needsDisplay = true }
    }

    override var isFlipped: Bool { false }  // Standard macOS coordinates (y=0 at bottom)

    override func draw(_ dirtyRect: NSRect) {
        guard let context = NSGraphicsContext.current?.cgContext else { return }

        // We want text to fade as it goes UP toward the header.
        // Draw a gradient that goes from transparent at bottom to background color at top.
        // This covers the text with increasing opacity as it nears the header.
        let colorSpace = CGColorSpaceCreateDeviceRGB()
        let bottomColor = backgroundColor.withAlphaComponent(0).cgColor  // Transparent at bottom (text visible)
        let topColor = backgroundColor.cgColor  // Opaque at top (text hidden under header)

        guard let gradient = CGGradient(
            colorsSpace: colorSpace,
            colors: [bottomColor, topColor] as CFArray,
            locations: [0.0, 1.0]
        ) else { return }

        // Draw from bottom (y=0) to top (y=height)
        let startPoint = CGPoint(x: bounds.midX, y: 0)  // Bottom - transparent
        let endPoint = CGPoint(x: bounds.midX, y: bounds.height)  // Top - opaque

        context.drawLinearGradient(
            gradient,
            start: startPoint,
            end: endPoint,
            options: []
        )
    }
}

/// Custom view for echo area tint overlay
final class EchoAreaOverlayView: NSView {
    var isDarkTheme: Bool = false {
        didSet { needsDisplay = true }
    }

    var tintOpacity: CGFloat = 1.0 {
        didSet { needsDisplay = true }
    }

    override func draw(_ dirtyRect: NSRect) {
        // Light themes get lighter tint (white overlay), dark themes get darker tint (black overlay)
        let tintColor: NSColor
        if isDarkTheme {
            tintColor = NSColor.black.withAlphaComponent(tintOpacity)
        } else {
            tintColor = NSColor.white.withAlphaComponent(tintOpacity)
        }

        tintColor.setFill()
        bounds.fill()  // Fill entire bounds, not just dirtyRect
    }

    override var isOpaque: Bool { false }
}

/// Controls Hyalo window appearance:
/// - Transparent titlebar with rounded corners
/// - NSVisualEffectView for blur
/// - Header gradient overlay
/// - Echo area tint overlay
/// - Traffic light hover behavior (using NSTrackingArea, no timers)
final class HyaloController: NSObject {
    private weak var _window: NSWindow?

    /// Public accessor for the managed window
    var window: NSWindow? { _window }

    private var blurView: NSVisualEffectView?
    private var gradientView: GradientOverlayView?
    private var echoAreaView: EchoAreaOverlayView?
    private var trafficLightTrackingArea: NSTrackingArea?
    private var trafficLightsVisible: Bool = true
    private var trafficLightsEnabled: Bool = true

    /// Header view controller (macOS 15+)
    private var headerController: Any?  // Type-erased for version compatibility

    /// Current sidebar offset (width in pixels, for header position adjustment)
    private var sidebarOffset: CGFloat = 0

    /// Corner radius configuration
    static var cornerConfig = CornerRadiusConfig.tahoe

    /// Window corner radius (for external access)
    static var cornerRadius: CGFloat { cornerConfig.outer }

    /// Header capsule corner radius
    static var headerCornerRadius: CGFloat { cornerConfig.inner }

    /// Hover area size for traffic lights
    private let hoverAreaSize = NSSize(width: 80, height: 40)

    /// Current background color from Emacs
    private var currentBackgroundColor: NSColor = .windowBackgroundColor

    /// Current echo area height (from Emacs)
    private var echoAreaHeight: CGFloat = 0

    /// Current header top padding (from Emacs config)
    private var headerTopPadding: CGFloat = 12

    init(window: NSWindow) {
        self._window = window
        super.init()
    }

    // MARK: - Full Setup

    /// Apply full Hyalo styling to the window
    func setup() {
        guard let window = window else { return }

        // Configure titlebar
        window.titlebarAppearsTransparent = true
        window.titleVisibility = .hidden
        window.styleMask.insert(.fullSizeContentView)

        // Make window non-opaque for transparency
        window.isOpaque = false
        window.backgroundColor = .clear

        // Add blur view
        setupBlurView()

        // Add gradient overlay for header blend effect
        setupGradientView()

        // Add echo area overlay
        setupEchoAreaView()

        // Add header view (macOS 15+)
        setupHeaderView()

        // NOTE: Sidebar glass is now handled by SidebarFrameManager for child frames
        // The child frame approach creates a separate NSWindow with its own glass effect

        // Hide native traffic lights - we use custom SwiftUI buttons
        hideNativeTrafficLightsPermanently()

        // Setup tracking area for traffic light hover (no timers)
        setupTrafficLightTracking()
    }

    /// Hide native traffic light buttons permanently (we draw custom ones)
    private func hideNativeTrafficLightsPermanently() {
        guard let window = window else { return }
        window.standardWindowButton(.closeButton)?.isHidden = true
        window.standardWindowButton(.miniaturizeButton)?.isHidden = true
        window.standardWindowButton(.zoomButton)?.isHidden = true
    }

    /// Remove Hyalo styling
    func teardown() {
        guard let window = window else { return }

        // NOTE: Sidebar child frame is managed by SidebarFrameManager

        // Remove header view
        teardownHeaderView()

        // Remove gradient view
        gradientView?.removeFromSuperview()
        gradientView = nil

        // Remove echo area view
        echoAreaView?.removeFromSuperview()
        echoAreaView = nil

        // Remove blur view
        blurView?.removeFromSuperview()
        blurView = nil

        // Remove tracking area
        if let trackingArea = trafficLightTrackingArea,
           let contentView = window.contentView {
            contentView.removeTrackingArea(trackingArea)
            trafficLightTrackingArea = nil
        }

        // Restore titlebar
        window.titlebarAppearsTransparent = false
        window.titleVisibility = .visible

        // Restore native traffic lights
        window.standardWindowButton(.closeButton)?.isHidden = false
        window.standardWindowButton(.miniaturizeButton)?.isHidden = false
        window.standardWindowButton(.zoomButton)?.isHidden = false
    }

    // MARK: - Blur View

    private func setupBlurView() {
        guard let window = window,
              let contentView = window.contentView else { return }

        // Create blur view
        let blur = NSVisualEffectView(frame: contentView.bounds)
        blur.autoresizingMask = [.width, .height]
        blur.blendingMode = .behindWindow
        blur.material = .hudWindow
        blur.state = .active
        blur.wantsLayer = true
        blur.layer?.cornerRadius = Self.cornerRadius
        blur.layer?.masksToBounds = true

        // Insert behind all other views
        contentView.addSubview(blur, positioned: .below, relativeTo: contentView.subviews.first)

        // Apply corner radius to window
        if let layer = contentView.layer {
            layer.cornerRadius = Self.cornerRadius
            layer.masksToBounds = true
        }

        blurView = blur
    }

    // MARK: - Gradient Overlay (Header Blend Effect)

    private func setupGradientView() {
        guard let window = window,
              let contentView = window.contentView else { return }

        let gradient = GradientOverlayView(frame: .zero)
        gradient.wantsLayer = true
        gradient.layer?.zPosition = 900  // Above Emacs content, below header
        gradient.backgroundColor = currentBackgroundColor
        gradient.gradientHeight = 30  // Height of the fade effect

        // Position at top
        updateGradientViewFrame(gradient, in: contentView)

        // Add on TOP of all views to composite over Emacs content
        contentView.addSubview(gradient, positioned: .above, relativeTo: nil)

        gradientView = gradient

        // Observe window resize
        NotificationCenter.default.addObserver(
            forName: NSWindow.didResizeNotification,
            object: window,
            queue: .main
        ) { [weak self, weak gradient, weak contentView] _ in
            guard let self = self, let gradient = gradient, let contentView = contentView else { return }
            self.updateGradientViewFrame(gradient, in: contentView)
        }
    }

    private func updateGradientViewFrame(_ gradient: NSView, in contentView: NSView) {
        // Position gradient at the VERY TOP of window content area
        // Text scrolls up and fades as it approaches the header
        let gradientHeight: CGFloat = 60  // Height of the fade zone

        // macOS coordinates: y=0 at bottom, so top is at bounds.height
        gradient.frame = NSRect(
            x: 0,
            y: contentView.bounds.height - gradientHeight,
            width: contentView.bounds.width,
            height: gradientHeight
        )
        gradient.autoresizingMask = [.width, .minYMargin]
    }

    private func getHeaderHeight() -> CGFloat {
        if #available(macOS 15.0, *) {
            return HeaderHostingController.headerHeight + headerTopPadding
        }
        return 56  // Fallback
    }

    // MARK: - Echo Area Overlay

    private func setupEchoAreaView() {
        guard let window = window,
              let contentView = window.contentView else { return }

        let echoView = EchoAreaOverlayView(frame: .zero)
        echoView.wantsLayer = true
        echoView.layer?.backgroundColor = NSColor.clear.cgColor
        echoView.layer?.zPosition = 1000  // On top of everything
        echoView.isDarkTheme = false
        echoView.tintOpacity = 0.2  // More visible

        // Start with 0 height - will be set from Emacs
        echoView.frame = NSRect(x: 0, y: 0, width: contentView.bounds.width, height: 0)
        echoView.autoresizingMask = [.width]

        // Add on top of all other views
        contentView.addSubview(echoView, positioned: .above, relativeTo: nil)

        echoAreaView = echoView
    }

    // MARK: - Traffic Lights (using NSTrackingArea, no timers)

    private func setupTrafficLightTracking() {
        guard let window = window,
              let contentView = window.contentView else { return }

        // Create tracking area in top-left corner
        let trackingRect = NSRect(
            x: 0,
            y: contentView.bounds.height - hoverAreaSize.height,
            width: hoverAreaSize.width,
            height: hoverAreaSize.height
        )

        let trackingArea = NSTrackingArea(
            rect: trackingRect,
            options: [.mouseEnteredAndExited, .activeAlways, .inVisibleRect],
            owner: self,
            userInfo: nil
        )

        contentView.addTrackingArea(trackingArea)
        trafficLightTrackingArea = trackingArea
    }

    @objc func mouseEntered(with event: NSEvent) {
        guard trafficLightsEnabled else { return }
        showTrafficLights()
    }

    @objc func mouseExited(with event: NSEvent) {
        guard trafficLightsEnabled else { return }
        hideTrafficLights()
    }

    func showTrafficLights() {
        guard let window = window else { return }

        NSAnimationContext.runAnimationGroup { context in
            context.duration = 0.2
            window.standardWindowButton(.closeButton)?.animator().alphaValue = 1
            window.standardWindowButton(.miniaturizeButton)?.animator().alphaValue = 1
            window.standardWindowButton(.zoomButton)?.animator().alphaValue = 1
        }
        trafficLightsVisible = true
    }

    func hideTrafficLights() {
        guard trafficLightsEnabled else { return }
        guard let window = window else { return }

        NSAnimationContext.runAnimationGroup { context in
            context.duration = 0.2
            window.standardWindowButton(.closeButton)?.animator().alphaValue = 0
            window.standardWindowButton(.miniaturizeButton)?.animator().alphaValue = 0
            window.standardWindowButton(.zoomButton)?.animator().alphaValue = 0
        }
        trafficLightsVisible = false
    }

    func setTrafficLightsAutoHide(_ enabled: Bool) {
        trafficLightsEnabled = enabled
        if enabled {
            hideTrafficLights()
        } else {
            showTrafficLights()
        }
    }

    deinit {
        // Tracking area is automatically removed when contentView is deallocated
    }

    // MARK: - Header View

    private func setupHeaderView() {
        guard let window = window else { return }

        if #available(macOS 15.0, *) {
            let controller = HeaderHostingController()
            controller.attach(to: window)
            headerController = controller
        }
    }

    private func teardownHeaderView() {
        if #available(macOS 15.0, *) {
            if let controller = headerController as? HeaderHostingController {
                controller.detach()
            }
        }
        headerController = nil
    }

    // MARK: - Sidebar Offset (for header position adjustment)
    // NOTE: Sidebar glass effect is now handled by SidebarFrameManager for child frames
    // This method only adjusts the header position when sidebar is visible

    /// Set the sidebar offset (width in pixels)
    /// This adjusts the header to not extend over the sidebar area
    func setSidebarOffset(_ offset: CGFloat) {
        sidebarOffset = offset

        // Update header position to account for sidebar
        if #available(macOS 15.0, *) {
            if let controller = headerController as? HeaderHostingController {
                // Adjust left padding to start after sidebar
                let leftPadding = offset > 0 ? offset + 12 : 78
                controller.setPosition(
                    top: headerTopPadding,
                    left: leftPadding,
                    right: 12
                )
            }
        }

        // Update gradient view position (offset from sidebar)
        updateGradientForSidebar()

        // NOTE: Echo area spans full width - no sidebar offset
    }

    private func updateGradientForSidebar() {
        guard let gradient = gradientView, let contentView = window?.contentView else { return }

        let gradientHeight: CGFloat = 60
        gradient.frame = NSRect(
            x: sidebarOffset,
            y: contentView.bounds.height - gradientHeight,
            width: contentView.bounds.width - sidebarOffset,
            height: gradientHeight
        )
    }

    /// Update header content from Emacs with formatted mode-line string
    func updateHeader(modeLineString: String) {
        if #available(macOS 15.0, *) {
            if let controller = headerController as? HeaderHostingController {
                controller.updateModeLine(modeLineString)
            }
        }
    }

    /// Update header-line content
    func updateHeaderLine(_ content: String) {
        if #available(macOS 15.0, *) {
            if let controller = headerController as? HeaderHostingController {
                controller.updateHeaderLine(content)
            }
        }
    }
    
    /// Show or hide the floating header view
    /// Used when NavigationSplitView is active (toolbar replaces floating header)
    func setHeaderHidden(_ hidden: Bool) {
        if #available(macOS 15.0, *) {
            if let controller = headerController as? HeaderHostingController {
                controller.setHidden(hidden)
            }
        }
    }
    
    /// Show or hide the gradient overlay view
    /// Hidden when NavigationSplitView handles its own gradient
    func setGradientHidden(_ hidden: Bool) {
        gradientView?.isHidden = hidden
    }

    /// Set header position
    func setHeaderPosition(top: CGFloat, left: CGFloat, right: CGFloat) {
        headerTopPadding = top
        if #available(macOS 15.0, *) {
            if let controller = headerController as? HeaderHostingController {
                controller.setPosition(top: top, left: left, right: right)
            }
        }
        // Update gradient position with new padding
        if let gradient = gradientView, let contentView = window?.contentView {
            updateGradientViewFrame(gradient, in: contentView)
        }
    }

    // MARK: - Background Color (for gradient and theme detection)

    /// Set the background color from Emacs frame
    /// Also determines if theme is dark based on luminance
    func setBackgroundColor(_ colorString: String) {
        guard let color = parseEmacsColor(colorString) else { return }
        currentBackgroundColor = color

        // Update gradient view
        gradientView?.backgroundColor = color
        gradientView?.needsDisplay = true

        // Determine if dark theme based on luminance
        let isDark = isColorDark(color)
        echoAreaView?.isDarkTheme = isDark
        echoAreaView?.needsDisplay = true
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

    /// Check if a color is dark based on perceived luminance
    private func isColorDark(_ color: NSColor) -> Bool {
        guard let rgb = color.usingColorSpace(.sRGB) else { return false }
        // Perceived luminance formula
        let luminance = 0.299 * rgb.redComponent + 0.587 * rgb.greenComponent + 0.114 * rgb.blueComponent
        return luminance < 0.5
    }

    // MARK: - Echo Area Height

    /// Set the echo area height from Emacs
    /// Height should be (window-pixel-height (minibuffer-window)) + window-divider width
    /// Echo area spans full width (no sidebar offset)
    func setEchoAreaHeight(_ height: CGFloat) {
        echoAreaHeight = height

        guard let echoView = echoAreaView,
              let contentView = window?.contentView else { return }

        // Add 1 pixel for the window divider line at the top of echo area
        let totalHeight = height + 1

        // Position at bottom of window, FULL WIDTH (no sidebar offset)
        echoView.frame = NSRect(
            x: 0,
            y: 0,
            width: contentView.bounds.width,
            height: totalHeight
        )
        echoView.setNeedsDisplay(echoView.bounds)
        echoView.displayIfNeeded()
    }

    /// Set the echo area tint opacity from Emacs
    func setEchoAreaTintOpacity(_ opacity: CGFloat) {
        echoAreaView?.tintOpacity = opacity
        echoAreaView?.needsDisplay = true
    }

    /// Set the theme mode for echo area overlay (affects tint color)
    func setEchoAreaDarkTheme(_ isDark: Bool) {
        echoAreaView?.isDarkTheme = isDark
        echoAreaView?.needsDisplay = true
    }
}
