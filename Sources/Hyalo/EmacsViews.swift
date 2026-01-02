// EmacsViews.swift - Emacs content and container views
// Copyright (C) 2025
// Target: macOS 26 Tahoe with Liquid Glass design

import AppKit
import SwiftUI

// MARK: - Embedded Emacs View

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

// MARK: - Emacs Content View

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

// MARK: - Emacs Container View

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

// MARK: - Emacs NSViewRepresentable

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
