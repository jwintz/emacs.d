// GlassEffectView.swift - Specialized visual effect view for minibuffer
// Copyright (C) 2025

import AppKit

/// Tracks active mini-frame for modal behavior enforcement
enum MiniFrameTracker {
    /// The currently active mini-frame window (if any)
    static weak var activeMiniFrame: NSWindow?

    /// Observer for window becoming key
    static var windowObserver: Any?

    /// Start tracking a mini-frame as active (modal)
    static func activate(_ window: NSWindow) {
        activeMiniFrame = window

        // Remove any existing observer
        if let observer = windowObserver {
            NotificationCenter.default.removeObserver(observer)
        }

        // Add observer to enforce focus back to mini-frame
        windowObserver = NotificationCenter.default.addObserver(
            forName: NSWindow.didBecomeKeyNotification,
            object: nil,
            queue: .main
        ) { notification in
            guard let keyWindow = notification.object as? NSWindow,
                  let miniFrame = activeMiniFrame,
                  keyWindow !== miniFrame,
                  miniFrame.isVisible else {
                return
            }

            // Another window became key while mini-frame is active - refocus mini-frame
            DispatchQueue.main.async {
                miniFrame.makeKeyAndOrderFront(nil)
            }
        }
    }

    /// Deactivate mini-frame tracking
    static func deactivate() {
        activeMiniFrame = nil
        if let observer = windowObserver {
            NotificationCenter.default.removeObserver(observer)
            windowObserver = nil
        }
    }

    /// Check if a mini-frame is currently active
    static var isActive: Bool {
        return activeMiniFrame?.isVisible ?? false
    }
}

/// A specialized Visual Effect View to provide the "Glass" look for the minibuffer.
@available(macOS 10.14, *)
final class GlassEffectView: NSVisualEffectView {

    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        setup()
    }

    required init?(coder: NSCoder) {
        super.init(coder: coder)
        setup()
    }

    private func setup() {
        // Configure for the "Glass" look
        // .hudWindow provides a nice, slightly lighter glass effect often used for floating panels
        // .headerView is more transparent/modern
        self.material = .hudWindow
        self.blendingMode = .behindWindow
        self.state = .active
        self.autoresizingMask = [.width, .height]
    }

    /// Attach this glass effect to a window as its background
    static func attach(to window: NSWindow) {
        // 1. Make the window itself fully transparent
        window.isOpaque = false
        window.backgroundColor = .clear
        window.hasShadow = false  // Remove shadow for cleaner glass look

        // For child frames, also try to make titlebar transparent
        if window.parent != nil {
            // Child frame - ensure it's truly transparent
            window.titlebarAppearsTransparent = true
            window.titleVisibility = .hidden
        }

        // 2. Identify the Emacs view (current content view)
        guard let emacsView = window.contentView else {
            return
        }

        // If we already swapped, don't do it again
        if emacsView is GlassEffectView {
            return
        }
        if emacsView.identifier?.rawValue == "HyaloGlassEffect" {
            return
        }

        // 3. True Liquid Glass: Use NSGlassEffectView.contentView as per WWDC2025-310
        // "avoid placing the NSGlassEffectView behind your content as a sibling view"
        let glassId = NSUserInterfaceItemIdentifier("HyaloGlassEffect")

        // Inherit appearance from parent window (for light/dark mode)
        let parentAppearance = window.parent?.effectiveAppearance ?? window.effectiveAppearance

        if #available(macOS 26.0, *) {
            // True Liquid Glass: set emacsView as contentView
            let glassView = NSGlassEffectView(frame: emacsView.frame)
            glassView.identifier = glassId
            glassView.cornerRadius = 12
            glassView.appearance = parentAppearance
            glassView.autoresizingMask = [.width, .height]

            // Use .clear style for less vibrancy/more transparency
            glassView.style = .clear // .clear or .regular

            // Apply tint color from theme (via NavigationSidebarController state)
            if let parentWindow = window.parent ?? findEmacsWindow() {
                let controller = NavigationSidebarManager.shared.getController(for: parentWindow)
                let tintColor = controller.state.backgroundColor.withAlphaComponent(controller.state.backgroundAlpha * 0.3)
                glassView.tintColor = tintColor
            }

            // IMPORTANT: Set emacsView as contentView for proper Liquid Glass effect
            // The glass material shows through transparent areas of the content
            emacsView.autoresizingMask = [.width, .height]
            glassView.contentView = emacsView

            window.contentView = glassView
        } else {
            // Fallback: NSVisualEffectView for older macOS
            let container = NSView(frame: emacsView.frame)
            container.wantsLayer = true
            container.layer?.masksToBounds = true
            container.layer?.cornerRadius = 12
            container.identifier = glassId
            container.appearance = parentAppearance

            let visualEffect = NSVisualEffectView(frame: container.bounds)
            visualEffect.material = .popover
            visualEffect.blendingMode = .behindWindow
            visualEffect.state = .active
            visualEffect.appearance = parentAppearance
            visualEffect.autoresizingMask = [.width, .height]
            container.addSubview(visualEffect)

            emacsView.frame = container.bounds
            emacsView.autoresizingMask = [.width, .height]
            container.addSubview(emacsView)

            window.contentView = container
        }
    }

    /// Update appearance on an existing glass-enabled window
    static func updateAppearance(for window: NSWindow) {
        guard let glassContainer = window.contentView,
              glassContainer.identifier?.rawValue == "HyaloGlassEffect" else {
            return
        }

        // Get appearance from NavigationSidebarController state (source of truth)
        // This ensures we match what the AppearancePanel set
        var newAppearance: NSAppearance?
        if #available(macOS 26.0, *), let parentWindow = window.parent ?? findEmacsWindow() {
            let controller = NavigationSidebarManager.shared.getController(for: parentWindow)
            let appearanceMode = controller.state.windowAppearance
            switch appearanceMode {
            case "light":
                newAppearance = NSAppearance(named: .aqua)
            case "dark":
                newAppearance = NSAppearance(named: .darkAqua)
            default:
                // "auto" - use system appearance
                newAppearance = NSApp.effectiveAppearance
            }
        } else {
            newAppearance = window.parent?.effectiveAppearance ?? NSApp.effectiveAppearance
        }

        guard let appearance = newAppearance else {
            return
        }

        // Update the glass view's appearance
        glassContainer.appearance = appearance

        // If it's an NSGlassEffectView (macOS 26+), update directly
        if #available(macOS 26.0, *) {
            if let glassView = glassContainer as? NSGlassEffectView {
                glassView.appearance = appearance

                // Also update tint color from controller state
                if let parentWindow = window.parent ?? findEmacsWindow() {
                    let controller = NavigationSidebarManager.shared.getController(for: parentWindow)
                    let tintColor = controller.state.backgroundColor.withAlphaComponent(controller.state.backgroundAlpha * 0.3)
                    glassView.tintColor = tintColor
                }
            }
        }

        // For fallback container, update visual effect view
        if let visualEffect = glassContainer.subviews.first as? NSVisualEffectView {
            visualEffect.appearance = appearance
            visualEffect.state = .inactive
            visualEffect.state = .active
        }

        // Force redraw
        glassContainer.needsDisplay = true
        window.display()
    }
}
