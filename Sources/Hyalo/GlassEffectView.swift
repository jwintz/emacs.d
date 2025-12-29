// GlassEffectView.swift - Specialized visual effect view for minibuffer
// Copyright (C) 2025

import AppKit
import SwiftUI
import Carbon.HIToolbox

// MARK: - Mini-Frame Tracker

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

        // 2. Identify the current content view
        guard let contentView = window.contentView else {
            return
        }

        // If we already have glass applied, just update appearance and return
        if contentView.identifier?.rawValue == "HyaloGlassEffect" {
            updateAppearance(for: window)
            return
        }
        if contentView is GlassEffectView {
            return
        }
        // Check if it's already an NSGlassEffectView
        if #available(macOS 26.0, *) {
            if contentView is NSGlassEffectView {
                return
            }
        }
        
        // The emacsView is the current content view
        let emacsView = contentView

        // Mark the glass ID for detection
        let glassId = NSUserInterfaceItemIdentifier("HyaloGlassEffect")

        // Inherit appearance from parent window (for light/dark mode)
        let parentAppearance = window.parent?.effectiveAppearance ?? window.effectiveAppearance

        if #available(macOS 26.0, *) {
            let windowFrame = NSRect(origin: .zero, size: window.frame.size)
            
            // Design constants - beautiful consistent spacing
            let outerPadding: CGFloat = 16      // Glass edge to content
            let headerHeight: CGFloat = 32      // Header bar height
            let headerPadding: CGFloat = 12     // Padding inside header area
            let contentPadding: CGFloat = 8     // Gap between header and content
            let cornerRadius: CGFloat = 14      // Content container corner radius
            let glassCornerRadius: CGFloat = 18 // Outer glass corner radius
            
            // Guard: ensure window has minimum valid size
            guard windowFrame.width >= 150 && windowFrame.height >= 100 else {
                return
            }
            
            // Create outer container to hold glass + content
            let outerContainer = NSView(frame: windowFrame)
            outerContainer.identifier = glassId
            outerContainer.wantsLayer = true
            outerContainer.autoresizingMask = [.width, .height]
            
            // 1. Glass effect view as background (fills entire container)
            let glassView = NSGlassEffectView(frame: windowFrame)
            glassView.cornerRadius = glassCornerRadius
            glassView.appearance = parentAppearance
            glassView.autoresizingMask = [.width, .height]
            glassView.style = .clear
            
            // Apply tint color from theme
            if let parentWindow = window.parent ?? findEmacsWindow() {
                let controller = NavigationSidebarManager.shared.getController(for: parentWindow)
                let tintColor = controller.state.backgroundColor.withAlphaComponent(controller.state.backgroundAlpha * 0.3)
                glassView.tintColor = tintColor
            }
            outerContainer.addSubview(glassView)
            
            // 2. Header - positioned at top with proper alignment
            // Header sits inside the outer padding, aligned with content container
            let headerY = windowFrame.height - outerPadding - headerHeight
            let headerView = NSTextField(labelWithString: "⌘ Command")
            headerView.identifier = NSUserInterfaceItemIdentifier("HyaloGlassHeader")
            headerView.font = NSFont.systemFont(ofSize: 13, weight: .semibold)
            headerView.appearance = parentAppearance
            headerView.textColor = NSColor.labelColor
            // Header text aligned with content container's left edge + internal padding
            headerView.frame = NSRect(
                x: outerPadding + headerPadding,
                y: headerY + (headerHeight - 16) / 2,  // Vertically centered
                width: windowFrame.width - (outerPadding * 2) - (headerPadding * 2),
                height: 16
            )
            headerView.autoresizingMask = [.width, .minYMargin]
            outerContainer.addSubview(headerView)
            
            // 3. Content container with theme background (rounded rect)
            // Positioned below header with consistent padding
            let contentX = outerPadding
            let contentY = outerPadding
            let contentWidth = windowFrame.width - (outerPadding * 2)
            let contentHeight = windowFrame.height - outerPadding - headerHeight - contentPadding - outerPadding
            
            // Ensure valid dimensions
            guard contentWidth >= 100 && contentHeight >= 30 else {
                return
            }
            
            let contentFrame = NSRect(x: contentX, y: contentY, width: contentWidth, height: contentHeight)
            let contentContainer = NSView(frame: contentFrame)
            contentContainer.identifier = NSUserInterfaceItemIdentifier("HyaloGlassContent")
            contentContainer.wantsLayer = true
            contentContainer.layer?.cornerRadius = cornerRadius
            contentContainer.layer?.masksToBounds = true
            contentContainer.autoresizingMask = [.width, .height]
            
            // Get background color from theme
            if let parentWindow = window.parent ?? findEmacsWindow() {
                let controller = NavigationSidebarManager.shared.getController(for: parentWindow)
                contentContainer.layer?.backgroundColor = controller.state.backgroundColor.cgColor
            } else {
                contentContainer.layer?.backgroundColor = NSColor.windowBackgroundColor.cgColor
            }
            outerContainer.addSubview(contentContainer)
            
            // 4. Emacs view fills content container
            let emacsFrame = NSRect(origin: .zero, size: CGSize(width: contentWidth, height: contentHeight))
            emacsView.frame = emacsFrame
            emacsView.autoresizingMask = [.width, .height]
            contentContainer.addSubview(emacsView)
            
            // Set minimum window size to prevent IOSurface errors
            window.minSize = NSSize(width: 300, height: 150)
            
            window.contentView = outerContainer
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
        guard let outerContainer = window.contentView,
              outerContainer.identifier?.rawValue == "HyaloGlassEffect" else {
            return
        }

        // Get appearance from NavigationSidebarController state (source of truth)
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
                newAppearance = NSApp.effectiveAppearance
            }
        } else {
            newAppearance = window.parent?.effectiveAppearance ?? NSApp.effectiveAppearance
        }

        guard let appearance = newAppearance else {
            return
        }

        // Update the outer container's appearance
        outerContainer.appearance = appearance

        // macOS 26+: Update glass view, header, and content container
        if #available(macOS 26.0, *) {
            // Glass view is first subview
            if let glassView = outerContainer.subviews.first as? NSGlassEffectView {
                glassView.appearance = appearance

                // Update tint color
                if let parentWindow = window.parent ?? findEmacsWindow() {
                    let controller = NavigationSidebarManager.shared.getController(for: parentWindow)
                    let tintColor = controller.state.backgroundColor.withAlphaComponent(controller.state.backgroundAlpha * 0.3)
                    glassView.tintColor = tintColor
                }
            }
            
            // Header is the view with identifier "HyaloGlassHeader"
            if let headerView = outerContainer.subviews.first(where: { $0.identifier?.rawValue == "HyaloGlassHeader" }) as? NSTextField {
                headerView.appearance = appearance
                // Force text color update by re-setting it
                headerView.textColor = NSColor.labelColor
            }
            
            // Content container has identifier "HyaloGlassContent"
            if let contentContainer = outerContainer.subviews.first(where: { $0.identifier?.rawValue == "HyaloGlassContent" }),
               let parentWindow = window.parent ?? findEmacsWindow() {
                let controller = NavigationSidebarManager.shared.getController(for: parentWindow)
                contentContainer.layer?.backgroundColor = controller.state.backgroundColor.cgColor
            }
        }

        // For fallback container, update visual effect view
        if let visualEffect = outerContainer.subviews.first as? NSVisualEffectView {
            visualEffect.appearance = appearance
            visualEffect.state = .inactive
            visualEffect.state = .active
        }

        // Force redraw
        outerContainer.needsDisplay = true
        window.display()
    }
}