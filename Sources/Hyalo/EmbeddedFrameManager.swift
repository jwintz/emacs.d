// EmbeddedFrameManager.swift - Manages embedded child-frame NSViews
// Copyright (C) 2025

import AppKit
import SwiftUI

/// Manages detaching child-frame NSViews and embedding them in SwiftUI
@available(macOS 26.0, *)
final class EmbeddedFrameManager {
    static let shared = EmbeddedFrameManager()

    /// Stored detached views per slot
    private var embeddedViews: [String: EmbeddedFrameView] = [:]

    struct EmbeddedFrameView {
        weak var originalWindow: NSWindow?
        let contentView: NSView
        let slot: String
    }

    private init() {}

    // MARK: - Frame Registration (called from Emacs - now a no-op)

    /// Registration is now a no-op - we find child-frames directly in embedFrames
    func registerFrame(slot: String, windowId: String, parentWindow: NSWindow) {
        // Store the slot name for later use
        pendingSlots.insert(slot)
    }

    /// Pending slots waiting to be embedded
    private var pendingSlots: Set<String> = []

    // MARK: - Frame Embedding

    /// Find off-screen child-frame windows (EmacsWindows positioned at x < -5000)
    private func findOffScreenChildFrames(parentWindow: NSWindow) -> [NSWindow] {
        var results: [NSWindow] = []

        for window in NSApp.windows {
            let className = String(describing: type(of: window))

            // Must be EmacsWindow
            guard className.contains("EmacsWindow") else { continue }

            // Skip the parent window only (the main Emacs window we're embedding into)
            if window === parentWindow { continue }

            // Skip already embedded windows
            if embeddedViews.values.contains(where: { $0.originalWindow === window }) {
                continue
            }

            // Check if off-screen (child-frames are created at x=-10000)
            let isOffScreen = window.frame.origin.x < -5000
            if isOffScreen {
                results.append(window)
            }
        }

        return results
    }

    /// Embed child-frames for the specified panel
    /// Scans all windows to find off-screen child-frames and embeds them
    /// Returns true if any frames were embedded
    func embedFrames(for panel: String, parentWindow: NSWindow) -> Bool {
        // Find all off-screen child-frames
        let childFrames = findOffScreenChildFrames(parentWindow: parentWindow)
        var embedded = false

        // Determine slots based on panel
        // "right" panel -> "right" slot (one window)
        // "left" panel -> "left-top" and "left-bottom" slots (two windows)
        var availableSlots: [String]
        if panel == "right" {
            availableSlots = ["right"]
        } else if panel == "left" {
            availableSlots = ["left-top", "left-bottom"]
        } else {
            availableSlots = [panel]
        }

        // Filter out already-embedded slots
        availableSlots = availableSlots.filter { embeddedViews[$0] == nil }

        // Match child-frames to slots (in order found)
        for (index, childWindow) in childFrames.enumerated() {
            guard index < availableSlots.count else { break }

            let slot = availableSlots[index]

            // Get the content view
            guard let contentView = childWindow.contentView else { continue }

            // Clear first responder before removing view
            childWindow.makeFirstResponder(nil)

            // Remove from original window
            contentView.removeFromSuperview()

            // Store reference
            embeddedViews[slot] = EmbeddedFrameView(
                originalWindow: childWindow,
                contentView: contentView,
                slot: slot
            )

            // Hide the original window (keep frame alive)
            childWindow.orderOut(nil)

            // Notify NavigationSidebarController
            NavigationSidebarManager.shared.setEmbeddedView(view: contentView, slot: slot, for: parentWindow)

            embedded = true
        }

        // Clear pending slots for this panel
        pendingSlots = pendingSlots.filter { !$0.hasPrefix(panel) }

        return embedded
    }

    /// Return embedded views to their original windows
    func detachFrames(for panel: String) {
        let slotsToDetach = embeddedViews.keys.filter { $0.hasPrefix(panel) }

        for slot in slotsToDetach {
            guard let embedded = embeddedViews[slot] else { continue }

            if let window = embedded.originalWindow {
                // Remove from SwiftUI container
                embedded.contentView.removeFromSuperview()

                // Return to original window
                window.contentView = embedded.contentView

                // Show window again (off-screen)
                window.setFrame(NSRect(x: -10000, y: -10000, width: 400, height: 300), display: false)
                window.orderBack(nil)
            }

            embeddedViews.removeValue(forKey: slot)
        }

        // Clear NavigationSidebarState via manager
        if let mainWindow = findEmacsWindow() {
            for slot in slotsToDetach {
                NavigationSidebarManager.shared.clearEmbeddedView(slot: slot, for: mainWindow)
            }
        }
    }

    /// Get the embedded view for a slot
    func getView(for slot: String) -> NSView? {
        embeddedViews[slot]?.contentView
    }

    /// Check if a slot has an embedded view
    func hasView(for slot: String) -> Bool {
        embeddedViews[slot] != nil
    }

    /// Clear registration for a panel (called before re-setup)
    func clearRegistration(for panel: String) {
        pendingSlots = pendingSlots.filter { !$0.hasPrefix(panel) }
    }
}
