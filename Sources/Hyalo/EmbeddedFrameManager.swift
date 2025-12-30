// EmbeddedFrameManager.swift - Manages embedded child-frame NSViews
// Copyright (C) 2025

import AppKit
import SwiftUI

// MARK: - Event Forwarding Container

/// A container view that forwards all events to a hidden original EmacsWindow.
/// This solves the problem of detached NSViews losing their connection to Emacs' event loop.
/// When an Emacs child-frame's contentView is removed and embedded in SwiftUI, events no longer
/// reach the Emacs event loop. This container intercepts events and forwards them to the
/// original hidden window using NSWindow.sendEvent().
@available(macOS 26.0, *)
final class EmacsEventForwardingContainer: NSView {

    // MARK: - Properties

    /// The embedded Emacs content view
    weak var embeddedView: NSView?

    /// The original hidden EmacsWindow
    weak var originalWindow: NSWindow?

    /// Cached window number for event creation
    private var cachedWindowNumber: Int = 0

    /// Tracking area for mouse events
    private var eventTrackingArea: NSTrackingArea?

    // MARK: - Configuration

    func configure(embeddedView: NSView, originalWindow: NSWindow) {
        self.embeddedView = embeddedView
        self.originalWindow = originalWindow
        self.cachedWindowNumber = originalWindow.windowNumber

        // Setup view
        wantsLayer = true
        layer?.backgroundColor = .clear

        // Add embedded view
        embeddedView.translatesAutoresizingMaskIntoConstraints = false
        addSubview(embeddedView)

        NSLayoutConstraint.activate([
            embeddedView.leadingAnchor.constraint(equalTo: leadingAnchor),
            embeddedView.trailingAnchor.constraint(equalTo: trailingAnchor),
            embeddedView.topAnchor.constraint(equalTo: topAnchor),
            embeddedView.bottomAnchor.constraint(equalTo: bottomAnchor)
        ])

        // Ensure we can receive events
        setupEventTracking()
    }

    private func setupEventTracking() {
        if let existing = eventTrackingArea {
            removeTrackingArea(existing)
        }

        eventTrackingArea = NSTrackingArea(
            rect: bounds,
            options: [
                .activeInKeyWindow,
                .activeAlways,
                .mouseMoved,
                .mouseEnteredAndExited,
                .inVisibleRect
            ],
            owner: self,
            userInfo: nil
        )
        addTrackingArea(eventTrackingArea!)
    }

    override func updateTrackingAreas() {
        super.updateTrackingAreas()
        setupEventTracking()
    }

    // MARK: - First Responder

    override var acceptsFirstResponder: Bool { true }

    override func becomeFirstResponder() -> Bool {
        return true
    }

    override func resignFirstResponder() -> Bool {
        return true
    }

    // MARK: - Event Forwarding Infrastructure

    /// Convert a point from our view to the original window's content view coordinates.
    /// Since we detached the contentView wholesale, coordinates map 1:1.
    private func mapToOriginalWindow(_ localPoint: NSPoint) -> NSPoint {
        return localPoint
    }

    /// Forward a mouse event to the original hidden window
    private func forwardMouseEvent(_ event: NSEvent) {
        guard let originalWindow = originalWindow else { return }

        // Get point in our view coordinates
        let localPoint = convert(event.locationInWindow, from: nil)

        // Map to original window coordinates (for detached contentView, this is 1:1)
        let mappedPoint = mapToOriginalWindow(localPoint)

        // Create new event targeting original window
        guard let forwardedEvent = NSEvent.mouseEvent(
            with: event.type,
            location: mappedPoint,
            modifierFlags: event.modifierFlags,
            timestamp: event.timestamp,
            windowNumber: cachedWindowNumber,
            context: nil,
            eventNumber: event.eventNumber,
            clickCount: event.clickCount,
            pressure: event.pressure
        ) else { return }

        // Send directly to the window
        originalWindow.sendEvent(forwardedEvent)
    }

    /// Forward a keyboard event to the original hidden window
    private func forwardKeyEvent(_ event: NSEvent) {
        guard let originalWindow = originalWindow else { return }

        // Create new event targeting original window
        guard let forwardedEvent = NSEvent.keyEvent(
            with: event.type,
            location: event.locationInWindow,
            modifierFlags: event.modifierFlags,
            timestamp: event.timestamp,
            windowNumber: cachedWindowNumber,
            context: nil,
            characters: event.characters ?? "",
            charactersIgnoringModifiers: event.charactersIgnoringModifiers ?? "",
            isARepeat: event.isARepeat,
            keyCode: event.keyCode
        ) else { return }

        originalWindow.sendEvent(forwardedEvent)
    }

    /// Forward a scroll event to the original hidden window
    private func forwardScrollEvent(_ event: NSEvent) {
        guard let originalWindow = originalWindow else { return }

        let localPoint = convert(event.locationInWindow, from: nil)
        let mappedPoint = mapToOriginalWindow(localPoint)

        // For scroll events, use CGEvent for full fidelity
        if let cgEvent = event.cgEvent?.copy() {
            cgEvent.location = CGPoint(x: mappedPoint.x, y: mappedPoint.y)

            if let forwardedEvent = NSEvent(cgEvent: cgEvent) {
                originalWindow.sendEvent(forwardedEvent)
                return
            }
        }

        // Fallback: forward original event
        originalWindow.sendEvent(event)
    }

    // MARK: - Mouse Event Handlers

    override func mouseDown(with event: NSEvent) {
        forwardMouseEvent(event)
    }

    override func mouseUp(with event: NSEvent) {
        forwardMouseEvent(event)
    }

    override func mouseDragged(with event: NSEvent) {
        forwardMouseEvent(event)
    }

    override func mouseMoved(with event: NSEvent) {
        forwardMouseEvent(event)
    }

    override func mouseEntered(with event: NSEvent) {
        forwardMouseEvent(event)
    }

    override func mouseExited(with event: NSEvent) {
        forwardMouseEvent(event)
    }

    override func rightMouseDown(with event: NSEvent) {
        forwardMouseEvent(event)
    }

    override func rightMouseUp(with event: NSEvent) {
        forwardMouseEvent(event)
    }

    override func rightMouseDragged(with event: NSEvent) {
        forwardMouseEvent(event)
    }

    override func otherMouseDown(with event: NSEvent) {
        forwardMouseEvent(event)
    }

    override func otherMouseUp(with event: NSEvent) {
        forwardMouseEvent(event)
    }

    override func otherMouseDragged(with event: NSEvent) {
        forwardMouseEvent(event)
    }

    override func scrollWheel(with event: NSEvent) {
        forwardScrollEvent(event)
    }

    // MARK: - Keyboard Event Handlers

    override func keyDown(with event: NSEvent) {
        forwardKeyEvent(event)
    }

    override func keyUp(with event: NSEvent) {
        forwardKeyEvent(event)
    }

    override func flagsChanged(with event: NSEvent) {
        forwardKeyEvent(event)
    }

    // MARK: - Gesture Events

    override func magnify(with event: NSEvent) {
        originalWindow?.sendEvent(event)
    }

    override func rotate(with event: NSEvent) {
        originalWindow?.sendEvent(event)
    }

    override func swipe(with event: NSEvent) {
        originalWindow?.sendEvent(event)
    }

    override func smartMagnify(with event: NSEvent) {
        originalWindow?.sendEvent(event)
    }
}

// MARK: - Embedded Frame Manager

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

            // Notify NavigationSidebarController (pass originalWindow for event forwarding)
            NavigationSidebarManager.shared.setEmbeddedView(view: contentView, slot: slot, for: parentWindow, originalWindow: childWindow)

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

    /// Get the embedded view AND its original window for a slot
    /// Used by EmbeddedEmacsView to set up event forwarding
    func getViewWithWindow(for slot: String) -> (view: NSView, window: NSWindow?)? {
        guard let embedded = embeddedViews[slot] else { return nil }
        return (embedded.contentView, embedded.originalWindow)
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
