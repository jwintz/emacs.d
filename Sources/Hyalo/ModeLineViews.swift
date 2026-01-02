// ModeLineViews.swift - Mode-line toolbar item view and segment view
// Copyright (C) 2025
// Target: macOS 26 Tahoe with Liquid Glass design

import AppKit
import SwiftUI

// MARK: - Custom NSToolbar for Modeline

/// Custom toolbar item identifier for the mode-line
extension NSToolbarItem.Identifier {
    static let modeLineItem = NSToolbarItem.Identifier("com.hyalo.modeline")
}

/// Custom view for the modeline toolbar item with proper layout
/// NO background - relies on toolbar's native glass effect
/// Uses autoresizing to fill available space
@available(macOS 26.0, *)
final class ModeLineToolbarItemView: NSView {

    // MARK: - Segment View

    final class SegmentView: NSTextField {
        var onClick: ((ModeLineSegment, NSView) -> Void)?
        let segment: ModeLineSegment

        /// Whether the segment is interactive (has menu or command)
        private var isInteractive: Bool {
            (segment.menuItems != nil && !segment.menuItems!.isEmpty) || segment.command != nil
        }

        init(segment: ModeLineSegment) {
            self.segment = segment
            super.init(frame: .zero)

            // Use attributed string for font fallback (Icons)
            self.attributedStringValue = SegmentView.createAttributedString(from: segment.text, side: segment.side)

            self.isBezeled = false
            self.drawsBackground = false
            self.isEditable = false
            self.isSelectable = false
            self.wantsLayer = true

            self.cell?.truncatesLastVisibleLine = true
            self.cell?.lineBreakMode = .byTruncatingTail

            // Allow tooltips and clicks to pass through naturally
            self.refusesFirstResponder = true

            // Tooltip
            if let help = segment.helpEcho, !help.isEmpty {
                self.toolTip = help
            }

            self.sizeToFit()
            self.invalidateIntrinsicContentSize()
        }

        required init?(coder: NSCoder) { fatalError("init(coder:) has not been implemented") }

        private var hoverTrackingArea: NSTrackingArea?

        override func updateTrackingAreas() {
            super.updateTrackingAreas()

            // Manage only our specific tracking area
            if let existing = hoverTrackingArea {
                removeTrackingArea(existing)
                hoverTrackingArea = nil
            }

            // Add new tracking area if needed (for hover and interaction)
            let hasInteraction = segment.menuItems != nil || segment.command != nil
            let hasTooltip = segment.helpEcho != nil && !segment.helpEcho!.isEmpty
            if hasInteraction || hasTooltip {
                let options: NSTrackingArea.Options = [.activeAlways, .mouseEnteredAndExited, .inVisibleRect, .mouseMoved]
                let trackingArea = NSTrackingArea(rect: bounds, options: options, owner: self, userInfo: nil)
                addTrackingArea(trackingArea)
                self.hoverTrackingArea = trackingArea
            }
        }

        override func mouseEntered(with event: NSEvent) {
            super.mouseEntered(with: event)
        }

        override func mouseExited(with event: NSEvent) {
            super.mouseExited(with: event)
        }

        override func mouseMoved(with event: NSEvent) {
            super.mouseMoved(with: event)
        }

        /// Cached Nerd Font for performance
        private static var cachedNerdFont: NSFont?
        private static var cachedNerdFontSize: CGFloat = 0

        /// Find a suitable Nerd Font (with caching)
        static func findNerdFont(size: CGFloat) -> NSFont? {
            // Return cached font if size matches
            if let cached = cachedNerdFont, cachedNerdFontSize == size {
                return cached
            }

            // Try common Nerd Font names - Symbols Nerd Font Mono is the dedicated icon font
            let names = [
                "Symbols Nerd Font Mono",
                "SymbolsNerdFontMono-Regular",
                "Symbols Nerd Font",
                "SauceCodePro Nerd Font Mono",
                "JetBrainsMono Nerd Font Mono",
                "JetBrainsMono Nerd Font",
                "MesloLGS NF"
            ]
            for name in names {
                if let font = NSFont(name: name, size: size) {
                    cachedNerdFont = font
                    cachedNerdFontSize = size
                    return font
                }
            }
            return nil
        }

        /// Create attributed string with Nerd Font fallback for PUA characters
        static func createAttributedString(from text: String, side: String) -> NSAttributedString {
            let fontSize: CGFloat = 11
            let primaryFont = ModeLineToolbarItemView.findModeLineFont(size: fontSize)
            // Use found Nerd Font or fallback to primary
            let iconFont = SegmentView.findNerdFont(size: fontSize) ?? primaryFont

            let color: NSColor = (side == "lhs") ? .labelColor : .secondaryLabelColor

            let attributed = NSMutableAttributedString(string: text)
            let fullRange = NSRange(location: 0, length: text.utf16.count)

            // Default attributes
            attributed.addAttributes([
                .font: primaryFont,
                .foregroundColor: color
            ], range: fullRange)

            // Build comprehensive Nerd Font icon character set:
            // - E000-F8FF: Private Use Area (main Nerd Font icons)
            // - F0000-FFFFF: Supplementary Private Use Area-A (extended icons)
            // - 23FB-23FE: Power symbols
            // - 2B58: Heavy circle (power symbol)
            var iconCharSet = CharacterSet(charactersIn: "\u{E000}"..."\u{F8FF}")
            iconCharSet.insert(charactersIn: "\u{F0000}"..."\u{FFFFF}")
            iconCharSet.insert(charactersIn: "\u{23FB}"..."\u{23FE}")
            iconCharSet.insert(Unicode.Scalar(0x2B58)!)

            // Scan for icon characters and apply Nerd Font
            if text.rangeOfCharacter(from: iconCharSet) != nil {
                let nsString = text as NSString
                var index = 0
                while index < nsString.length {
                    let range = nsString.rangeOfCharacter(from: iconCharSet, options: [], range: NSRange(location: index, length: nsString.length - index))
                    if range.location == NSNotFound {
                        break
                    }
                    // Apply icon font to this character
                    attributed.addAttribute(.font, value: iconFont, range: range)
                    index = range.location + range.length
                }
            }

            return attributed
        }

        override func mouseDown(with event: NSEvent) {
            if segment.menuItems != nil || segment.command != nil {
                onClick?(segment, self)
            } else {
                super.mouseDown(with: event)
            }
        }

        override func hitTest(_ point: NSPoint) -> NSView? {
            // Force hit test success if point is within bounds and we have interactions
            if (segment.menuItems != nil || segment.command != nil || (segment.helpEcho != nil && !segment.helpEcho!.isEmpty)) {
                let localPoint = convert(point, from: superview)
                if bounds.contains(localPoint) {
                    return self
                }
            }
            return super.hitTest(point)
        }

        override func resetCursorRects() {
            if segment.menuItems != nil || segment.command != nil {
                addCursorRect(bounds, cursor: .pointingHand)
            }
        }

        override func acceptsFirstMouse(for event: NSEvent?) -> Bool {
            return true
        }

        override var mouseDownCanMoveWindow: Bool {
            return false
        }
    }

    // MARK: - Properties

    /// Left-hand side stack view (internal for event monitoring access)
    let lhsStack: NSStackView = {
        let stack = NSStackView()
        stack.orientation = .horizontal
        stack.alignment = .centerY
        stack.spacing = 6  // Spacing between LHS segments
        stack.distribution = .fill
        stack.translatesAutoresizingMaskIntoConstraints = false
        // LHS can shrink
        stack.setContentCompressionResistancePriority(.defaultLow, for: .horizontal)
        return stack
    }()

    /// Right-hand side stack view (internal for event monitoring access)
    let rhsStack: NSStackView = {
        let stack = NSStackView()
        stack.orientation = .horizontal
        stack.alignment = .centerY
        stack.spacing = 6  // Spacing between RHS segments
        stack.distribution = .fill
        stack.translatesAutoresizingMaskIntoConstraints = false
        // RHS stays visible
        stack.setContentCompressionResistancePriority(.required, for: .horizontal)
        return stack
    }()

    /// Callback for mode-line clicks (fallback for empty space)
    var onModeLineClick: ((String, Double) -> Void)?

    /// Callback for specific segment click (includes view for positioning)
    var onSegmentClick: ((ModeLineSegment, NSView) -> Void)?

    // MARK: - Init

    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        setupView()
    }

    required init?(coder: NSCoder) {
        super.init(coder: coder)
        setupView()
    }

    private var parentTrackingArea: NSTrackingArea?

    private func setupView() {
        // NO background - toolbar provides glass effect
        wantsLayer = true
        layer?.backgroundColor = NSColor.clear.cgColor
        autoresizingMask = [.width, .height] // Fill container

        // Ensure we accept mouse events
        self.allowedTouchTypes = .direct

        addSubview(lhsStack)
        addSubview(rhsStack)

        NSLayoutConstraint.activate([
            lhsStack.leadingAnchor.constraint(equalTo: leadingAnchor, constant: 12),
            lhsStack.centerYAnchor.constraint(equalTo: centerYAnchor),
            lhsStack.topAnchor.constraint(greaterThanOrEqualTo: topAnchor),

            rhsStack.trailingAnchor.constraint(equalTo: trailingAnchor, constant: -12),
            rhsStack.centerYAnchor.constraint(equalTo: centerYAnchor),

            // Prevent overlap - LHS shrinks if needed
            rhsStack.leadingAnchor.constraint(greaterThanOrEqualTo: lhsStack.trailingAnchor, constant: 12)
        ])
    }

    override func updateTrackingAreas() {
        super.updateTrackingAreas()

        // Remove old tracking area
        if let existing = parentTrackingArea {
            removeTrackingArea(existing)
            parentTrackingArea = nil
        }

        // Add tracking area for the whole view to capture enter/exit
        let options: NSTrackingArea.Options = [.activeAlways, .mouseEnteredAndExited, .mouseMoved, .inVisibleRect]
        let trackingArea = NSTrackingArea(rect: bounds, options: options, owner: self, userInfo: nil)
        addTrackingArea(trackingArea)
        parentTrackingArea = trackingArea
    }

    override func hitTest(_ point: NSPoint) -> NSView? {
        return super.hitTest(point)
    }

    override func acceptsFirstMouse(for event: NSEvent?) -> Bool {
        return true
    }

    // Prevent the window from moving when clicking the toolbar items
    override var mouseDownCanMoveWindow: Bool {
        return false
    }

    // MARK: - Mouse Event Handling

    override func mouseDown(with event: NSEvent) {
        // Find the segment view under the click and forward the event
        if let hitView = hitTest(convert(event.locationInWindow, from: nil)) as? SegmentView {
            hitView.mouseDown(with: event)
        } else {
            super.mouseDown(with: event)
        }
    }

    override func mouseEntered(with event: NSEvent) {
        super.mouseEntered(with: event)
    }

    override func mouseExited(with event: NSEvent) {
        super.mouseExited(with: event)
    }

    override func mouseMoved(with event: NSEvent) {
        // Find segment under cursor and update hover state
        for case let segmentView as SegmentView in lhsStack.arrangedSubviews + rhsStack.arrangedSubviews {
            let segmentLocation = segmentView.convert(event.locationInWindow, from: nil)
            let isHovered = segmentView.bounds.contains(segmentLocation)
            if isHovered {
                // Trigger cursor update
                segmentView.window?.invalidateCursorRects(for: segmentView)
            }
        }
    }

    // MARK: - Update

    /// Flag to track if we have structured segments with menus
    private var hasStructuredSegments = false

    /// Update with raw strings (legacy/simple support)
    /// NOTE: This is a fallback - if we have structured JSON segments, skip this
    func updateContent(lhs: String, rhs: String) {
        // Don't overwrite structured segments with plain text
        if hasStructuredSegments {
            return
        }

        var segments: [ModeLineSegment] = []

        if !lhs.isEmpty {
            segments.append(ModeLineSegment(
                text: lhs,
                relStart: 0.0,
                relEnd: 0.5,
                helpEcho: nil,
                side: "lhs",
                menuItems: nil,
                command: nil
            ))
        }

        if !rhs.isEmpty {
            segments.append(ModeLineSegment(
                text: rhs,
                relStart: 0.5,
                relEnd: 1.0,
                helpEcho: nil,
                side: "rhs",
                menuItems: nil,
                command: nil
            ))
        }

        updateWithSegmentsInternal(segments)
    }

    func updateWithSegments(_ segments: [ModeLineSegment]) {
        // Mark that we have structured segments if any have menus or helpEcho
        let hasInteractiveContent = segments.contains { seg in
            (seg.menuItems != nil && !seg.menuItems!.isEmpty) ||
            seg.helpEcho != nil ||
            seg.command != nil
        }
        if hasInteractiveContent || segments.count > 2 {
            hasStructuredSegments = true
        }
        updateWithSegmentsInternal(segments)
    }

    private func updateWithSegmentsInternal(_ segments: [ModeLineSegment]) {
        // Clear stacks
        lhsStack.arrangedSubviews.forEach { $0.removeFromSuperview() }
        rhsStack.arrangedSubviews.forEach { $0.removeFromSuperview() }

        for segment in segments {
            let view = SegmentView(segment: segment)
            view.onClick = { [weak self] seg, segView in
                self?.onSegmentClick?(seg, segView)
            }

            if segment.side == "lhs" {
                lhsStack.addArrangedSubview(view)
            } else {
                rhsStack.addArrangedSubview(view)
            }
        }
    }

    // MARK: - Helpers

    static func findModeLineFont(size: CGFloat) -> NSFont {
        return NSFont(name: "SF Mono", size: size) ??
               NSFont(name: "Symbols Nerd Font Mono", size: size) ??
               NSFont(name: "Menlo", size: size) ??
               .monospacedSystemFont(ofSize: size, weight: .medium)
    }
}

// MARK: - SwiftUI Mode-line Views

/// Mode-line toolbar view with Liquid Glass styling
/// Uses full-width layout that spans the entire toolbar area
@available(macOS 26.0, *)
struct ModeLineToolbarView: View {
    let content: String

    init(content: String) {
        self.content = content
    }

    /// Parse a string into LHS and RHS by splitting on 3+ consecutive spaces
    private func parseSegments(_ input: String) -> (lhs: String, rhs: String) {
        let trimmed = input.trimmingCharacters(in: .whitespaces)
        if let range = trimmed.range(of: "   +", options: .regularExpression) {
            let lhs = String(trimmed[..<range.lowerBound]).trimmingCharacters(in: .whitespaces)
            let rhs = String(trimmed[range.upperBound...]).trimmingCharacters(in: .whitespaces)
            return (lhs, rhs)
        }
        return (trimmed, "")
    }

    var body: some View {
        let segments = parseSegments(content)
        HStack(spacing: 8) {
            // Left padding for traffic lights area (approximately 78px)
            Spacer()
                .frame(width: 78)

            Text(segments.lhs.isEmpty ? " " : segments.lhs)
                .font(.system(size: 11, design: .monospaced))
                .foregroundStyle(.primary)
                .lineLimit(1)
                .truncationMode(.tail)

            Spacer(minLength: 20)

            if !segments.rhs.isEmpty {
                Text(segments.rhs)
                    .font(.system(size: 11, design: .monospaced))
                    .foregroundStyle(.secondary)
                    .lineLimit(1)
                    .truncationMode(.head)
            }

            // Right margin
            Spacer()
                .frame(width: 12)
        }
        .frame(maxWidth: .infinity)
        .frame(height: 28)
    }
}

/// SwiftUI view for the modeline displayed in the titlebar accessory
/// Designed to sit alongside traffic lights and sidebar toggle
@available(macOS 26.0, *)
struct ModeLineTitlebarView: View {
    let content: String

    /// Parse a string into LHS and RHS by splitting on 3+ consecutive spaces
    private func parseSegments(_ input: String) -> (lhs: String, rhs: String) {
        let trimmed = input.trimmingCharacters(in: .whitespaces)
        if let range = trimmed.range(of: "   +", options: .regularExpression) {
            let lhs = String(trimmed[..<range.lowerBound]).trimmingCharacters(in: .whitespaces)
            let rhs = String(trimmed[range.upperBound...]).trimmingCharacters(in: .whitespaces)
            return (lhs, rhs)
        }
        return (trimmed, "")
    }

    var body: some View {
        let segments = parseSegments(content)
        HStack(spacing: 8) {
            // Left side content
            Text(segments.lhs.isEmpty ? " " : segments.lhs)
                .font(.system(size: 11, design: .monospaced))
                .foregroundStyle(.primary)
                .lineLimit(1)
                .truncationMode(.tail)

            Spacer(minLength: 20)

            // Right side content
            if !segments.rhs.isEmpty {
                Text(segments.rhs)
                    .font(.system(size: 11, design: .monospaced))
                    .foregroundStyle(.secondary)
                    .lineLimit(1)
                    .truncationMode(.head)
            }
        }
        .padding(.horizontal, 12)
        .padding(.vertical, 4)
        .frame(maxWidth: .infinity)
        .frame(height: 28)
    }
}
