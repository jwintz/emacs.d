// HyaloShared.swift - Shared utilities, constants, and components
// Copyright (C) 2025
// Target: macOS 26 Tahoe with Liquid Glass design

import AppKit
import SwiftUI

// MARK: - ModeLine Parsing

/// Utilities for parsing Emacs mode-line strings into display segments
enum ModeLineParser {

    /// Parse a mode-line string into LHS and RHS segments
    /// - Parameter input: The raw mode-line string from Emacs
    /// - Returns: A tuple containing (leftHandSide, rightHandSide) text
    ///
    /// The parsing splits on 3+ consecutive spaces, which creates a visual gap
    /// in doom-modeline that separates anchored left content from anchored right content.
    static func parseSegments(_ input: String) -> (lhs: String, rhs: String) {
        let trimmed = input.trimmingCharacters(in: .whitespaces)

        // Split on 3+ consecutive spaces (doom-modeline's :align-to spacer)
        if let range = trimmed.range(of: "   +", options: .regularExpression) {
            let lhs = String(trimmed[..<range.lowerBound]).trimmingCharacters(in: .whitespaces)
            let rhs = String(trimmed[range.upperBound...]).trimmingCharacters(in: .whitespaces)
            return (lhs, rhs)
        }

        return (trimmed, "")
    }
}

// MARK: - Design Constants

/// Design system constants for Hyalo UI components
enum HyaloDesign {

    /// Corner radii for various UI elements
    enum CornerRadius {
        /// Standard capsule/badge radius
        static let capsule: CGFloat = 14

        /// Glass effect outer container radius
        static let glass: CGFloat = 18

        /// Content container inner radius
        static let content: CGFloat = 14

        /// Menu/picker radius
        static let menu: CGFloat = 12

        /// Small button/radio radius
        static let small: CGFloat = 6
    }

    /// Padding and spacing values
    enum Padding {
        /// Standard horizontal padding for row content
        static let horizontal: CGFloat = 12

        /// Outer padding for glass containers
        static let outer: CGFloat = 16

        /// Sidebar horizontal padding
        static let sidebar: CGFloat = 14

        /// Compact padding for dense UI
        static let compact: CGFloat = 8

        /// Extra spacing between major sections
        static let section: CGFloat = 16
    }

    /// Standard height values
    enum Height {
        /// Mode-line row height
        static let modeLine: CGFloat = 24

        /// Header-line row height
        static let headerLine: CGFloat = 22

        /// Toolbar/tab bar standard height
        static let toolbar: CGFloat = 28

        /// Editor tab bar height
        static let tabBar: CGFloat = 28
    }

    /// Width values for specific components
    enum Width {
        /// Traffic lights button group (close, minimize, zoom)
        static let trafficLights: CGFloat = 69

        /// Sidebar/inspector toggle button width
        static let sidebarToggle: CGFloat = 47

        /// Minimum sidebar panel width
        static let sidebarMin: CGFloat = 200

        /// Ideal sidebar panel width
        static let sidebarIdeal: CGFloat = 280

        /// Maximum sidebar panel width
        static let sidebarMax: CGFloat = 400

        /// Minimum inspector panel width
        static let inspectorMin: CGFloat = 300

        /// Ideal inspector panel width
        static let inspectorIdeal: CGFloat = 400

        /// Maximum inspector panel width
        static let inspectorMax: CGFloat = 500
    }

    /// Spacing between elements
    enum Spacing {
        /// Tight spacing (e.g., icon labels)
        static let tight: CGFloat = 4

        /// Compact spacing (e.g., row items)
        static let compact: CGFloat = 8

        /// Standard spacing (e.g., section items)
        static let standard: CGFloat = 12

        /// Comfortable spacing (e.g., major sections)
        static let comfortable: CGFloat = 16

        /// Generous spacing (e.g., outer margins)
        static let generous: CGFloat = 24
    }

    /// Font sizes
    enum FontSize {
        /// Small label (e.g., secondary info)
        static let small: CGFloat = 9

        /// Caption (e.g., tertiary labels)
        static let caption: CGFloat = 10

        /// Body text (e.g., mode-line content)
        static let body: CGFloat = 11

        /// Emphasized text (e.g., headers)
        static let emphasized: CGFloat = 12

        /// Large text (e.g., section headers)
        static let large: CGFloat = 13
    }

    /// Icon sizes
    enum IconSize {
        /// Small icon (e.g., inline indicators)
        static let small: CGFloat = 10

        /// Medium icon (e.g., section headers)
        static let medium: CGFloat = 12

        /// Standard icon (e.g., toolbar buttons)
        static let standard: CGFloat = 14

        /// Large icon (e.g., placeholder graphics)
        static let large: CGFloat = 28
    }

    /// Animation durations
    enum Animation {
        /// Instant transition
        static let instant: Double = 0.0

        /// Quick feedback (e.g., hover)
        static let quick: Double = 0.1

        /// Standard transition (e.g., panel slide)
        static let standard: Double = 0.25

        /// Slow transition (e.g., complex layout)
        static let slow: Double = 0.3
    }
}

// MARK: - Geometry Tracking

/// A reusable view that tracks geometry changes and reports them via a callback
struct GeometrySizeTracker: View {
    /// Callback invoked when the tracked dimension changes
    let onChange: (CGFloat) -> Void

    /// Which dimension to track
    let dimension: Dimension

    /// Whether to call an additional callback when size changes
    let onUpdate: (() -> Void)?

    enum Dimension {
        case width
        case height
    }

    init(
        dimension: Dimension = .width,
        onChange: @escaping (CGFloat) -> Void,
        onUpdate: (() -> Void)? = nil
    ) {
        self.dimension = dimension
        self.onChange = onChange
        self.onUpdate = onUpdate
    }

    var body: some View {
        GeometryReader { geometry in
            Color.clear
                .onAppear {
                    let size = dimension == .width ? geometry.size.width : geometry.size.height
                    onChange(size)
                }
                .onChange(of: dimension == .width ? geometry.size.width : geometry.size.height) { _, newSize in
                    onChange(newSize)
                    onUpdate?()
                }
        }
    }
}

/// Convenience initializer for width tracking
extension GeometrySizeTracker {
    init(dimension: Dimension = .width, onChange: @escaping (CGFloat) -> Void) {
        self.init(dimension: dimension, onChange: onChange, onUpdate: nil)
    }
}

// MARK: - Button Styles

/// Custom button style for traffic light buttons with hover and press states
struct TrafficLightButtonStyle: ButtonStyle {
    let color: Color

    @State private var isHovering = false

    func makeBody(configuration: Configuration) -> some View {
        Circle()
            .fill(color)
            .overlay(
                Circle()
                    .strokeBorder(Color.black.opacity(0.2), lineWidth: 0.5)
            )
            .scaleEffect(configuration.isPressed ? 0.85 : (isHovering ? 1.1 : 1.0))
            .animation(.easeInOut(duration: HyaloDesign.Animation.quick), value: isHovering)
            .animation(.easeInOut(duration: 0.05), value: configuration.isPressed)
            .onHover { isHovering = $0 }
    }
}

/// Custom button style for icon buttons (e.g., toolbar toggles)
struct IconButtonStyle: ButtonStyle {
    let isActive: Bool
    let size: CGSize

    @State private var isHovering = false

    init(isActive: Bool = false, size: CGSize = CGSize(width: 24, height: 24)) {
        self.isActive = isActive
        self.size = size
    }

    func makeBody(configuration: Configuration) -> some View {
        configuration.label
            .frame(width: size.width, height: size.height)
            .background(isHovering ? Color.primary.opacity(0.08) : Color.clear)
            .cornerRadius(HyaloDesign.CornerRadius.small)
            .foregroundStyle(
                isActive ? Color.accentColor : Color.primary
            )
            .onHover { isHovering = $0 }
    }
}

// MARK: - Subview Components
// Note: ModeLineSegmentView was considered but not needed.
// ModeLineTextView from AttributedTextView.swift handles click gestures directly.

