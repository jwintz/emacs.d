// VibrancyViews.swift - Vibrancy and gradient views
// Copyright (C) 2025
// Target: macOS 26 Tahoe with Liquid Glass design

import AppKit
import SwiftUI

// MARK: - Vibrancy Background View

/// NSVisualEffectView wrapper for fine-grained blur control
@available(macOS 26.0, *)
struct VibrancyBackgroundView: NSViewRepresentable {
    var material: NSVisualEffectView.Material
    var blendingMode: NSVisualEffectView.BlendingMode
    var isActive: Bool

    func makeNSView(context: Context) -> NSVisualEffectView {
        let view = NSVisualEffectView()
        view.material = material
        view.blendingMode = blendingMode
        view.state = isActive ? .active : .inactive
        view.isEmphasized = true
        view.wantsLayer = true
        return view
    }

    func updateNSView(_ nsView: NSVisualEffectView, context: Context) {
        nsView.material = material
        nsView.blendingMode = blendingMode
        nsView.state = isActive ? .active : .inactive
    }
}

// MARK: - Gradient Fade View

/// Custom NSView for gradient fade that properly resizes with layout
/// Creates a Safari-like fade effect from toolbar down into content
@available(macOS 26.0, *)
final class GradientFadeView: NSView {
    var fadeColor: NSColor = .windowBackgroundColor {
        didSet { updateGradientColors() }
    }

    /// Target opacity at the top of the gradient (interpolates from 0 to this value)
    var topOpacity: CGFloat = 0.5 {
        didSet { updateGradientColors() }
    }

    private var gradientLayer: CAGradientLayer?

    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        setupGradient()
    }

    required init?(coder: NSCoder) {
        super.init(coder: coder)
        setupGradient()
    }

    private func setupGradient() {
        wantsLayer = true

        let gradient = CAGradientLayer()
        gradient.name = "toolbarFadeGradient"
        // CAGradientLayer uses Core Graphics coordinates: y=0 is BOTTOM, y=1 is TOP
        // We want solid at top fading to transparent at bottom
        gradient.startPoint = CGPoint(x: 0.5, y: 1.0)  // Top (visually)
        gradient.endPoint = CGPoint(x: 0.5, y: 0.0)    // Bottom (visually)
        gradient.locations = [0.0, 0.4, 1.0]
        layer?.addSublayer(gradient)
        gradientLayer = gradient
        updateGradientColors()
    }

    private func updateGradientColors() {
        // Scale the gradient stops by topOpacity (window's background alpha)
        // Base values: 0.85, 0.3, 0.0 - scaled by current window opacity
        let solidColor = fadeColor.withAlphaComponent(1.0 * topOpacity)
        let midColor = fadeColor.withAlphaComponent(0.5 * topOpacity)
        let clearColor = fadeColor.withAlphaComponent(0.0)
        gradientLayer?.colors = [solidColor.cgColor, midColor.cgColor, clearColor.cgColor]
    }

    override func layout() {
        super.layout()
        // Update gradient frame to match view bounds on every layout pass
        gradientLayer?.frame = bounds
    }
}

// MARK: - Toolbar Fade Overlay

/// Gradient fade overlay for toolbar area - Safari-like effect
/// This view spans the full window width and fades from solid at top to transparent at bottom
/// Positioned between tint layer and Emacs content for subtle toolbar blending
@available(macOS 26.0, *)
struct ToolbarFadeOverlay: NSViewRepresentable {
    var fadeColor: NSColor
    var topOpacity: CGFloat

    func makeNSView(context: Context) -> GradientFadeView {
        let view = GradientFadeView()
        view.fadeColor = fadeColor
        view.topOpacity = topOpacity
        return view
    }

    func updateNSView(_ nsView: GradientFadeView, context: Context) {
        nsView.fadeColor = fadeColor
        nsView.topOpacity = topOpacity
    }
}
