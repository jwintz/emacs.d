// FooterPattern.swift - Footer pattern types and views
// Copyright (C) 2025
// Target: macOS 26 Tahoe with Liquid Glass design

import AppKit
import SwiftUI

// MARK: - Footer Pattern Types

/// Available footer patterns from heropatterns.com
enum FooterPattern: String, CaseIterable {
    case none = "none"
    case hideout = "hideout"
    case hexagons = "hexagons"
    case tinyCheckers = "tiny-checkers"
    case plus = "plus"
    case cage = "cage"
    case diagonalStripes = "diagonal-stripes"
    case stripes = "stripes"
    case diagonalLines = "diagonal-lines"
    case signal = "signal"
    case wallpaper = "wallpaper"

    /// SVG path data and viewBox for each pattern
    var svgData: (path: String, width: CGFloat, height: CGFloat) {
        switch self {
        case .none:
            return ("", 40, 40)
        case .hideout:
            // ViewBox: 0 0 40 40
            return ("M0 38.59l2.83-2.83 1.41 1.41L1.41 40H0v-1.41zM0 1.4l2.83 2.83 1.41-1.41L1.41 0H0v1.41zM38.59 40l-2.83-2.83 1.41-1.41L40 38.59V40h-1.41zM40 1.41l-2.83 2.83-1.41-1.41L38.59 0H40v1.41zM20 18.6l2.83-2.83 1.41 1.41L21.41 20l2.83 2.83-1.41 1.41L20 21.41l-2.83 2.83-1.41-1.41L18.59 20l-2.83-2.83 1.41-1.41L20 18.59z", 40, 40)
        case .hexagons:
            // ViewBox: 0 0 28 49
            return ("M13.99 9.25l13 7.5v15l-13 7.5L1 31.75v-15l12.99-7.5zM3 17.9v12.7l10.99 6.34 11-6.35V17.9l-11-6.34L3 17.9zM0 15l12.98-7.5V0h-2v6.35L0 12.69v2.3zm0 18.5L12.98 41v8h-2v-6.85L0 35.81v-2.3zM15 0v7.5L27.99 15H28v-2.31h-.01L17 6.35V0h-2zm0 49v-8l12.99-7.5H28v2.31h-.01L17 42.15V49h-2z", 28, 49)
        case .tinyCheckers:
            // ViewBox: 0 0 8 8
            return ("M0 0h4v4H0V0zm4 4h4v4H4V4z", 8, 8)
        case .plus:
            // ViewBox: 0 0 60 60
            return ("M36 34v-4h-2v4h-4v2h4v4h2v-4h4v-2h-4zm0-30V0h-2v4h-4v2h4v4h2V6h4V4h-4zM6 34v-4H4v4H0v2h4v4h2v-4h4v-2H6zM6 4V0H4v4H0v2h4v4h2V6h4V4H6z", 60, 60)
        case .cage:
            // ViewBox: 0 0 36 36 (simplified version)
            return ("M0 0h36v36H0V0zm2 2v32h32V2H2zm7 7h18v18H9V9zm2 2v14h14V11H11z", 36, 36)
        case .diagonalStripes:
            // ViewBox: 0 0 40 40
            return ("M0 40L40 0H20L0 20M40 40V20L20 40", 40, 40)
        case .stripes:
            // ViewBox: 0 0 40 1
            return ("M0 0h20v1H0z", 40, 1)
        case .diagonalLines:
            // ViewBox: 0 0 6 6
            return ("M5 0h1L0 6V5zM6 5v1H5z", 6, 6)
        case .signal:
            // ViewBox: 0 0 84 48
            return ("M0 0h12v6H0V0zm28 8h12v6H28V8zm14-8h12v6H42V0zm14 0h12v6H56V0zm0 8h12v6H56V8zM42 8h12v6H42V8zm0 16h12v6H42v-6zm14-8h12v6H56v-6zm14 0h12v6H70v-6zm0-16h12v6H70V0zM28 32h12v6H28v-6zM14 16h12v6H14v-6zM0 24h12v6H0v-6zm0 8h12v6H0v-6zm14 0h12v6H14v-6zm14 8h12v6H28v-6zm-14 0h12v6H14v-6zm28 0h12v6H42v-6zm14-8h12v6H56v-6zm0-8h12v6H56v-6zm14 8h12v6H70v-6zm0 8h12v6H70v-6zM14 24h12v6H14v-6zm14-8h12v6H28v-6zM14 8h12v6H14V8zM0 8h12v6H0V8z", 84, 48)
        case .wallpaper:
            // ViewBox: 0 0 84 48 (uses plus signs and bars)
            return ("M78 7V4h-2v3h-3v2h3v3h2V9h3V7h-3zM30 7V4h-2v3h-3v2h3v3h2V9h3V7h-3zM10 0h2v16h-2V0zm6 0h4v16h-4V0zM2 0h4v16H2V0zm50 0h2v16h-2V0zM38 0h2v16h-2V0zm28 0h2v16h-2V0zm-8 0h6v16h-6V0zM42 0h6v16h-6V0z", 84, 16)
        }
    }

    /// Whether pattern uses stroke (lines) vs fill (shapes)
    var usesStroke: Bool {
        switch self {
        case .diagonalLines, .diagonalStripes:
            return false  // These use fill
        default:
            return false
        }
    }
}

// MARK: - Footer Pattern Layer

/// A view that draws a tiled pattern at the bottom of the window
/// positioned to overlay the echo area/minibuffer region
@available(macOS 26.0, *)
struct FooterPatternLayer: View {
    var pattern: FooterPattern
    var height: CGFloat
    var tintColor: NSColor
    var backgroundAlpha: CGFloat  // Alpha for background tint
    var patternAlpha: CGFloat     // Alpha for pattern foreground
    var isDarkMode: Bool

    var body: some View {
        if height > 0 {
            VStack(spacing: 0) {
                Spacer()
                ZStack {
                    // Background tint layer - darker for dark mode, lighter for light mode
                    let bgColor: Color = isDarkMode ? .black : .white
                    bgColor.opacity(Double(backgroundAlpha))

                    // Pattern layer on top (only if pattern is not none)
                    if pattern != .none {
                        PatternTileView(
                            pattern: pattern,
                            tintColor: tintColor,
                            patternAlpha: patternAlpha,
                            isDarkMode: isDarkMode
                        )
                        .frame(maxWidth: .infinity, maxHeight: .infinity)
                    }
                }
                .frame(maxWidth: .infinity)
                .frame(height: height)
            }
            .allowsHitTesting(false)  // Don't intercept mouse events
        }
    }
}

// MARK: - Pattern Tile View

/// NSView that renders tiled SVG patterns using Core Graphics
@available(macOS 26.0, *)
struct PatternTileView: NSViewRepresentable {
    var pattern: FooterPattern
    var tintColor: NSColor
    var patternAlpha: CGFloat
    var isDarkMode: Bool

    func makeNSView(context: Context) -> PatternTileNSView {
        let view = PatternTileNSView()
        view.pattern = pattern
        view.tintColor = tintColor
        view.patternAlpha = patternAlpha
        view.isDarkMode = isDarkMode
        return view
    }

    func updateNSView(_ nsView: PatternTileNSView, context: Context) {
        nsView.pattern = pattern
        nsView.tintColor = tintColor
        nsView.patternAlpha = patternAlpha
        nsView.isDarkMode = isDarkMode
        nsView.needsDisplay = true
    }
}

/// NSView subclass that renders the pattern tile
@available(macOS 26.0, *)
final class PatternTileNSView: NSView {
    var pattern: FooterPattern = .hideout
    var tintColor: NSColor = .windowBackgroundColor
    var patternAlpha: CGFloat = 0.15
    var isDarkMode: Bool = false

    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        wantsLayer = true
        layer?.backgroundColor = .clear
    }

    required init?(coder: NSCoder) {
        super.init(coder: coder)
        wantsLayer = true
        layer?.backgroundColor = .clear
    }

    override var isFlipped: Bool { true }  // Use standard top-left origin like SVG

    override func draw(_ dirtyRect: NSRect) {
        guard pattern != .none else { return }

        guard let context = NSGraphicsContext.current?.cgContext else { return }

        // Calculate pattern color:
        // For dark mode: use lighter color on dark background
        // For light mode: use darker color on light background
        let adjustedColor: NSColor
        if isDarkMode {
            adjustedColor = tintColor.blended(withFraction: 0.5, of: .white) ?? .white
        } else {
            adjustedColor = tintColor.blended(withFraction: 0.5, of: .black) ?? .black
        }

        let patternColor = adjustedColor.withAlphaComponent(patternAlpha)

        // Get pattern tile size from SVG data
        let svgData = pattern.svgData
        let tileWidth = svgData.width
        let tileHeight = svgData.height

        // Draw tiled pattern across the entire bounds
        context.saveGState()

        // Set up pattern color
        patternColor.setFill()
        patternColor.setStroke()

        // Tile across the entire view bounds (not just dirtyRect)
        let cols = Int(ceil(bounds.width / tileWidth)) + 1
        let rows = Int(ceil(bounds.height / tileHeight)) + 1

        for row in 0..<rows {
            for col in 0..<cols {
                let x = CGFloat(col) * tileWidth
                let y = CGFloat(row) * tileHeight

                context.saveGState()
                context.translateBy(x: x, y: y)

                // Draw the pattern
                drawPattern(in: context)

                context.restoreGState()
            }
        }

        context.restoreGState()
    }

    private func drawPattern(in context: CGContext) {
        // Parse and draw the SVG path
        let svgData = pattern.svgData
        let pathData = svgData.path
        guard !pathData.isEmpty else { return }

        if let path = parseSVGPath(pathData) {
            context.addPath(path)
            if pattern.usesStroke {
                context.setLineWidth(1.0)
                context.strokePath()
            } else {
                context.fillPath()
            }
        }
    }

    /// Simple SVG path parser for basic path commands (M, L, H, V, Z, A, C)
    private func parseSVGPath(_ data: String) -> CGPath? {
        let path = CGMutablePath()

        // Tokenize the path data
        let tokens = tokenizeSVGPath(data)
        var i = 0
        var currentX: CGFloat = 0
        var currentY: CGFloat = 0
        var startX: CGFloat = 0
        var startY: CGFloat = 0

        while i < tokens.count {
            let token = tokens[i]

            switch token {
            case "M", "m":  // MoveTo
                let relative = token == "m"
                i += 1
                while i < tokens.count && isNumber(tokens[i]) {
                    let x = CGFloat(Double(tokens[i]) ?? 0)
                    let y = CGFloat(Double(tokens[i+1]) ?? 0)
                    i += 2
                    if relative {
                        currentX += x
                        currentY += y
                    } else {
                        currentX = x
                        currentY = y
                    }
                    path.move(to: CGPoint(x: currentX, y: currentY))
                    startX = currentX
                    startY = currentY
                }

            case "L", "l":  // LineTo
                let relative = token == "l"
                i += 1
                while i < tokens.count && isNumber(tokens[i]) {
                    let x = CGFloat(Double(tokens[i]) ?? 0)
                    let y = CGFloat(Double(tokens[i+1]) ?? 0)
                    i += 2
                    if relative {
                        currentX += x
                        currentY += y
                    } else {
                        currentX = x
                        currentY = y
                    }
                    path.addLine(to: CGPoint(x: currentX, y: currentY))
                }

            case "H", "h":  // Horizontal LineTo
                let relative = token == "h"
                i += 1
                while i < tokens.count && isNumber(tokens[i]) {
                    let x = CGFloat(Double(tokens[i]) ?? 0)
                    i += 1
                    if relative {
                        currentX += x
                    } else {
                        currentX = x
                    }
                    path.addLine(to: CGPoint(x: currentX, y: currentY))
                }

            case "V", "v":  // Vertical LineTo
                let relative = token == "v"
                i += 1
                while i < tokens.count && isNumber(tokens[i]) {
                    let y = CGFloat(Double(tokens[i]) ?? 0)
                    i += 1
                    if relative {
                        currentY += y
                    } else {
                        currentY = y
                    }
                    path.addLine(to: CGPoint(x: currentX, y: currentY))
                }

            case "Z", "z":  // ClosePath
                path.closeSubpath()
                currentX = startX
                currentY = startY
                i += 1

            case "A", "a":  // Arc (simplified - draw as line for now)
                let relative = token == "a"
                i += 1
                while i + 6 < tokens.count && isNumber(tokens[i]) {
                    // Skip arc parameters: rx, ry, rotation, large-arc, sweep
                    i += 5
                    let x = CGFloat(Double(tokens[i]) ?? 0)
                    let y = CGFloat(Double(tokens[i+1]) ?? 0)
                    i += 2
                    if relative {
                        currentX += x
                        currentY += y
                    } else {
                        currentX = x
                        currentY = y
                    }
                    // Simplified: just draw line to end point
                    path.addLine(to: CGPoint(x: currentX, y: currentY))
                }

            case "C", "c":  // Cubic bezier
                let relative = token == "c"
                i += 1
                while i + 5 < tokens.count && isNumber(tokens[i]) {
                    var x1 = CGFloat(Double(tokens[i]) ?? 0)
                    var y1 = CGFloat(Double(tokens[i+1]) ?? 0)
                    var x2 = CGFloat(Double(tokens[i+2]) ?? 0)
                    var y2 = CGFloat(Double(tokens[i+3]) ?? 0)
                    var x = CGFloat(Double(tokens[i+4]) ?? 0)
                    var y = CGFloat(Double(tokens[i+5]) ?? 0)
                    i += 6
                    if relative {
                        x1 += currentX; y1 += currentY
                        x2 += currentX; y2 += currentY
                        x += currentX; y += currentY
                    }
                    path.addCurve(to: CGPoint(x: x, y: y),
                                  control1: CGPoint(x: x1, y: y1),
                                  control2: CGPoint(x: x2, y: y2))
                    currentX = x
                    currentY = y
                }

            case "S", "s":  // Smooth cubic bezier
                let relative = token == "s"
                i += 1
                while i + 3 < tokens.count && isNumber(tokens[i]) {
                    var x2 = CGFloat(Double(tokens[i]) ?? 0)
                    var y2 = CGFloat(Double(tokens[i+1]) ?? 0)
                    var x = CGFloat(Double(tokens[i+2]) ?? 0)
                    var y = CGFloat(Double(tokens[i+3]) ?? 0)
                    i += 4
                    if relative {
                        x2 += currentX; y2 += currentY
                        x += currentX; y += currentY
                    }
                    // Use current point as first control point
                    path.addCurve(to: CGPoint(x: x, y: y),
                                  control1: CGPoint(x: currentX, y: currentY),
                                  control2: CGPoint(x: x2, y: y2))
                    currentX = x
                    currentY = y
                }

            default:
                i += 1
            }
        }

        return path
    }

    private func tokenizeSVGPath(_ data: String) -> [String] {
        var tokens: [String] = []
        var current = ""

        for char in data {
            if char.isLetter {
                if !current.isEmpty {
                    tokens.append(current)
                    current = ""
                }
                tokens.append(String(char))
            } else if char == " " || char == "," {
                if !current.isEmpty {
                    tokens.append(current)
                    current = ""
                }
            } else if char == "-" {
                // Negative number - might be start of new number
                if !current.isEmpty {
                    tokens.append(current)
                }
                current = String(char)
            } else {
                current.append(char)
            }
        }
        if !current.isEmpty {
            tokens.append(current)
        }

        return tokens
    }

    private func isNumber(_ str: String) -> Bool {
        return Double(str) != nil
    }
}
