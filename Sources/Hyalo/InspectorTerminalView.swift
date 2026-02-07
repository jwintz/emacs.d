// InspectorTerminalView.swift - SwiftTerm-based terminal for the inspector panel
// Copyright (C) 2025
// Target: macOS 26 Tahoe with Liquid Glass design

#if os(macOS)
import AppKit
import SwiftTerm
import SwiftUI

// MARK: - Terminal Color Palette

/// Holds the 16 ANSI colors plus foreground/background derived from hyalo theme.
/// Updated from Elisp via `hyalo-set-terminal-palette`.
@available(macOS 26.0, *)
@Observable
final class TerminalPalette {
    static let shared = TerminalPalette()

    /// 16 ANSI colors (0-7 normal, 8-15 bright) as hex strings
    var ansiColors: [String] = TerminalPalette.defaultDarkAnsi
    /// Foreground color hex
    var foreground: String = "#F4F4F5"
    /// Background color hex (used only for the terminal internal bg, not the view)
    var background: String = "#18181B"
    /// Cursor color hex
    var cursor: String = "#A58AF9"

    // Default dark ANSI palette derived from hyalo-theme + nano-theme material colors
    static let defaultDarkAnsi: [String] = [
        // Normal (0-7): black, red, green, yellow, blue, magenta, cyan, white
        "#27272A",  // Zinc 800 (black)
        "#EF5350",  // Material Red L400
        "#66BB6A",  // Material Green L400
        "#FFEE58",  // Material Yellow L400
        "#42A5F5",  // Material Blue L400
        "#AB47BC",  // Material Purple L400
        "#26C6DA",  // Material Cyan L400
        "#F4F4F5",  // Zinc 100 (white)
        // Bright (8-15)
        "#52525B",  // Zinc 600 (bright black)
        "#F87171",  // Red 400
        "#4ADE80",  // Green 400
        "#FDE047",  // Yellow 300
        "#60A5FA",  // Blue 400
        "#C084FC",  // Purple 400
        "#22D3EE",  // Cyan 400
        "#FFFFFF",  // White
    ]

    private init() {}
}

// MARK: - Default Font Size

/// Default terminal font size in points, matching Emacs default.
let terminalDefaultFontSize: CGFloat = 11

// MARK: - TerminalView Subclass with Overlay Scroller

/// Subclass of `LocalProcessTerminalView` that:
/// - Intercepts Ctrl, arrow, and function keys via `performKeyEquivalent`
///   so Emacs' NSWindow `sendEvent:` does not consume them.
/// - Uses overlay scroller style instead of the hardcoded legacy style.
@available(macOS 26.0, *)
final class HyaloTerminalView: LocalProcessTerminalView {

    // MARK: - Key Event Interception

    /// Claim keyboard events that Emacs would otherwise intercept.
    /// `performKeyEquivalent` is called before `keyDown` in the
    /// responder chain. Returning `true` prevents the event from
    /// propagating to Emacs' own key handling.
    override func performKeyEquivalent(with event: NSEvent) -> Bool {
        // Only intercept when WE are first responder
        guard window?.firstResponder === self else {
            return super.performKeyEquivalent(with: event)
        }

        let flags = event.modifierFlags.intersection(.deviceIndependentFlagsMask)

        // Ctrl+key — send to terminal via keyDown
        if flags.contains(.control) && !flags.contains(.command) {
            self.keyDown(with: event)
            return true
        }

        // Arrow keys and function keys (no Cmd)
        if !flags.contains(.command),
           let chars = event.charactersIgnoringModifiers,
           let scalar = chars.unicodeScalars.first {
            let v = Int(scalar.value)
            let isFunctionKey = (v >= NSUpArrowFunctionKey && v <= NSRightArrowFunctionKey)
                || (v >= NSF1FunctionKey && v <= NSF35FunctionKey)
                || v == NSDeleteFunctionKey
                || v == NSHomeFunctionKey || v == NSEndFunctionKey
                || v == NSPageUpFunctionKey || v == NSPageDownFunctionKey
            if isFunctionKey {
                self.keyDown(with: event)
                return true
            }
        }

        return super.performKeyEquivalent(with: event)
    }

}

// MARK: - Terminal Container

/// Container NSView that hosts the `HyaloTerminalView`.
/// Handles click-to-focus (making the terminal first responder)
/// and Cmd+/Cmd- font scaling.
@available(macOS 26.0, *)
final class TerminalContainerView: NSView {
    var terminalView: HyaloTerminalView?
    private var currentFontSize: CGFloat = terminalDefaultFontSize

    override var acceptsFirstResponder: Bool { true }

    override func mouseDown(with event: NSEvent) {
        // Make the terminal first responder on click
        if let tv = terminalView {
            window?.makeFirstResponder(tv)
        }
    }

    override func performKeyEquivalent(with event: NSEvent) -> Bool {
        guard event.modifierFlags.contains(.command),
              let chars = event.charactersIgnoringModifiers
        else {
            return super.performKeyEquivalent(with: event)
        }

        // Only handle when the terminal has focus
        guard let tv = terminalView,
              window?.firstResponder === tv
        else {
            return super.performKeyEquivalent(with: event)
        }

        switch chars {
        case "+", "=":
            adjustFontSize(delta: 1)
            return true
        case "-":
            adjustFontSize(delta: -1)
            return true
        case "0":
            setFontSize(terminalDefaultFontSize)
            return true
        default:
            return super.performKeyEquivalent(with: event)
        }
    }

    private func adjustFontSize(delta: CGFloat) {
        let newSize = max(8, min(24, currentFontSize + delta))
        setFontSize(newSize)
    }

    private func setFontSize(_ size: CGFloat) {
        currentFontSize = size
        guard let tv = terminalView else { return }
        if let sfMono = NSFont(name: "SF Mono", size: size) {
            tv.font = sfMono
        } else {
            tv.font = NSFont.monospacedSystemFont(ofSize: size, weight: .regular)
        }
    }
}

// MARK: - NSViewRepresentable Wrapper

/// Wraps `HyaloTerminalView` inside a `TerminalContainerView` for SwiftUI.
/// Configures transparent background, SF Mono font at 11pt, steady underline cursor,
/// overlay scroller, and hyalo-derived colors.
@available(macOS 26.0, *)
struct InspectorTerminalView: NSViewRepresentable {
    var palette: TerminalPalette

    func makeNSView(context: Context) -> TerminalContainerView {
        let container = TerminalContainerView()
        container.autoresizesSubviews = true

        let terminalView = HyaloTerminalView(frame: container.bounds)
        terminalView.autoresizingMask = [.width, .height]
        terminalView.processDelegate = context.coordinator

        // Font: SF Mono at default size
        if let sfMono = NSFont(name: "SF Mono", size: terminalDefaultFontSize) {
            terminalView.font = sfMono
        } else {
            terminalView.font = NSFont.monospacedSystemFont(ofSize: terminalDefaultFontSize, weight: .regular)
        }

        // Transparent background: the inspector panel already provides vibrancy
        terminalView.nativeBackgroundColor = NSColor.clear
        terminalView.layer?.backgroundColor = NSColor.clear.cgColor
        terminalView.layer?.isOpaque = false

        // Cursor: non-blinking underscore
        terminalView.terminal.setCursorStyle(.steadyUnderline)

        // Apply palette colors
        applyPalette(to: terminalView)

        // Option key sends Meta (ESC prefix) for terminal apps
        terminalView.optionAsMetaKey = true

        container.addSubview(terminalView)
        container.terminalView = terminalView

        // Start a login shell
        let shell = ProcessInfo.processInfo.environment["SHELL"] ?? "/bin/zsh"
        terminalView.startProcess(
            executable: shell,
            args: ["--login"],
            environment: Terminal.getEnvironmentVariables(termName: "xterm-256color"),
            currentDirectory: NSHomeDirectory()
        )

        // Store reference for palette updates and process restart
        context.coordinator.terminalView = terminalView

        return container
    }

    func updateNSView(_ container: TerminalContainerView, context: Context) {
        if let tv = container.terminalView {
            applyPalette(to: tv)
        }
    }

    func makeCoordinator() -> Coordinator {
        Coordinator()
    }

    /// Apply the current palette to the terminal view.
    private func applyPalette(to terminalView: HyaloTerminalView) {
        // Foreground
        if let fg = NSColor.fromHex(palette.foreground) {
            terminalView.nativeForegroundColor = fg
        }

        // Background stays clear for transparency
        terminalView.nativeBackgroundColor = NSColor.clear

        // Cursor
        if let cursorColor = NSColor.fromHex(palette.cursor) {
            terminalView.caretColor = cursorColor
        }

        // 16 ANSI colors
        guard palette.ansiColors.count == 16 else { return }
        let swiftTermColors: [SwiftTerm.Color] = palette.ansiColors.compactMap { hex in
            guard let ns = NSColor.fromHex(hex) else { return nil }
            var r: CGFloat = 0, g: CGFloat = 0, b: CGFloat = 0, a: CGFloat = 0
            let converted = ns.usingColorSpace(.sRGB) ?? ns
            converted.getRed(&r, green: &g, blue: &b, alpha: &a)
            return SwiftTerm.Color(
                red: UInt16(r * 65535),
                green: UInt16(g * 65535),
                blue: UInt16(b * 65535)
            )
        }
        if swiftTermColors.count == 16 {
            terminalView.installColors(swiftTermColors)
        }
    }

    // MARK: - Coordinator

    final class Coordinator: NSObject, LocalProcessTerminalViewDelegate {
        weak var terminalView: HyaloTerminalView?

        func sizeChanged(source: LocalProcessTerminalView, newCols: Int, newRows: Int) {}
        func setTerminalTitle(source: LocalProcessTerminalView, title: String) {}
        func hostCurrentDirectoryUpdate(source: TerminalView, directory: String?) {}

        func processTerminated(source: TerminalView, exitCode: Int32?) {
            // Restart the shell when the process exits
            guard let tv = terminalView else { return }

            // Clear the terminal and reset state using RIS (Reset to Initial State)
            tv.terminal.feed(text: "\u{001b}c")
            tv.terminal.feed(text: "\u{001b}[2J\u{001b}[H")

            let shell = ProcessInfo.processInfo.environment["SHELL"] ?? "/bin/zsh"
            tv.startProcess(
                executable: shell,
                args: ["--login"],
                environment: Terminal.getEnvironmentVariables(termName: "xterm-256color"),
                currentDirectory: NSHomeDirectory()
            )
        }
    }
}

// MARK: - NSColor Hex Helper

private extension NSColor {
    static func fromHex(_ hex: String) -> NSColor? {
        let trimmed = hex.trimmingCharacters(in: .whitespaces)
        let hexString = trimmed.hasPrefix("#") ? String(trimmed.dropFirst()) : trimmed
        guard hexString.count == 6 else { return nil }
        var rgb: UInt64 = 0
        Scanner(string: hexString).scanHexInt64(&rgb)
        let r = CGFloat((rgb >> 16) & 0xFF) / 255.0
        let g = CGFloat((rgb >> 8) & 0xFF) / 255.0
        let b = CGFloat(rgb & 0xFF) / 255.0
        return NSColor(red: r, green: g, blue: b, alpha: 1.0)
    }
}

#endif
