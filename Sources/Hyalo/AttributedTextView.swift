import SwiftUI
import AppKit

/// Preferred fonts for mode-line rendering (in order of preference)
/// These fonts have nerd font icons
private let preferredFonts = [
    "Symbols Nerd Font Mono",
    "Hack Nerd Font Mono", 
    "FiraCode Nerd Font Mono",
    "JetBrainsMono Nerd Font Mono",
    "SauceCodePro Nerd Font Mono",
    "Menlo"
]

/// Find a suitable font for mode-line rendering
private func findModeLineFont(size: CGFloat) -> NSFont {
    // Try preferred nerd fonts first
    for fontName in preferredFonts {
        if let font = NSFont(name: fontName, size: size) {
            return font
        }
    }
    // Fall back to monospaced system font
    return NSFont.monospacedSystemFont(ofSize: size, weight: .regular)
}

/// A SwiftUI view that renders an NSAttributedString
/// This allows proper rendering of nerd fonts and other special characters
@available(macOS 15.0, *)
struct AttributedTextView: NSViewRepresentable {
    let text: String
    let fontSize: CGFloat
    let textColor: NSColor
    
    func makeNSView(context: Context) -> NSTextField {
        let field = NSTextField(labelWithString: "")
        field.isEditable = false
        field.isSelectable = false
        field.isBordered = false
        field.drawsBackground = false
        field.lineBreakMode = .byTruncatingMiddle
        field.maximumNumberOfLines = 1
        field.cell?.truncatesLastVisibleLine = true
        return field
    }
    
    func updateNSView(_ field: NSTextField, context: Context) {
        // Use a nerd font if available
        let font = findModeLineFont(size: fontSize)
        let attributes: [NSAttributedString.Key: Any] = [
            .font: font,
            .foregroundColor: textColor
        ]
        field.attributedStringValue = NSAttributedString(string: text, attributes: attributes)
    }
}

/// A view that renders mode-line text with proper font handling
/// Uses SwiftUI Text with a nerd font for correct icon rendering
@available(macOS 15.0, *)
struct ModeLineTextView: View {
    let text: String
    let fontSize: CGFloat
    
    /// Find a suitable nerd font name
    private var fontName: String {
        for name in preferredFonts {
            if NSFont(name: name, size: fontSize) != nil && name.contains("Nerd") {
                return name
            }
        }
        return "Menlo"  // Fallback
    }
    
    var body: some View {
        Text(text)
            .font(.custom(fontName, size: fontSize))
            .lineLimit(1)
            .truncationMode(.tail)
    }
}
