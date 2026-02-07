// PiMarkdownTheme.swift - Appearance-aware Markdown theme for Hyalo inspector
// Copyright (C) 2025
// Target: macOS 26 Tahoe
//
// Uses semantic system colors instead of hardcoded dark-mode values.
// Adapts automatically to light and dark appearance.

@preconcurrency import MarkdownUI
import SwiftUI

// MARK: - Hyalo Markdown Theme

extension Theme {

    /// Appearance-aware theme for the Hyalo inspector chat.
    /// Uses semantic colors that adapt to system light/dark mode.
    /// Typography: SF Mono (system monospaced) for body text and code.
    @MainActor static let hyaloInspector = Theme()
        .text {
            ForegroundColor(.primary)
            FontSize(12)
            FontFamilyVariant(.monospaced)
        }
        .code {
            FontFamilyVariant(.monospaced)
            FontSize(11)
            // Hyalo Violet 400 (#A78BFA)
            ForegroundColor(Color(red: 0.65, green: 0.54, blue: 0.98))
        }
        .strong {
            FontWeight(.semibold)
        }
        .link {
            // Hyalo Violet 600 (#7C3AED)
            ForegroundColor(Color(red: 0.49, green: 0.23, blue: 0.93))
        }
        .heading1 { configuration in
            configuration.label
                .markdownTextStyle {
                    FontWeight(.bold)
                    FontSize(14)
                    ForegroundColor(.primary)
                    FontFamilyVariant(.monospaced)
                }
                .padding(.bottom, 6)
        }
        .heading2 { configuration in
            configuration.label
                .markdownTextStyle {
                    FontWeight(.semibold)
                    FontSize(13)
                    ForegroundColor(.primary)
                    FontFamilyVariant(.monospaced)
                }
                .padding(.bottom, 4)
        }
        .heading3 { configuration in
            configuration.label
                .markdownTextStyle {
                    FontWeight(.medium)
                    FontSize(12)
                    ForegroundColor(.primary)
                    FontFamilyVariant(.monospaced)
                }
                .padding(.bottom, 3)
        }
        .paragraph { configuration in
            configuration.label
                .padding(.vertical, 2)
        }
        .listItem { configuration in
            configuration.label
                .markdownMargin(top: 2, bottom: 2)
        }
        .codeBlock { configuration in
            HyaloCodeBlockView(configuration: configuration)
        }
        .blockquote { configuration in
            HStack(spacing: 0) {
                RoundedRectangle(cornerRadius: 1)
                    .fill(Color(nsColor: .separatorColor))
                    .frame(width: 3)
                configuration.label
                    .markdownTextStyle {
                        ForegroundColor(.secondary)
                        FontSize(12)
                    }
                    .padding(.leading, 8)
            }
            .padding(.vertical, 4)
        }
}

// MARK: - Code Block View (Appearance-Aware)

struct HyaloCodeBlockView: View {
    let configuration: CodeBlockConfiguration

    @Environment(\.colorScheme) private var colorScheme

    var body: some View {
        VStack(alignment: .leading, spacing: 0) {
            // Language header with copy button
            if let language = configuration.language, !language.isEmpty {
                HStack {
                    Text(language)
                        .font(.system(size: 9, weight: .medium, design: .monospaced))
                        .foregroundStyle(.tertiary)
                    Spacer()
                    Button(action: copyCode) {
                        Image(systemName: "doc.on.doc")
                            .font(.system(size: 9))
                            .foregroundStyle(.tertiary)
                    }
                    .buttonStyle(.plain)
                }
                .padding(.horizontal, 8)
                .padding(.vertical, 4)
                .background(Color(nsColor: .separatorColor).opacity(0.3))
            }

            // Code content with line numbers
            ScrollView(.horizontal, showsIndicators: false) {
                HStack(alignment: .top, spacing: 0) {
                    // Line numbers
                    VStack(alignment: .trailing, spacing: 0) {
                        ForEach(Array(lines.enumerated()), id: \.offset) { index, _ in
                            Text("\(index + 1)")
                                .font(.system(size: 9, design: .monospaced))
                                .foregroundStyle(.tertiary)
                                .frame(height: lineHeight)
                        }
                    }
                    .padding(.trailing, 8)
                    .padding(.leading, 8)

                    // Separator
                    Rectangle()
                        .fill(Color(nsColor: .separatorColor).opacity(0.5))
                        .frame(width: 1)

                    // Code lines with syntax highlighting
                    VStack(alignment: .leading, spacing: 0) {
                        ForEach(Array(lines.enumerated()), id: \.offset) { _, line in
                            highlightedLine(line)
                                .frame(height: lineHeight, alignment: .leading)
                        }
                    }
                    .padding(.horizontal, 8)
                }
                .padding(.vertical, 6)
            }
        }
        .background(codeBackground)
        .clipShape(RoundedRectangle(cornerRadius: 6))
        .overlay(
            RoundedRectangle(cornerRadius: 6)
                .stroke(Color(nsColor: .separatorColor).opacity(0.5), lineWidth: 1)
        )
    }

    /// Code block background adapts to appearance
    private var codeBackground: Color {
        colorScheme == .dark
            ? Color(nsColor: .textBackgroundColor).opacity(0.4)
            : Color(nsColor: .textBackgroundColor).opacity(0.8)
    }

    private var lines: [String] {
        configuration.content.components(separatedBy: "\n")
    }

    private let lineHeight: CGFloat = 15

    private var language: String {
        configuration.language ?? ""
    }

    private func copyCode() {
        NSPasteboard.general.clearContents()
        NSPasteboard.general.setString(configuration.content, forType: .string)
    }

    // MARK: - Syntax Highlighting

    @ViewBuilder
    private func highlightedLine(_ line: String) -> some View {
        Text(attributedLine(line))
            .font(.system(size: 11, design: .monospaced))
    }

    private func attributedLine(_ line: String) -> AttributedString {
        var result = AttributedString(line.isEmpty ? " " : line)
        result.foregroundColor = Color.primary.opacity(0.85)
        applyHighlighting(to: &result, line: line)
        return result
    }

    private func applyHighlighting(to result: inout AttributedString, line: String) {
        let lang = language.lowercased()

        let keywords: [String]
        let typeKeywords: [String]

        switch lang {
        case "swift":
            keywords = ["func", "var", "let", "if", "else", "guard", "return", "import",
                        "struct", "class", "enum", "protocol", "extension", "private",
                        "public", "internal", "fileprivate", "static", "override",
                        "mutating", "throws", "async", "await", "try", "catch", "for",
                        "while", "switch", "case", "default", "break", "continue",
                        "where", "in", "self", "Self", "nil", "true", "false",
                        "@State", "@Binding", "@Observable", "@MainActor", "some",
                        "any", "init", "deinit"]
            typeKeywords = ["String", "Int", "Bool", "Double", "Float", "Array",
                           "Dictionary", "Set", "Optional", "Result", "View", "Text",
                           "VStack", "HStack", "ZStack", "Button", "Image", "Color"]
        case "python", "py":
            keywords = ["def", "class", "if", "elif", "else", "for", "while", "return",
                        "import", "from", "as", "try", "except", "finally", "with",
                        "lambda", "yield", "raise", "pass", "break", "continue",
                        "and", "or", "not", "in", "is", "None", "True", "False",
                        "self", "async", "await"]
            typeKeywords = ["str", "int", "float", "bool", "list", "dict", "set",
                           "tuple", "type"]
        case "javascript", "js", "typescript", "ts":
            keywords = ["function", "const", "let", "var", "if", "else", "return",
                        "import", "export", "from", "class", "extends", "new", "this",
                        "try", "catch", "finally", "throw", "async", "await", "for",
                        "while", "switch", "case", "default", "break", "continue",
                        "typeof", "instanceof", "null", "undefined", "true", "false"]
            typeKeywords = ["string", "number", "boolean", "object", "Array", "Promise",
                           "void", "any", "never"]
        case "rust", "rs":
            keywords = ["fn", "let", "mut", "const", "if", "else", "match", "loop",
                        "while", "for", "in", "return", "break", "continue", "struct",
                        "enum", "impl", "trait", "pub", "use", "mod", "self", "Self",
                        "true", "false", "async", "await", "move", "ref", "where"]
            typeKeywords = ["String", "str", "i32", "i64", "u32", "u64", "f32", "f64",
                           "bool", "Vec", "Option", "Result", "Box", "Rc", "Arc"]
        case "go":
            keywords = ["func", "var", "const", "if", "else", "for", "range", "return",
                        "switch", "case", "default", "break", "continue", "type",
                        "struct", "interface", "map", "chan", "go", "defer", "select",
                        "package", "import", "nil", "true", "false"]
            typeKeywords = ["string", "int", "int32", "int64", "float32", "float64",
                           "bool", "byte", "rune", "error"]
        case "bash", "sh", "zsh", "shell":
            keywords = ["if", "then", "else", "elif", "fi", "for", "do", "done",
                        "while", "case", "esac", "function", "return", "exit", "export",
                        "local", "readonly", "declare", "source", "alias", "echo", "cd",
                        "ls", "rm", "cp", "mv", "mkdir", "cat", "grep", "sed", "awk",
                        "find", "xargs"]
            typeKeywords = []
        case "elisp", "emacs-lisp":
            keywords = ["defun", "defvar", "defcustom", "defmacro", "let", "let*",
                        "if", "when", "unless", "cond", "progn", "lambda", "setq",
                        "require", "provide", "interactive", "save-excursion",
                        "with-current-buffer", "cl-loop", "dolist"]
            typeKeywords = []
        default:
            keywords = ["if", "else", "for", "while", "return", "function", "class",
                        "import", "export", "true", "false", "null", "nil"]
            typeKeywords = []
        }

        // Keywords (Hyalo Violet 500: #8B5CF6)
        for keyword in keywords {
            highlightPattern(in: &result, line: line,
                           pattern: "\\b\(keyword)\\b",
                           color: Color(red: 0.55, green: 0.36, blue: 0.96))
        }

        // Types (Hyalo Violet 400: #A78BFA)
        for type in typeKeywords {
            highlightPattern(in: &result, line: line,
                           pattern: "\\b\(type)\\b",
                           color: Color(red: 0.65, green: 0.54, blue: 0.98))
        }

        // Strings (Hyalo/Emerald 400: #34D399)
        highlightPattern(in: &result, line: line,
                        pattern: "\"[^\"]*\"",
                        color: Color(red: 0.2, green: 0.83, blue: 0.6))
        highlightPattern(in: &result, line: line,
                        pattern: "'[^']*'",
                        color: Color(red: 0.2, green: 0.83, blue: 0.6))

        // Numbers (Hyalo Violet 300: #C4B5FD)
        highlightPattern(in: &result, line: line,
                        pattern: "\\b\\d+(\\.\\d+)?\\b",
                        color: Color(red: 0.77, green: 0.71, blue: 0.99))

        // Comments (Secondary - Zinc)
        highlightPattern(in: &result, line: line,
                        pattern: "//.*$",
                        color: .secondary)
        highlightPattern(in: &result, line: line,
                        pattern: "#.*$",
                        color: .secondary)
        // Elisp comments
        if language.lowercased() == "elisp" || language.lowercased() == "emacs-lisp" {
            highlightPattern(in: &result, line: line,
                            pattern: ";.*$",
                            color: .secondary)
        }
    }

    private func highlightPattern(
        in result: inout AttributedString,
        line: String,
        pattern: String,
        color: Color
    ) {
        guard let regex = try? NSRegularExpression(pattern: pattern, options: []) else { return }
        let range = NSRange(line.startIndex..., in: line)
        let matches = regex.matches(in: line, options: [], range: range)

        for match in matches {
            guard let swiftRange = Range(match.range, in: line) else { continue }
            let startOffset = line.distance(from: line.startIndex, to: swiftRange.lowerBound)
            let endOffset = line.distance(from: line.startIndex, to: swiftRange.upperBound)

            let attrStart = result.index(result.startIndex, offsetByCharacters: startOffset)
            let attrEnd = result.index(result.startIndex, offsetByCharacters: endOffset)

            if attrStart < attrEnd {
                result[attrStart..<attrEnd].foregroundColor = color
            }
        }
    }
}