// BufferListView.swift - Buffer list for sidebar
// Copyright (C) 2025
// Target: macOS 26 Tahoe with Liquid Glass design
//
// Design: Monochrome doc icon, grayscale selection with alpha, hover effects.

import AppKit
import SwiftUI

// MARK: - Buffer Data Model

@available(macOS 26.0, *)
struct BufferInfo: Identifiable, Codable, Equatable {
    let id: String           // Buffer name (unique identifier)
    let name: String         // Display name
    let path: String?        // File path if file-backed
    let modified: Bool       // Unsaved changes indicator
    let icon: String         // SF Symbol name (from Emacs, not used for display)

    var displayName: String {
        if let path = path {
            return (path as NSString).lastPathComponent
        }
        return name
    }

    /// Two icons only: special buffer or document
    var resolvedIcon: String {
        if path == nil && name.hasPrefix("*") {
            return "square.and.pencil"
        }
        return "doc"
    }
}

// MARK: - Buffer Row View

@available(macOS 26.0, *)
struct BufferRow: View {
    let buffer: BufferInfo
    let isSelected: Bool
    var onClose: (() -> Void)?

    @State private var isHovered = false

    var body: some View {
        HStack(spacing: 6) {
            // Monochrome icon
            Image(systemName: buffer.resolvedIcon)
                .font(.system(size: 12))
                .foregroundStyle(.secondary)
                .frame(width: 16)

            // Buffer name
            Text(buffer.displayName)
                .font(.system(size: 11.5, weight: isSelected ? .medium : .regular, design: .monospaced))
                .foregroundStyle(isSelected ? .primary : .secondary)
                .lineLimit(1)
                .truncationMode(.middle)

            Spacer()

            // Trailing area: modified dot or close button on hover
            ZStack {
                if buffer.modified {
                    Circle()
                        .fill(Color.orange)
                        .frame(width: 6, height: 6)
                        .help("Unsaved changes")
                        .opacity(isHovered ? 0 : 1)
                }

                if isHovered, let onClose {
                    Button(action: onClose) {
                        Image(systemName: "xmark")
                            .font(.system(size: 8, weight: .medium))
                            .foregroundStyle(.tertiary)
                    }
                    .buttonStyle(.plain)
                    .transition(.opacity)
                    .help("Close buffer")
                }
            }
            .frame(width: 16)
        }
        .padding(.horizontal, 10)
        .padding(.vertical, 5)
        .background {
            if isSelected {
                RoundedRectangle(cornerRadius: 4)
                    .fill(Color.primary.opacity(0.10))
            } else if isHovered {
                RoundedRectangle(cornerRadius: 4)
                    .fill(Color.primary.opacity(0.05))
            }
        }
        .contentShape(Rectangle())
        .onHover { hovering in
            withAnimation(.easeOut(duration: 0.12)) {
                isHovered = hovering
            }
        }
    }
}

// MARK: - Buffer List View

@available(macOS 26.0, *)
struct BufferListView: View {
    let buffers: [BufferInfo]
    let selectedBuffer: String?
    let onSelect: (String) -> Void
    var onClose: ((String) -> Void)?

    var body: some View {
        ScrollView(.vertical, showsIndicators: false) {
            LazyVStack(alignment: .leading, spacing: 1) {
                ForEach(buffers) { buffer in
                    BufferRow(
                        buffer: buffer,
                        isSelected: buffer.id == selectedBuffer,
                        onClose: onClose.map { callback in
                            { callback(buffer.id) }
                        }
                    )
                    .onTapGesture {
                        onSelect(buffer.id)
                    }
                }
            }
            .padding(.horizontal, 4)
            .padding(.vertical, 4)
        }
    }
}

// MARK: - Preview

#if DEBUG
@available(macOS 26.0, *)
#Preview {
    BufferListView(
        buffers: [
            BufferInfo(id: "init.el", name: "init.el", path: "/Users/test/init.el", modified: false, icon: "doc.text"),
            BufferInfo(id: "Module.swift", name: "Module.swift", path: "/Users/test/Module.swift", modified: true, icon: "swift"),
            BufferInfo(id: "*scratch*", name: "*scratch*", path: nil, modified: false, icon: "square.and.pencil"),
            BufferInfo(id: "styles.css", name: "styles.css", path: "/Users/test/styles.css", modified: false, icon: "paintbrush"),
        ],
        selectedBuffer: "Module.swift",
        onSelect: { print("Selected: \($0)") }
    )
    .frame(width: 260, height: 220)
    .background(.black.opacity(0.1))
}
#endif
