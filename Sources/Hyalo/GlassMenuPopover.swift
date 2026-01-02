// GlassMenuPopover.swift - Liquid Glass menu and popover components
// Copyright (C) 2025
// Target: macOS 26 Tahoe with Liquid Glass design

import AppKit
import SwiftUI

// MARK: - Liquid Glass Menu and Tooltip Views

/// A single menu item row with Liquid Glass styling
@available(macOS 26.0, *)
struct GlassMenuItemRow: View {
    let item: ModeLineMenuItem
    let onSelect: (String) -> Void
    @State private var isHovered = false

    var body: some View {
        Button(action: {
            if item.enabled ?? true {
                onSelect(item.command)
            }
        }) {
            HStack(spacing: 8) {
                if item.checked == true {
                    Image(systemName: "checkmark")
                        .font(.system(size: 10, weight: .medium))
                        .foregroundStyle(.primary)
                } else {
                    Spacer().frame(width: 12)
                }

                Text(item.title)
                    .font(.system(size: 12))
                    .foregroundStyle((item.enabled ?? true) ? .primary : .tertiary)

                Spacer()
            }
            .padding(.horizontal, 10)
            .padding(.vertical, 5)
            .background(isHovered ? Color.primary.opacity(0.1) : Color.clear)
            .clipShape(RoundedRectangle(cornerRadius: 4))
        }
        .buttonStyle(.plain)
        .disabled(!(item.enabled ?? true))
        .onHover { hovering in
            isHovered = hovering
        }
    }
}

/// Liquid Glass popup menu for mode-line segments (picker style)
@available(macOS 26.0, *)
struct GlassMenuView: View {
    let title: String
    let items: [ModeLineMenuItem]
    let onSelect: (String) -> Void

    var body: some View {
        VStack(alignment: .leading, spacing: 0) {
            // Title header
            if !title.isEmpty {
                Text(title)
                    .font(.system(size: 11, weight: .semibold))
                    .foregroundStyle(.secondary)
                    .padding(.horizontal, 16)
                    .padding(.top, 10)
                    .padding(.bottom, 6)

                Divider()
                    .padding(.horizontal, 12)
                    .padding(.bottom, 4)
            }

            // Menu items with proper margins
            VStack(alignment: .leading, spacing: 2) {
                ForEach(Array(items.enumerated()), id: \.offset) { index, item in
                    if item.separator == true {
                        Divider()
                            .padding(.horizontal, 16)  // More padding around separator
                            .padding(.vertical, 8)     // Vertical spacing around separator
                    } else {
                        GlassMenuItemRow(item: item, onSelect: onSelect)
                            .padding(.horizontal, 8)   // Consistent horizontal margins
                    }
                }
            }
            .padding(.horizontal, 4)  // Left/right margins for the menu content
            .padding(.bottom, 10)
        }
        .frame(minWidth: 220)
        // NOTE: No background - popover provides glass effect
        .glassEffect(.regular.interactive(), in: .rect(cornerRadius: 12))
    }
}

/// Liquid Glass tooltip for mode-line segments
@available(macOS 26.0, *)
struct GlassTooltipView: View {
    let text: String

    var body: some View {
        Text(text)
            .font(.system(size: 11))
            .foregroundStyle(.primary)
            .padding(.horizontal, 10)
            .padding(.vertical, 6)
            .background(.ultraThinMaterial)
            .glassEffect(.regular, in: .capsule)
    }
}

/// NSPopover-based picker for mode-line menus with arrow
/// Uses transparent background to let the glass effect show through
@available(macOS 26.0, *)
final class GlassMenuPopover: NSPopover, NSPopoverDelegate {
    private var onCommandSelect: ((String) -> Void)?

    override init() {
        super.init()
        self.behavior = .transient
        self.animates = true
        self.delegate = self
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    // MARK: - NSPopoverDelegate

    func popoverWillShow(_ notification: Notification) {
        // Make popover window transparent immediately when it appears
        makePopoverTransparent()
    }

    private func makePopoverTransparent() {
        guard let popoverWindow = self.contentViewController?.view.window else { return }

        // Make window fully transparent
        popoverWindow.isOpaque = false
        popoverWindow.backgroundColor = .clear
        popoverWindow.hasShadow = true

        // Recursively make all background views transparent
        if let contentView = popoverWindow.contentView {
            makeViewsTransparent(contentView)
        }
    }

    private func makeViewsTransparent(_ view: NSView) {
        let className = String(describing: type(of: view))

        // Hide popover chrome views (background, border, etc.)
        if className.contains("Background") ||
           className.contains("Chrome") ||
           className.contains("_NSPopoverFrame") {
            view.isHidden = true
        }

        // Hide NSVisualEffectView (the popover's default material)
        if view is NSVisualEffectView {
            view.isHidden = true
        }

        // Process subviews
        for subview in view.subviews {
            makeViewsTransparent(subview)
        }
    }

    /// Show menu as popover below the given view
    func showMenu(
        items: [ModeLineMenuItem],
        title: String,
        relativeTo view: NSView,
        onSelect: @escaping (String) -> Void
    ) {
        self.onCommandSelect = onSelect

        let menuView = GlassMenuView(
            title: title,
            items: items,
            onSelect: { [weak self] command in
                // Call the callback BEFORE closing - closing disrupts the channel context
                onSelect(command)
                self?.close()
            }
        )

        let hostingController = NSHostingController(rootView: menuView)
        hostingController.view.wantsLayer = true
        hostingController.view.layer?.backgroundColor = .clear
        hostingController.view.layer?.isOpaque = false

        self.contentViewController = hostingController

        // Position at bottom center of the segment view
        // Use a 1x1 rect centered at the bottom edge
        let rect = NSRect(
            x: view.bounds.midX - 0.5,
            y: 0,  // Bottom edge
            width: 1,
            height: 1
        )

        self.show(relativeTo: rect, of: view, preferredEdge: .minY)
    }
}
