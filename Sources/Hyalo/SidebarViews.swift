// SidebarViews.swift - Sidebar SwiftUI views
// Copyright (C) 2025
// Target: macOS 26 Tahoe with Liquid Glass design

import AppKit
import SwiftUI

// MARK: - Sidebar SwiftUI Views

/// Section header view for sidebar panels
@available(macOS 26.0, *)
struct SidebarSectionHeader: View {
    let title: String
    let systemImage: String
    var subtitle: String = ""
    var isBusy: Bool = false
    var isFirst: Bool = false

    @State private var rotation: Double = 0

    /// Standard margin matching sidebar padding
    private let sideMargin: CGFloat = 14
    /// Icon width + spacing to align subtitle with title
    private let iconWidth: CGFloat = 10
    private let iconSpacing: CGFloat = 6

    var body: some View {
        VStack(alignment: .leading, spacing: 2) {
            // Primary row: icon + title
            HStack(spacing: iconSpacing) {
                Image(systemName: systemImage)
                    .font(.system(size: iconWidth, weight: .medium))
                    .foregroundStyle(.secondary)
                    .symbolEffect(.pulse, isActive: isBusy)
                    .rotationEffect(.degrees(rotation))
                    .onChange(of: isBusy, initial: true) { _, busy in
                        if busy {
                            rotation = 0
                            withAnimation(.linear(duration: 2).repeatForever(autoreverses: false)) {
                                rotation = 360
                            }
                        } else {
                            withAnimation(.default) {
                                rotation = 0
                            }
                        }
                    }
                Text(title)
                    .font(.system(size: 10, weight: .semibold, design: .monospaced))
                    .foregroundStyle(.secondary)
                Spacer()
            }
            // Secondary row: subtitle (token stats), aligned with title
            if !subtitle.isEmpty {
                Text(subtitle)
                    .font(.system(size: 9, weight: .regular, design: .monospaced))
                    .foregroundStyle(.tertiary)
                    .lineLimit(1)
                    .truncationMode(.middle)
                    .help(subtitle)  // Full text in tooltip on hover
                    .padding(.leading, iconWidth + iconSpacing)  // Align with title
            }
        }
        .padding(.horizontal, sideMargin)
        .padding(.top, isFirst ? 8 : 14)
        .padding(.bottom, 6)
    }
}

// MARK: - Sidebar Content View

/// The sidebar content with Liquid Glass styling
/// Shows buffer list and file tree from Emacs data
@available(macOS 26.0, *)
struct SidebarContentView: View {
    var state: NavigationSidebarState
    var onBufferSelect: (String) -> Void
    var onBufferClose: ((String) -> Void)?
    var onFileSelect: (String) -> Void

    var body: some View {
        VStack(spacing: 0) {
            // Open Buffers section
            SidebarSectionHeader(
                title: "OPEN BUFFERS",
                systemImage: "doc.on.doc",
                isFirst: true
            )
            
            if !state.bufferList.isEmpty {
                BufferListView(
                    buffers: state.bufferList,
                    selectedBuffer: state.selectedBuffer,
                    onSelect: onBufferSelect,
                    onClose: onBufferClose
                )
                .frame(maxHeight: 180)
            } else {
                // Empty buffer list placeholder
                HStack {
                    Text("No buffers open")
                        .font(.system(size: 10, design: .monospaced))
                        .foregroundStyle(.tertiary)
                }
                .frame(height: 40)
            }

            Divider()
                .padding(.horizontal, 14)
                .padding(.vertical, 6)

            // Project Files section
            SidebarSectionHeader(
                title: "PROJECT",
                systemImage: "folder",
                subtitle: (state.projectRoot as NSString).lastPathComponent
            )
            
            FileTreeView(
                root: state.fileTree,
                activeFilePath: state.activeFilePath,
                onSelect: onFileSelect
            )
            .frame(maxWidth: .infinity, maxHeight: .infinity)
        }
        .background {
            Color(nsColor: state.backgroundColor)
                .opacity(Double(state.backgroundAlpha))
                .ignoresSafeArea()
        }
    }
}

// MARK: - Pi Logo Shape

/// Stylized Pi (π) letter shape for the inspector header.
/// Derived from the pi-island logo: horizontal bar with two vertical legs,
/// plus a dot for the "i" in "Pi".
@available(macOS 26.0, *)
struct PiLogoShape: Shape {
    func path(in rect: CGRect) -> Path {
        let w = rect.width
        let h = rect.height
        var path = Path()

        // Horizontal bar (top) — slightly curved ends
        let barY = h * 0.18
        let barH = h * 0.10
        path.addRoundedRect(
            in: CGRect(x: w * 0.05, y: barY, width: w * 0.90, height: barH),
            cornerSize: CGSize(width: barH * 0.5, height: barH * 0.5))

        // Left leg
        let legW = w * 0.12
        path.addRoundedRect(
            in: CGRect(x: w * 0.22, y: barY + barH * 0.5, width: legW, height: h * 0.65),
            cornerSize: CGSize(width: legW * 0.3, height: legW * 0.3))

        // Right leg
        path.addRoundedRect(
            in: CGRect(x: w * 0.58, y: barY + barH * 0.5, width: legW, height: h * 0.55),
            cornerSize: CGSize(width: legW * 0.3, height: legW * 0.3))

        // Dot (the "i" in Pi) — above the right leg
        let dotR = w * 0.07
        path.addEllipse(in: CGRect(
            x: w * 0.82 - dotR, y: h * 0.04,
            width: dotR * 2, height: dotR * 2))

        return path
    }
}

/// Inspector header with Pi logo shape, title, and subtitle.
/// Vertically centered with the toolbar using `state.toolbarHeight`.
@available(macOS 26.0, *)
struct PiLogoHeader: View {
    var state: NavigationSidebarState

    var body: some View {
        HStack(spacing: 8) {
            PiLogoShape()
                .fill(.secondary)
                .frame(width: 16, height: 16)

            VStack(alignment: .leading, spacing: 1) {
                Text(state.inspectorTitle.uppercased())
                    .font(.system(size: 10, weight: .semibold, design: .monospaced))
                    .foregroundStyle(.secondary)

                if !state.inspectorSubtitle.isEmpty {
                    Text(state.inspectorSubtitle)
                        .font(.system(size: 9, weight: .regular, design: .monospaced))
                        .foregroundStyle(.tertiary)
                        .lineLimit(1)
                        .truncationMode(.middle)
                }
            }

            Spacer()
        }
        .frame(height: state.toolbarHeight)
        .padding(.horizontal, 14)
    }
}

// MARK: - Inspector Content View

/// Inspector content view for the detail column (right panel)
/// Will show Pi chat interface
@available(macOS 26.0, *)
struct DetailPlaceholderView: View {
    var state: NavigationSidebarState

    /// Map vibrancy material to NSVisualEffectView.Material
    private var effectMaterial: NSVisualEffectView.Material {
        switch state.vibrancyMaterial {
        case .none: return .windowBackground
        case .ultraThick: return .headerView
        case .thick: return .titlebar
        case .regular: return .menu
        case .thin: return .popover
        case .ultraThin: return .hudWindow
        }
    }

    var body: some View {
        ZStack {
            // Vibrancy background matching the rest of the UI
            if state.vibrancyMaterial != .none {
                VibrancyBackgroundView(
                    material: effectMaterial,
                    blendingMode: .behindWindow,
                    isActive: true
                )
            }

            // Tint layer
            Color(nsColor: state.backgroundColor)
                .opacity(Double(state.backgroundAlpha))

            // Pi Chat Interface (full-bleed, no header)
            PiChatView()
                .frame(maxWidth: .infinity, maxHeight: .infinity)
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity)
        .transaction { $0.animation = nil }
        .ignoresSafeArea(.container, edges: .top)
    }
}
