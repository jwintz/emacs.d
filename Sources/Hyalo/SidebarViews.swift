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
    var isBusy: Bool = false
    var isFirst: Bool = false

    @State private var rotation: Double = 0

    /// Standard margin matching embedded frame padding
    private let sideMargin: CGFloat = 12

    var body: some View {
        HStack(spacing: 6) {
            Image(systemName: systemImage)
                .font(.system(size: 10, weight: .medium))
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
                .font(.system(size: 11, weight: .semibold))
                .foregroundStyle(.secondary)
            Spacer()
        }
        .padding(.horizontal, sideMargin)
        .padding(.top, isFirst ? 6 : sideMargin)
        .padding(.bottom, 6)
    }
}

/// Custom NSSplitView wrapper for sidebar that allows divider customization
@available(macOS 26.0, *)
struct StyledSplitView<TopContent: View, BottomContent: View>: NSViewRepresentable {
    let topContent: TopContent
    let bottomContent: BottomContent
    let dividerColor: NSColor
    let dividerThickness: CGFloat

    init(
        dividerColor: NSColor = .separatorColor,
        dividerThickness: CGFloat = 1,
        @ViewBuilder top: () -> TopContent,
        @ViewBuilder bottom: () -> BottomContent
    ) {
        self.dividerColor = dividerColor
        self.dividerThickness = dividerThickness
        self.topContent = top()
        self.bottomContent = bottom()
    }

    func makeNSView(context: Context) -> NSSplitView {
        let splitView = CustomDividerSplitView()
        splitView.isVertical = false  // Vertical stacking (horizontal divider)
        splitView.dividerStyle = .thin
        splitView.customDividerColor = dividerColor
        splitView.customDividerThickness = dividerThickness
        splitView.delegate = context.coordinator

        // Create hosting views for SwiftUI content
        let topHost = NSHostingView(rootView: topContent)
        let bottomHost = NSHostingView(rootView: bottomContent)

        topHost.translatesAutoresizingMaskIntoConstraints = false
        bottomHost.translatesAutoresizingMaskIntoConstraints = false

        splitView.addArrangedSubview(topHost)
        splitView.addArrangedSubview(bottomHost)

        // Set initial proportions
        splitView.setHoldingPriority(.defaultLow, forSubviewAt: 0)
        splitView.setHoldingPriority(.defaultLow, forSubviewAt: 1)

        return splitView
    }

    func updateNSView(_ nsView: NSSplitView, context: Context) {
        if let customSplit = nsView as? CustomDividerSplitView {
            customSplit.customDividerColor = dividerColor
            customSplit.customDividerThickness = dividerThickness
            customSplit.needsDisplay = true
        }
    }

    func makeCoordinator() -> Coordinator {
        Coordinator()
    }

    class Coordinator: NSObject, NSSplitViewDelegate {
        func splitView(_ splitView: NSSplitView, constrainMinCoordinate proposedMinimumPosition: CGFloat, ofSubviewAt dividerIndex: Int) -> CGFloat {
            return 80  // Minimum height for top pane
        }

        func splitView(_ splitView: NSSplitView, constrainMaxCoordinate proposedMaximumPosition: CGFloat, ofSubviewAt dividerIndex: Int) -> CGFloat {
            return splitView.bounds.height - 150  // Minimum height for bottom pane
        }
    }
}

/// Custom NSSplitView subclass with configurable divider appearance
@available(macOS 26.0, *)
final class CustomDividerSplitView: NSSplitView {
    var customDividerColor: NSColor = .separatorColor
    var customDividerThickness: CGFloat = 1

    override var dividerColor: NSColor {
        return customDividerColor
    }

    override var dividerThickness: CGFloat {
        return customDividerThickness
    }
}

/// The sidebar content with Liquid Glass styling
/// Shows embedded Emacs views when available, falls back to SwiftUI List
@available(macOS 26.0, *)
struct SidebarContentView: View {
    var state: NavigationSidebarState
    var onResize: ((String, CGFloat, CGFloat) -> Void)?

    /// SwiftUI-side margin for embedded frames (matches Emacs internal-border-width)
    private let embeddedMargin: CGFloat = 12

    var body: some View {
        // If embedded views are available, use custom StyledSplitView with Emacs frames
        if let topView = state.leftTopView, let bottomView = state.leftBottomView {
            StyledSplitView(
              dividerColor: .separatorColor,
                dividerThickness: 1,
                top: {
                    VStack(spacing: 0) {
                        SidebarSectionHeader(title: "OPEN BUFFERS", systemImage: "document.on.document.fill", isFirst: true)
                        EmbeddedEmacsView(embeddedView: topView, originalWindow: state.leftTopWindow, slot: "left-top", onResize: onResize)
                            .padding(.horizontal, embeddedMargin)
                            .padding(.bottom, embeddedMargin)
                            .frame(maxWidth: .infinity, maxHeight: .infinity)
                    }
                },
                bottom: {
                    VStack(spacing: 0) {
                        SidebarSectionHeader(title: "WORKSPACE", systemImage: "folder.fill")
                        EmbeddedEmacsView(embeddedView: bottomView, originalWindow: state.leftBottomWindow, slot: "left-bottom", onResize: onResize)
                            .padding(.horizontal, embeddedMargin)
                            .padding(.bottom, embeddedMargin)
                            .frame(maxWidth: .infinity, maxHeight: .infinity)
                    }
                }
            )
            .transaction { $0.animation = nil }  // Disable animation to prevent resize flicker
            .background {
                Color(nsColor: state.backgroundColor)
                    .opacity(Double(state.backgroundAlpha))
                    .ignoresSafeArea()
            }
        } else if let topView = state.leftTopView {
            // Only top view available
            VStack(spacing: 0) {
                SidebarSectionHeader(title: "OPEN BUFFERS", systemImage: "doc.on.doc", isFirst: true)
                EmbeddedEmacsView(embeddedView: topView, originalWindow: state.leftTopWindow, slot: "left-top", onResize: onResize)
                    .padding(.horizontal, embeddedMargin)
                    .padding(.bottom, embeddedMargin)
                    .frame(maxWidth: .infinity, maxHeight: .infinity)
            }
            .transaction { $0.animation = nil }
            .background {
                Color(nsColor: state.backgroundColor)
                    .opacity(Double(state.backgroundAlpha))
                    .ignoresSafeArea()
            }
        } else if let bottomView = state.leftBottomView {
            // Only bottom view available
            VStack(spacing: 0) {
                SidebarSectionHeader(title: "WORKSPACE", systemImage: "folder", isFirst: true)
                EmbeddedEmacsView(embeddedView: bottomView, originalWindow: state.leftBottomWindow, slot: "left-bottom", onResize: onResize)
                    .padding(.horizontal, embeddedMargin)
                    .padding(.bottom, embeddedMargin)
                    .frame(maxWidth: .infinity, maxHeight: .infinity)
            }
            .transaction { $0.animation = nil }
            .background {
                Color(nsColor: state.backgroundColor)
                    .opacity(Double(state.backgroundAlpha))
                    .ignoresSafeArea()
            }
        } else {
            // Empty placeholder when no embedded views
            Color.clear
                .background {
                    Color(nsColor: state.backgroundColor)
                        .opacity(Double(state.backgroundAlpha))
                        .ignoresSafeArea()
                }
        }
    }
}

/// Inspector content view for the detail column (right panel)
/// Shows embedded agent-shell when available, otherwise placeholder
@available(macOS 26.0, *)
struct DetailPlaceholderView: View {
    var state: NavigationSidebarState
    var onResize: ((String, CGFloat, CGFloat) -> Void)?

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

            // Show embedded agent-shell or placeholder
            if let rightView = state.rightView {
                VStack(spacing: 0) {
                    SidebarSectionHeader(
                        title: state.inspectorTitle.uppercased(),
                        systemImage: state.inspectorIcon,
                        isBusy: state.inspectorBusy
                    )
                    EmbeddedEmacsView(embeddedView: rightView, originalWindow: state.rightWindow, slot: "right", onResize: onResize)
                        .padding(.top, 6)
                        .padding(.horizontal, 12)
                        .padding(.bottom, 12)
                        .frame(maxWidth: .infinity, maxHeight: .infinity)
                }
                .padding(.top, 6)
            } else {
                // Placeholder content
                VStack(spacing: 16) {
                    Image(systemName: "sparkles")
                        .font(.system(size: 48, weight: .ultraLight))
                        .foregroundStyle(.tertiary)

                    Text("Inspector")
                        .font(.headline)
                        .foregroundStyle(.secondary)

                    Text("Run hyalo-sidebar-right-setup\nto embed agent-shell")
                        .font(.caption)
                        .foregroundStyle(.tertiary)
                        .multilineTextAlignment(.center)
                }
            }
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity)
        .transaction { $0.animation = nil }  // Disable all animation to prevent resize
        .ignoresSafeArea(.container, edges: .top)
    }
}
