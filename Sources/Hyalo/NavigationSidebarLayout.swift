// NavigationSidebarLayout.swift - Main NavigationSplitView layout
// Copyright (C) 2025
// Target: macOS 26 Tahoe with Liquid Glass design

import AppKit
import SwiftUI

// MARK: - Main Navigation Layout

/// The main NavigationSplitView layout with Liquid Glass styling
@available(macOS 26.0, *)
struct HyaloNavigationLayout: View {
    var state: NavigationSidebarState
    let emacsView: NSView

    var onGeometryUpdate: (() -> Void)?  // Called when sidebar/inspector visibility changes
    var onEmbeddedResize: ((String, CGFloat, CGFloat) -> Void)?  // Called when embedded view resizes
    var onSidebarVisibilityChanged: ((Bool, Bool) -> Void)?  // Called when sidebar toggles (visible, needsSetup)
    var onDetailVisibilityChanged: ((Bool, Bool) -> Void)?  // Called when inspector toggles (visible, needsSetup)

    @State private var columnVisibility: NavigationSplitViewVisibility = .detailOnly

    /// Binding for inspector (right panel) visibility
    /// Uses withTransaction to disable animation and prevent window resize flicker
    private var inspectorVisibleBinding: Binding<Bool> {
        Binding(
            get: { state.detailVisible },
            set: { newValue in
                var transaction = Transaction()
                transaction.disablesAnimations = true
                withTransaction(transaction) {
                    state.detailVisible = newValue
                }
            }
        )
    }

    /// Map vibrancy material to NSVisualEffectView.Material
    private var effectMaterialForState: NSVisualEffectView.Material {
        switch state.vibrancyMaterial {
        case .none: return .windowBackground
        case .ultraThick: return .headerView
        case .thick: return .titlebar
        case .regular: return .menu
        case .thin: return .popover
        case .ultraThin: return .hudWindow
        }
    }

    /// Parse modeline into LHS and RHS segments
    private func parseSegments(_ input: String) -> (lhs: String, rhs: String) {
        let trimmed = input.trimmingCharacters(in: .whitespaces)
        if let range = trimmed.range(of: "   +", options: .regularExpression) {
            let lhs = String(trimmed[..<range.lowerBound]).trimmingCharacters(in: .whitespaces)
            let rhs = String(trimmed[range.upperBound...]).trimmingCharacters(in: .whitespaces)
            return (lhs, rhs)
        }
        return (trimmed, "")
    }

    /// Log toolbar metrics for debugging layout calculations
    private func logToolbarMetrics() {
        guard let window = NSApp.keyWindow else {
            return
        }

        // Traffic light buttons (close, minimize, zoom)
        var trafficLightWidth: CGFloat = 0
        var trafficLightDetails: [String] = []

        if let closeButton = window.standardWindowButton(.closeButton) {
            let frame = closeButton.frame
            trafficLightDetails.append("close: x=\(frame.origin.x), w=\(frame.width)")
            trafficLightWidth = max(trafficLightWidth, frame.maxX)
        }
        if let miniButton = window.standardWindowButton(.miniaturizeButton) {
            let frame = miniButton.frame
            trafficLightDetails.append("minimize: x=\(frame.origin.x), w=\(frame.width)")
            trafficLightWidth = max(trafficLightWidth, frame.maxX)
        }
        if let zoomButton = window.standardWindowButton(.zoomButton) {
            let frame = zoomButton.frame
            trafficLightDetails.append("zoom: x=\(frame.origin.x), w=\(frame.width)")
            trafficLightWidth = max(trafficLightWidth, frame.maxX)
        }

        // Toolbar and its items
        if window.toolbar != nil {
            if let contentView = window.contentView {
                findToolbarItems(in: window, contentView: contentView)
            }
        }

        // Sidebar toggle button
        if let contentView = window.contentView {
            findSidebarToggle(in: contentView)
        }
    }

    /// Find toolbar items in the view hierarchy and log their frames
    private func findToolbarItems(in window: NSWindow, contentView: NSView) {
        if let titlebarView = window.standardWindowButton(.closeButton)?.superview?.superview {
            enumerateViews(titlebarView, depth: 0) { view, depth in
                let viewType = String(describing: type(of: view))
                let _ = String(repeating: "  ", count: depth)

                if viewType.contains("ToolbarItem") || viewType.contains("Button") || viewType.contains("Sidebar") {
                    // Debug logging if needed
                }
            }
        }
    }

    /// Find the sidebar toggle button
    private func findSidebarToggle(in view: NSView) {
        enumerateViews(view, depth: 0) { subview, _ in
            let viewType = String(describing: type(of: subview))

            if viewType.contains("SidebarToggle") || viewType.contains("NSSplitViewItemViewControllerWrapperView") {
                // Debug logging if needed
            }

            if let button = subview as? NSButton {
                if let image = button.image, image.name()?.contains("sidebar") == true {
                    // Debug logging if needed
                }
            }
        }
    }

    /// Enumerate views recursively
    private func enumerateViews(_ view: NSView, depth: Int, handler: (NSView, Int) -> Void) {
        handler(view, depth)
        for subview in view.subviews {
            enumerateViews(subview, depth: depth + 1, handler: handler)
        }
    }

    var body: some View {
        GeometryReader { geometry in
            // 2-column NavigationSplitView with inspector for symmetric toolbar behavior
            NavigationSplitView(columnVisibility: $columnVisibility) {
                // Sidebar column (left)
                SidebarContentView(
                    state: state,
                    onResize: onEmbeddedResize
                )
                .navigationSplitViewColumnWidth(min: 200, ideal: 280, max: 400)
                .background {
                    // Track sidebar width for modeline calculation
                    GeometryReader { sidebarGeometry in
                        Color.clear
                            .onAppear {
                                state.sidebarWidth = sidebarGeometry.size.width
                            }
                            .onChange(of: sidebarGeometry.size.width) { _, newWidth in
                                state.sidebarWidth = newWidth
                                onGeometryUpdate?()
                            }
                    }
                }
            } detail: {
                // Content/Detail column - Emacs with inspector
                EmacsContentView(
                    emacsView: emacsView,
                    state: state
                )
                .frame(maxWidth: .infinity, maxHeight: .infinity)
                .background {
                    // Track content column width for modeline calculation
                    GeometryReader { contentGeometry in
                        Color.clear
                            .onAppear {
                                state.contentWidth = contentGeometry.size.width
                            }
                            .onChange(of: contentGeometry.size.width) { _, newWidth in
                                state.contentWidth = newWidth
                                onGeometryUpdate?()
                            }
                    }
                }
                // Inspector (right panel) - symmetric to sidebar
                .inspector(isPresented: inspectorVisibleBinding) {
                    DetailPlaceholderView(state: state, onResize: onEmbeddedResize)
                        .inspectorColumnWidth(min: 300, ideal: 400, max: 500)
                        .background {
                            // Track inspector width
                            GeometryReader { inspectorGeometry in
                                Color.clear
                                    .onAppear {
                                        state.detailWidth = inspectorGeometry.size.width
                                    }
                                    .onChange(of: inspectorGeometry.size.width) { _, newWidth in
                                        state.detailWidth = newWidth
                                        onGeometryUpdate?()
                                    }
                            }
                        }
                }
            }
            .navigationSplitViewStyle(.balanced)
            .transaction { $0.disablesAnimations = true }
            .toolbarBackgroundVisibility(.visible, for: .windowToolbar)
            .toolbarTitleDisplayMode(.inline)
            .toolbar {
                // Inspector toggle button - symmetric to sidebar toggle
                ToolbarItem(placement: .primaryAction) {
                    Button {
                        var transaction = Transaction()
                        transaction.disablesAnimations = true
                        withTransaction(transaction) {
                            state.detailVisible.toggle()
                        }
                    } label: {
                        Image(systemName: "sidebar.right")
                            .symbolVariant(state.detailVisible ? .none : .none)
                    }
                    .help(state.detailVisible ? "Hide Inspector" : "Show Inspector")
                }
            }
            .background {
                ZStack {
                    VibrancyBackgroundView(
                        material: effectMaterialForState,
                        blendingMode: .behindWindow,
                        isActive: state.vibrancyMaterial != .none
                    )
                    Color(nsColor: state.backgroundColor)
                        .opacity(Double(state.backgroundAlpha))
                }
                .ignoresSafeArea()
            }
            .onChange(of: state.sidebarVisible) { _, newValue in
                withAnimation {
                    columnVisibility = newValue ? .all : .detailOnly
                }
                let needsSetup = newValue && (state.leftTopView == nil || state.leftBottomView == nil)
                onSidebarVisibilityChanged?(newValue, needsSetup)
                DispatchQueue.main.asyncAfter(deadline: .now() + 0.3) {
                    onGeometryUpdate?()
                }
            }
            .onChange(of: columnVisibility) { _, newValue in
                // Sync state.sidebarVisible when NavigationSplitView's built-in toggle is used
                let isSidebarNowVisible = (newValue == .all || newValue == .doubleColumn)
                if state.sidebarVisible != isSidebarNowVisible {
                    state.sidebarVisible = isSidebarNowVisible
                    let needsSetup = isSidebarNowVisible && (state.leftTopView == nil || state.leftBottomView == nil)
                    onSidebarVisibilityChanged?(isSidebarNowVisible, needsSetup)
                }
                DispatchQueue.main.asyncAfter(deadline: .now() + 0.3) {
                    onGeometryUpdate?()
                }
            }
            .onChange(of: state.detailVisible) { _, newValue in
                let needsSetup = newValue && state.rightView == nil
                onDetailVisibilityChanged?(newValue, needsSetup)
                DispatchQueue.main.asyncAfter(deadline: .now() + 0.3) {
                    onGeometryUpdate?()
                }
            }
            .onChange(of: geometry.size) { _, newSize in
                state.toolbarWidth = newSize.width
                onGeometryUpdate?()
            }
            .onAppear {
                state.toolbarWidth = geometry.size.width
                logToolbarMetrics()
            }
        }
    }
}
