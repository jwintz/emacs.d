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
    var onBufferSelect: ((String) -> Void)?  // Called when a buffer is selected in sidebar
    var onBufferClose: ((String) -> Void)?  // Called when a buffer close button is clicked
    var onFileSelect: ((String) -> Void)?  // Called when a file is selected in sidebar
    var onSidebarVisibilityChanged: ((Bool) -> Void)?  // Called when sidebar toggles
    var onDetailVisibilityChanged: ((Bool) -> Void)?  // Called when inspector toggles

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
    var body: some View {
        GeometryReader { geometry in
            // 2-column NavigationSplitView with inspector for symmetric toolbar behavior
            NavigationSplitView(columnVisibility: $columnVisibility) {
                // Sidebar column (left)
                SidebarContentView(
                    state: state,
                    onBufferSelect: { bufferName in onBufferSelect?(bufferName) },
                    onBufferClose: onBufferClose.map { callback in
                        { bufferName in callback(bufferName) }
                    },
                    onFileSelect: { filePath in onFileSelect?(filePath) }
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
                    DetailPlaceholderView(state: state)
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
            .toolbarBackgroundVisibility(state.decorationsVisible ? .visible : .hidden, for: .windowToolbar)
            .toolbarTitleDisplayMode(.inline)
            .toolbar {
                // Inspector toggle button - symmetric to sidebar toggle
                // Visibility animated via AppKit in NavigationSidebarController.setToolbarItemsVisible
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
                onSidebarVisibilityChanged?(newValue)
                DispatchQueue.main.asyncAfter(deadline: .now() + 0.3) {
                    onGeometryUpdate?()
                }
            }
            .onChange(of: columnVisibility) { _, newValue in
                // Sync state.sidebarVisible when NavigationSplitView's built-in toggle is used
                let isSidebarNowVisible = (newValue == .all || newValue == .doubleColumn)
                if state.sidebarVisible != isSidebarNowVisible {
                    state.sidebarVisible = isSidebarNowVisible
                    onSidebarVisibilityChanged?(isSidebarNowVisible)
                }
                DispatchQueue.main.asyncAfter(deadline: .now() + 0.3) {
                    onGeometryUpdate?()
                }
            }
            .onChange(of: state.detailVisible) { _, newValue in
                onDetailVisibilityChanged?(newValue)
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
            }
        }
    }
}
