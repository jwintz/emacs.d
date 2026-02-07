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

    var onGeometryUpdate: (() -> Void)?
    var onBufferSelect: ((String) -> Void)?
    var onBufferClose: ((String) -> Void)?
    var onFileSelect: ((String) -> Void)?
    var onSidebarVisibilityChanged: ((Bool) -> Void)?
    var onDetailVisibilityChanged: ((Bool) -> Void)?

    @State private var columnVisibility: NavigationSplitViewVisibility = .detailOnly

    // MARK: - Bindings

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

    // MARK: - Computed Properties

    /// Map vibrancy material to NSVisualEffectView.Material
    private var vibrancyMaterial: NSVisualEffectView.Material {
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
        GeometryReader { geometry in
            // 2-column NavigationSplitView with inspector for symmetric toolbar behavior
            NavigationSplitView(columnVisibility: $columnVisibility) {
                sidebarColumn
            } detail: {
                contentColumn
                    .inspector(isPresented: inspectorVisibleBinding) {
                        inspectorColumn
                    }
            }
            .navigationSplitViewStyle(.balanced)
            .transaction { $0.disablesAnimations = true }
            .toolbarBackgroundVisibility(state.decorationsVisible ? .visible : .hidden, for: .windowToolbar)
            .toolbarTitleDisplayMode(.inline)
            .toolbar {
                NavigationToolbarContent(state: state)
            }
            .background {
                ZStack {
                    vibrancyBackground
                    tintOverlay
                }
                .ignoresSafeArea()
            }
            .onChange(of: state.sidebarVisible) { _, newValue in
                updateSidebarVisibility(newValue)
            }
            .onChange(of: columnVisibility) { _, newValue in
                syncSidebarVisibility(newValue)
            }
            .onChange(of: state.detailVisible) { _, newValue in
                updateDetailVisibility(newValue)
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

    // MARK: - Column Views

    private var sidebarColumn: some View {
        SidebarContentView(
            state: state,
            onBufferSelect: { onBufferSelect?($0) },
            onBufferClose: onBufferClose.map { callback in
                { bufferName in callback(bufferName) }
            },
            onFileSelect: { onFileSelect?($0) }
        )
        .navigationSplitViewColumnWidth(
            min: HyaloDesign.Width.sidebarMin,
            ideal: HyaloDesign.Width.sidebarIdeal,
            max: HyaloDesign.Width.sidebarMax
        )
        .background(
            GeometrySizeTracker(
                onChange: { state.sidebarWidth = $0 },
                onUpdate: { onGeometryUpdate?() }
            )
        )
    }

    private var contentColumn: some View {
        EmacsContentView(
            emacsView: emacsView,
            state: state
        )
        .frame(maxWidth: .infinity, maxHeight: .infinity)
        .background(
            GeometrySizeTracker(
                onChange: { state.contentWidth = $0 },
                onUpdate: { onGeometryUpdate?() }
            )
        )
    }

    private var inspectorColumn: some View {
        DetailPlaceholderView(state: state)
            .inspectorColumnWidth(
                min: HyaloDesign.Width.inspectorMin,
                ideal: HyaloDesign.Width.inspectorIdeal,
                max: HyaloDesign.Width.inspectorMax
            )
            .background(
                GeometrySizeTracker(
                    onChange: { state.detailWidth = $0 },
                    onUpdate: { onGeometryUpdate?() }
                )
            )
    }

    // MARK: - Background Layers

    private var vibrancyBackground: some View {
        VibrancyBackgroundView(
            material: vibrancyMaterial,
            blendingMode: .behindWindow,
            isActive: state.vibrancyMaterial != .none
        )
    }

    private var tintOverlay: some View {
        Color(nsColor: state.backgroundColor)
            .opacity(Double(state.backgroundAlpha))
    }

    // MARK: - Visibility Updates

    private func updateSidebarVisibility(_ visible: Bool) {
        withAnimation {
            columnVisibility = visible ? .all : .detailOnly
        }
        onSidebarVisibilityChanged?(visible)
        triggerGeometryUpdate()
    }

    private func syncSidebarVisibility(_ visibility: NavigationSplitViewVisibility) {
        // Sync state.sidebarVisible when NavigationSplitView's built-in toggle is used
        let isSidebarNowVisible = (visibility == .all || visibility == .doubleColumn)
        if state.sidebarVisible != isSidebarNowVisible {
            state.sidebarVisible = isSidebarNowVisible
            onSidebarVisibilityChanged?(isSidebarNowVisible)
        }
        triggerGeometryUpdate()
    }

    private func updateDetailVisibility(_ visible: Bool) {
        onDetailVisibilityChanged?(visible)
        triggerGeometryUpdate()
    }

    private func triggerGeometryUpdate() {
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.3) {
            onGeometryUpdate?()
        }
    }
}

// MARK: - Navigation Toolbar Content

@available(macOS 26.0, *)
struct NavigationToolbarContent: ToolbarContent {
    @Bindable var state: NavigationSidebarState

    var body: some ToolbarContent {
        ToolbarItem(placement: .primaryAction) {
            inspectorToggleButton
        }
    }

    private var inspectorToggleButton: some View {
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
