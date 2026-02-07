import SwiftUI

/// Hyalo header view for Emacs window
/// Displays the formatted mode-line and header-line from Emacs in a unified capsule
/// Mode-line is fixed at top, header-line expands BELOW it
/// Traffic lights appear when hovering the left edge area
@available(macOS 26.0, *)
struct HeaderView: View {
    @Bindable var viewModel: HeaderViewModel
    @State private var showTrafficLights: Bool = false
    @State private var isTrafficLightAreaHovered: Bool = false

    // MARK: - Computed Properties

    /// Whether header-line should be visible
    private var showHeaderLine: Bool {
        !viewModel.headerLineContent.isEmpty
    }

    /// Current capsule height
    private var capsuleHeight: CGFloat {
        let baseHeight = HyaloDesign.Height.modeLine
        return showHeaderLine
            ? baseHeight + 1 + HyaloDesign.Height.headerLine
            : baseHeight
    }

    var body: some View {
        GeometryReader { geometry in
            ZStack(alignment: .topLeading) {
                // Main capsule content
                capsuleContent
                    .frame(width: geometry.size.width, alignment: .topLeading)

                // Traffic light hover zone - overlay at top-left
                trafficLightHoverZone
            }
            // Block click-through
            .contentShape(RoundedRectangle(cornerRadius: HyaloDesign.CornerRadius.capsule))
            .onTapGesture { }
        }
    }

    // MARK: - Capsule Content

    private var capsuleContent: some View {
        VStack(spacing: 0) {
            // Mode-line row
            ModeLineRowView(
                viewModel: viewModel,
                showTrafficLights: showTrafficLights
            )
            .frame(height: HyaloDesign.Height.modeLine)

            // Header-line row (expands below)
            if showHeaderLine {
                Divider()
                    .background(Color.primary.opacity(0.2))

                HeaderLineRowView(
                    viewModel: viewModel
                )
                .frame(height: HyaloDesign.Height.headerLine)
            }
        }
        .frame(height: capsuleHeight, alignment: .top)
        .background(capsuleBackground)
        .clipShape(RoundedRectangle(cornerRadius: HyaloDesign.CornerRadius.capsule))
        .animation(.spring(response: 0.3, dampingFraction: 0.8), value: capsuleHeight)
    }

    // MARK: - Traffic Light Hover Zone

    private var trafficLightHoverZone: some View {
        Color.clear
            .frame(width: HyaloDesign.Width.trafficLights + 4, height: capsuleHeight)
            .contentShape(Rectangle())
            .onHover { hovering in
                isTrafficLightAreaHovered = hovering
                withAnimation(.spring(response: 0.25, dampingFraction: 0.8)) {
                    showTrafficLights = hovering
                }
            }
    }

    // MARK: - Background

    @ViewBuilder
    private var capsuleBackground: some View {
        if #available(macOS 26.0, *) {
            Color.clear
                .glassEffect(.regular.interactive(), in: .rect(cornerRadius: HyaloDesign.CornerRadius.capsule))
        } else {
            ZStack {
                Rectangle()
                    .fill(.ultraThinMaterial)
                Rectangle()
                    .fill(Color.white.opacity(0.1))
            }
        }
    }
}

// MARK: - Mode Line Row

@available(macOS 26.0, *)
struct ModeLineRowView: View {
    @Bindable var viewModel: HeaderViewModel
    let showTrafficLights: Bool

    var body: some View {
        let segments = ModeLineParser.parseSegments(viewModel.modeLineString)

        return HStack(spacing: 0) {
            // Traffic lights (animated slide-in from left)
            if showTrafficLights {
                TrafficLightsView()
                    .transition(.asymmetric(
                        insertion: .move(edge: .leading).combined(with: .opacity),
                        removal: .move(edge: .leading).combined(with: .opacity)
                    ))
            }

            // LHS - anchored left, has priority (truncates last)
            ModeLineTextView(
                text: segments.lhs,
                fontSize: HyaloDesign.FontSize.body,
                onTap: { relativeX in
                    viewModel.handleModeLineClick(segment: "lhs", relativePosition: Double(relativeX))
                }
            )
            .lineLimit(1)
            .layoutPriority(1)

            // Flexible space between LHS and RHS
            Spacer(minLength: HyaloDesign.Spacing.compact)

            // RHS - anchored right, truncates first when space is limited
            if !segments.rhs.isEmpty {
                ModeLineTextView(
                    text: segments.rhs,
                    fontSize: HyaloDesign.FontSize.body,
                    onTap: { relativeX in
                        viewModel.handleModeLineClick(segment: "rhs", relativePosition: Double(relativeX))
                    }
                )
                .lineLimit(1)
                .truncationMode(.head)
                .layoutPriority(0)
            }
        }
        .padding(.leading, showTrafficLights ? 0 : HyaloDesign.Padding.horizontal)
        .padding(.trailing, HyaloDesign.Padding.horizontal)
        .animation(.spring(response: 0.25, dampingFraction: 0.8), value: showTrafficLights)
    }
}

// MARK: - Header Line Row

@available(macOS 26.0, *)
struct HeaderLineRowView: View {
    @Bindable var viewModel: HeaderViewModel

    var body: some View {
        let segments = ModeLineParser.parseSegments(viewModel.headerLineContent)

        return HStack(spacing: 0) {
            // LHS - has priority (truncates last)
            ModeLineTextView(
                text: segments.lhs,
                fontSize: HyaloDesign.FontSize.caption
            )
            .lineLimit(1)
            .layoutPriority(1)
            .opacity(0.8)

            Spacer(minLength: HyaloDesign.Spacing.compact)

            // RHS - truncates first when space is limited
            if !segments.rhs.isEmpty {
                ModeLineTextView(
                    text: segments.rhs,
                    fontSize: HyaloDesign.FontSize.caption
                )
                .lineLimit(1)
                .truncationMode(.head)
                .layoutPriority(0)
                .opacity(0.8)
            }
        }
        .padding(.horizontal, HyaloDesign.Padding.horizontal)
        .transition(.move(edge: .top).combined(with: .opacity))
    }
}

// MARK: - Traffic Lights

@available(macOS 26.0, *)
struct TrafficLightsView: View {
    var body: some View {
        HStack(spacing: HyaloDesign.Spacing.standard) {
            TrafficLightButton(type: .close, color: .red) {
                NSApp.keyWindow?.performClose(nil)
            }
            TrafficLightButton(type: .minimize, color: .yellow) {
                NSApp.keyWindow?.performMiniaturize(nil)
            }
            TrafficLightButton(type: .zoom, color: .green) {
                if let window = NSApp.keyWindow {
                    if NSEvent.modifierFlags.contains(.option) {
                        window.zoom(nil)
                    } else {
                        window.toggleFullScreen(nil)
                    }
                }
            }
        }
        .padding(.leading, HyaloDesign.Padding.horizontal)
        .padding(.trailing, HyaloDesign.Padding.compact)
    }
}

// MARK: - Traffic Light Button

@available(macOS 26.0, *)
struct TrafficLightButton: View {
    enum ButtonType {
        case close
        case minimize
        case zoom
    }

    let type: ButtonType
    let color: Color
    let action: () -> Void

    @State private var isPressed = false

    private let size: CGFloat = 12

    var body: some View {
        Button(action: action) {
            Circle()
                .fill(color)
                .frame(width: size, height: size)
        }
        .buttonStyle(TrafficLightButtonStyle(color: color))
        .simultaneousGesture(
            DragGesture(minimumDistance: 0)
                .onChanged { _ in isPressed = true }
                .onEnded { _ in isPressed = false }
        )
    }
}

// MARK: - Preview

@available(macOS 26.0, *)
#Preview {
    let viewModel = HeaderViewModel()

    return HeaderView(viewModel: viewModel)
        .frame(width: 800, height: 100)
        .background(Color.gray.opacity(0.3))
        .onAppear {
            Task { @MainActor in
                viewModel.updateModeLine(" U:--- init.el  Top L1     Git-main (ELisp/l ElDoc)")
            }
        }
}
