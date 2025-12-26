import SwiftUI

/// Hyalo header view for Emacs window
/// Displays the formatted mode-line and header-line from Emacs in a unified capsule
/// Mode-line is fixed at top, header-line expands BELOW it
/// Traffic lights appear when hovering the left edge area
@available(macOS 26.0, *)
struct HeaderView: View {
    @ObservedObject var viewModel: HeaderViewModel
    @State private var showTrafficLights: Bool = false
    @State private var isTrafficLightAreaHovered: Bool = false
    
    // MARK: - Layout Constants
    
    private let capsuleCornerRadius: CGFloat = 14
    private let modeLineHeight: CGFloat = 24
    private let headerLineHeight: CGFloat = 22
    private let trafficLightAreaWidth: CGFloat = 70
    private let horizontalPadding: CGFloat = 12
    
    /// Whether header-line should be visible
    private var showHeaderLine: Bool {
        !viewModel.headerLineContent.isEmpty
    }
    
    /// Current capsule height
    private var capsuleHeight: CGFloat {
        showHeaderLine ? modeLineHeight + 1 + headerLineHeight : modeLineHeight
    }
    
    /// Parse a string into LHS and RHS by splitting on 3+ consecutive spaces
    private func parseSegments(_ input: String) -> (lhs: String, rhs: String) {
        // Strip leading/trailing whitespace first
        let content = input.trimmingCharacters(in: .whitespaces)
        // Split on 3+ consecutive spaces (doom-modeline's :align-to spacer)
        if let range = content.range(of: "   +", options: .regularExpression) {
            let lhs = String(content[..<range.lowerBound]).trimmingCharacters(in: .whitespaces)
            let rhs = String(content[range.upperBound...]).trimmingCharacters(in: .whitespaces)
            return (lhs, rhs)
        }
        return (content, "")
    }
    
    /// Mode-line segments (LHS and RHS)
    private var modeLineSegments: (lhs: String, rhs: String) {
        parseSegments(viewModel.modeLineString)
    }
    
    /// Header-line segments (LHS and RHS)
    private var headerLineSegments: (lhs: String, rhs: String) {
        parseSegments(viewModel.headerLineContent)
    }
    
    var body: some View {
        GeometryReader { geometry in
            ZStack(alignment: .topLeading) {
                // Main capsule content
                capsuleContent
                    .frame(width: geometry.size.width, alignment: .topLeading)
                
                // Traffic light hover zone - overlay at top-left
                Color.clear
                    .frame(width: trafficLightAreaWidth, height: capsuleHeight)
                    .contentShape(Rectangle())
                    .onHover { hovering in
                        isTrafficLightAreaHovered = hovering
                        withAnimation(.spring(response: 0.25, dampingFraction: 0.8)) {
                            showTrafficLights = hovering
                        }
                    }
            }
            // Block click-through
            .contentShape(RoundedRectangle(cornerRadius: capsuleCornerRadius))
            .onTapGesture { }
        }
    }
    
    // MARK: - Capsule Content
    
    private var capsuleContent: some View {
        VStack(spacing: 0) {
            // Mode-line row
            modeLineRow
                .frame(height: modeLineHeight)
            
            // Header-line row (expands below)
            if showHeaderLine {
                Divider()
                    .background(Color.primary.opacity(0.2))
                
                headerLineRow
                    .frame(height: headerLineHeight)
            }
        }
        .frame(height: capsuleHeight, alignment: .top)
        .background(capsuleBackground)
        .clipShape(RoundedRectangle(cornerRadius: capsuleCornerRadius))
        .animation(.spring(response: 0.3, dampingFraction: 0.8), value: capsuleHeight)
    }
    
    // MARK: - Mode-Line Row
    
    private var modeLineRow: some View {
        let segments = modeLineSegments
        
        return HStack(spacing: 0) {
            // Traffic lights (animated slide-in from left)
            if showTrafficLights {
                trafficLightsView
                    .transition(.asymmetric(
                        insertion: .move(edge: .leading).combined(with: .opacity),
                        removal: .move(edge: .leading).combined(with: .opacity)
                    ))
            }
            
            // LHS - anchored left, truncates if needed
            ModeLineTextView(text: segments.lhs, fontSize: 11)
                .lineLimit(1)
            
            // Flexible space between LHS and RHS
            Spacer(minLength: 8)
            
            // RHS - anchored right
            if !segments.rhs.isEmpty {
                ModeLineTextView(text: segments.rhs, fontSize: 11)
                    .lineLimit(1)
            }
        }
        .padding(.leading, showTrafficLights ? 0 : horizontalPadding)
        .padding(.trailing, horizontalPadding)
        .animation(.spring(response: 0.25, dampingFraction: 0.8), value: showTrafficLights)
    }
    
    // MARK: - Traffic Lights
    
    private var trafficLightsView: some View {
        HStack(spacing: 8) {
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
        .padding(.leading, 12)
        .padding(.trailing, 8)  // Space after traffic lights before mode-line text
    }
    
    // MARK: - Header-Line Row
    
    private var headerLineRow: some View {
        let segments = headerLineSegments
        
        return HStack(spacing: 0) {
            // LHS
            ModeLineTextView(text: segments.lhs, fontSize: 10)
                .lineLimit(1)
                .opacity(0.8)
            
            Spacer(minLength: 8)
            
            // RHS
            if !segments.rhs.isEmpty {
                ModeLineTextView(text: segments.rhs, fontSize: 10)
                    .lineLimit(1)
                    .opacity(0.8)
            }
        }
        .padding(.horizontal, horizontalPadding)
        .transition(.move(edge: .top).combined(with: .opacity))
    }
    
    // MARK: - Background
    
    @ViewBuilder
    private var capsuleBackground: some View {
        if #available(macOS 26.0, *) {
            Color.clear
                .glassEffect(.regular.interactive(), in: .rect(cornerRadius: capsuleCornerRadius))
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

// MARK: - Traffic Light Button

/// Traffic light button type
enum TrafficLightType {
    case close
    case minimize
    case zoom
}

@available(macOS 26.0, *)
struct TrafficLightButton: View {
    let type: TrafficLightType
    let color: Color
    let action: () -> Void
    
    @State private var isHovering = false
    @State private var isPressed = false
    
    private let size: CGFloat = 12
    
    var body: some View {
        // Simple filled circle button
        Circle()
            .fill(color)
            .frame(width: size, height: size)
            .overlay(
                Circle()
                    .strokeBorder(Color.black.opacity(0.2), lineWidth: 0.5)
            )
            .scaleEffect(isPressed ? 0.85 : (isHovering ? 1.1 : 1.0))
            .animation(.easeInOut(duration: 0.1), value: isHovering)
            .animation(.easeInOut(duration: 0.05), value: isPressed)
            .onHover { hovering in
                isHovering = hovering
            }
            .onTapGesture {
                action()
            }
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
