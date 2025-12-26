// AppearancePanel.swift - SwiftUI popup for Hyalo appearance settings
// Follows macOS Tahoe Liquid Glass design principles

import SwiftUI
import AppKit

// MARK: - Appearance Preset

@available(macOS 26.0, *)
enum AppearancePreset: String, CaseIterable, Identifiable {
    case clear = "Clear"
    case balanced = "Balanced"
    case solid = "Solid"
    case custom = "Custom"

    var id: String { rawValue }

    var transparency: Double {
        switch self {
        case .clear: return 0.9      // 90% see-through
        case .balanced: return 0.5   // 50% see-through
        case .solid: return 0.1      // 10% see-through
        case .custom: return 0.5
        }
    }
}

// MARK: - Appearance Settings Model

@available(macOS 26.0, *)
@Observable
final class AppearanceSettings {
    static let shared = AppearanceSettings()

    /// Transparency (0 = opaque theme color, 1 = fully see-through)
    /// This ONLY affects the tint overlay alpha
    var transparency: Double = 0.5

    /// Vibrancy material choice (affects blur only, NOT tint)
    var vibrancyMaterial: VibrancyMaterial = .ultraThin

    /// Current preset (computed from transparency only)
    var preset: AppearancePreset {
        get {
            for p in [AppearancePreset.clear, .balanced, .solid] {
                if abs(transparency - p.transparency) < 0.1 {
                    return p
                }
            }
            return .custom
        }
        set {
            if newValue != .custom {
                transparency = newValue.transparency
            }
        }
    }

    /// Current tint color (from Emacs theme)
    var tintColor: NSColor = .windowBackgroundColor

    private init() {}

    /// The alpha value for the tint overlay (inverse of transparency)
    var tintAlpha: CGFloat {
        CGFloat(1.0 - transparency)
    }
}

// MARK: - Liquid Glass Slider

@available(macOS 26.0, *)
struct LiquidGlassSlider: View {
    let label: String
    let icon: String
    @Binding var value: Double
    var onChange: () -> Void

    var body: some View {
        VStack(alignment: .leading, spacing: 8) {
            HStack {
                Image(systemName: icon)
                    .font(.system(size: 12))
                    .foregroundStyle(.secondary)
                Text(label)
                    .font(.subheadline)
                Spacer()
                Text("\(Int(value * 100))%")
                    .font(.system(size: 12, design: .monospaced))
                    .foregroundStyle(.tertiary)
            }

            Slider(value: $value, in: 0...1)
                .controlSize(.small)
                .onChange(of: value) { _, _ in onChange() }
        }
        .padding(.vertical, 4)
    }
}

// MARK: - Liquid Glass Segmented Control

@available(macOS 26.0, *)
struct LiquidGlassSegmentedControl: View {
    @Binding var selection: AppearancePreset
    var onChange: (AppearancePreset) -> Void

    var body: some View {
        HStack(spacing: 2) {
            ForEach(AppearancePreset.allCases) { preset in
                Button {
                    selection = preset
                    onChange(preset)
                } label: {
                    Text(preset.rawValue)
                        .font(.system(size: 11, weight: selection == preset ? .semibold : .regular))
                        .frame(maxWidth: .infinity)
                        .padding(.vertical, 6)
                }
                .buttonStyle(.plain)
                .background {
                    if selection == preset {
                        RoundedRectangle(cornerRadius: 6)
                            .fill(.white.opacity(0.15))
                    }
                }
            }
        }
        .padding(3)
        .background(.white.opacity(0.1), in: RoundedRectangle(cornerRadius: 8))
    }
}

// MARK: - Appearance Panel View (Liquid Glass Design)

@available(macOS 26.0, *)
struct AppearancePanelView: View {
    @Bindable var settings: AppearanceSettings
    var onDismiss: () -> Void
    var onApply: () -> Void

    var body: some View {
        VStack(spacing: 0) {
            // Header
            HStack {
                Text("Appearance")
                    .font(.system(size: 13, weight: .semibold))
                Spacer()
                Button(action: onDismiss) {
                    Image(systemName: "xmark")
                        .font(.system(size: 9, weight: .bold))
                        .foregroundStyle(.tertiary)
                        .frame(width: 18, height: 18)
                        .background(.white.opacity(0.1), in: Circle())
                }
                .buttonStyle(.plain)
            }
            .padding(.horizontal, 16)
            .padding(.top, 14)
            .padding(.bottom, 10)

            // Preset segmented control
            LiquidGlassSegmentedControl(selection: Binding(
                get: { settings.preset },
                set: { settings.preset = $0 }
            )) { _ in
                onApply()
            }
            .padding(.horizontal, 12)
            .padding(.bottom, 12)

            Divider()
                .opacity(0.3)
                .padding(.horizontal, 12)

            // Controls
            VStack(spacing: 12) {
                // Vibrancy material picker (affects blur only)
                HStack {
                    Image(systemName: "circle.dotted")
                        .font(.system(size: 12))
                        .foregroundStyle(.secondary)
                    Text("Vibrancy")
                        .font(.subheadline)
                    Spacer()
                    Picker("", selection: $settings.vibrancyMaterial) {
                        Text("Off").tag(VibrancyMaterial.none)
                        Text("Light").tag(VibrancyMaterial.ultraThin)
                        Text("Medium").tag(VibrancyMaterial.regular)
                        Text("Heavy").tag(VibrancyMaterial.ultraThick)
                    }
                    .pickerStyle(.menu)
                    .fixedSize()
                    .onChange(of: settings.vibrancyMaterial) { _, _ in onApply() }
                }

                if settings.vibrancyMaterial != .none {
                    // Transparency slider - controls theme color tint only
                    LiquidGlassSlider(
                        label: "Transparency",
                        icon: "square.on.square.dashed",
                        value: $settings.transparency,
                        onChange: onApply
                    )
                }
            }
            .padding(.horizontal, 16)
            .padding(.vertical, 12)
        }
        .frame(width: 260)
        .background {
            RoundedRectangle(cornerRadius: 14)
                .fill(.ultraThinMaterial)
        }
        .glassEffect(.regular, in: RoundedRectangle(cornerRadius: 14))
        .overlay {
            RoundedRectangle(cornerRadius: 14)
                .strokeBorder(.white.opacity(0.2), lineWidth: 0.5)
        }
        .shadow(color: .black.opacity(0.25), radius: 30, y: 15)
    }
}

// MARK: - Panel Controller

@available(macOS 26.0, *)
final class AppearancePanelController {
    static let shared = AppearancePanelController()

    private var panel: NSPanel?
    private var hostingView: NSHostingView<AppearancePanelView>?

    /// Callback when settings change - applies to NavigationSidebarController
    var onSettingsChanged: ((AppearanceSettings) -> Void)?

    private init() {
        // Wire up the settings change handler
        onSettingsChanged = { settings in
            guard let window = findEmacsWindow() else { return }
            let controller = NavigationSidebarManager.shared.getController(for: window)

            // Apply vibrancy material (blur only)
            controller.setVibrancyMaterial(settings.vibrancyMaterial.rawValue)

            // Apply tint alpha (transparency only)
            controller.state.backgroundAlpha = settings.tintAlpha

            // Force Emacs to redraw
            DispatchQueue.main.async {
                window.contentView?.setNeedsDisplay(window.contentView?.bounds ?? .zero)
                window.display()
            }
        }
    }

    func show(relativeTo window: NSWindow) {
        if let existingPanel = panel, existingPanel.isVisible {
            existingPanel.makeKeyAndOrderFront(nil)
            return
        }

        let settings = AppearanceSettings.shared

        let panelView = AppearancePanelView(
            settings: settings,
            onDismiss: { [weak self] in
                self?.dismiss()
            },
            onApply: { [weak self] in
                self?.onSettingsChanged?(settings)
            }
        )

        let hosting = NSHostingView(rootView: panelView)
        let size = hosting.fittingSize
        hosting.frame = NSRect(origin: .zero, size: size)

        let panel = NSPanel(
            contentRect: NSRect(origin: .zero, size: size),
            styleMask: [.borderless, .nonactivatingPanel],
            backing: .buffered,
            defer: false
        )

        panel.contentView = hosting
        panel.backgroundColor = .clear
        panel.isOpaque = false
        panel.hasShadow = false  // We use SwiftUI shadow
        panel.level = .floating
        panel.isMovableByWindowBackground = true
        panel.collectionBehavior = [.canJoinAllSpaces, .fullScreenAuxiliary, .transient]
        panel.animationBehavior = .utilityWindow

        // Position centered horizontally, near top of parent window
        let windowFrame = window.frame
        let panelSize = panel.frame.size
        let x = windowFrame.midX - panelSize.width / 2
        let y = windowFrame.maxY - panelSize.height - 80
        panel.setFrameOrigin(NSPoint(x: x, y: y))

        panel.makeKeyAndOrderFront(nil)

        self.panel = panel
        self.hostingView = hosting
    }

    func dismiss() {
        panel?.orderOut(nil)
        panel?.close()
        panel = nil
        hostingView = nil
    }

    func isVisible() -> Bool {
        panel?.isVisible ?? false
    }

    func toggle(relativeTo window: NSWindow) {
        if isVisible() {
            dismiss()
        } else {
            show(relativeTo: window)
        }
    }

}
