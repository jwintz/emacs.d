// AppearancePanel.swift - Modern SwiftUI popup for Hyalo appearance settings
// Uses vibrancy materials and rounded corners for Liquid Glass aesthetic

import SwiftUI
import AppKit

// MARK: - Appearance Preset

@available(macOS 26.0, *)
enum AppearancePreset: String, CaseIterable, Identifiable {
    case clear = "Clear"
    case balanced = "Balanced"
    case solid = "Solid"

    var id: String { rawValue }

    /// Vibrancy level (0 = no blur, 1 = maximum blur)
    var vibrancy: Double {
        switch self {
        case .clear: return 0.9
        case .balanced: return 0.65  // Regular vibrancy
        case .solid: return 0.1
        }
    }

    /// Opacity level (0 = full vibrancy/see-through, 1 = solid theme color)
    var opacity: Double {
        switch self {
        case .clear: return 0.1
        case .balanced: return 0.6  // 60% opacity
        case .solid: return 0.9
        }
    }
}

// MARK: - Appearance Mode

enum AppearanceMode: String, CaseIterable, Identifiable {
    case auto = "Auto"
    case light = "Light"
    case dark = "Dark"

    var id: String { rawValue }

    var emacsValue: String {
        switch self {
        case .auto: return "auto"
        case .light: return "light"
        case .dark: return "dark"
        }
    }

    static func from(emacsValue: String) -> AppearanceMode {
        switch emacsValue {
        case "light": return .light
        case "dark": return .dark
        default: return .auto
        }
    }
}

// MARK: - Appearance Settings Model

@available(macOS 26.0, *)
@Observable
final class AppearanceSettings {
    static let shared = AppearanceSettings()

    var vibrancy: Double = 0.5
    var opacity: Double = 0.5
    var tintColor: NSColor = .windowBackgroundColor
    var appearanceMode: AppearanceMode = .auto

    private init() {}

    var tintAlpha: CGFloat { CGFloat(opacity) }

    var vibrancyMaterial: VibrancyMaterial {
        if vibrancy < 0.15 { return .none }
        else if vibrancy < 0.35 { return .ultraThick }
        else if vibrancy < 0.55 { return .thick }
        else if vibrancy < 0.75 { return .regular }
        else if vibrancy < 0.9 { return .thin }
        else { return .ultraThin }
    }

    func setVibrancyFromMaterial(_ material: VibrancyMaterial) {
        switch material {
        case .none: vibrancy = 0.0
        case .ultraThick: vibrancy = 0.25
        case .thick: vibrancy = 0.45
        case .regular: vibrancy = 0.65
        case .thin: vibrancy = 0.8
        case .ultraThin: vibrancy = 0.95
        }
    }

    func matchesPreset(_ preset: AppearancePreset) -> Bool {
        abs(vibrancy - preset.vibrancy) < 0.1 && abs(opacity - preset.opacity) < 0.1
    }

    func applyPreset(_ preset: AppearancePreset) {
        vibrancy = preset.vibrancy
        opacity = preset.opacity
    }

    func syncFromController(_ controller: NavigationSidebarController) {
        setVibrancyFromMaterial(controller.state.vibrancyMaterial)
        opacity = Double(controller.state.backgroundAlpha)
        appearanceMode = AppearanceMode.from(emacsValue: controller.state.windowAppearance)
    }
}

// MARK: - Modern Glass Slider (for Opacity)

@available(macOS 26.0, *)
struct GlassSlider: View {
    let label: String
    let icon: String
    @Binding var value: Double
    var onChange: () -> Void

    var body: some View {
        VStack(alignment: .leading, spacing: 8) {
            HStack {
                Image(systemName: icon)
                    .font(.system(size: 12, weight: .medium))
                    .foregroundStyle(.primary.opacity(0.7))
                Text(label)
                    .font(.system(size: 13, weight: .medium))
                Spacer()
                Text("\(Int(value * 100))%")
                    .font(.system(size: 12, design: .monospaced))
                    .foregroundStyle(.secondary)
            }

            Slider(value: $value, in: 0...1)
                .controlSize(.regular)
                .onChange(of: value) { _, _ in onChange() }
        }
    }
}

// MARK: - Vibrancy Picker

/// Vibrancy levels ordered from less vibrant (none) to more vibrant (ultraThin)
@available(macOS 26.0, *)
struct VibrancyPicker: View {
    @Bindable var settings: AppearanceSettings
    var onChange: () -> Void

    /// Ordered from less vibrant to more vibrant
    private let levels: [VibrancyMaterial] = [
        .none, .ultraThick, .thick, .regular, .thin, .ultraThin
    ]

    private var selectedIndex: Double {
        Double(levels.firstIndex(of: settings.vibrancyMaterial) ?? 0)
    }

    private func labelFor(_ material: VibrancyMaterial) -> String {
        switch material {
        case .none: return "None"
        case .ultraThick: return "Ultra Thick"
        case .thick: return "Thick"
        case .regular: return "Regular"
        case .thin: return "Thin"
        case .ultraThin: return "Ultra Thin"
        }
    }

    var body: some View {
        VStack(alignment: .leading, spacing: 8) {
            // Header
            HStack {
                Image(systemName: "sparkles")
                    .font(.system(size: 12, weight: .medium))
                    .foregroundStyle(.primary.opacity(0.7))
                Text("Vibrancy")
                    .font(.system(size: 13, weight: .medium))
                Spacer()
                Text(labelFor(settings.vibrancyMaterial))
                    .font(.system(size: 12))
                    .foregroundStyle(.secondary)
            }

            // Native discrete slider
            VStack(spacing: 2) {
                Slider(
                    value: Binding(
                        get: { selectedIndex },
                        set: { newValue in
                            let index = Int(newValue.rounded())
                            if index >= 0 && index < levels.count {
                                settings.setVibrancyFromMaterial(levels[index])
                                onChange()
                            }
                        }
                    ),
                    in: 0...Double(levels.count - 1),
                    step: 1
                )
                .controlSize(.regular)

                // Labels
                HStack {
                    Text("None")
                        .font(.system(size: 9))
                        .foregroundStyle(.secondary)
                    Spacer()
                    Text("Max")
                        .font(.system(size: 9))
                        .foregroundStyle(.secondary)
                }
            }
        }
    }
}

// MARK: - Preset Picker

@available(macOS 26.0, *)
struct PresetPicker: View {
    @Bindable var settings: AppearanceSettings
    var onChange: () -> Void

    var body: some View {
        HStack(spacing: 8) {
            ForEach(AppearancePreset.allCases) { preset in
                Button {
                    settings.applyPreset(preset)
                    onChange()
                } label: {
                    Text(preset.rawValue)
                        .font(.system(size: 12, weight: settings.matchesPreset(preset) ? .semibold : .regular))
                        .frame(maxWidth: .infinity)
                        .padding(.vertical, 8)
                }
                .buttonStyle(.plain)
                .background {
                    if settings.matchesPreset(preset) {
                        Capsule()
                            .fill(.white.opacity(0.15))
                    }
                }
            }
        }
        .padding(4)
        .background(.ultraThinMaterial, in: Capsule())
    }
}

// MARK: - Appearance Mode Picker

@available(macOS 26.0, *)
struct AppearanceModePicker: View {
    @Bindable var settings: AppearanceSettings
    var onChange: () -> Void

    var body: some View {
        HStack(spacing: 8) {
            ForEach(AppearanceMode.allCases) { mode in
                Button {
                    settings.appearanceMode = mode
                    onChange()
                } label: {
                    HStack(spacing: 4) {
                        Image(systemName: iconName(for: mode))
                            .font(.system(size: 10))
                        Text(mode.rawValue)
                            .font(.system(size: 12, weight: settings.appearanceMode == mode ? .semibold : .regular))
                    }
                    .frame(maxWidth: .infinity)
                    .padding(.vertical, 8)
                }
                .buttonStyle(.plain)
                .background {
                    if settings.appearanceMode == mode {
                        Capsule()
                            .fill(.white.opacity(0.15))
                    }
                }
            }
        }
        .padding(4)
        .background(.ultraThinMaterial, in: Capsule())
    }

    private func iconName(for mode: AppearanceMode) -> String {
        switch mode {
        case .auto: return "circle.lefthalf.filled"
        case .light: return "sun.max"
        case .dark: return "moon"
        }
    }
}

// MARK: - Modern Appearance Panel View

@available(macOS 26.0, *)
struct AppearancePanelView: View {
    @Bindable var settings: AppearanceSettings
    var onDismiss: () -> Void
    var onApply: () -> Void
    var onAppearanceModeChange: ((AppearanceMode) -> Void)?

    var body: some View {
        VStack(spacing: 16) {
            // Header
            HStack {
                Label("Appearance", systemImage: "paintpalette")
                    .font(.system(size: 14, weight: .semibold))
                Spacer()
                Button(action: onDismiss) {
                    Image(systemName: "xmark.circle.fill")
                        .font(.system(size: 16))
                        .foregroundStyle(.secondary)
                }
                .buttonStyle(.plain)
            }

            // Appearance Mode Picker
            AppearanceModePicker(settings: settings) {
                onAppearanceModeChange?(settings.appearanceMode)
            }

            // Presets
            PresetPicker(settings: settings, onChange: onApply)

            // Controls
            VStack(spacing: 16) {
                // Discrete vibrancy picker
                VibrancyPicker(settings: settings, onChange: onApply)

                // Continuous opacity slider
                GlassSlider(
                    label: "Opacity",
                    icon: "drop.halffull",
                    value: $settings.opacity,
                    onChange: onApply
                )
            }
        }
        .padding(20)
        .frame(width: 280)
        .background(.ultraThinMaterial, in: RoundedRectangle(cornerRadius: 16))
        .overlay {
            RoundedRectangle(cornerRadius: 16)
                .strokeBorder(.white.opacity(0.2), lineWidth: 0.5)
        }
        .shadow(color: .black.opacity(0.3), radius: 20, y: 10)
    }
}

// MARK: - Panel Controller

@available(macOS 26.0, *)
final class AppearancePanelController {
    static let shared = AppearancePanelController()

    private var panel: NSPanel?
    private var hostingView: NSHostingView<AppearancePanelView>?

    var onSettingsChanged: ((AppearanceSettings) -> Void)?
    var onAppearanceModeChanged: ((AppearanceMode) -> Void)?

    private init() {
        onSettingsChanged = { settings in
            guard let window = findEmacsWindow() else { return }
            let controller = NavigationSidebarManager.shared.getController(for: window)

            controller.setVibrancyMaterial(settings.vibrancyMaterial.rawValue)
            controller.state.backgroundAlpha = settings.tintAlpha

            DispatchQueue.main.async {
                window.contentView?.setNeedsDisplay(window.contentView?.bounds ?? .zero)
                window.display()
            }
        }

        // Default implementation - can be overridden by Module.swift
        onAppearanceModeChanged = { mode in
            guard let window = findEmacsWindow() else { return }
            let controller = NavigationSidebarManager.shared.getController(for: window)
            controller.setWindowAppearance(mode.emacsValue)
            controller.state.windowAppearance = mode.emacsValue
        }
    }

    func refreshPanel() {
        guard let window = findEmacsWindow() else { return }
        let controller = NavigationSidebarManager.shared.getController(for: window)
        AppearanceSettings.shared.syncFromController(controller)
    }

    func show(relativeTo window: NSWindow) {
        if let existingPanel = panel, existingPanel.isVisible {
            refreshPanel()
            existingPanel.makeKeyAndOrderFront(nil)
            return
        }

        let settings = AppearanceSettings.shared
        let controller = NavigationSidebarManager.shared.getController(for: window)
        settings.syncFromController(controller)

        let panelView = AppearancePanelView(
            settings: settings,
            onDismiss: { [weak self] in self?.dismiss() },
            onApply: { [weak self] in self?.onSettingsChanged?(settings) },
            onAppearanceModeChange: { [weak self] mode in self?.onAppearanceModeChanged?(mode) }
        )

        let hosting = NSHostingView(rootView: panelView)
        let size = hosting.fittingSize
        hosting.frame = NSRect(origin: .zero, size: size)

        // Set up layer with corner radius for proper visual clipping
        hosting.wantsLayer = true
        hosting.layer?.cornerRadius = 16
        hosting.layer?.masksToBounds = true

        let panel = NSPanel(
            contentRect: NSRect(origin: .zero, size: size),
            styleMask: [.borderless, .nonactivatingPanel],
            backing: .buffered,
            defer: false
        )

        panel.contentView = hosting
        panel.backgroundColor = .clear
        panel.isOpaque = false
        panel.hasShadow = false  // SwiftUI provides shadow
        panel.level = .floating
        panel.isMovableByWindowBackground = true
        panel.isMovable = true
        panel.collectionBehavior = [.canJoinAllSpaces, .fullScreenAuxiliary, .transient]
        panel.animationBehavior = .utilityWindow
        panel.becomesKeyOnlyIfNeeded = false  // Allow panel to become key for button clicks
        panel.acceptsMouseMovedEvents = true

        let windowFrame = window.frame
        let panelSize = panel.frame.size
        let x = windowFrame.midX - panelSize.width / 2
        let y = windowFrame.maxY - panelSize.height - 80
        panel.setFrameOrigin(NSPoint(x: x, y: y))

        // Parent the panel to the root NSWindow
        window.addChildWindow(panel, ordered: .above)

        panel.makeKeyAndOrderFront(nil)

        self.panel = panel
        self.hostingView = hosting
    }

    func dismiss() {
        if let panel = panel {
            panel.parent?.removeChildWindow(panel)
            panel.orderOut(nil)
            panel.close()
        }
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
