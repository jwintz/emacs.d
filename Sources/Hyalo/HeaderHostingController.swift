import AppKit
import SwiftUI

/// Configuration for header position
struct HeaderPosition {
    var topPadding: CGFloat = 6
    var leftPadding: CGFloat = 78  // Space for traffic lights (70px buttons + 8px margin)
    var rightPadding: CGFloat = 12
}

/// Controller that hosts the SwiftUI HeaderView in an AppKit window
@available(macOS 26.0, *)
final class HeaderHostingController {
    
    // MARK: - Properties
    
    private weak var window: NSWindow?
    private var hostingView: NSHostingView<HeaderView>?
    private var viewModel: HeaderViewModel?
    
    /// Header position configuration
    var position = HeaderPosition()
    
    // MARK: - Layout Constants
    
    /// Standard macOS title bar height
    static let titleBarHeight: CGFloat = 32
    
    /// Mode-line row height (fixed, always at top)
    static let modeLineHeight: CGFloat = 24
    
    /// Header-line row height (expansion below mode-line)
    static let headerLineHeight: CGFloat = 22
    
    /// Total max height (mode-line + divider + header-line)
    static var headerHeight: CGFloat { modeLineHeight + 1 + headerLineHeight }
    
    // MARK: - Initialization
    
    init() {}
    
    // MARK: - Setup
    
    /// Attach header view to window (must be called on main thread)
    func attach(to window: NSWindow) {
        self.window = window
        
        guard let contentView = window.contentView else { return }
        
        // Create view model on main actor
        let vm = MainActor.assumeIsolated { HeaderViewModel() }
        self.viewModel = vm
        
        // Create SwiftUI hosting view
        let headerView = HeaderView(viewModel: vm)
        let hosting = NSHostingView(rootView: headerView)
        
        // Make hosting view transparent
        hosting.wantsLayer = true
        hosting.layer?.backgroundColor = .clear
        
        // Add to the content view
        contentView.addSubview(hosting, positioned: .above, relativeTo: nil)
        
        // Use frame-based positioning
        hosting.translatesAutoresizingMaskIntoConstraints = true
        updateFrame(for: hosting, in: contentView)
        
        // Observe window resize
        NotificationCenter.default.addObserver(
            forName: NSWindow.didResizeNotification,
            object: window,
            queue: .main
        ) { [weak self, weak hosting, weak contentView] _ in
            guard let self = self, let hosting = hosting, let contentView = contentView else { return }
            self.updateFrame(for: hosting, in: contentView)
        }
        
        // Ensure header is on top
        hosting.layer?.zPosition = 1000
        
        self.hostingView = hosting
    }
    
    private func updateFrame(for hosting: NSView, in parentView: NSView) {
        let parentBounds = parentView.bounds
        
        // The hosting view is positioned so its TOP edge aligns with the desired header position
        // Mode-line is at the TOP of the hosting view (pinned there by GeometryReader + alignment)
        // Header-line expands BELOW the mode-line (downward expansion)
        
        // In macOS coordinates (y=0 at bottom):
        // To place top edge at `topPadding` from window top:
        // y = parentHeight - topPadding - hostingHeight + titleBarHeight
        let y = parentBounds.height - position.topPadding - Self.headerHeight + Self.titleBarHeight
        
        hosting.frame = NSRect(
            x: position.leftPadding,
            y: y,
            width: parentBounds.width - position.leftPadding - position.rightPadding,
            height: Self.headerHeight
        )
        
        // minYMargin = bottom margin is flexible (keeps view at top)
        hosting.autoresizingMask = [.width, .minYMargin]
    }
    
    /// Update header position
    func setPosition(top: CGFloat, left: CGFloat, right: CGFloat) {
        position = HeaderPosition(topPadding: top, leftPadding: left, rightPadding: right)
        if let hosting = hostingView, let superview = hosting.superview {
            updateFrame(for: hosting, in: superview)
        }
    }
    
    /// Remove header view from window
    func detach() {
        // Remove resize observer
        if let window = window {
            NotificationCenter.default.removeObserver(self, name: NSWindow.didResizeNotification, object: window)
        }
        hostingView?.removeFromSuperview()
        hostingView = nil
        viewModel = nil
        window = nil
    }
    
    /// Show or hide the header view
    /// Used when NavigationSplitView is active (toolbar replaces floating header)
    func setHidden(_ hidden: Bool) {
        hostingView?.isHidden = hidden
    }
    
    /// Check if header view is hidden
    var isHidden: Bool {
        hostingView?.isHidden ?? true
    }
    
    // MARK: - Content Updates
    
    /// Update mode-line content (must be called on main thread)
    func updateModeLine(_ content: String) {
        guard let vm = viewModel else { return }
        MainActor.assumeIsolated {
            vm.updateModeLine(content)
        }
    }
    
    /// Update header-line content (must be called on main thread)
    func updateHeaderLine(_ content: String) {
        guard let vm = viewModel else { return }
        MainActor.assumeIsolated {
            vm.updateHeaderLine(content)
        }
    }
    
    // MARK: - State

    /// Get the view model for external access
    var model: HeaderViewModel? {
        viewModel
    }

    // MARK: - Mode-line Click Callback

    /// Set callback for mode-line clicks
    /// Callback receives (segment: String, relativePosition: Double)
    /// segment is "lhs" or "rhs", relativePosition is 0.0-1.0
    func setModeLineClickCallback(_ callback: @escaping (String, Double) -> Void) {
        guard let vm = viewModel else { return }
        MainActor.assumeIsolated {
            vm.onModeLineClick = callback
        }
    }
}
