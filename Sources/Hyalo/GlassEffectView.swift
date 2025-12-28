// GlassEffectView.swift - Specialized visual effect view for minibuffer
// Copyright (C) 2025

import AppKit

/// A specialized Visual Effect View to provide the "Glass" look for the minibuffer.
@available(macOS 10.14, *)
final class GlassEffectView: NSVisualEffectView {
    
    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        setup()
    }
    
    required init?(coder: NSCoder) {
        super.init(coder: coder)
        setup()
    }
    
    private func setup() {
        // Configure for the "Glass" look
        // .hudWindow provides a nice, slightly lighter glass effect often used for floating panels
        // .headerView is more transparent/modern
        self.material = .hudWindow 
        self.blendingMode = .behindWindow
        self.state = .active
        self.autoresizingMask = [.width, .height]
    }
    
    /// Attach this glass effect to a window as its background
    static func attach(to window: NSWindow) {
        // 1. Make the window itself transparent
        window.isOpaque = false
        window.backgroundColor = .clear
        
        // 2. Identify the Emacs view (current content view)
        guard let emacsView = window.contentView else { return }
        
        // If we already swapped, don't do it again
        if emacsView is GlassEffectView { return }
        
        // 3. Create the glass view
        let glassId = NSUserInterfaceItemIdentifier("HyaloGlassEffect")
        let glass: NSVisualEffectView
        if let GlassClass = NSClassFromString("NSGlassEffectView") as? NSVisualEffectView.Type {
            glass = GlassClass.init(frame: emacsView.frame)
        } else {
            glass = NSVisualEffectView(frame: emacsView.frame)
            // .fullScreenUI provides a very strong glass/blur effect
            glass.material = .fullScreenUI
        }
        
        glass.identifier = glassId
        // For child frames / mini-frames, withinWindow can sometimes be more reliable 
        // than behindWindow to ensure the blur composites correctly.
        glass.blendingMode = .withinWindow 
        glass.state = .active
        glass.autoresizingMask = [.width, .height]
        glass.wantsLayer = true
        
        // 4. Force transparency on the Emacs view layer
        emacsView.wantsLayer = true
        emacsView.layer?.backgroundColor = NSColor.clear.cgColor
        
        // 5. Swap!
        window.contentView = glass
        
        // Add emacsView as subview of glass
        emacsView.frame = glass.bounds
        emacsView.autoresizingMask = [.width, .height]
        glass.addSubview(emacsView)
    }
}
