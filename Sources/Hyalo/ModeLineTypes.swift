// ModeLineTypes.swift - Data structures for mode-line segments and menus
// Copyright (C) 2025
// Target: macOS 26 Tahoe with Liquid Glass design

import Foundation

// MARK: - Mode-line Segment Data Structures

/// A clickable segment in the mode-line
struct ModeLineSegment: Codable {
    let text: String
    let relStart: Double
    let relEnd: Double
    let helpEcho: String?
    let side: String  // "lhs" or "rhs"
    let menuItems: [ModeLineMenuItem]?
    let command: String?  // Direct command if no menu
}

/// A menu item for mode-line segment popup
struct ModeLineMenuItem: Codable {
    let title: String
    let command: String
    let checked: Bool?
    let enabled: Bool?
    let separator: Bool?
}

/// Menu structure for showing popup
struct ModeLineMenu: Codable {
    let title: String?
    let items: [ModeLineMenuItem]
}

// MARK: - Vibrancy Material Types

/// Available vibrancy material styles matching SwiftUI's Material enum
enum VibrancyMaterial: String, CaseIterable {
    case ultraThin = "ultraThin"
    case thin = "thin"
    case regular = "regular"
    case thick = "thick"
    case ultraThick = "ultraThick"
    case none = "none"
}
