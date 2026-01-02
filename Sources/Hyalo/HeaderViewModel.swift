import Foundation
import SwiftUI

/// Observable view model for header content
@MainActor
final class HeaderViewModel: ObservableObject {

    // MARK: - Published Properties

    /// Mode-line content (single string, parsed in view for LHS/RHS)
    @Published private(set) var modeLineString: String = ""

    /// Header-line content (single string, parsed in view for LHS/RHS)
    @Published private(set) var headerLineContent: String = ""

    // MARK: - Callbacks

    /// Callback for mode-line clicks. Parameters: segment ("lhs" or "rhs"), relative position (0.0-1.0)
    var onModeLineClick: ((String, Double) -> Void)?

    // MARK: - Update Methods

    /// Update mode-line content
    func updateModeLine(_ content: String) {
        modeLineString = content
    }

    /// Update header-line content
    func updateHeaderLine(_ content: String) {
        headerLineContent = content
    }

    /// Handle mode-line segment click
    func handleModeLineClick(segment: String, relativePosition: Double) {
        onModeLineClick?(segment, relativePosition)
    }
}
