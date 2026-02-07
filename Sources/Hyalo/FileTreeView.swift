// FileTreeView.swift - NSOutlineView-backed project navigator
// Copyright (C) 2025
// Target: macOS 26 Tahoe with Liquid Glass design
// Inspired by CodeEdit's ProjectNavigatorViewController pattern
//
// Design: Folders on top, two icon types only (folder/doc),
// monochrome colors, grayscale selection, hover effects.

import AppKit
import SwiftUI

// MARK: - File Tree Data Model

@available(macOS 26.0, *)
struct FileTreeNode: Identifiable, Codable, Equatable {
    let id: String              // Full path (unique identifier)
    let name: String            // File/directory name
    let isDirectory: Bool
    let children: [FileTreeNode]?
    let icon: String            // SF Symbol name (from Emacs, not used for display)

    /// Decode from Emacs JSON
    static func fromJSON(_ data: Data) throws -> FileTreeNode {
        let decoder = JSONDecoder()
        return try decoder.decode(FileTreeNode.self, from: data)
    }

    /// Two icons only: folder or document
    var resolvedIcon: String {
        isDirectory ? "folder" : "doc"
    }

    /// Children sorted: directories first, then alphabetically
    var sortedChildren: [FileTreeNode]? {
        children?.sorted { a, b in
            if a.isDirectory != b.isDirectory { return a.isDirectory }
            return a.name.localizedCaseInsensitiveCompare(b.name) == .orderedAscending
        }
    }
}

// MARK: - Custom NSOutlineView (grayscale selection)

/// Overrides the default source-list blue highlight with a grayscale fill.
@available(macOS 26.0, *)
private final class GrayscaleOutlineView: NSOutlineView {

    /// Draw row background: grayscale selection instead of system blue
    override func drawRow(_ row: Int, clipRect: NSRect) {
        if selectedRowIndexes.contains(row) {
            let rowRect = rect(ofRow: row).insetBy(dx: 4, dy: 1)
            let path = NSBezierPath(roundedRect: rowRect, xRadius: 4, yRadius: 4)
            NSColor.labelColor.withAlphaComponent(0.10).setFill()
            path.fill()
        }
        super.drawRow(row, clipRect: clipRect)
    }

    /// Suppress the default highlight drawing
    override var selectionHighlightStyle: NSTableView.SelectionHighlightStyle {
        get { .none }
        set { _ = newValue }
    }

    /// Prevent the system from drawing a focus ring on the outline view
    override var focusRingType: NSFocusRingType {
        get { .none }
        set { _ = newValue }
    }
}

// MARK: - Hover-tracking Row View

/// An NSTableRowView that tracks mouse hover and draws a subtle background.
@available(macOS 26.0, *)
private final class HoverRowView: NSTableRowView {
    private var isHovered = false
    private var trackingArea: NSTrackingArea?

    override func updateTrackingAreas() {
        super.updateTrackingAreas()
        if let existing = trackingArea { removeTrackingArea(existing) }
        let area = NSTrackingArea(
            rect: bounds,
            options: [.mouseEnteredAndExited, .activeInActiveApp, .inVisibleRect],
            owner: self, userInfo: nil)
        addTrackingArea(area)
        trackingArea = area
    }

    override func mouseEntered(with event: NSEvent) {
        isHovered = true
        needsDisplay = true
    }

    override func mouseExited(with event: NSEvent) {
        isHovered = false
        needsDisplay = true
    }

    override func drawBackground(in dirtyRect: NSRect) {
        // No default background — handled by outline view and hover
        if isHovered && !isSelected {
            let rowRect = bounds.insetBy(dx: 4, dy: 1)
            let path = NSBezierPath(roundedRect: rowRect, xRadius: 4, yRadius: 4)
            NSColor.labelColor.withAlphaComponent(0.05).setFill()
            path.fill()
        }
    }

    override func drawSelection(in dirtyRect: NSRect) {
        // Selection drawing handled by GrayscaleOutlineView.drawRow
    }

    /// Suppress emphasis (accent color) changes on focus/unfocus
    override var isEmphasized: Bool {
        get { false }
        set { _ = newValue }
    }
}

// MARK: - Outline View Controller

@available(macOS 26.0, *)
final class FileTreeOutlineViewController: NSViewController {
    var scrollView: NSScrollView!
    var outlineView: NSOutlineView!

    /// The root node; children are displayed as top-level items
    var root: FileTreeNode? {
        didSet {
            guard root != oldValue else { return }
            outlineView?.reloadData()
            expandToActiveFile()
        }
    }

    /// Currently active file path for smart folder expansion
    var activeFilePath: String? {
        didSet {
            guard activeFilePath != oldValue else { return }
            expandToActiveFile()
        }
    }

    /// Callback when a file (non-directory) is selected
    var onSelect: ((String) -> Void)?

    /// Expand only the ancestor chain of the active file.
    /// Falls back to expanding only the first top-level directory.
    /// Uses outline view item references (not tree copies) to work with value-type nodes.
    private func expandToActiveFile() {
        guard let outlineView, let root else { return }

        // Collapse everything first
        outlineView.collapseItem(nil, collapseChildren: true)

        // Collect ancestor path IDs
        let ancestorIDs: Set<String>
        if let activePath = activeFilePath {
            ancestorIDs = findAncestorIDs(of: activePath, in: root)
        } else {
            ancestorIDs = []
        }

        if ancestorIDs.isEmpty {
            // No active file — do nothing (all collapsed)
            return
        }

        // Walk the outline view's data source items and expand matching ancestors.
        // This ensures we use the same object references that the outline view holds.
        expandMatchingItems(in: outlineView, parent: nil, ancestorIDs: ancestorIDs)
    }

    /// Collect the IDs of all ancestor directories of the target path.
    private func findAncestorIDs(of targetPath: String, in node: FileTreeNode) -> Set<String> {
        var ids = Set<String>()

        func walk(_ current: FileTreeNode) {
            if current.isDirectory && targetPath.hasPrefix(current.id + "/") {
                ids.insert(current.id)
                if let children = current.sortedChildren {
                    for child in children {
                        walk(child)
                    }
                }
            }
        }

        // Check root itself
        if node.isDirectory && targetPath.hasPrefix(node.id + "/") {
            ids.insert(node.id)
        }
        if let children = node.sortedChildren {
            for child in children {
                walk(child)
            }
        }

        return ids
    }

    /// Expand items in the outline view whose IDs match the ancestor set.
    /// Walks the data source to get correct item references for NSOutlineView.
    private func expandMatchingItems(in outlineView: NSOutlineView, parent: Any?, ancestorIDs: Set<String>) {
        let count = outlineView.dataSource?.outlineView?(outlineView, numberOfChildrenOfItem: parent) ?? 0
        for i in 0..<count {
            guard let child = outlineView.dataSource?.outlineView?(outlineView, child: i, ofItem: parent) else { continue }
            if let node = child as? FileTreeNode, ancestorIDs.contains(node.id) {
                outlineView.expandItem(child)
                // Recurse into expanded item to expand deeper ancestors
                expandMatchingItems(in: outlineView, parent: child, ancestorIDs: ancestorIDs)
            }
        }
    }

    private let rowHeight: CGFloat = 22

    override func loadView() {
        scrollView = NSScrollView()
        scrollView.hasVerticalScroller = true
        scrollView.scrollerStyle = .overlay
        scrollView.hasHorizontalScroller = false
        scrollView.autohidesScrollers = true
        scrollView.drawsBackground = false
        view = scrollView

        outlineView = GrayscaleOutlineView()
        outlineView.dataSource = self
        outlineView.delegate = self
        outlineView.headerView = nil
        outlineView.rowHeight = rowHeight
        outlineView.indentationPerLevel = 14
        outlineView.allowsMultipleSelection = false
        outlineView.allowsEmptySelection = true
        outlineView.style = .plain
        outlineView.intercellSpacing = NSSize(width: 0, height: 0)
        outlineView.backgroundColor = .clear
        outlineView.usesAlternatingRowBackgroundColors = false

        let column = NSTableColumn(identifier: .init(rawValue: "Cell"))
        column.title = "Cell"
        outlineView.addTableColumn(column)
        outlineView.outlineTableColumn = column

        scrollView.documentView = outlineView
        scrollView.contentView.automaticallyAdjustsContentInsets = false
        scrollView.contentView.contentInsets = .init(top: 6, left: 4, bottom: 6, right: 0)
    }

    init() {
        super.init(nibName: nil, bundle: nil)
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) not implemented")
    }

    deinit {
        outlineView?.removeFromSuperview()
        scrollView?.removeFromSuperview()
    }
}

// MARK: - Data Source

@available(macOS 26.0, *)
extension FileTreeOutlineViewController: NSOutlineViewDataSource {

    func outlineView(_ outlineView: NSOutlineView, numberOfChildrenOfItem item: Any?) -> Int {
        if let node = item as? FileTreeNode {
            return node.sortedChildren?.count ?? 0
        }
        return root?.sortedChildren?.count ?? 0
    }

    func outlineView(_ outlineView: NSOutlineView, child index: Int, ofItem item: Any?) -> Any {
        if let node = item as? FileTreeNode {
            return node.sortedChildren![index]
        }
        return root!.sortedChildren![index]
    }

    func outlineView(_ outlineView: NSOutlineView, isItemExpandable item: Any) -> Bool {
        if let node = item as? FileTreeNode {
            return node.isDirectory
        }
        return false
    }
}

// MARK: - Delegate

@available(macOS 26.0, *)
extension FileTreeOutlineViewController: NSOutlineViewDelegate {

    func outlineView(_ outlineView: NSOutlineView, rowViewForItem item: Any) -> NSTableRowView? {
        HoverRowView()
    }

    func outlineView(_ outlineView: NSOutlineView, viewFor tableColumn: NSTableColumn?, item: Any) -> NSView? {
        guard let node = item as? FileTreeNode else { return nil }

        let cellIdentifier = NSUserInterfaceItemIdentifier("FileTreeCell")
        var cell = outlineView.makeView(withIdentifier: cellIdentifier, owner: self) as? NSTableCellView

        if cell == nil {
            cell = NSTableCellView()
            cell?.identifier = cellIdentifier

            let imageView = NSImageView()
            imageView.translatesAutoresizingMaskIntoConstraints = false
            imageView.setContentHuggingPriority(.defaultHigh, for: .horizontal)
            cell?.addSubview(imageView)
            cell?.imageView = imageView

            let textField = NSTextField(labelWithString: "")
            textField.translatesAutoresizingMaskIntoConstraints = false
            textField.lineBreakMode = .byTruncatingMiddle
            textField.isEditable = false
            textField.isSelectable = false
            textField.drawsBackground = false
            textField.isBordered = false
            cell?.addSubview(textField)
            cell?.textField = textField

            NSLayoutConstraint.activate([
                imageView.leadingAnchor.constraint(equalTo: cell!.leadingAnchor, constant: 6),
                imageView.centerYAnchor.constraint(equalTo: cell!.centerYAnchor),
                imageView.widthAnchor.constraint(equalToConstant: 16),
                imageView.heightAnchor.constraint(equalToConstant: 16),

                textField.leadingAnchor.constraint(equalTo: imageView.trailingAnchor, constant: 4),
                textField.trailingAnchor.constraint(equalTo: cell!.trailingAnchor, constant: -8),
                textField.centerYAnchor.constraint(equalTo: cell!.centerYAnchor),
            ])
        }

        // Icon — monochrome, secondary label color
        let iconName = node.resolvedIcon
        let iconImage = NSImage(systemSymbolName: iconName, accessibilityDescription: nil)
        let config = NSImage.SymbolConfiguration(pointSize: 12, weight: .regular, scale: .medium)
        cell?.imageView?.image = iconImage?.withSymbolConfiguration(config)
        cell?.imageView?.contentTintColor = .secondaryLabelColor

        // Label
        cell?.textField?.font = .monospacedSystemFont(ofSize: 11.5, weight: .regular)
        cell?.textField?.stringValue = node.name
        cell?.textField?.textColor = .labelColor

        return cell
    }

    func outlineView(_ outlineView: NSOutlineView, heightOfRowByItem item: Any) -> CGFloat {
        rowHeight
    }

    func outlineViewSelectionDidChange(_ notification: Notification) {
        guard let outlineView = notification.object as? NSOutlineView else { return }
        let selectedRow = outlineView.selectedRow
        guard selectedRow >= 0,
              let node = outlineView.item(atRow: selectedRow) as? FileTreeNode else { return }

        if !node.isDirectory {
            onSelect?(node.id)
        }
    }

    func outlineView(_ outlineView: NSOutlineView, shouldShowOutlineCellForItem item: Any) -> Bool {
        true
    }

    func outlineView(
        _ outlineView: NSOutlineView,
        toolTipFor cell: NSCell,
        rect: NSRectPointer,
        tableColumn: NSTableColumn?,
        item: Any,
        mouseLocation: NSPoint
    ) -> String {
        if let node = item as? FileTreeNode {
            return node.id
        }
        return ""
    }
}

// MARK: - SwiftUI Wrapper

@available(macOS 26.0, *)
struct FileTreeOutlineView: NSViewControllerRepresentable {
    let root: FileTreeNode?
    let activeFilePath: String?
    let onSelect: (String) -> Void

    func makeNSViewController(context: Context) -> FileTreeOutlineViewController {
        let controller = FileTreeOutlineViewController()
        controller.activeFilePath = activeFilePath
        controller.root = root
        controller.onSelect = onSelect
        return controller
    }

    func updateNSViewController(_ controller: FileTreeOutlineViewController, context: Context) {
        controller.activeFilePath = activeFilePath
        controller.root = root
        controller.onSelect = onSelect
    }
}

// MARK: - File Tree View (Public API)

@available(macOS 26.0, *)
struct FileTreeView: View {
    let root: FileTreeNode?
    var activeFilePath: String?
    let onSelect: (String) -> Void

    var body: some View {
        if root != nil, root?.children?.isEmpty == false {
            FileTreeOutlineView(root: root, activeFilePath: activeFilePath, onSelect: onSelect)
        } else {
            VStack(spacing: 12) {
                Image(systemName: "folder.badge.questionmark")
                    .font(.system(size: 28))
                    .foregroundStyle(.tertiary)
                Text("No project open")
                    .font(.system(size: 11, design: .monospaced))
                    .foregroundStyle(.secondary)
                Text("Open a file to see project structure")
                    .font(.system(size: 9, design: .monospaced))
                    .foregroundStyle(.tertiary)
            }
            .frame(maxWidth: .infinity, minHeight: 120)
            .padding()
        }
    }
}

// MARK: - Preview

#if DEBUG
@available(macOS 26.0, *)
#Preview {
    FileTreeView(
        root: FileTreeNode(
            id: "/Users/test/project",
            name: "project",
            isDirectory: true,
            children: [
                FileTreeNode(id: "/Users/test/project/Package.swift", name: "Package.swift", isDirectory: false, children: nil, icon: "swift"),
                FileTreeNode(id: "/Users/test/project/Sources", name: "Sources", isDirectory: true, children: [
                    FileTreeNode(id: "/Users/test/project/Sources/Module.swift", name: "Module.swift", isDirectory: false, children: nil, icon: "swift"),
                    FileTreeNode(id: "/Users/test/project/Sources/Views", name: "Views", isDirectory: true, children: [
                        FileTreeNode(id: "/Users/test/project/Sources/Views/Main.swift", name: "Main.swift", isDirectory: false, children: nil, icon: "swift"),
                    ], icon: "folder"),
                ], icon: "folder"),
                FileTreeNode(id: "/Users/test/project/README.md", name: "README.md", isDirectory: false, children: nil, icon: "doc.richtext"),
            ],
            icon: "folder"
        ),
        onSelect: { path in print("Selected: \(path)") }
    )
    .frame(width: 260, height: 350)
}
#endif
