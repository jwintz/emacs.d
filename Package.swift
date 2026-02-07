// swift-tools-version: 6.2
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
    name: "Hyalo",
    platforms: [
        .macOS(.v26)  // macOS Tahoe for Liquid Glass support
    ],
    products: [
        .library(
            name: "Hyalo",
            type: .dynamic,
            targets: ["Hyalo"]
        )
    ],
    dependencies: [
        .package(
            url: "https://github.com/SavchenkoValeriy/emacs-swift-module.git",
            branch: "main"
        ),
        .package(
            url: "https://github.com/gonzalezreal/swift-markdown-ui",
            from: "2.4.0"
        )
    ],
    targets: [
        .target(
            name: "Hyalo",
            dependencies: [
                .product(name: "EmacsSwiftModule", package: "emacs-swift-module"),
                .product(name: "MarkdownUI", package: "swift-markdown-ui")
            ],
            swiftSettings: [
                .swiftLanguageMode(.v5)
            ],
            plugins: [
                .plugin(name: "ModuleFactoryPlugin", package: "emacs-swift-module")
            ]
        )
    ]
)
