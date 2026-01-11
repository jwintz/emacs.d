# Hyalo Emacs

```
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
                            H Y A L O  Emacs
                      Liquid Glass for macOS Tahoe
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```

<!-- Screenshot placeholder -->
<p align="center">
  <img src="screenshot.png" alt="Hyalo Emacs Screenshot" width="800"/>
</p>

> **hy·a·lo** | ˈhīəˌlo | *adjective*: glassy; transparent.

A modern Emacs configuration featuring deep macOS integration through a custom Swift dynamic module. Hyalo brings Apple's Liquid Glass design language to Emacs with transparent backgrounds, vibrancy effects, and native SwiftUI sidebars embedded in a `NavigationSplitView`.

This repository serves as both a personal Emacs configuration and a showcase for the Hyalo dynamic module system.

---

## Features

- **Liquid Glass UI** — Transparent backgrounds with vibrancy blur effects
- **SwiftUI Integration** — Native `NavigationSplitView` with embedded Emacs frames
- **Semantic Fonts** — Monaspace variants for code, prose, and comments
- **AI Agents** — Claude Code, Gemini, and GitHub Copilot integration
- **Weight-based Highlighting** — No background colors, only font weight differentiation

---

## Requirements

| Requirement | Version       | Notes                          |
|-------------|---------------|--------------------------------|
| **macOS**   | 26.0+ (Tahoe) | Required for Liquid Glass APIs |
| **Emacs**   | 31.0+         | With dynamic module support    |
| **Swift**   | 6.0+          | For building the Hyalo module  |

### Emacs Build Requirements

Emacs must be built with the following:

```
--with-modules          # Dynamic module support
--with-json
```

### Emacs Patches

The following patches are recommended for full functionality:

| Patch                      | Purpose                       |
|----------------------------|-------------------------------|
| `frame-transparency.patch` | Transparent frame backgrounds |
| `system-appearance.patch`  | macOS integration             |

### Build Recipe

See [github.com/jwintz/emacs](https://github.com/jwintz/emacs) for the complete build recipe with all patches applied.

---

## Installation

```bash
# Clone the repository
git clone https://github.com/jwintz/emacs.d ~/.config/emacs

# Build the Swift module
cd ~/.config/emacs
swift build -c release

# Launch Emacs
emacs
```

The Hyalo module auto-(re-)builds on launch if `hyalo-auto-build` is enabled.

---

## Testing & Showcase

You can test the Hyalo module in a minimal environment (without affecting your existing Emacs configuration) using the provided `init.example.el`.

```bash
# Run minimal Hyalo showcase
emacs -Q -l init.example.el --init-directory /tmp/emacs-hyalo
```

This will:
1. Load only the necessary Hyalo libraries from `lisp/`
2. Compile the Swift module if needed
3. Enable transparency, vibrancy, and the native SwiftUI header

---

## Core Libraries (`lisp/`)

Custom Elisp libraries providing Hyalo functionality and TTY-compatible utilities.

### Hyalo Libraries (macOS GUI)

| Library                    | Purpose                              | Key Commands                                                       |
|----------------------------|--------------------------------------|--------------------------------------------------------------------|
| **hyalo.el**               | Module loader and build system       | `hyalo-load`, `hyalo-build`, `hyalo-rebuild-and-reload`            |
| **hyalo-appearance.el**    | Theme, vibrancy, and opacity control | `hyalo-appearance-set`, `hyalo-appearance-show-panel`              |
| **hyalo-header.el**        | SwiftUI header and mode-line sync    | `hyalo-toggle-decorations-command`                                 |
| **hyalo-footer.el**        | Decorative footer patterns           | `hyalo-footer-set-pattern`                                         |
| **hyalo-viewport.el**      | Buffer viewport offset for header    | *(automatic)*                                                      |
| **hyalo-sidebar.el**       | Embedded sidebar frames              | `hyalo-sidebar-toggle-left`, `hyalo-sidebar-toggle-right`          |
| **hyalo-minibuffer.el**    | Glass effect for mini-frame          | *(automatic)*                                                      |
| **hyalo-fonts.el**         | Fontaine presets with Monaspace      | *(configuration)*                                                  |
| **hyalo-icons.el**         | Monochromatic icon helpers           | *(helper functions)*                                               |
| **hyalo-ibuffer.el**       | Sidebar ibuffer configuration        | *(automatic)*                                                      |
| **hyalo-minimap.el**       | Demap mouse interaction              | `hyalo-minimap-click`, `hyalo-minimap-drag-scroll`                 |
| **hyalo-markdown-tags.el** | SVG tag pills in markdown            | *(automatic via mode)*                                             |
| **hyalo-tengwar.el**       | Tengwar script rendering             | `hyalo-tengwar-minor-mode`, `hyalo-tengwar-partial-mode`           |
| **hyalo-system.el**        | macOS system integration             | `hyalo-reveal-in-finder`, `hyalo-share`, `hyalo-show-emoji-picker` |
| **hyalo-agent-extras.el**  | Agent-shell UI customization         | *(automatic)*                                                      |

### Iota Libraries (TTY Compatible)

| Library            | Purpose                     | Key Commands                                   |
|--------------------|-----------------------------|------------------------------------------------|
| **iota-dimmer.el** | Inactive window dimming     | `iota-dimmer-mode`, `iota-dimmer-apply-preset` |
| **iota-shell.el**  | Starship-like eshell prompt | `iota-shell-mode`                              |

### Hooks Reference

<details>
<summary>Click to expand hooks by library</summary>

| Library          | Hooks Used                                                                                      |
|------------------|-------------------------------------------------------------------------------------------------|
| hyalo-appearance | `ns-system-appearance-change-functions`, `enable-theme-functions`, `after-make-frame-functions` |
| hyalo-header     | `post-command-hook`, `window-configuration-change-hook`, `after-change-major-mode-hook`         |
| hyalo-footer     | `minibuffer-setup-hook`, `minibuffer-exit-hook`, `post-command-hook`                            |
| hyalo-viewport   | `window-scroll-functions`, `post-command-hook`, `window-configuration-change-hook`              |
| hyalo-sidebar    | `dired-sidebar-mode-hook`, `agent-shell-mode-hook`, `buffer-list-update-hook`                   |
| hyalo-tengwar    | `post-command-hook`, `window-scroll-functions`                                                  |
| hyalo-fonts      | `fontaine-set-preset-hook`, `enable-theme-functions`                                            |
| iota-dimmer      | `window-selection-change-functions`, `after-focus-change-function`                              |

</details>

---

## Module Configurations (`modules/`)

Use-package based configuration modules loaded in sequence.

### Module Overview

| Module              | Theme             | Primary Packages                                              |
|---------------------|-------------------|---------------------------------------------------------------|
| **init-core**       | Infrastructure    | `general`, `which-key`, `diminish`, `elog`                    |
| **init-emacs**      | Built-in settings | `recentf`, `saveplace`, `savehist`, `autorevert`              |
| **init-help**       | Help system       | `helpful`, `elisp-refs`                                       |
| **init-appearance** | Visual styling    | `fontaine`, `nerd-icons`, `modus-themes`, `ligature`, `demap` |
| **init-modeline**   | Status line       | `doom-modeline`, `keycast`                                    |
| **init-completion** | Completion UI     | `vertico`, `consult`, `marginalia`, `orderless`, `corfu`, `completion-preview` |
| **init-editing**    | Editing features  | `god-mode`, `multiple-cursors`, `outline`, `hl-todo`          |
| **init-hyalo**      | Liquid Glass      | `hyalo`, `hyalo-header`, `hyalo-appearance`, `hyalo-footer`   |
| **init-tools**      | Development       | `project`, `magit`, `diff-hl`, `eglot`, `swift-mode`          |
| **init-dired**      | File browsing     | `dired`, `dired-sidebar`, `hyalo-sidebar`                     |
| **init-agents**     | AI assistants     | `copilot`, `agent-shell`                                      |
| **init-terminal**   | Terminal          | `eat`, `iota-shell`                                           |
| **init-markdown**   | Markdown/Notes    | `markdown-mode`, `obsidian`, `polymode`                       |
| **init-tengwar**    | Tengwar script    | `hyalo-tengwar`                                               |

### Key Bindings

All leader bindings use `C-c` prefix (configured via `general.el`).

| Prefix  | Group    | Key Bindings                                                                        |
|---------|----------|-------------------------------------------------------------------------------------|
| `C-c b` | Buffer   | *(buffer operations)*                                                               |
| `C-c f` | File     | *(file operations)*                                                                 |
| `C-c h` | Help     | `f` callable, `v` variable, `k` key, `s` symbol, `.` at-point                       |
| `C-c l` | Hyalo    | `v` vibrancy, `o` opacity, `p` profile, `P` panel, `r` reveal, `s` share, `e` emoji |
| `C-c n` | Notes    | `n` capture, `j` jump, `d` daily, `s` search, `l` link, `w` wikilink                |
| `C-c p` | Project  | `f` find-file, `b` buffer, `d` dired, `k` kill, `p` switch, `s` search              |
| `C-c s` | Shell    | `s` terminal, `p` project, `e` eshell                                               |
| `C-c t` | Toggle   | `e` sidebar, `i` inspector, `E` focus sidebar, `I` focus inspector, `m` minimap     |
| `C-c v` | Version  | `s` status, `l` log, `b` blame, `d` diff                                            |
| `C-c a` | Agents   | `c` Claude, `g` Gemini, `s` sidebar, `S` screenshot, `q` queue                      |
| `C-c m` | Markdown | `l` link, `c` code, `C` code-block, `b` bold, `i` italic, `h` header                |

### Editing Bindings

| Binding                   | Command                |
|---------------------------|------------------------|
| `M-<up>` / `M-<down>`     | Move line up/down      |
| `M-s-<up>` / `M-s-<down>` | Add cursor above/below |
| `C-x <arrow>`             | Navigate windows       |
| `C-M-n` / `C-M-p`         | Scroll line            |

---

## Swift Module (`Sources/Hyalo/`)

The Hyalo Swift module provides native macOS integration via EmacsSwiftModule.

### Architecture

```
Sources/Hyalo/
├── Module.swift                    # Elisp function definitions
├── HyaloController.swift           # Main window controller
├── NavigationSidebar*.swift        # SwiftUI sidebar system
├── EmacsViews.swift                # Emacs NSView wrappers
├── ModeLineController.swift        # Mode-line rendering
├── GlassEffectView.swift           # Vibrancy/transparency
└── AppearancePanel.swift           # Settings panel
```

### Exposed Functions

Key functions exposed to Elisp:

| Function                                    | Purpose                          |
|---------------------------------------------|----------------------------------|
| `hyalo-setup`                               | Initialize NavigationSplitView   |
| `hyalo-sidebar-show` / `hyalo-sidebar-hide` | Control sidebar visibility       |
| `hyalo-content-width`                       | Get content area width in pixels |
| `hyalo-set-vibrancy`                        | Set blur material                |
| `hyalo-set-opacity`                         | Set background opacity           |
| `hyalo-set-appearance`                      | Set light/dark/auto mode         |

---

## Appearance Profiles

Three pre-configured profiles for different contexts:

| Profile   | Appearance | Theme             | Vibrancy | Opacity |
|-----------|------------|-------------------|----------|---------|
| **Light** | Light      | `ef-summer`       | regular  | 0.60    |
| **Dark**  | Dark       | `ef-melissa-dark` | regular  | 0.60    |

Switch profiles with `C-c l .` or `M-x hyalo-load-profile`.

---

## References

- [Emacs Build Recipe](https://github.com/jwintz/emacs) — Patched Emacs build
- [EmacsSwiftModule](https://savchenkovaleriy.github.io/emacs-swift-module/) — Swift dynamic module framework
- [GNU Emacs Lisp Manual](https://www.gnu.org/software/emacs/manual/html_node/elisp/) — Elisp reference

---

## License

MIT License. See individual packages for their respective licenses.
