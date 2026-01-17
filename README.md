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

## Package Management

Hyalo uses `package.el` with `use-package` for configuration. To ensure fast startup and stability, package contents are **not** refreshed automatically on launch.

### Update Workflow

1.  **Installing New Packages**:
    Add the `(use-package ...)` block to your config and restart. If the package is not found, run `M-x package-refresh-contents` manually and try again.

2.  **Updating Packages**:
    - **ELPA/MELPA**: Run `M-x list-packages`, press `U` to mark upgrades, and `x` to execute.
    - **Git/Source Packages**: Run `M-x package-vc-upgrade-all` to update packages installed via `:vc`.

### Contrib Packages

The `contrib/` directory holds forked or local packages that take precedence over ELPA/MELPA. Each subdirectory is automatically added to `load-path` during bootstrap.

**Current contrib packages:**

| Package | Source | Purpose |
|---------|--------|---------|
| `pi-coding-agent` | [jwintz/pi-coding-agent](https://github.com/jwintz/pi-coding-agent) | AI coding agent with editor features |

**Usage:**

1. Clone the fork into `contrib/`:
   ```bash
   cd contrib
   git clone https://github.com/jwintz/pi-coding-agent
   ```

2. Configure with `use-package` using `:ensure nil`:
   ```elisp
   (use-package pi-coding-agent
     :ensure nil  ; loaded from contrib/
     :commands (pi-coding-agent))
   ```

3. To update, pull in the contrib directory:
   ```bash
   cd contrib/pi-coding-agent && git pull
   ```

---

## Shell & Environment

Hyalo manages its environment variables (specifically `PATH` and `exec-path`) using a native Elisp-based approach, replacing the need for external tools like `exec-path-from-shell`.

### Dynamic Path Resolution

The configuration in `conf/eshlogin` is loaded at startup to dynamically resolve and prepend critical paths to the environment:

- **Node.js (NVM)**: Automatically resolves the default Node version path from `~/.nvm` by reading aliases or detecting the latest installed version.
- **Pixi**: Adds `~/.pixi/bin` if present.
- **Local Binaries**: Adds `~/.local/bin` if present.

This ensures that language servers (LSP), compilers, and Eshell commands are always available without requiring manual path management or slow shell-spawning during startup.

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
| **hyalo-splash.el**        | Startup splash screen                | `hyalo-splash`, `hyalo-splash-setup`                               |
| **hyalo-agent.el** | SVG backgrounds for pi-coding-agent | *(automatic via hook)*                                             |

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
| hyalo-sidebar    | `dired-sidebar-mode-hook`, `pi-coding-agent-input-mode-hook`, `buffer-list-update-hook`               |
| hyalo-tengwar    | `post-command-hook`, `window-scroll-functions`                                                  |
| hyalo-splash     | `window-setup-hook`, `window-configuration-change-hook`                                         |
| hyalo-fonts      | `fontaine-set-preset-hook`, `enable-theme-functions`                                            |
| hyalo-agent | `enable-theme-functions`, `window-configuration-change-hook`, `after-change-functions`     |
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
| **init-agents**     | AI assistants     | `copilot`, `pi-coding-agent`                                  |
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
| `C-c a` | Agents   | `p` pi, `m` menu, `s` sidebar, `f` focus                                        |
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

## SVG Transparency with Alpha-Background

When using `alpha-background` for frame transparency, SVG images by default render with an opaque canvas (Emacs/librsvg limitation). To achieve true transparency:

```elisp
(create-image svg-data 'svg t
              :mask 'heuristic)
```

The `:mask 'heuristic` property instructs Emacs to detect and mask transparent regions, allowing the frame's alpha-background to show through. This is currently the only reliable method for transparent SVG rendering in frames with `alpha-background`.

**Note**: Without `:mask 'heuristic`, transparent SVG regions are filled with the `default` face background color.

---

## References

- [Emacs Build Recipe](https://github.com/jwintz/emacs) — Patched Emacs build
- [EmacsSwiftModule](https://savchenkovaleriy.github.io/emacs-swift-module/) — Swift dynamic module framework
- [GNU Emacs Lisp Manual](https://www.gnu.org/software/emacs/manual/html_node/elisp/) — Elisp reference

---

## License

MIT License. See individual packages for their respective licenses.
