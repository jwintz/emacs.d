<h1 align="center">
  <hr/>
  H Y A L O &nbsp; E m a c s
  <br>
  <sub>Liquid Glass for macOS Tahoe</sub>
  <hr/>
</h1>

<p align="center">
  <img src="screenshot.png" alt="Hyalo Emacs Screenshot" width="100%"/>
</p>

> **hy·a·lo** | ˈhīəˌlo | *adjective*: glassy; transparent.

A modern Emacs configuration featuring deep macOS integration through a custom Swift dynamic module. Hyalo brings Apple's Liquid Glass design language to Emacs with transparent backgrounds, vibrancy effects, and native SwiftUI components.

This repository serves as both a personal Emacs configuration and a showcase for the Hyalo dynamic module system.

---

## Features

- **Liquid Glass UI** — Transparent backgrounds with vibrancy blur effects
- **SwiftUI Integration** — Native SwiftUI components with embedded Emacs frames
- **San Francisco Fonts** — SF Mono and SF Pro for consistent macOS aesthetic
- **Inspector Terminal** — SwiftTerm-based terminal in the inspector panel with theme-derived colors
- **AI Agents** — GitHub Copilot integration
- **Hyalo Theme** — Dichromatic Violet/Zinc palette with automatic appearance switching
- **Nano Layout** — Minimalist layout based on N Λ N O Emacs

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

## Shell & Environment

Hyalo manages its environment variables using `exec-path-from-shell` for seamless integration with the macOS user environment.

---

## Core Libraries (`lisp/`)

Custom Elisp libraries providing Hyalo functionality and TTY-compatible utilities.

### Hyalo Libraries (macOS GUI)

| Library                       | Purpose                              | Key Commands                                                       |
|-------------------------------|--------------------------------------|--------------------------------------------------------------------|
| **hyalo.el**                  | Module loader and build system       | `hyalo-load`, `hyalo-build`, `hyalo-rebuild-and-reload`            |
| **hyalo-appearance.el**       | Vibrancy and opacity control         | `hyalo-appearance-set`, `hyalo-appearance-show-panel`              |
| **hyalo-header.el**           | SwiftUI header and mode-line sync    | `hyalo-toggle-decorations-command`                                 |
| **hyalo-footer.el**           | Decorative footer patterns           | `hyalo-footer-set-pattern`                                         |
| **hyalo-viewport.el**         | Buffer viewport offset for header    | *(automatic)*                                                      |
| **hyalo-sidebar.el**          | Embedded sidebar frames              | `hyalo-sidebar-toggle-left`, `hyalo-sidebar-toggle-right`          |
| **hyalo-minibuffer.el**       | Glass effect for mini-frame          | *(automatic)*                                                      |
| **hyalo-fonts.el**            | SF family font configuration         | *(configuration)*                                                  |
| **hyalo-icons.el**            | Monochromatic icon helpers           | *(helper functions)*                                               |
| **hyalo-minimap.el**          | Demap mouse interaction              | `hyalo-minimap-click`, `hyalo-minimap-drag-scroll`                 |
| **hyalo-markdown-mode.el**    | Markdown mode enhancements           | *(automatic via mode)*                                             |
| **hyalo-tengwar.el**          | Tengwar script rendering             | `hyalo-tengwar-minor-mode`, `hyalo-tengwar-partial-mode`           |
| **hyalo-tengwar-tutorial.el** | Interactive Tengwar tutorial         | `hyalo-tengwar-tutorial`                                           |
| **hyalo-system.el**           | macOS system integration             | `hyalo-reveal-in-finder`, `hyalo-share`, `hyalo-show-emoji-picker` |
| **hyalo-splash.el**           | Startup splash screen                | `hyalo-splash`, `hyalo-splash-setup`                               |
| **hyalo-theme.el**            | Hyalo theme and terminal palette     | `hyalo-theme-setup`, `hyalo-theme-light`, `hyalo-theme-dark`, `hyalo-theme-send-palette` |

### Iota Libraries (TTY Compatible)

| Library            | Purpose                     | Key Commands                                   |
|--------------------|-----------------------------|------------------------------------------------|
| **iota-dimmer.el** | Inactive window dimming     | `iota-dimmer-mode`, `iota-dimmer-apply-preset` |

---

## Module Configurations (`init/`)

Use-package based configuration modules loaded in sequence.

| Module              | Purpose           |
|---------------------|-------------------|
| **init-core**       | Infrastructure    |
| **init-emacs**      | Built-in settings |
| **init-help**       | Help system       |
| **init-appearance** | Visual styling    |
| **init-modeline**   | Status line       |
| **init-completion** | Completion UI     |
| **init-editing**    | Editing features  |
| **init-hyalo**      | Liquid Glass      |
| **init-tools**      | Development       |
| **init-dired**      | File browsing     |
| **init-agents**     | AI assistants     |
| **init-terminal**   | Eat terminal      |
| **init-markdown**   | Markdown/Notes    |
| **init-tengwar**    | Tengwar script    |
| **init-modes**      | Language modes    |

---

## License

MIT License. See individual packages for their respective licenses.