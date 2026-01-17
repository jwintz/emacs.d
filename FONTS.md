# Fonts

This repository uses several font families for semantic differentiation and visual styling.

## Monaspace Superfamily

The [Monaspace](https://monaspace.githubnext.com/) superfamily provides semantic font mixing:

| Font | Usage |
|------|-------|
| **Monaspace Neon Frozen** | Default code, fixed-pitch, input buffers |
| **Monaspace Xenon Frozen** | Variable-pitch, prose, documentation |
| **Monaspace Radon Frozen** | Comments, italics (handwriting style) |
| **Monaspace Krypton Frozen** | Inspector/chat buffers (pi-coding-agent) |
| **Monaspace Argon Frozen** | Available for custom use |

## System Fonts

| Font | Usage |
|------|-------|
| **SF Mono** | Fallback when Monaspace unavailable |

## Icon Fonts

| Font | Usage |
|------|-------|
| **Nerd Fonts** | Icons via `nerd-icons` package (file icons, mode-line) |

## Special Purpose Fonts

| Font | Usage |
|------|-------|
| **Redacted Script** | Tengwar partial mode (obscures non-Tengwar text) |
| **Tengwar Annatar** | Primary Tengwar script rendering |
| **Tengwar Beleriand** | Alternative Tengwar font |
| **Tengwar Eldamar** | Alternative Tengwar font |
| **Tengwar Elfica** | Alternative Tengwar font |
| **Tengwar Formal** | Alternative Tengwar font |

## Installation

### Monaspace

Download from [monaspace.githubnext.com](https://monaspace.githubnext.com/) and install the "Frozen" variants.

### Nerd Fonts

Install via `M-x nerd-icons-install-fonts` or download from [nerdfonts.com](https://www.nerdfonts.com/).

### Redacted Script

Download from [github.com/christiannaths/redacted-font](https://github.com/christiannaths/redacted-font).

### Tengwar Fonts

Download Tengwar Annatar and other Tengwar fonts from various Tolkien fan sites. Tengwar Annatar is commonly available.

## Configuration

Font configuration is managed in:
- `lisp/hyalo-fonts.el` — Fontaine presets and semantic font mixing
- `init/init-appearance.el` — Font initialization and nerd-icons setup
- `lisp/hyalo-tengwar.el` — Tengwar font detection and rendering
