# Fonts

This repository uses the San Francisco font family for all UI and code display.

## SF Family

| Font | Usage |
|------|-------|
| **SF Mono** | Default code, fixed-pitch, input buffers, terminals |
| **SF Pro Display** | Variable-pitch, prose, documentation, UI elements |

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

### SF Family

Download and install from [developer.apple.com](https://developer.apple.com/fonts/).

### Nerd Fonts

Install via `M-x nerd-icons-install-fonts` or download from [nerdfonts.com](https://www.nerdfonts.com/).

### Redacted Script

Download from [github.com/christiannaths/redacted-font](https://github.com/christiannaths/redacted-font).

### Tengwar Fonts

Download Tengwar Annatar and other Tengwar fonts from various Tolkien fan sites. Tengwar Annatar is commonly available.

## Configuration

Font configuration is managed in:
- `lisp/hyalo-fonts.el` — Weight-based highlighting and font setup
- `init/init-appearance.el` — Font initialization and nerd-icons setup
- `lisp/hyalo-tengwar.el` — Tengwar font detection and rendering