# Coding Conventions

## Elisp Naming

| Prefix | Scope | Example |
|--------|-------|---------|
| `hyalo/abc` | Public, interactive command (`M-x` or keybinding) | `hyalo/toggle-sidebar` |
| `hyalo:abc` | Public Evil operator, motion, or command | `hyalo:scroll-up` |
| `hyalo|abc` | Public, non-interactive hook function | `hyalo|on-theme-change` |
| `hyalo*abc` | Advice function for other functions | `hyalo*after-load-theme` |
| `hyalo--abc` | Private implementation detail | `hyalo--update-state` |

## Swift Naming

- Use `Hyalo` prefix for public types
- Subdirectories group by domain: `Navigation/`, `Toolbar/`, `Sidebar/`, `Emacs/`
- Files named after primary type they contain

## Minimalism

- No dead code
- No commented-out blocks
- Every function should be called somewhere
- Remove unused imports/requires

## Font Stack

Uses the San Francisco font family:

| Context | Font |
|---------|------|
| Default code | SF Mono |
| Markdown, Info | SF Mono / SF Pro Display |
| Comments, italics | SF Mono (slanted) |
| Terminals (eat) | SF Mono |
| UI elements | SF Pro Display |
| Nerd icons | Symbols Nerd Font Mono |

No fallbacks to other font families are used.

## Highlight System

Uses weight-based differentiation (no backgrounds for Liquid Glass transparency):

| Highlight | Weight |
|-----------|--------|
| `hl-line` | medium |
| `region` | bold |
| `isearch` | ultra-bold + underline |
| `lazy-highlight` | bold |
| Magit | ultra-bold |