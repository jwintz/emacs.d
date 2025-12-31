# Coding Conventions

## Elisp Naming

| Prefix | Scope | Example |
|--------|-------|---------|
| `hyalo/abc` | Public, interactive command (`M-x` or keybinding) | `hyalo/toggle-sidebar` |
| `hyalo:abc` | Public Evil operator, motion, or command | `hyalo:scroll-up` |
| `hyalo\|abc` | Public, non-interactive hook function | `hyalo\|on-theme-change` |
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

Uses [Fontaine](https://protesilaos.com/emacs/fontaine) with [Monaspace](https://monaspace.githubnext.com/) fonts:

| Context | Font |
|---------|------|
| Default code | Monaspace Neon Var |
| Markdown, Info | Monaspace Xenon Var |
| Comments, italics | Monaspace Radon Var |
| Terminals (eshell, eat) | Monaspace Argon Var |
| Agent-shell prose | Hubot Sans |
| Agent-shell code blocks | Monaspace Krypton Var |
| Variable-pitch prose | Mona Sans |
| UI (modeline, tabs) | Hubot Sans |
| Nerd icons | Symbols Nerd Font Mono |

### Font Fallback Strategy

```elisp
;; Fontaine fallback preset (system fonts only)
(t
 :default-family "SF Mono"
 :fixed-pitch-family "SF Mono"
 :variable-pitch-family "SF Pro Text")
```

Fallback order:
1. **Monaspace Var fonts** - Primary, variable font with texture healing
2. **Monaspace Frozen fonts** - Static weight variant if Var unavailable
3. **SF family** - macOS system fonts (always available)
4. **Menlo** - Legacy fallback

## Highlight System

Uses weight-based differentiation (no backgrounds for Liquid Glass transparency):

| Highlight | Weight |
|-----------|--------|
| `hl-line` | medium |
| `region` | bold |
| `isearch` | extra-bold + box |
| `lazy-highlight` | semi-bold |
| Magit | extra-bold |
