;;; hyalo-fonts.el --- Fontaine font configuration for Hyalo -*- lexical-binding: t; -*- no-byte-compile: t -*-

;; Copyright (C) 2025

;; Semantic font mixing using Monaspace superfamily
;; Based on https://monaspace.githubnext.com "What if?" philosophy

;;; Commentary:

;; Provides Fontaine presets with Monaspace font variants:
;; - Neon: Default code (neo-grotesque sans)
;; - Argon: Terminals (humanist sans)
;; - Radon: Comments, italics (handwriting)
;; - Xenon: Slab serif (text with serifs)
;; - Krypton: Agent-shell code blocks (mechanical sans)
;; - Hubot Sans: Agent-shell prose, UI elements
;; - Mona Sans: Variable-pitch prose

;;; Code:

(require 'color)

(eval-when-compile
  (require 'fontaine nil t))

(defgroup hyalo-fonts nil
  "Hyalo font configuration."
  :group 'hyalo)

;; -----------------------------------------------------------------------------
;; Fontaine Presets
;; -----------------------------------------------------------------------------

(defun hyalo-fonts--apply-comment-font ()
  "Apply Monaspace Radon to comment faces."
  (when (display-graphic-p)
    (set-face-attribute 'font-lock-comment-face nil
                        :family "Monaspace Radon Frozen"
                        :slant 'normal)
    (set-face-attribute 'font-lock-comment-delimiter-face nil
                        :family "Monaspace Radon Frozen"
                        :slant 'normal)))

(defun hyalo-fonts-set-highlights (&rest _)
  "Set highlight faces using weight differentiation.
Updates weights for region, search, and completion faces to ensure
proper visual hierarchy without relying solely on colors."
  (let ((m 'medium)
        (b 'bold)
        (wb 'ultra-bold))
    (set-face-attribute 'region nil :weight b)
    (set-face-attribute 'isearch nil :weight wb :underline t)
    (set-face-attribute 'lazy-highlight nil :weight b)
    (set-face-attribute 'match nil :weight b)
    (set-face-attribute 'show-paren-match nil :weight b)
    (when (facep 'hl-line)
      (set-face-attribute 'hl-line nil :weight m))
    (when (facep 'highlight)
      (set-face-attribute 'highlight nil :weight m))
    (when (facep 'vertico-current)
      (set-face-attribute 'vertico-current nil :weight b))
    ;; Completion faces
    (dolist (face '(orderless-match-face-0
                    orderless-match-face-1
                    orderless-match-face-2
                    orderless-match-face-3
                    consult-highlight-match
                    consult-preview-match))
      (when (facep face)
        (set-face-attribute face nil :weight wb)))
    ;; Magit faces
    (dolist (mface '(magit-section-highlight
                     magit-diff-hunk-heading-highlight
                     magit-diff-context-highlight))
      (when (facep mface)
        (set-face-attribute mface nil :weight wb)))
    ;; Markdown faces
    (dolist (mdface '(markdown-bold-face
                      markdown-italic-face
                      markdown-header-face
                      markdown-header-face-1
                      markdown-header-face-2
                      markdown-header-face-3
                      markdown-header-face-4
                      markdown-link-face
                      markdown-url-face))
      (when (facep mdface)
        (set-face-attribute mdface nil :weight b)))))

(add-hook 'fontaine-set-preset-hook #'hyalo-fonts--apply-comment-font)
(add-hook 'after-init-hook #'hyalo-fonts--apply-comment-font)
(add-hook 'enable-theme-functions (lambda (&rest _) (hyalo-fonts--apply-comment-font)))

(add-hook 'fontaine-set-preset-hook #'hyalo-fonts-set-highlights)
(add-hook 'enable-theme-functions #'hyalo-fonts-set-highlights)
(with-eval-after-load 'magit
  (hyalo-fonts-set-highlights))

(when (fboundp 'fontaine-mode)
  (setq fontaine-presets
        '((default
           ;; Default code: Monaspace Neon
           :default-family "SF Mono" ;; "Monaspace Neon Frozen"
           :default-height 110
           :line-spacing 0.2
           :fixed-pitch-family "SF Mono" ;; "Monaspace Neon Frozen"

           ;; Prose, documentation: Monaspace Xenon Var
           :variable-pitch-family "SF Mono" ;; "Monaspace Xenon Frozen"
           :variable-pitch-height 1.0  ; Relative height (1.0x) ensures scaling with text-scale-increase

           ;; Comments, italics: Monaspace Radon (handwriting style)
           :italic-family "Monaspace Radon Frozen"
           :italic-slant normal  ; Radon is inherently italic-looking
	   )

          (presentation
           :inherit default
           :default-height 180)

          (compact
           :inherit default
           :default-height 90)

          ;; Fallback preset using only system fonts
          (t
           :default-family "SF Mono"
           :default-height 110
           :fixed-pitch-family "SF Mono"
           :variable-pitch-family "SF Mono")))

  (fontaine-mode 1)
  (fontaine-set-preset 'default)

  ;; Apply once immediately in case we are reloading
  (hyalo-fonts--apply-comment-font)
  (hyalo-fonts-set-highlights))

(defun hyalo-fonts--fix-line-numbers ()
  "Ensure line number faces use SF Mono at 0.9 height.
Prevents line height changes when toggling display-line-numbers-mode."
  (when (display-graphic-p)
    (dolist (face '(line-number line-number-current-line))
      (when (facep face)
        (set-face-attribute face nil
                            :family "SF Mono"
                            :height 0.9
                            :weight 'normal)))))

(add-hook 'fontaine-set-preset-hook #'hyalo-fonts--fix-line-numbers)
(add-hook 'enable-theme-functions (lambda (&rest _) (hyalo-fonts--fix-line-numbers)))
;; Apply immediately
(hyalo-fonts--fix-line-numbers)

;; -----------------------------------------------------------------------------
;; Mode-Specific Font Hooks
;; -----------------------------------------------------------------------------

;; Markdown Code Faces
(defun hyalo-fonts--reset-markdown-faces ()
  "Reset markdown code faces to default height to prevent theme scaling.
This ensures they match the prose size exactly."
  (when (display-graphic-p)
    (dolist (face '(markdown-code-face
                    markdown-inline-code-face
                    markdown-pre-face
                    markdown-language-keyword-face))
      (when (facep face)
        (set-face-attribute face nil :height 'unspecified)))))

(add-hook 'enable-theme-functions (lambda (&rest _) (hyalo-fonts--reset-markdown-faces)))
(add-hook 'fontaine-set-preset-hook #'hyalo-fonts--reset-markdown-faces)
;; Also run immediately
(hyalo-fonts--reset-markdown-faces)

;; Pi Coding Agent / Inspector

(defcustom hyalo-inspector-font "Monaspace Krypton Frozen"
  "Font family to use for inspector (right sidebar) frames.
Used for pi-coding-agent and other inspector content."
  :type 'string
  :group 'hyalo-fonts)

(defcustom hyalo-inspector-font-height 100
  "Font height for inspector frames (100 = 10pt)."
  :type 'integer
  :group 'hyalo-fonts)

(defun hyalo-fonts--pi-coding-agent-chat-fonts ()
  "Set pi-coding-agent chat buffer to use Monaspace Krypton.
Disables mixed-pitch-mode to ensure monospace display."
  (message "DEBUG hyalo-fonts: pi-chat-fonts called in buffer %s" (buffer-name))
  (message "DEBUG hyalo-fonts: mixed-pitch-mode BEFORE = %s" (bound-and-true-p mixed-pitch-mode))
  ;; Disable mixed-pitch-mode if enabled (from text-mode-hook or markdown-mode-hook)
  (when (bound-and-true-p mixed-pitch-mode)
    (message "DEBUG hyalo-fonts: disabling mixed-pitch-mode NOW")
    (mixed-pitch-mode -1))
  (message "DEBUG hyalo-fonts: mixed-pitch-mode AFTER = %s" (bound-and-true-p mixed-pitch-mode))
  ;; CRITICAL: Clear ALL face remappings to remove mixed-pitch leftovers
  ;; mixed-pitch-mode -1 doesn't clean up its remappings properly
  (setq-local face-remapping-alist nil)
  (message "DEBUG hyalo-fonts: cleared face-remapping-alist")
  (let ((family "Monaspace Krypton Frozen")
        (height (or (bound-and-true-p hyalo-inspector-font-height) 100)))
    (message "DEBUG hyalo-fonts: setting chat font to %s height %s" family height)
    ;; Remap default face for monospace
    (face-remap-add-relative 'default :family family :height height)
    ;; Also remap variable-pitch to prevent mixed-pitch interference
    (face-remap-add-relative 'variable-pitch :family family :height height)
    ;; Markdown faces
    (face-remap-add-relative 'markdown-code-face :family family :height height)
    (face-remap-add-relative 'markdown-inline-code-face :family family :height height)))

(defun hyalo-fonts--pi-coding-agent-input-fonts ()
  "Set pi-coding-agent input buffer to use Monaspace Neon.
Also configures cursor style and header-line font."
  (message "DEBUG hyalo-fonts: pi-input-fonts called in buffer %s" (buffer-name))
  ;; Disable mixed-pitch-mode if enabled (from text-mode-hook)
  (when (bound-and-true-p mixed-pitch-mode)
    (message "DEBUG hyalo-fonts: disabling mixed-pitch-mode in input buffer")
    (mixed-pitch-mode -1))
  (let ((family "Monaspace Neon Frozen")
        (height (or (bound-and-true-p hyalo-inspector-font-height) 100)))
    (message "DEBUG hyalo-fonts: setting input font to %s height %s" family height)
    (face-remap-add-relative 'default :family family :height height)
    ;; Header-line must have explicit family and height to display correctly
    ;; even when the window is not selected (Emacs 29+ uses header-line-inactive)
    (message "DEBUG hyalo-fonts: setting header-line font explicitly")
    (face-remap-add-relative 'header-line :family family :height height)
    (when (facep 'header-line-inactive)
      (face-remap-add-relative 'header-line-inactive :family family :height height)))
  ;; Cursor style: horizontal bar
  (message "DEBUG hyalo-fonts: setting cursor-type to (hbar . 2)")
  (setq-local cursor-type '(hbar . 2)))

(with-eval-after-load 'pi-coding-agent
  (add-hook 'pi-coding-agent-chat-mode-hook #'hyalo-fonts--pi-coding-agent-chat-fonts)
  (add-hook 'pi-coding-agent-input-mode-hook #'hyalo-fonts--pi-coding-agent-input-fonts))

;; Markdown
(defun hyalo-fonts--markdown-setup ()
  "Ensure mixed-pitch-mode is active for markdown.
Skips pi-coding-agent buffers which use monospace."
  (message "DEBUG hyalo-fonts: markdown-setup called in buffer %s" (buffer-name))
  (message "DEBUG hyalo-fonts: is pi-coding-agent buffer? %s"
           (string-match-p "\\*pi-coding-agent-" (buffer-name)))
  (if (string-match-p "\\*pi-coding-agent-" (buffer-name))
      (message "DEBUG hyalo-fonts: SKIPPING mixed-pitch for pi-coding-agent buffer")
    (unless (bound-and-true-p mixed-pitch-mode)
      (message "DEBUG hyalo-fonts: ENABLING mixed-pitch-mode")
      (mixed-pitch-mode 1))))

(add-hook 'markdown-mode-hook #'hyalo-fonts--markdown-setup)

;; Magit
(defun hyalo-fonts--magit-fonts ()
  "Set Magit buffers to use Monaspace Neon for italics (avoiding Radon)."
  ;; (face-remap-add-relative 'italic :family "Monaspace Neon Frozen" :slant 'italic)
  (face-remap-add-relative 'italic :family "SF Mono" :slant 'italic)
  )

(add-hook 'magit-mode-hook #'hyalo-fonts--magit-fonts)
(add-hook 'magit-status-mode-hook #'hyalo-fonts--magit-fonts)
(add-hook 'magit-log-mode-hook #'hyalo-fonts--magit-fonts)
(add-hook 'magit-diff-mode-hook #'hyalo-fonts--magit-fonts)
(add-hook 'git-rebase-mode-hook #'hyalo-fonts--magit-fonts)

;; Git Commit
(defun hyalo-fonts--git-commit-fonts ()
  "Set git-commit buffers to use fixed-pitch (not variable-pitch).
Git commit messages should use monospace font for proper formatting."
  ;; Disable mixed-pitch-mode if it was enabled
  (when (bound-and-true-p mixed-pitch-mode)
    (mixed-pitch-mode -1))
  ;; Ensure fixed-pitch
  (face-remap-add-relative 'default :family "SF Mono") ;; "Monaspace Neon Frozen")
  ;; Remove variable-pitch inheritance
  (face-remap-add-relative 'variable-pitch :family "SF Mono")) ;; "Monaspace Neon Frozen"))

(add-hook 'git-commit-mode-hook #'hyalo-fonts--git-commit-fonts)

;; Sidebars
(defun hyalo-fonts--sidebar-fonts ()
  "Set sidebar buffers to use configured font."
  (let ((family (if (bound-and-true-p hyalo-sidebar-font)
                    hyalo-sidebar-font
                  "SF Mono"))
        (height (if (boundp 'hyalo-sidebar-font-height)
                    hyalo-sidebar-font-height
                  100)))
    (face-remap-add-relative 'default :family family :height height)
    ;; Ensure nerd-icons still display correctly
    (face-remap-add-relative 'nerd-icons-dired-dir-face :family "Symbols Nerd Font Mono")))

(add-hook 'dired-sidebar-mode-hook #'hyalo-fonts--sidebar-fonts)
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (when (string= (buffer-name) "*Hyalo-Ibuffer*")
              (hyalo-fonts--sidebar-fonts))))

;; Terminals
(defun hyalo-fonts--terminal-fonts ()
  "Set terminal buffers to use Monaspace Argon."
  (face-remap-add-relative 'default :family "Monaspace Argon Frozen"))

(add-hook 'eshell-mode-hook #'hyalo-fonts--terminal-fonts)
(add-hook 'eat-mode-hook #'hyalo-fonts--terminal-fonts)
(add-hook 'term-mode-hook #'hyalo-fonts--terminal-fonts)
(add-hook 'vterm-mode-hook #'hyalo-fonts--terminal-fonts)

;; Info
(defun hyalo-fonts--info-fonts ()
  "Set Info buffers to use Monaspace Xenon."
  (face-remap-add-relative 'default :family "Monaspace Xenon Frozen"))

(add-hook 'Info-mode-hook #'hyalo-fonts--info-fonts)

;; -----------------------------------------------------------------------------
;; Nerd Icons
;; -----------------------------------------------------------------------------

(with-eval-after-load 'nerd-icons
  (setq nerd-icons-font-family "Symbols Nerd Font Mono"))

(when (display-graphic-p)
  (set-fontset-font t '(#xe000 . #xffdd)
                    (font-spec :name "Symbols Nerd Font Mono"
                               :size 11) nil))

(defun hyalo-fonts--demap-fonts ()
  "Configure demap buffer to use consistent fonts (no Radon).
Ensures comments and delimiters use Redacted Script without a fixed height,
allowing them to scale with the minimap."
  (face-remap-add-relative 'font-lock-comment-face
                           :family "Redacted Script"
                           :weight 'regular
                           :slant 'normal)
  (face-remap-add-relative 'font-lock-comment-delimiter-face
                           :family "Redacted Script"
                           :weight 'regular
                           :slant 'normal)

  (face-remap-add-relative 'demap-minimap-font-face
                      :family "Redacted Script"
                      :weight 'regular
                      :height 0.3)

  ;; Ensure remappings persist if buffer is rebuilt
  (when (fboundp 'demap-minimap-protect-variables)
    (demap-minimap-protect-variables t 'face-remapping-alist)))

(defun hyalo-fonts--update-demap-face (&rest _args)
  "Update demap visible region face with blended box color.
Only runs when demap-visible-region-face exists."
  (when (facep 'demap-visible-region-face)
    (let* ((base-color (face-attribute 'default :background))
           (tint-color (face-attribute 'highlight :background))
           (alpha 0.9)
           (blended-color (apply 'color-rgb-to-hex
                                 (color-blend (color-name-to-rgb tint-color)
                                              (color-name-to-rgb base-color)
                                              alpha))))
      (set-face-attribute 'demap-visible-region-face nil
                          :box (list :line-width -4
                                     :color blended-color)))))

(with-eval-after-load 'demap
  (hyalo-fonts--update-demap-face)
  (add-hook 'demap-minimap-construct-hook #'hyalo-fonts--update-demap-face)
  (add-hook 'demap-minimap-construct-hook #'hyalo-fonts--demap-fonts)
  (add-hook 'enable-theme-functions #'hyalo-fonts--update-demap-face))

(provide 'hyalo-fonts)

;;; hyalo-fonts.el ends here
