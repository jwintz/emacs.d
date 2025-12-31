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

(eval-when-compile
  (require 'fontaine nil t))

(defgroup hyalo-fonts nil
  "Hyalo font configuration."
  :group 'hyalo)

;; -----------------------------------------------------------------------------
;; Fontaine Presets
;; -----------------------------------------------------------------------------

(when (fboundp 'fontaine-mode)
  (setq fontaine-presets
        '((default
           ;; Default code: Monaspace Neon
           :default-family "Monaspace Neon Var"
           :default-height 130
           :fixed-pitch-family "Monaspace Neon Var"

           ;; Prose, documentation: Mona Sans
           :variable-pitch-family "Mona Sans"

           ;; UI elements: Hubot Sans
           :mode-line-active-family "Hubot Sans"
           :mode-line-inactive-family "Hubot Sans"
           :header-line-family "Hubot Sans"
           :tab-bar-family "Hubot Sans"

           ;; Comments, italics: Monaspace Radon (handwriting style)
           :italic-family "Monaspace Radon Var"
           :italic-slant normal  ; Radon is inherently italic-looking

           ;; Line numbers: slightly lighter
           :line-number-height 0.9)

          (presentation
           :inherit default
           :default-height 180)

          (compact
           :inherit default
           :default-height 110)

          ;; Fallback preset using only system fonts
          (t
           :default-family "SF Mono"
           :default-height 130
           :fixed-pitch-family "SF Mono"
           :variable-pitch-family "SF Pro Text")))

  (fontaine-mode 1)

  ;; Explicitly apply Monaspace Radon to comment faces
  ;; (fontaine's :italic-family doesn't auto-apply to font-lock-comment-face)
  (defun hyalo-fonts--apply-comment-font ()
    "Apply Monaspace Radon to comment faces."
    (when (display-graphic-p)
      (set-face-attribute 'font-lock-comment-face nil
                          :family "Monaspace Radon Var"
                          :slant 'normal)
      (set-face-attribute 'font-lock-comment-delimiter-face nil
                          :family "Monaspace Radon Var"
                          :slant 'normal)))
  (add-hook 'fontaine-set-preset-hook #'hyalo-fonts--apply-comment-font)
  (add-hook 'after-init-hook #'hyalo-fonts--apply-comment-font))

;; -----------------------------------------------------------------------------
;; Mode-Specific Font Hooks
;; -----------------------------------------------------------------------------

(defun hyalo-fonts--agent-shell-fonts ()
  "Set agent-shell buffer to use Hubot Sans with Krypton code blocks."
  (face-remap-add-relative 'default :family "Hubot Sans")
  (face-remap-add-relative 'markdown-code-face :family "Monaspace Krypton Var")
  (face-remap-add-relative 'markdown-inline-code-face :family "Monaspace Krypton Var"))

(defun hyalo-fonts--terminal-fonts ()
  "Set terminal buffers to use Monaspace Argon."
  (face-remap-add-relative 'default :family "Monaspace Argon Var"))

;; Install hooks
(with-eval-after-load 'agent-shell
  (add-hook 'agent-shell-mode-hook #'hyalo-fonts--agent-shell-fonts))

(add-hook 'eshell-mode-hook #'hyalo-fonts--terminal-fonts)
(add-hook 'eat-mode-hook #'hyalo-fonts--terminal-fonts)
(add-hook 'term-mode-hook #'hyalo-fonts--terminal-fonts)
(add-hook 'vterm-mode-hook #'hyalo-fonts--terminal-fonts)

;; -----------------------------------------------------------------------------
;; Nerd Icons
;; -----------------------------------------------------------------------------

(with-eval-after-load 'nerd-icons
  (setq nerd-icons-font-family "Symbols Nerd Font Mono"))

(provide 'hyalo-fonts)

;;; hyalo-fonts.el ends here
