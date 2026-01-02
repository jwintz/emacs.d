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

(when (fboundp 'fontaine-mode)
  (setq fontaine-presets
        '((default
           ;; Default code: Monaspace Neon
           :default-family "Monaspace Neon Var"
           :default-height 110
           :line-spacing 0.1
           :fixed-pitch-family "Monaspace Neon Var"

           ;; Prose, documentation: Monaspace Xenon Var
           :variable-pitch-family "Monaspace Xenon Var"
           :variable-pitch-height 1.0  ; Relative height (1.0x) ensures scaling with text-scale-increase

           ;; Comments, italics: Monaspace Radon (handwriting style)
           :italic-family "Monaspace Radon Var"
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
  (add-hook 'after-init-hook #'hyalo-fonts--apply-comment-font)
  (add-hook 'enable-theme-functions (lambda (&rest _) (hyalo-fonts--apply-comment-font)))

  (fontaine-mode 1)
  (fontaine-set-preset 'default)

  ;; Apply once immediately in case we are reloading
  (hyalo-fonts--apply-comment-font))

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

;; Agent Shell
(defun hyalo-fonts--agent-shell-fonts ()
  "Set agent-shell buffer to use Hubot Sans with Krypton code blocks."
  (let ((height (if (boundp 'hyalo-sidebar-font-height)
                    hyalo-sidebar-font-height
                  100)))
    (face-remap-add-relative 'default :family "Monaspace Krypton Var" :height height)
    (face-remap-add-relative 'markdown-code-face :family "Monaspace Krypton Var" :height height)
    (face-remap-add-relative 'markdown-inline-code-face :family "Monaspace Krypton Var" :height height)))

(with-eval-after-load 'agent-shell
  (add-hook 'agent-shell-mode-hook #'hyalo-fonts--agent-shell-fonts))

;; Markdown
(defun hyalo-fonts--markdown-setup ()
  "Ensure mixed-pitch-mode is active for markdown."
  (unless (bound-and-true-p mixed-pitch-mode)
    (mixed-pitch-mode 1)))

(add-hook 'markdown-mode-hook #'hyalo-fonts--markdown-setup)

;; Magit
(defun hyalo-fonts--magit-fonts ()
  "Set Magit buffers to use Monaspace Neon for italics (avoiding Radon)."
  (face-remap-add-relative 'italic :family "Monaspace Neon Var" :slant 'italic))

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
  (face-remap-add-relative 'default :family "Monaspace Neon Var")
  ;; Remove variable-pitch inheritance
  (face-remap-add-relative 'variable-pitch :family "Monaspace Neon Var"))

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
  (face-remap-add-relative 'default :family "Monaspace Argon Var"))

(add-hook 'eshell-mode-hook #'hyalo-fonts--terminal-fonts)
(add-hook 'eat-mode-hook #'hyalo-fonts--terminal-fonts)
(add-hook 'term-mode-hook #'hyalo-fonts--terminal-fonts)
(add-hook 'vterm-mode-hook #'hyalo-fonts--terminal-fonts)

;; Info
(defun hyalo-fonts--info-fonts ()
  "Set Info buffers to use Monaspace Xenon."
  (face-remap-add-relative 'default :family "Monaspace Xenon Var"))

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
  (let* ((base-color (face-attribute 'default :background))
       (tint-color (face-attribute 'highlight :background))
       (alpha      0.9)
       (blended-color (apply 'color-rgb-to-hex
                             (color-blend (color-name-to-rgb tint-color)
                                          (color-name-to-rgb base-color)
                                          alpha))))
  ;; Apply the face attribute
  (set-face-attribute 'demap-visible-region-face nil
                      :box (list :line-width -4          ;; -4 ??
                                 :color blended-color))))

(hyalo-fonts--update-demap-face)

(with-eval-after-load 'demap
  (add-hook 'demap-minimap-construct-hook #'hyalo-fonts--update-demap-face)
  (add-hook 'demap-minimap-construct-hook #'hyalo-fonts--demap-fonts)
  (add-hook 'enable-theme-functions #'hyalo-fonts--update-demap-face))

(provide 'hyalo-fonts)

;;; hyalo-fonts.el ends here
