;;; init.el -*- lexical-binding: t; no-byte-compile: t -*-

(setq debug-on-error t)

;;; ============================================================================
;;; Early Protection (must be before any package loads)
;;; ============================================================================

;; Protect against nerd-icons returning nil (breaks doom-modeline)
;; Advise doom-modeline directly since nerd-icons advice timing is unreliable
(with-eval-after-load 'doom-modeline-core
  (defun hyalo--doom-modeline-safe-icon (orig-fun &rest args)
    "Wrap doom-modeline icon update to handle nil icons."
    (condition-case nil
        (apply orig-fun args)
      (wrong-type-argument nil)))
  (advice-add 'doom-modeline-update-buffer-file-icon :around #'hyalo--doom-modeline-safe-icon))

;;; ============================================================================
;;; Bootstrap
;;; ============================================================================

;;;; Suppress Warnings

;; Suppress byte-compilation warnings during startup
(setq byte-compile-warnings nil)
(setq warning-suppress-types '((comp) (bytecomp)))
(setq warning-minimum-level :error)

;;;; Directory Organization
;;
;; Redirect user-emacs-directory so packages automatically store generated
;; files in .local/, keeping the config directory clean.
;;
;; Structure:
;;   ~/.config/emacs/           <- emacs-config-dir (init.el, hyalo-module, etc.)
;;   ~/.config/emacs/.local/    <- user-emacs-directory (all generated files)

(defvar emacs-config-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "The root directory of Emacs configuration.")

(setq user-emacs-directory (expand-file-name ".local/" emacs-config-dir))

(dolist (subdir '("" "auto-save" "auto-save-list" "transient" "eshell" "etc"))
  (make-directory (expand-file-name subdir user-emacs-directory) t))

;; Suppress the load-path warning for our config directory
(defun emacs--suppress-load-path-warning (orig-fun type message &rest args)
  "Suppress warning about config directory in `load-path'."
  (unless (and (eq type 'initialization)
               (string-prefix-p "Your `load-path' seems to contain\nyour `.emacs.d' directory"
                                message t))
    (apply orig-fun type message args)))
(advice-add 'display-warning :around #'emacs--suppress-load-path-warning)

;; Files that need explicit paths
(setq auto-save-list-file-prefix (locate-user-emacs-file "auto-save-list/.saves-"))
(setq auto-save-file-name-transforms `((".*" ,(locate-user-emacs-file "auto-save/") t)))
(setq custom-file (locate-user-emacs-file "etc/custom.el"))
(setq package-user-dir (locate-user-emacs-file "elpa/"))

;; Load custom file if it exists
(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))

;; Suppress "Saving file..." and "Wrote..." messages for custom-file
(defun emacs--suppress-custom-save-message (orig-fun &rest args)
  "Suppress messages during `custom-save-all` and `customize-save-variable`."
  (let ((inhibit-message t)
        (save-silently t))
    (apply orig-fun args)))
(advice-add 'custom-save-all :around #'emacs--suppress-custom-save-message)
(advice-add 'customize-save-variable :around #'emacs--suppress-custom-save-message)

;;;; Package System

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(package-initialize)

;; Ensure use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;;; Core Packages

(use-package exec-path-from-shell
  :ensure t
  :demand t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package diminish
  :demand t)

(use-package general
  :demand t
  :config
  (general-create-definer leader-def :prefix "C-c")

  ;; Define prefix groups for which-key
  (leader-def
    "b" '(:ignore t :wk "buffer")
    "f" '(:ignore t :wk "file")
    "h" '(:ignore t :wk "help")
    "l" '(:ignore t :wk "hyalo")
    "n" '(:ignore t :wk "notes")
    "p" '(:ignore t :wk "project")
    "t" '(:ignore t :wk "toggle")
    "v" '(:ignore t :wk "versionning")))

(use-package which-key
  :diminish
  :custom
  (which-key-idle-delay 0.3)
  (which-key-separator " -> ")
  (which-key-prefix-prefix "")
  (which-key-inhibit-regexps '("^ESC"))
  :config
  (which-key-mode 1))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key helpful-symbol)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol)
  :general
  (leader-def
    "h f" 'helpful-callable
    "h v" 'helpful-variable
    "h k" 'helpful-key
    "h s" 'helpful-symbol
    "h ." 'helpful-at-point))

(use-package elisp-refs
  :general
  (:prefix "C-c h r"
   "" '(:ignore t :which-key "elisp-refs")
   "f" 'elisp-refs-function
   "m" 'elisp-refs-macro
   "v" 'elisp-refs-variable
   "s" 'elisp-refs-special
   "r" 'elisp-refs-symbol)
  :commands (elisp-refs-function
             elisp-refs-macro
             elisp-refs-variable
             elisp-refs-special
             elisp-refs-symbol))

(defvar elog-emacs nil
  "Main Emacs logger (nil if elog not loaded).")

(defvar emacs-init-section-start nil
  "Start time of current init section.")

(defvar emacs-init-start-time (current-time)
  "Start time of Emacs init.")

(defun emacs-log-info (msg)
  "Log MSG using elog if available, otherwise use `message'."
  (if (and elog-emacs (fboundp 'elog-info))
      (elog-info elog-emacs msg)
    nil))

(defun emacs-section-start (name)
  "Start timing section NAME."
  (setq emacs-init-section-start (current-time))
  (emacs-log-info (format "Loading section: %s" name)))

(defun emacs-section-end ()
  "End current section and report elapsed time."
  (when emacs-init-section-start
    (let ((elapsed (float-time (time-subtract (current-time) emacs-init-section-start))))
      (emacs-log-info (format "  -> %.3fs" elapsed))
      (setq emacs-init-section-start nil))))

(defun emacs-log-init ()
  "Initialize elog logger if available."
  (when (fboundp 'elog-make-logger)
    (setq elog-emacs
          (elog-make-logger
           :name "emacs"
           :level 'info
           :appenders (list (elog-make-appender
                             :type 'buffer
                             :name "*elog*"))))))

(use-package elog
  :vc (:url "https://github.com/Kinneyzhang/elog")
  :defer t
  :init
  (add-hook 'after-init-hook #'emacs-log-init))

;;; ============================================================================
;;; Core
;;; ============================================================================

(emacs-section-start "Core")

;;;; Defaults

(use-package emacs
  :ensure nil
  :custom
  ;; Startup
  (inhibit-startup-screen t)
  (inhibit-startup-message t)
  (inhibit-startup-echo-area-message t)
  (initial-scratch-message ";;; Hyalo Footer Pattern Test (C-x C-e each line)

;; 1. Load and enable footer module:
(require 'hyalo-module-footer)
(hyalo-module-footer-mode 1)

;; 2. Set pattern:
(hyalo-module-footer-set-pattern \"hexagons\")
;; Options: hideout, hexagons, death-star, bathroom-floor,
;;          tiny-checkers, plus, cage, diagonal-stripes,
;;          stripes, diagonal-lines, polka-dots, signal, wallpaper

;; 3. Adjust background alpha (tint layer - darker/lighter):
(hyalo-module-footer-set-background-alpha 0.4)

;; 4. Adjust pattern alpha (foreground pattern):
(hyalo-module-footer-set-pattern-alpha 0.3)

;; 5. Trigger message to see pattern:
(message \"Hello from the echo area!\")

")
  (initial-buffer-choice nil)
  ;; Cursor
  (cursor-in-non-selected-windows nil)
  (cursor-type '(hbar . 2))
  (cursor-intangible-mode t)
  (x-stretch-cursor nil)
  ;; Text
  (text-scale-mode-step 1.1)
  ;; Backups
  (make-backup-files nil)
  ;; Misc
  (use-short-answers t)
  (use-dialog-box nil)
  (frame-title-format nil)
  :config
  (setq visible-bell nil)
  (setq ring-bell-function 'ignore)
  (advice-add 'display-startup-echo-area-message :override #'ignore)
  (blink-cursor-mode 0)
  (show-paren-mode 1)
  ;; Coding systems
  (set-default-coding-systems 'utf-8)
  (prefer-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-language-environment "English")
  ;; UI cleanup
  (when (fboundp 'scroll-bar-mode) (set-scroll-bar-mode nil))
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (if (display-graphic-p) (menu-bar-mode 1) (menu-bar-mode -1))
  ;; Fringes
  (setq-default fringe-indicator-alist
                (delq (assq 'continuation fringe-indicator-alist)
                      fringe-indicator-alist))
  ;; Scrolling
  (general-def
    "C-M-n" 'scroll-up-line
    "C-M-p" 'scroll-down-line
    "<home>" 'scroll-up-line
    "<end>" 'scroll-down-line
    "s-<down>" 'scroll-up-line
    "s-<up>" 'scroll-down-line))

;;;; Built-in Packages

(use-package recentf
  :ensure nil
  :demand t
  :init
  (defvar recentf-mode nil)
  (require 'recentf)
  :custom
  (recentf-auto-cleanup (if (daemonp) 300 'never))
  (recentf-max-saved-items 200)
  (recentf-exclude
   '("\\.tar$" "\\.tbz2$" "\\.tbz$" "\\.tgz$" "\\.bz2$"
     "\\.bz$" "\\.gz$" "\\.gzip$" "\\.xz$" "\\.zip$"
     "\\.7z$" "\\.rar$" "COMMIT_EDITMSG\\'"
     "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
     "-autoloads\\.el$" "autoload\\.el$"))
  :config
  (recentf-mode 1)
  (add-hook 'kill-emacs-hook #'recentf-cleanup -90))

(use-package savehist
  :ensure nil
  :demand t
  :init
  (defvar savehist-mode nil)
  (require 'savehist)
  :custom
  (savehist-autosave-interval 600)
  (savehist-additional-variables
   '(kill-ring register-alist mark-ring global-mark-ring
     search-ring regexp-search-ring extended-command-history))
  :config
  (savehist-mode 1))

(use-package saveplace
  :ensure nil
  :demand t
  :init
  (defvar save-place-mode nil)
  (require 'saveplace)
  :custom
  (save-place-limit 400)
  :config
  (save-place-mode 1))

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator "•")
  (uniquify-after-kill-buffer-p t))

(use-package autorevert
  :ensure nil
  :init (global-auto-revert-mode 1)
  :custom
  (global-auto-revert-non-file-buffers nil)
  (auto-revert-interval 3)
  (auto-revert-verbose t))

(use-package eldoc
  :ensure nil
  :diminish)

(emacs-section-end)

;;; ============================================================================
;;; Appearance
;;; ============================================================================

(emacs-section-start "Appearance")

;;;; Fonts

(use-package faces
  :ensure nil
  :if (display-graphic-p)
  :config
  (set-face-attribute 'default nil
                      :font "Monaspace Neon Frozen"
                      :height 110
                      :weight 'thin)
  (set-face-attribute 'fixed-pitch nil
                      :font "Monaspace Neon Frozen"
                      :height 110
                      :weight 'thin)
  (set-face-attribute 'variable-pitch nil
                      :font "Monaspace Radon Frozen"
                      :height 110
                      :weight 'thin)
  ;; Nerd Font symbols support
  (set-fontset-font t '(#xe000 . #xffdd)
                    (font-spec :name "Symbols Nerd Font Mono"
                               :size 11) nil))

;;;; Icons

(use-package nerd-icons
  :demand t
  :config
  ;; Force-require to ensure advice from top of init.el is installed
  ;; before any other package can autoload nerd-icons
  (require 'nerd-icons))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

;; Disabled - hyalo-ibuffer provides its own monochromatic icons
;; (use-package nerd-icons-ibuffer
;;   :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

;; True monochromatic icons - strips foreground from nerd-icons
;; Icons inherit color from context, allowing packages to control appearance
(use-package hyalo-icons
  :load-path emacs-config-dir
  :after nerd-icons
  :custom
  (hyalo-icons-monochrome t))

;;;; Highlighting

(use-package hl-line
  :ensure nil
  :demand t
  :config
  (global-hl-line-mode 1))

;;;; Themes

(use-package modus-themes
  :demand t
  :custom
  (modus-themes-mixed-fonts t)
  (modus-themes-variable-pitch-ui t)
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs nil)
  (modus-themes-completions '((t . (semibold))))
  (modus-themes-prompts '(semibold))
  :config
  (defun hyalo-switch-to-dark (&rest _)
    (when (fboundp 'hyalo-module-appearance-set)
      (hyalo-module-appearance-set 'dark)
      (hyalo-module-footer-set-pattern-alpha 0.02)))
  (advice-add 'modus-themes-load-random-dark :after #'hyalo-switch-to-dark)

  (defun hyalo-switch-to-light (&rest _)
    (when (fboundp 'hyalo-module-appearance-set)
      (hyalo-module-appearance-set 'light)
      (hyalo-module-footer-set-pattern-alpha 0.06)))
  (advice-add 'modus-themes-load-random-light :after #'hyalo-switch-to-light))

(use-package ef-themes
  :demand t
  :after modus-themes
  :config
  (modus-themes-include-derivatives-mode 1))

;;;; Mixed Pitch

(use-package mixed-pitch
  :vc (:url "https://gitlab.com/jabranham/mixed-pitch" :rev :newest)
  :hook ((text-mode Info-mode) . (lambda ()
                                   (when (display-graphic-p)
                                     (mixed-pitch-mode 1)))))

;;;; Page Breaks

(use-package page-break-lines
  :disabled t
  :diminish
  :config
  (add-to-list 'page-break-lines-modes 'special-mode)
  (global-page-break-lines-mode))

;;;; Minions (Minor Mode Menu)

(use-package minions
  :config
  (minions-mode 1))

;;;; Dimmer

(use-package iota-dimmer
  :load-path emacs-config-dir
  :custom
  (iota-dimmer-saturation-fraction 0.90)
  (iota-dimmer-luminance-fraction 0.30)
  :config
  (iota-dimmer-mode 1))

(emacs-section-end)

;;; ============================================================================
;;; Modeline
;;; ============================================================================

(emacs-section-start "Modeline")

(defvar rcirc-track-minor-mode nil) ;; Fix doom-modeline error

(use-package keycast
  :ensure t
  :config
  ;; Redefine keycast-mode to work with doom-modeline
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line."
    :global t
    (if keycast-mode
        (progn
          (add-hook 'pre-command-hook 'keycast--update t)
          (add-to-list 'global-mode-string '("" keycast-mode-line " ")))
      (progn
        (remove-hook 'pre-command-hook 'keycast--update)
        (setq global-mode-string (delete '("" keycast-mode-line " ") global-mode-string))))))

(use-package doom-modeline
  :ensure t
  :after nerd-icons
  :config
  (doom-modeline-mode 1)
  :custom
  ;; Height and bar
  (doom-modeline-height 22)
  (doom-modeline-bar-width 4)
  (doom-modeline-hud t)

  ;; Enable nerd-icons for rich visual feedback
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-time-icon t)
  (doom-modeline-buffer-encoding-icon t)

  ;; Modal editing state (god-mode, evil, etc.)
  (doom-modeline-modal t)
  (doom-modeline-modal-icon t)
  (doom-modeline-modal-modern-icon t)

  ;; Buffer display
  (doom-modeline-buffer-file-name-style 'file-name-with-project)

  ;; Project integration (uses built-in project.el)
  (doom-modeline-project-detection 'project)

  ;; VCS/Git settings (integrates with magit)
  (doom-modeline-vcs-max-length 24)
  (doom-modeline-check-simple-format t)

  ;; Environment display
  (doom-modeline-env-version nil)  ; Don't show Python/Node versions
  (doom-modeline-buffer-encoding nil)  ; Usually utf-8, no need to show
  (doom-modeline-indent-info nil)

  ;; Minor modes
  (doom-modeline-minor-modes nil)  ; Keep modeline clean

  ;; Markdown support
  (doom-modeline-enable-word-count nil)
  (doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))

  ;; Unicode fallback for any missing glyphs
  (doom-modeline-unicode-fallback t)

  ;; No IRC
  (doom-modeline-irc nil)

  ;; Performance
  (doom-modeline-checker-simple-format t))

(emacs-section-end)

;;; ============================================================================
;;; Completion
;;; ============================================================================

(emacs-section-start "Completion")

(use-package vertico
  :init (vertico-mode)
  :custom
  (vertico-count 8))
;;(vertico-resize nil))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :hook (after-init . marginalia-mode))

(use-package consult
  :general
  ("C-c M-x" 'consult-mode-command)
  ("C-c h"   'consult-history)
  ("C-c k"   'consult-kmacro)
  ("C-c m"   'consult-man)
  ("C-c i"   'consult-info)
  ("C-x M-:" 'consult-complex-command)
  ("C-x C-r" 'consult-recent-file)
  ("C-x b"   'consult-buffer)
  ("C-x h"   'consult-outline)
  ("C-x 4 b" 'consult-buffer-other-window)
  ("C-x 5 b" 'consult-buffer-other-frame)
  ("C-x t b" 'consult-buffer-other-tab)
  ("C-x r b" 'consult-bookmark)
  ("C-x p b" 'consult-project-buffer)
  ([remap Info-search] 'consult-info)
  :custom
  (consult-preview-key nil))

(emacs-section-end)

;;; ============================================================================
;;; Editing
;;; ============================================================================

(emacs-section-start "Editing")

(use-package display-line-numbers
  :ensure nil
  :diminish display-line-numbers-mode
;;:hook (prog-mode . display-line-numbers-mode)
  :custom
  (display-line-numbers-width 2)
  (display-line-numbers-widen t)
  (display-line-numbers-grow-only t))
;;(display-line-numbers-type 'relative))

(use-package move-dup
  :general
  ("M-<up>"   'move-dup-move-lines-up)
  ("M-<down>" 'move-dup-move-lines-down))

(use-package windmove
  :ensure nil
  :general
  (:prefix "C-x"
   "<left>"  'windmove-left
   "<right>" 'windmove-right
   "<up>"    'windmove-up
   "<down>"  'windmove-down))

(use-package stripspace
  :diminish stripspace-local-mode
  :hook ((prog-mode text-mode conf-mode) . stripspace-local-mode)
  :custom
  (stripspace-only-if-initially-clean nil)
  (stripspace-restore-column t))

(use-package outline
  :ensure nil
  :diminish outline-minor-mode
  :hook (prog-mode . outline-minor-mode)
  :config
  (add-hook 'outline-minor-mode-hook
            (lambda ()
              (require 'nerd-icons)
              (let* ((display-table (or buffer-display-table (make-display-table)))
                     (face-offset (* (face-id 'shadow) (ash 1 22)))
                     (chevron (concat " " (nerd-icons-codicon "nf-cod-chevron_down")))
                     (value (vconcat (mapcar (lambda (c) (+ face-offset c)) chevron))))
                (set-display-table-slot display-table 'selective-display value)
                (setq buffer-display-table display-table)))))

(use-package outline-indent
  :after nerd-icons
  :custom
  (outline-indent-ellipsis (concat " " (nerd-icons-codicon "nf-cod-chevron_down")))
  :hook ((yaml-mode markdown-mode) . outline-indent-minor-mode))

(use-package ultra-scroll
  :vc (:url "https://github.com/jdtsmith/ultra-scroll")
  :custom
  (scroll-conservatively 3)
  (scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

(emacs-section-end)

;;; ============================================================================
;;; macOS / Hyalo
;;; ============================================================================

(emacs-section-start "macOS / Hyalo")

;;;; macOS Settings

(use-package ns-win
  :ensure nil
  :if (and (eq system-type 'darwin) (display-graphic-p))
  :config
  (setq frame-resize-pixelwise t)
  ;; Prevent frame resize when face/font heights change (e.g., during theme load)
  ;; This helps avoid unwanted window resize in NavigationSplitView
  (setq frame-inhibit-implied-resize t)
  (set-frame-parameter nil 'internal-border-width 0)
  ;; Enable transparent titlebar for NavigationSplitView integration
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (set-frame-parameter nil 'ns-transparent-titlebar t)
  (general-unbind "C-z" "C-x C-z"))

;;;; Hyalo Module

(use-package hyalo-module
  :if (eq window-system 'ns)
  :load-path emacs-config-dir
  :custom
  (hyalo-module-auto-build t)
  :config
  (emacs-log-info "Loading hyalo-module")
  (hyalo-module-load))

(use-package hyalo-module-header
  :if (eq window-system 'ns)
  :after hyalo-module
  :load-path emacs-config-dir
;;:diminish " ηHeader"
  :config
  (hyalo-module-header-mode 1))

(use-package hyalo-module-appearance
  :if (eq window-system 'ns)
  :after hyalo-module
  :load-path emacs-config-dir
  :demand t
;;:diminish " ηAppearance"
  :preface
  ;; Trying:
  ;; * 'secondary-selection - The color used for the "other" selection (often yellow/orange
  ;;    tinted).
  ;; * 'match - Used for matching parentheses or search terms.
  ;; * 'isearch - Used for the current search match (usually very high contrast).
  ;; * 'highlight - Used for mouse rollovers or generic highlighting.
  ;;
  ;; * `ultra-light` (or extra-light)
  ;; * `thin` (or hairline) — This is your current default.
  ;; * `light`
  ;; * `semi-light` (or demi-light)
  ;; * `normal` (or regular, book)
  ;; * `medium`
  ;; * `semi-bold` (or demi-bold)
  ;; * `bold`
  ;; * `extra-bold`
  ;; * `ultra-bold` (or heavy, black)
  (defun hyalo-set-highlights (&rest _)
    (let ((w 'bold)
          (wb 'ultra-bold))
      ;; Only enforce weight for standard highlights to preserve theme colors
      (set-face-attribute 'region nil :weight w)
      (set-face-attribute 'isearch nil :weight w)
      (set-face-attribute 'lazy-highlight nil :weight w)
      (set-face-attribute 'match nil :weight w)
      (set-face-attribute 'show-paren-match nil :weight w)
      (when (facep 'hl-line)
        (set-face-attribute 'hl-line nil :weight w))
      (when (facep 'highlight)
        (set-face-attribute 'highlight nil :weight w))
      (when (facep 'vertico-current)
        (set-face-attribute 'vertico-current nil :weight w))
      (dolist (face '(orderless-match-face-0
                      orderless-match-face-1
                      orderless-match-face-2
                      orderless-match-face-3
                      consult-highlight-match
                      consult-preview-match
                      consult-file
                      consult-bookmark))
        (when (facep face)
          (set-face-attribute face nil :weight w)))
      (when (facep 'consult-buffer)
        (set-face-attribute 'consult-buffer nil :weight wb))
      (dolist (face '(magit-item-highlight
                      magit-section-highlight
                      magit-diff-added-highlight
                      magit-diff-removed-highlight
                      magit-diff-context-highlight
                      magit-diff-file-heading-highlight
                      magit-diff-hunk-heading-highlight
                      magit-diff-base-highlight
                      magit-diff-our-highlight
                      magit-diff-their-highlight
                      magit-diff-added-selection
                      magit-diff-removed-selection
                      magit-diff-context-selection
                      magit-diff-file-heading-selection
                      magit-diff-hunk-heading-selection))
        (when (facep face)
          (set-face-attribute face nil :weight wb)))))

  (defvar hyalo-profiles
    '((focus . (:appearance light :theme modus-operandi :vibrancy "none"      :opacity 1.0))
      (glass . (:appearance dark  :theme modus-vivendi  :vibrancy "ultraThin" :opacity 0.85))
      (void  . (:appearance dark  :theme ef-winter      :vibrancy "thick"     :opacity 0.80)))
    "Alist of Hyalo appearance profiles.")

  (defun hyalo-load-profile (name)
    "Load the profile NAME and persist settings to custom.el."
    (let ((profile (alist-get name hyalo-profiles)))
      (when profile
        (let ((appearance (plist-get profile :appearance))
              (theme (plist-get profile :theme))
              (vibrancy (plist-get profile :vibrancy))
              (opacity (plist-get profile :opacity)))
          ;; Apply appearance mode with persistence (saves by default)
          (when appearance
            (hyalo-module-appearance-set appearance))
          ;; Load and save theme
          (when theme
            (mapc #'disable-theme custom-enabled-themes)
            (load-theme theme t)
            (hyalo-set-highlights)
            ;; Persist theme as current-theme
            (customize-save-variable 'hyalo-module-appearance-current-theme theme))
          ;; Apply vibrancy with persistence (set-vibrancy already saves)
          (when vibrancy
            (hyalo-module-appearance-set-vibrancy vibrancy))
          ;; Apply opacity with persistence (set-opacity already saves)
          (when opacity
            (hyalo-module-appearance-set-opacity opacity))
          (message "Loaded and saved profile: %s" name)))))

  (defun hyalo-switch-profile ()
    "Switch to a defined profile."
    (interactive)
    (let* ((choices (mapcar (lambda (x) (symbol-name (car x))) hyalo-profiles))
           (selection (completing-read "Select Profile: " choices nil t)))
      (hyalo-load-profile (intern selection))))

  :init
  ;; Set default themes only if not already customized
  ;; This prevents overwriting saved values in custom.el
  (unless (get 'hyalo-module-appearance-theme-light 'saved-value)
    (setq hyalo-module-appearance-theme-light 'modus-operandi))
  (unless (get 'hyalo-module-appearance-theme-dark 'saved-value)
    (setq hyalo-module-appearance-theme-dark 'modus-vivendi))
  :general
  (leader-def
    "l v" '(hyalo-module-appearance-set-vibrancy :wk "vibrancy")
    "l o" '(hyalo-module-appearance-set-opacity :wk "opacity")
    "l p" '((lambda () (interactive)
              (call-interactively 'hyalo-module-appearance-set)
              (customize-save-variable 'hyalo-module-appearance-mode-setting
                                       hyalo-module-appearance-mode-setting)
              (message "Appearance mode saved: %s" hyalo-module-appearance-mode-setting))
            :wk "appearance mode")
    "l P" '(hyalo-module-appearance-show-panel :wk "panel")
    "l ." '(hyalo-switch-profile :wk "profile"))
  :config
  (hyalo-module-appearance-mode 1)

  ;; Hook the highlight face update
  (add-hook 'enable-theme-functions #'hyalo-set-highlights)
  (hyalo-set-highlights)

  ;; Ensure highlights are applied to Magit faces if Magit loads later
  (with-eval-after-load 'magit
    (hyalo-set-highlights))

  ;; Add Hyalo Appearance menu
  (easy-menu-define hyalo-appearance-menu global-map
    "Hyalo Appearance Menu"
    '("Hyalo"
      ["Appearance Panel..." hyalo-module-appearance-show-panel
       :help "Open appearance settings panel"]
      "---"
      ["Set Vibrancy..." hyalo-module-appearance-set-vibrancy
       :help "Change vibrancy/blur material"]
      ["Set Opacity..." hyalo-module-appearance-set-opacity
       :help "Change tint opacity level"]
      "---"
      ("Profiles"
       ["Focus" (hyalo-load-profile 'focus) :help "Load Focus profile"]
       ["Glass" (hyalo-load-profile 'glass) :help "Load Glass profile"]
       ["Void"  (hyalo-load-profile 'void)  :help "Load Void profile"]
       ["Switch..." hyalo-switch-profile :help "Select a profile"])
      "---"
      ("Presets"
       ["Clear" (progn
                  (hyalo-module-appearance-set-vibrancy "ultraThin")
                  (hyalo-module-appearance-set-opacity 0.1))
        :help "Maximum see-through effect"]
       ["Balanced" (progn
                     (hyalo-module-appearance-set-vibrancy "thick")
                     (hyalo-module-appearance-set-opacity 0.5))
        :help "Balanced blur and opacity"]
       ["Solid" (progn
                  (hyalo-module-appearance-set-vibrancy "none")
                  (hyalo-module-appearance-set-opacity 0.9))
        :help "Minimal transparency"])
      "---"
      ("Appearance Mode"
       ["Auto (Follow System)" (hyalo-module-appearance-set 'auto)
        :style radio
        :selected (eq hyalo-module-appearance-mode-setting 'auto)]
       ["Light" (hyalo-module-appearance-set 'light)
        :style radio
        :selected (eq hyalo-module-appearance-mode-setting 'light)]
       ["Dark" (hyalo-module-appearance-set 'dark)
        :style radio
        :selected (eq hyalo-module-appearance-mode-setting 'dark)])))

  ;; Add menu to menu bar after Tools
  (easy-menu-add-item global-map '(menu-bar) hyalo-appearance-menu "tools")

  ;; Apply vibrancy settings on startup
  (add-hook 'hyalo-module-appearance-mode-hook
            #'hyalo-module-appearance--apply-vibrancy))

(use-package hyalo-module-viewport
  :if (eq window-system 'ns)
  :after hyalo-module-header
  :load-path emacs-config-dir
;;:diminish " ηViewport"
  :custom
  (hyalo-module-viewport-debug nil)
  (hyalo-module-viewport-excluded-modes '(agent-shell-mode
                                          agent-shell-viewport-view-mode
                                          agent-shell-viewport-edit-mode))
  :config
  (hyalo-module-viewport-mode 1))

(use-package hyalo-module-system
  :if (eq window-system 'ns)
  :after hyalo-module
  :load-path emacs-config-dir
  :general
  (leader-def
    "l r" '(hyalo-module-reveal-in-finder :wk "reveal")
    "l s" '(hyalo-module-share :wk "share")
    "l e" '(hyalo-module-show-emoji-picker :wk "emoji")))

(use-package hyalo-module-footer
  :if (eq window-system 'ns)
  :after hyalo-module
  :load-path emacs-config-dir
  :config
  (defun hyalo-module-footer-random-pattern ()
    "Set a random footer pattern."
	(interactive)
	(let* ((patterns '("hideout" "hexagons" "death-star" "bathroom-floor"
					   "tiny-checkers" "plus" "cage" "diagonal-stripes"
					   "stripes" "diagonal-lines" "polka-dots" "signal"
					   "wallpaper"))
		   (choice (nth (random (length patterns)) patterns)))
	  (hyalo-module-footer-set-pattern choice)
	  (message "Footer pattern set to: %s" choice)))

  (hyalo-module-footer-mode 1)
  (hyalo-module-footer-set-pattern "hexagons")
  (hyalo-module-footer-set-background-alpha 0.1))

(use-package hyalo-module-minibuffer
  :disabled t
  :if (eq window-system 'ns)
  :after hyalo-module
  :load-path emacs-config-dir
  :config
  (hyalo-module-minibuffer-mode 1)

  (use-package mini-frame
    :custom
    ;; Prevent resize-related freezes (see vertico#446, debbugs#69140)
    (mini-frame-resize nil)
    (mini-frame-resize-min-height 20)
    (mini-frame-show-parameters
     '(;; Position is handled by Swift (hyalo-minibuffer-enable)
       (width . 0.7)
       (height . 20)  ; Fixed height in lines
       (left . 0.5)
       (top . 100)    ; Initial top, Swift will reposition
       ;; Fully transparent - Swift provides glass + content container
       (background-color . "white")
       (alpha-background . 0)
       (ns-alpha-elements . (ns-alpha-all))
       ;; No internal border - Swift handles margins
       (internal-border-width . 0)
       (left-fringe . 0)
       (right-fringe . 0)))
    :config
    (mini-frame-mode 1)))

;;;; Hyalo Debug

(use-package hyalo-debug
  :ensure nil
  :if (eq window-system 'ns)
  :after hyalo-module
  :preface
  (defun hyalo-module-debug-status ()
    "Show hyalo-module status for debugging."
    (interactive)
    (message "hyalo-module status:
  - Module loaded: %s
  - Header mode: %s
  - Appearance mode: %s
  - Traffic lights mode: %s
  - Viewport mode: %s
  - Current appearance: %s
  - Opacity: %.2f
  - Vibrancy: %s
  - Header height: %s
  - Window top: %s
  - Echo area height: %s"
             (bound-and-true-p hyalo-module--loaded)
             (bound-and-true-p hyalo-module-header-mode)
             (bound-and-true-p hyalo-module-appearance-mode)
             (bound-and-true-p hyalo-module-traffic-lights-mode)
             (bound-and-true-p hyalo-module-viewport-mode)
             (if (fboundp 'hyalo-module-appearance-current)
                 (hyalo-module-appearance-current) "N/A")
             (if (boundp 'hyalo-module-appearance-opacity)
                 hyalo-module-appearance-opacity 0)
             (if (boundp 'hyalo-module-appearance-vibrancy-material)
                 hyalo-module-appearance-vibrancy-material "N/A")
             (if (fboundp 'hyalo-header-height)
                 (hyalo-header-height) "N/A")
             (nth 1 (window-pixel-edges))
             (window-pixel-height (minibuffer-window))))
  :general
  (leader-def
    "l d" '(hyalo-module-debug-status :wk "debug")))

(emacs-section-end)

;;; ============================================================================
;;; Tools
;;; ============================================================================

(emacs-section-start "Tools")

(use-package project
  :ensure nil
  :general
  (leader-def
    "p f" '(project-find-file :wk "find file")
    "p b" '(project-switch-to-buffer :wk "switch buffer")
    "p d" '(project-dired :wk "dired")
    "p k" '(project-kill-buffers :wk "kill buffers")
    "p p" '(project-switch-project :wk "switch project")
    "p s" '(project-search :wk "search")
    "p c" '(project-compile :wk "compile")
    "p v" '(magit-project-status :wk "magit"))
  :custom
  (project-switch-commands
   '((project-find-file "Find file")
     (project-find-regexp "Find regexp")
     (project-dired "Dired")
     (magit-project-status "Magit" ?v)
     (project-eshell "Eshell"))))

(use-package magit
  :defer t
  :general
  (leader-def
    "v s" '(magit-status :wk "status")
    "v l" '(magit-log :wk "log")
    "v b" '(magit-blame :wk "blame")
    "v d" '(magit-diff :wk "diff"))
  :custom
  (magit-display-buffer-function
   (lambda (buffer) (display-buffer buffer '(display-buffer-same-window))))
  :config
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t))

(use-package swift-mode
  :mode "\\.swift\\'")

;;;; Dired-Sidebar & File Browser

(use-package nerd-icons-dired
  :commands (nerd-icons-dired-mode))

(use-package dired-sidebar
  :defer t
  :commands (dired-sidebar-toggle-sidebar)
  :custom
  (dired-sidebar-theme 'nerd-icons)
  (dired-sidebar-width 35)
  (dired-sidebar-should-follow-file t)
  :config
  ;; Enable nerd-icons in dired-sidebar
  (add-hook 'dired-sidebar-mode-hook #'nerd-icons-dired-mode))

(use-package hyalo-dired-sidebar
  :load-path emacs-config-dir
  :after dired-sidebar)

(use-package hyalo-module-sidebar
  :load-path emacs-config-dir
  :after hyalo-module
  :demand t
  :custom
  ;; Font for sidebar buffers (nil = use default frame font)
  ;; Example: "SF Pro-12" or "SF Mono-11"
  (hyalo-sidebar-font nil)
  ;; Internal border width for embedded frames (12 = standard SwiftUI margin)
  (hyalo-sidebar-internal-border-width 12)
  :general
  (leader-def
    "t e" '(hyalo-sidebar-toggle-left :wk "sidebar (left)")
    "t i" '(hyalo-sidebar-toggle-right :wk "inspector (right)")
    "t E" '(hyalo-sidebar-focus-left :wk "focus sidebar")
    "t I" '(hyalo-sidebar-focus-right :wk "focus inspector")))

(emacs-section-end)

;;; ============================================================================
;;; Agents
;;; ============================================================================

(emacs-section-start "Agents")

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main")
  :custom
  (copilot-idle-delay 0.2)
  (copilot-indent-offset-warning-disable t)
  (copilot-install-dir (locate-user-emacs-file "copilot/"))
  :hook ((prog-mode text-mode) . copilot-mode)
  :config
  (diminish 'copilot-mode (concat " " (nerd-icons-codicon "nf-cod-copilot")))

  ;; Auto-install copilot server if not present
  (let ((server-bin (expand-file-name "bin" copilot-install-dir)))
    (unless (file-directory-p server-bin)
      (when (yes-or-no-p "Copilot language server not installed. Install now? ")
        (copilot-install-server))))

  (general-def copilot-completion-map
    "<tab>" 'copilot-accept-completion
    "TAB"   'copilot-accept-completion)

  ;; Suppress warnings
  (defun emacs/copilot-suppress-warnings (orig-fun &rest args)
    (let ((inhibit-message t))
      (apply orig-fun args)))

  (when (fboundp 'copilot--start-agent)
    (advice-add 'copilot--start-agent :around #'emacs/copilot-suppress-warnings))
  (when (fboundp 'copilot--start-server)
    (advice-add 'copilot--start-server :around #'emacs/copilot-suppress-warnings))

  ;; Redirect stderr
  (defun emacs/copilot-redirect-stderr (orig-fun &rest args)
    (let ((stderr-file (locate-user-emacs-file "copilot-stderr")))
      (if (string= (plist-get args :name) "copilot server")
          (apply orig-fun (plist-put args :stderr stderr-file))
        (apply orig-fun args))))

  (advice-add 'make-process :around #'emacs/copilot-redirect-stderr))

(use-package agent-shell
  :vc (:url "https://github.com/xenodium/agent-shell"
            :rev :newest)
  :custom
  (agent-shell-section-functions nil)
  :hook (agent-shell-mode . (lambda ()
                              (face-remap-add-relative 'default
                                                       :family "Monaspace Krypton Frozen"
                                                       :height 110)))
  :general
  (leader-def
    "a" '(:ignore t :wk "agents")
    "a c" '(agent-shell-anthropic-start-claude-code :wk "claude")
    "a g" '(agent-shell-google-start-gemini :wk "gemini")
    "a s" '(agent-shell-sidebar-toggle :wk "sidebar toggle")
    "a S" '(agent-shell-send-screenshot :wk "screenshot")
    "a q" '(agent-shell-queue-request :wk "queue request")
    "a f" '(agent-shell-sidebar-toggle-focus :wk "sidebar focus"))
  (:keymaps 'agent-shell-mode-map
   "C-p" 'agent-shell-previous-input
   "C-n" 'agent-shell-next-input))

(use-package hyalo-agent-extras
  :load-path emacs-config-dir
  :after agent-shell
  :config
  (hyalo-agent-extras-mode 1))

(use-package agent-shell-sidebar
  :vc (:url "https://github.com/cmacrae/agent-shell-sidebar"
            :rev :newest)
  :after agent-shell)

(use-package agent-shell-manager
  :vc (:url "https://github.com/jethrokuan/agent-shell-manager"
            :rev :newest)
  :after agent-shell
  :general
  (leader-def
    "a m" '(agent-shell-manager-toggle :wk "manager")))

;; (use-package mcp-server
;;   :vc (:url "https://github.com/rhblind/emacs-mcp-server"
;;             :rev :newest)
;;   :config
;;   (add-hook 'emacs-startup-hook #'mcp-server-start-unix))

(emacs-section-end)

;;; ===========================================================================
;;; Terminal Emulation
;;; ===========================================================================

(emacs-section-start "Terminal Emulation")

(use-package eat
  :ensure t
  :general
  (:prefix "C-c s"
   "" '(:ignore t :which-key "shell")
   "s" 'eat
   "p" 'eat-project
   "e" 'eat-eshell-mode
   "v" 'eat-eshell-visual-command-mode)
  :custom
  ;; For `eat-eshell-mode'
  (eat-enable-directory-tracking t)
  (eat-enable-shell-prompt-annotation t)
  ;; Set the correct terminal type
  (eat-term-name "xterm-256color")
  ;; Kill the terminal process when the buffer is killed
  (eat-kill-process-on-exit t)
  :config
  ;; Disable god-mode in eat buffers to prevent keystroke duplication
  (add-hook 'eat-mode-hook
            (lambda ()
              (when (fboundp 'god-local-mode)
                (god-local-mode -1))))

  ;; Fix backspace in semi-char-mode while keeping Emacs keybindings (C-x, M-x, etc.)
  ;; Semi-char mode should be the default - it reserves C-x, C-c, M-x for Emacs
  (with-eval-after-load 'eat
    ;; Make backspace work in semi-char-mode by binding it explicitly
    (when (boundp 'eat-semi-char-mode-map)
      (define-key eat-semi-char-mode-map (kbd "DEL") 'eat-self-input)
      (define-key eat-semi-char-mode-map (kbd "<backspace>") 'eat-self-input)
      (define-key eat-semi-char-mode-map (kbd "C-?") 'eat-self-input)

      ;; Mode toggle keybindings
      (define-key eat-semi-char-mode-map (kbd "C-c C-k") 'eat-char-mode))

    (when (boundp 'eat-mode-map)
      (define-key eat-mode-map (kbd "C-c C-j") 'eat-semi-char-mode)))

  ;; Enable eat integration with eshell
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode))

(emacs-section-end)

;;; ============================================================================
;;; Knowledge Management
;;; ============================================================================

(emacs-section-start "Knowledge Management")

(use-package markdown-mode
  :mode (("README\\.md\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :general
  (:keymaps 'markdown-mode-map
   :prefix "C-c m"
   "" '(:ignore t :which-key "markdown")
   "l" 'markdown-insert-link
   "u" 'markdown-insert-uri
   "f" 'markdown-insert-footnote
   "w" 'markdown-insert-wiki-link
   "c" 'markdown-insert-code
   "C" 'markdown-insert-gfm-code-block
   "p" 'markdown-insert-pre
   "t" 'markdown-insert-table
   "h" 'markdown-insert-header-dwim
   "b" 'markdown-insert-bold
   "i" 'markdown-insert-italic
   "s" 'markdown-insert-strike-through
   "q" 'markdown-insert-blockquote))

(use-package obsidian
  :ensure t
  :defer t
  :commands (obsidian-capture obsidian-jump obsidian-daily-note obsidian-search)
  :init
  (let ((path (expand-file-name "~/Library/Mobile Documents/iCloud~md~obsidian/Documents/Vault")))
    (setq obsidian-directory path)
    (setq obsidian--relative-path-length (length (file-name-as-directory path))))

  ;; Inhibit the "Setting obsidian-directory to..." message
  (with-eval-after-load 'obsidian
    (let ((setter (get 'obsidian-directory 'custom-set)))
      (when setter
        (put 'obsidian-directory 'custom-set
             (lambda (symbol value)
               (let ((inhibit-message t))
                 (funcall setter symbol value)))))))

  :config
  (global-obsidian-mode t)
  ;; Initialize vault cache after loading if not already done
  (unless (and (boundp 'obsidian-vault-cache) obsidian-vault-cache)
    (when (file-directory-p (expand-file-name obsidian-directory))
      (obsidian-update)))
  :custom
  ;; Default location for new notes from `obsidian-capture'
  (obsidian-inbox-directory "Inbox")
  ;; (obsidian-daily-notes-directory "Journal")
  ;; (obsidian-templates-directory "Templates")
  (obsidian-wiki-link-create-file-in-inbox t)
  ;; Useful if you're going to be using wiki links
  (markdown-enable-wiki-links t)
  :bind
  (:map obsidian-mode-map
   ("C-c C-o" . obsidian-follow-link-at-point))
  :general
  (:prefix "C-c n"
   "" '(:ignore t :which-key "notes")
   "n" 'obsidian-capture
   "j" 'obsidian-jump
   "d" 'obsidian-daily-note
   "t" 'obsidian-tag-find
   "l" 'obsidian-insert-link
   "w" 'obsidian-insert-wikilink
   "b" 'obsidian-backlink-jump
   "f" 'obsidian-follow-link-at-point
   "s" 'obsidian-search
   "m" 'obsidian-move-file
   "g" 'obsidian-grep
   "v" 'obsidian-jump))

(emacs-section-end)

;;; ============================================================================
;;; Finalize
;;; ============================================================================

(emacs-log-info (format "Init complete (%.3fs)"
                        (float-time (time-subtract (current-time)
                                                   emacs-init-start-time))))

;;; init.el ends here
