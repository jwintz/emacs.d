;;; init.el -*- lexical-binding: t; no-byte-compile: t -*-

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

(dolist (subdir '("" "auto-save-list" "transient" "eshell" "etc"))
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
    "g" '(:ignore t :wk "git")
    "h" '(:ignore t :wk "help")
    "l" '(:ignore t :wk "hyalo")
    "t" '(:ignore t :wk "toggle")))

(use-package which-key
  :diminish
  :custom
  (which-key-idle-delay 0.3)
  (which-key-separator " -> ")
  (which-key-prefix-prefix "")
  :config
  (which-key-mode 1))

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
    (message "[emacs] %s" msg)))

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
  (initial-scratch-message ";;\n\n;; ...\n;; ...\n")
  (initial-buffer-choice t)
  ;; Cursor
  (cursor-in-non-selected-windows nil)
  (cursor-type '(hbar . 2))
  (cursor-intangible-mode t)
  (x-stretch-cursor nil)
  ;; Backups
  (make-backup-files nil)
  ;; Misc
  (use-short-answers t)
  (use-dialog-box nil)
  (frame-title-format nil)
  :config
  (advice-add 'display-startup-echo-area-message :override #'ignore)
  (blink-cursor-mode 0)
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
    "<end>" 'scroll-down-line))

;;;; Built-in Packages

(use-package recentf
  :ensure nil
  :init (recentf-mode 1)
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
  (add-hook 'kill-emacs-hook #'recentf-cleanup -90))

(use-package savehist
  :ensure nil
  :init (savehist-mode 1)
  :custom
  (savehist-autosave-interval 600)
  (savehist-additional-variables
   '(kill-ring register-alist mark-ring global-mark-ring
     search-ring regexp-search-ring extended-command-history)))

(use-package saveplace
  :ensure nil
  :init (save-place-mode 1)
  :custom
  (save-place-limit 400))

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
  :demand t)

;;;; Themes

(use-package modus-themes
  :demand t
  :custom
  (modus-themes-mixed-fonts t)
  (modus-themes-variable-pitch-ui t)
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-completions '((t . (bold))))
  (modus-themes-prompts '(bold)))

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
  :diminish
  :config
  (add-to-list 'page-break-lines-modes 'special-mode)
  (global-page-break-lines-mode))

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
;;; Completion
;;; ============================================================================

(emacs-section-start "Completion")

(use-package vertico
  :init (vertico-mode)
  :custom
  (vertico-count 8)
  (vertico-resize nil))

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
;;:diminish " n"
  :config
  (hyalo-module-header-mode 1))

(use-package hyalo-module-appearance
  :if (eq window-system 'ns)
  :after hyalo-module
  :load-path emacs-config-dir
  :demand t
;;:diminish " n"
  :custom
  (hyalo-module-appearance-theme-light 'modus-operandi)
  (hyalo-module-appearance-theme-dark 'modus-vivendi)
  :general
  (leader-def
    "l a" '(hyalo-module-appearance-set-alpha :wk "alpha")
    "l p" '(hyalo-module-appearance-set :wk "appearance"))
  :config
  (hyalo-module-appearance-mode 1))

(use-package hyalo-module-traffic-lights
  :if (eq window-system 'ns)
  :after hyalo-module
  :load-path emacs-config-dir
  :custom
  (hyalo-module-traffic-lights-auto-hide t)
  :config
  (hyalo-module-traffic-lights-mode 1))

(use-package hyalo-module-viewport
  :if (eq window-system 'ns)
  :after hyalo-module-header
  :load-path emacs-config-dir
;;:diminish " n"
  :custom
  (hyalo-module-viewport-debug nil)
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
  - Alpha (light/dark): %.2f / %.2f
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
             (if (boundp 'hyalo-module-appearance-alpha-light)
                 hyalo-module-appearance-alpha-light 0)
             (if (boundp 'hyalo-module-appearance-alpha-dark)
                 hyalo-module-appearance-alpha-dark 0)
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

(use-package magit
  :defer t
  :general
  (leader-def
    "g s" '(magit-status :wk "status")
    "g l" '(magit-log :wk "log")
    "g b" '(magit-blame :wk "blame")
    "g d" '(magit-diff :wk "diff"))
  :custom
  (magit-display-buffer-function
   (lambda (buffer) (display-buffer buffer '(display-buffer-same-window))))
  :config
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t))

(use-package swift-mode
  :mode "\\.swift\\'")

;;;; Treemacs & Explorer

(use-package treemacs
  :defer t
  :custom
  (treemacs-position 'left)
  (treemacs-width 35)
  (treemacs-display-in-side-window t)
  (treemacs-is-never-other-window t)
  (treemacs-show-hidden-files t)
  (treemacs-follow-after-init t)
  (treemacs-expand-after-init t)
  (treemacs-space-between-root-nodes nil)
  (treemacs-no-png-images t)
  (treemacs-indentation 1)
  (treemacs-indentation-string " ")
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always))

(use-package hyalo-explorer-icons
  :load-path emacs-config-dir
  :after treemacs
  :config
  (hyalo-explorer-icons-config))

(use-package hyalo-explorer
  :load-path emacs-config-dir
  :after (hyalo-module nerd-icons)
  :commands (hyalo-explorer-toggle hyalo-explorer-show hyalo-explorer-hide hyalo-explorer-refresh)
  :general
  (leader-def
    "t e" '(hyalo-explorer-toggle :wk "explorer")
    "t E" '(hyalo-explorer-refresh :wk "refresh explorer")))

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

;; (use-package agent-shell
;;   :vc (:url "...")
;;   :config
;;   ...)

(emacs-section-end)

;;; ============================================================================
;;; Finalize
;;; ============================================================================

(emacs-log-info (format "Init complete (%.3fs)"
                        (float-time (time-subtract (current-time)
                                                   emacs-init-start-time))))

;;; init.el ends here
