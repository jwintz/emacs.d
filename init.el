;;; init.el -*- lexical-binding: t; no-byte-compile: t -*-

;; Minimal bootstrap - loads semantic modules from modules/

;;; ============================================================================
;;; Bootstrap
;;; ============================================================================

(setq debug-on-error t)

;; Suppress warnings during startup
(setq byte-compile-warnings nil)
(setq warning-suppress-types '((comp) (bytecomp)))
(setq warning-minimum-level :error)

;;;; Directories

(defvar emacs-config-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "The root directory of Emacs configuration.")

(setq user-emacs-directory (expand-file-name ".local/" emacs-config-dir))

(dolist (subdir '("" "auto-save" "auto-save-list" "transient" "eshell" "etc"))
  (make-directory (expand-file-name subdir user-emacs-directory) t))

;; Generated files
(setq auto-save-list-file-prefix (locate-user-emacs-file "auto-save-list/.saves-"))
(setq auto-save-file-name-transforms `((".*" ,(locate-user-emacs-file "auto-save/") t)))
(setq custom-file (locate-user-emacs-file "etc/custom.el"))
(setq package-user-dir (locate-user-emacs-file "elpa/"))

(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))

;;;; Package System

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;;; Load Paths

(add-to-list 'load-path (expand-file-name "lisp" emacs-config-dir))
(add-to-list 'load-path (expand-file-name "modules" emacs-config-dir))

;;; ============================================================================
;;; Essential Core (before modules)
;;; ============================================================================

;; Protect against mode variable checks during package install
(defvar cua-mode nil)
(defvar display-battery-mode nil)
(defvar display-time-mode nil)
(defvar size-indication-mode nil)
(defvar column-number-mode nil)
(defvar recentf-mode nil)
(defvar global-auto-revert-mode nil)
(defvar save-place-mode nil)

(use-package exec-path-from-shell
  :ensure t
  :demand t
  :custom
  (exec-path-from-shell-check-startup-files nil)
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;;; ============================================================================
;;; Modules
;;; ============================================================================

(require 'init-core)       ;; general, which-key, diminish
(require 'init-emacs)      ;; cursor, startup, recentf, saveplace
(require 'init-help)       ;; helpful, elisp-refs
(require 'init-appearance) ;; fonts, icons, themes
(require 'init-modeline)   ;; doom-modeline, keycast
(require 'init-completion) ;; vertico, consult, marginalia, orderless
(require 'init-editing)    ;; god-mode, windmove, outline
(require 'init-hyalo)      ;; macOS Liquid Glass (ns only)
(require 'init-tools)      ;; project, magit, diff-hl, eglot
(require 'init-dired)      ;; dired, sidebar
(require 'init-agents)     ;; copilot, agent-shell
(require 'init-terminal)   ;; eat
(require 'init-markdown)   ;; markdown-mode, obsidian

;;; ============================================================================
;;; Finalize
;;; ============================================================================

(message "Init complete (%.3fs)"
         (float-time (time-subtract (current-time) before-init-time)))

;;; init.el ends here
