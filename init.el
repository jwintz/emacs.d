;;; init.el -*- lexical-binding: t; no-byte-compile: t -*-

;; Minimal bootstrap - loads semantic modules from modules/

;;; ===========================================================================
;;; Performance Profiling
;;; ===========================================================================

;; (require 'profiler)

;; (profiler-start 'cpu)

;; (add-hook 'after-init-hook
;;           (lambda ()
;;             (profiler-stop)
;;             (profiler-report)))

;;; ===========================================================================
;;; Bootstrap
;;; ===========================================================================

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

;;; ===========================================================================
;;; Performance Profiling
;;; ===========================================================================

;; (require 'profiler)

;; (profiler-start 'cpu)
;; (add-hook 'after-init-hook
;;           (lambda ()
;;             (profiler-stop)
;;             (profiler-report)))

;;; ===========================================================================
;;; Package System
;;; ===========================================================================

(defvar demap--tools-demap-defined-start-p t)

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

;;; ===========================================================================
;;; Essential Core (before modules)
;;; ===========================================================================

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
  (exec-path-from-shell-debug t)
  (exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-warn-duration-millis 10000)
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-copy-env "PATH")))

;;; ===========================================================================
;;; Modules
;;; ===========================================================================

(require 'init-core)       ;; general, which-key, diminish

;; Helper to trace module initialization time
(defmacro init--require-with-trace (feature &optional filename)
  `(let ((start (float-time)))
     (require ,feature ,filename)
     (when (and (fboundp 'elog-info) (boundp 'emacs-logger))
       (elog-info emacs-logger "[%s] Loaded (%.3fs)" ,feature (- (float-time) start)))))

(init--require-with-trace 'init-emacs)      ;; cursor, startup, recentf, saveplace
(init--require-with-trace 'init-help)       ;; helpful, elisp-refs
(init--require-with-trace 'init-appearance) ;; fonts, icons, themes
(init--require-with-trace 'init-modeline)   ;; doom-modeline, keycast
(init--require-with-trace 'init-completion) ;; vertico, consult, marginalia, orderless
(init--require-with-trace 'init-editing)    ;; god-mode, windmove, outline
(init--require-with-trace 'init-hyalo)      ;; macOS Liquid Glass (ns only)
(init--require-with-trace 'init-tools)      ;; project, magit, diff-hl, eglot
(init--require-with-trace 'init-dired)      ;; dired, sidebar
(init--require-with-trace 'init-agents)     ;; copilot, agent-shell
(init--require-with-trace 'init-terminal)   ;; eat
(init--require-with-trace 'init-markdown)   ;; markdown-mode, obsidian
(init--require-with-trace 'init-tengwar)    ;; tengwar transliteration

;;; ===========================================================================
;;; Finalize
;;; ===========================================================================

(let ((duration (float-time (time-subtract (current-time) before-init-time))))
  (when (and (fboundp 'elog-info) (boundp 'emacs-logger))
    (elog-info emacs-logger "[init] Complete (%.3fs)" duration)))

;;; init.el ends here
