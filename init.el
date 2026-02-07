;;; init.el -*- lexical-binding: t; no-byte-compile: t -*-

;;; ===========================================================================
;;; Profiling
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

(defvar emacs-config-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "The root directory of Emacs configuration.")

(add-to-list 'load-path (expand-file-name "init" emacs-config-dir))

(require 'init-bootstrap)

;;; ===========================================================================
;;; Modules
;;; ===========================================================================

(require 'init-core)

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
(init--require-with-trace 'init-agents)     ;; copilot
(init--require-with-trace 'init-terminal)   ;; eat
(init--require-with-trace 'init-markdown)   ;; markdown-mode, obsidian
(init--require-with-trace 'init-tengwar)    ;; tengwar transliteration
(init--require-with-trace 'init-modes)      ;; major language modes

;;; ===========================================================================
;;; Finalize
;;; ===========================================================================

(let ((duration (float-time (time-subtract (current-time) before-init-time))))
  (when (and (fboundp 'elog-info) (boundp 'emacs-logger))
    (elog-info emacs-logger "[init] Complete (%.3fs)" duration)))

;;; init.el ends here
