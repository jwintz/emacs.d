;;; hyalo.el --- Core loader for hyalo dynamic module -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: Julien Wintz <julien.wintz@inria.fr>
;; Version: 1.0.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: frames, faces, macos

;;; Commentary:

;; Core loader for the hyalo Emacs dynamic module.
;; This file is responsible ONLY for:
;; - Loading the Swift dynamic module (.dylib)
;; - Feature detection
;; - Providing a minimal interface to check module availability
;;
;; Feature-specific functionality is in separate files:
;; - hyalo-header.el: Mode-line and header-line display
;; - hyalo-appearance.el: Theme and appearance management
;; - hyalo-traffic-lights.el: Traffic light button control
;; - hyalo-scroll.el: Buffer viewport offset for header zone
;;
;; Usage:
;;   (require 'hyalo)
;;   (when (hyalo-available-p)
;;     (require 'hyalo-header)
;;     ...)

;;; Code:

(require 'cl-lib)

(defgroup hyalo nil
  "Hyalo module for macOS Liquid Glass effects."
  :group 'frames
  :prefix "hyalo-")

(defcustom hyalo-auto-build t
  "If non-nil, automatically build the module when loading if needed.
The module will be built if the dylib doesn't exist or if any Swift
source file is newer than the built dylib."
  :type 'boolean
  :group 'hyalo)

;;; Logging

(defvar hyalo-elog nil
  "Hyalo module logger (nil if elog not loaded).")

(defun hyalo-trace (context msg &rest args)
  "Log trace level MSG with ARGS in CONTEXT."
  (let ((formatted (format "[%s] %s" context msg)))
    (if (and hyalo-elog (fboundp 'elog-trace))
        (apply #'elog-trace hyalo-elog formatted args)
      ;; Trace ignored if elog not available
      nil)))

(defun hyalo-debug (context msg &rest args)
  "Log debug level MSG with ARGS in CONTEXT."
  (let ((formatted (format "[%s] %s" context msg)))
    (if (and hyalo-elog (fboundp 'elog-debug))
        (apply #'elog-debug hyalo-elog formatted args)
      ;; Debug ignored if elog not available
      nil)))

(defun hyalo-info (context msg &rest args)
  "Log info level MSG with ARGS in CONTEXT."
  (let ((formatted (format "[%s] %s" context msg)))
    (if (and hyalo-elog (fboundp 'elog-info))
        (apply #'elog-info hyalo-elog formatted args)
      (let ((out (apply #'format formatted args)))
        (message "[hyalo] %s" out)))))

(defun hyalo-warn (context msg &rest args)
  "Log warning level MSG with ARGS in CONTEXT."
  (let ((formatted (format "[%s] %s" context msg)))
    (if (and hyalo-elog (fboundp 'elog-warn))
        (apply #'elog-warn hyalo-elog formatted args)
      (let ((out (apply #'format formatted args)))
        (message "[hyalo] WARNING: %s" out)))))

(defun hyalo-error (context msg &rest args)
  "Log error level MSG with ARGS in CONTEXT."
  (let ((formatted (format "[%s] %s" context msg)))
    (if (and hyalo-elog (fboundp 'elog-error))
        (apply #'elog-error hyalo-elog formatted args)
      (let ((out (apply #'format formatted args)))
        (message "[hyalo] ERROR: %s" out)))))

(defun hyalo-log (context msg &rest args)
  "Log MSG with ARGS in CONTEXT using elog if available, otherwise `message'.
This is a compatibility wrapper around `hyalo-info'.
It attempts to detect error/warning messages to route them appropriate."
  (cond
   ((or (string-match-p "ERROR" msg) (string-match-p "failed" msg))
    (apply #'hyalo-error context msg args))
   ((string-match-p "WARNING" msg)
    (apply #'hyalo-warn context msg args))
   (t
    (apply #'hyalo-info context msg args))))


(defun hyalo-log-init ()
  "Initialize hyalo logger if elog is available."
  (when (fboundp 'elog-logger)
    (setq hyalo-elog
          (elog-logger
           :name "hyalo"
           :level 'trace
           :buffer "*elog*"
           :handlers '(buffer)))))

;; Initialize logger when elog becomes available
(with-eval-after-load 'elog
  (hyalo-log-init))

;;; Module State

(defvar hyalo--loaded nil
  "Non-nil when the dynamic module has been successfully loaded.")

(defvar hyalo--base-dir
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Base directory where Sources/ and .build/ are located (parent of lisp/).")

;;; Build Support

(defun hyalo--source-files ()
  "Return a list of all Swift source files for the module."
  (let ((sources-dir (expand-file-name "Sources" hyalo--base-dir)))
    (when (file-directory-p sources-dir)
      (directory-files-recursively sources-dir "\\.swift$"))))

(defun hyalo--package-file ()
  "Return the path to Package.swift if it exists."
  (let ((pkg (expand-file-name "Package.swift" hyalo--base-dir)))
    (when (file-exists-p pkg) pkg)))

(defun hyalo--needs-rebuild-p ()
  "Return non-nil if the module needs to be rebuilt.
The module needs rebuilding if:
- The dylib doesn't exist
- Package.swift is newer than the dylib
- Any Swift source file is newer than the dylib"
  (let ((dylib (hyalo--find-dylib)))
    (if (null dylib)
        t  ; No dylib exists, need to build
      (let ((dylib-time (file-attribute-modification-time
                         (file-attributes dylib)))
            (pkg-file (hyalo--package-file))
            (source-files (hyalo--source-files)))
        (or
         ;; Check Package.swift
         (and pkg-file
              (time-less-p dylib-time
                           (file-attribute-modification-time
                            (file-attributes pkg-file))))
         ;; Check any source file is newer
         (cl-some
          (lambda (src)
            (time-less-p dylib-time
                         (file-attribute-modification-time
                          (file-attributes src))))
          source-files))))))

(defun hyalo-build (&optional release)
  "Build the hyalo module using Swift Package Manager.
With prefix arg RELEASE (or if called with non-nil), build in release mode.
Otherwise, build in release mode by default for production use.
Returns non-nil on success."
  (interactive "P")
  (let* ((default-directory hyalo--base-dir)
         (config (if release "release" "release"))  ; Always release for now
         (buffer-name "*hyalo-build*")
         (cmd (format "swift build -c %s" config)))
    (hyalo-log 'core "Building (%s)..." config)
    (let ((result (call-process-shell-command cmd nil buffer-name t)))
      (if (= result 0)
          (progn
            (hyalo-log 'core "Build successful")
            t)
        (pop-to-buffer buffer-name)
        (hyalo-log 'core "Build failed (see %s)" buffer-name)
        nil))))

(defun hyalo-rebuild-and-reload ()
  "Rebuild the module and reload it into Emacs.
This is useful during development to pick up Swift code changes."
  (interactive)
  (when (hyalo-build t)
    (let ((dylib-path (hyalo--find-dylib)))
      (if dylib-path
          (condition-case err
              (progn
                (module-load dylib-path)
                (setq hyalo--loaded t)
                (hyalo-log 'core "Rebuilt and reloaded from %s" dylib-path)
                t)
            (error
             (hyalo-log 'core "Reload failed: %s" (error-message-string err))
             nil))
        (hyalo-log 'core "No dylib found after build")
        nil))))

;;; Module Path Discovery

(defun hyalo--find-dylib ()
  "Find the path to the libHyalo.dylib dynamic module.
Searches in standard Swift build output locations."
  (let* ((release (expand-file-name ".build/release/libHyalo.dylib" hyalo--base-dir))
         (debug (expand-file-name ".build/debug/libHyalo.dylib" hyalo--base-dir)))
    (cond
     ((file-exists-p release) release)
     ((file-exists-p debug) debug)
     (t nil))))

;;; Module Loading

(defun hyalo-load ()
  "Load the hyalo dynamic module.
If `hyalo-auto-build' is non-nil and the module needs rebuilding,
it will be built automatically before loading.
Returns non-nil on success, nil on failure."
  (interactive)
  (cond
   (hyalo--loaded
    (hyalo-log 'core "Already loaded")
    t)
   ;; Auto-build if enabled and needed
   ((and hyalo-auto-build
         (hyalo--needs-rebuild-p))
    (hyalo-log 'core "Module needs building, auto-building...")
    (if (hyalo-build t)
        (hyalo--do-load)
      (hyalo-log 'core "Auto-build failed")
      nil))
   (t
    (hyalo--do-load))))

(defun hyalo--do-load ()
  "Internal function to load the dylib.
Returns non-nil on success."
  (let ((dylib-path (hyalo--find-dylib)))
    (if (null dylib-path)
        (progn
          (hyalo-log 'core "Dynamic module not found. Run: swift build -c release")
          nil)
      (condition-case err
          (progn
            (module-load dylib-path)
            (setq hyalo--loaded t)
            (hyalo-log 'core "Loaded from %s" dylib-path)
            t)
        (error
         (hyalo-log 'core "Failed to load module: %s" (error-message-string err))
         nil)))))

;;; Module Availability

(defun hyalo-available-p ()
  "Return non-nil if the hyalo module is loaded and available."
  hyalo--loaded)

(defun hyalo-ensure ()
  "Ensure the module is loaded. Signal error if loading fails."
  (unless (or hyalo--loaded (hyalo-load))
    (user-error "hyalo: Module not available. Run: swift build -c release")))

;;; Module Version (delegated to Swift)

(defun hyalo-version-check ()
  "Return the version string of the loaded module.
Returns nil if module is not loaded."
  (when (and hyalo--loaded (fboundp 'hyalo-version))
    (hyalo-version)))

(defun hyalo-get-corner-radius ()
  "Return the window corner radius configured in the module.
Returns nil if module is not loaded."
  (when (and hyalo--loaded (fboundp 'hyalo-corner-radius))
    (hyalo-corner-radius)))

;;; Unified Update Dispatcher (Performance Optimization)
;;
;; Consolidates multiple hook handlers into a single dispatcher.
;; Instead of each module (header, viewport, dimmer) adding separate hooks,
;; they register update functions here. The dispatcher calls them once per cycle.

(defvar hyalo--update-handlers nil
  "Alist of (NAME . FUNCTION) pairs for update handlers.
Each function is called with no arguments during the unified update.")

(defvar hyalo--update-cycle 0
  "Counter incremented each command to detect new cycles.")

(defvar hyalo--last-update-cycle -1
  "The cycle when last update ran. Prevents redundant updates.")

(defvar hyalo--update-enabled nil
  "Non-nil when the unified update dispatcher is active.")

(defun hyalo-register-update-handler (name function)
  "Register FUNCTION as an update handler under NAME.
FUNCTION will be called with no arguments during unified updates.
NAME should be a symbol identifying the handler (for removal)."
  (setq hyalo--update-handlers
        (cons (cons name function)
              (assq-delete-all name hyalo--update-handlers))))

(defun hyalo-unregister-update-handler (name)
  "Remove the update handler registered under NAME."
  (setq hyalo--update-handlers
        (assq-delete-all name hyalo--update-handlers)))

(defun hyalo--increment-update-cycle ()
  "Increment the update cycle counter for a new command."
  (cl-incf hyalo--update-cycle))

(defun hyalo--unified-update ()
  "Run all registered update handlers once per command cycle.
Skips if already updated this cycle."
  (when (and hyalo--update-enabled
             (not (= hyalo--last-update-cycle hyalo--update-cycle)))
    (setq hyalo--last-update-cycle hyalo--update-cycle)
    (dolist (handler hyalo--update-handlers)
      (condition-case err
          (funcall (cdr handler))
        (error
         (hyalo-warn 'update "Handler %s failed: %s" (car handler) err))))))

(defun hyalo-update-dispatcher-enable ()
  "Enable the unified update dispatcher.
Modules should call `hyalo-register-update-handler' to add their handlers."
  (unless hyalo--update-enabled
    (add-hook 'pre-command-hook #'hyalo--increment-update-cycle)
    (add-hook 'post-command-hook #'hyalo--unified-update)
    (add-hook 'window-configuration-change-hook #'hyalo--unified-update)
    (setq hyalo--update-enabled t)
    (hyalo-info 'update "Dispatcher enabled")))

(defun hyalo-update-dispatcher-disable ()
  "Disable the unified update dispatcher."
  (when hyalo--update-enabled
    (remove-hook 'pre-command-hook #'hyalo--increment-update-cycle)
    (remove-hook 'post-command-hook #'hyalo--unified-update)
    (remove-hook 'window-configuration-change-hook #'hyalo--unified-update)
    (setq hyalo--update-enabled nil
          hyalo--update-handlers nil)
    (hyalo-info 'update "Dispatcher disabled")))

(provide 'hyalo)
;;; hyalo.el ends here
