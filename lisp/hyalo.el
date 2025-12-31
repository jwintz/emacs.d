;;; hyalo-module.el --- Core loader for hyalo-module dynamic module -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: Julien Wintz <julien.wintz@inria.fr>
;; Version: 1.0.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: frames, faces, macos

;;; Commentary:

;; Core loader for the hyalo-module Emacs dynamic module.
;; This file is responsible ONLY for:
;; - Loading the Swift dynamic module (.dylib)
;; - Feature detection
;; - Providing a minimal interface to check module availability
;;
;; Feature-specific functionality is in separate files:
;; - hyalo-module-header.el: Mode-line and header-line display
;; - hyalo-module-appearance.el: Theme and appearance management
;; - hyalo-module-traffic-lights.el: Traffic light button control
;; - hyalo-module-viewport.el: Buffer viewport offset for header zone
;;
;; Usage:
;;   (require 'hyalo-module)
;;   (when (hyalo-module-available-p)
;;     (require 'hyalo-module-header)
;;     ...)

;;; Code:

(require 'cl-lib)

(defgroup hyalo-module nil
  "Hyalo module for macOS Liquid Glass effects."
  :group 'frames
  :prefix "hyalo-module-")

(defcustom hyalo-module-auto-build t
  "If non-nil, automatically build the module when loading if needed.
The module will be built if the dylib doesn't exist or if any Swift
source file is newer than the built dylib."
  :type 'boolean
  :group 'hyalo-module)

;;; Logging

(defvar hyalo-module-elog nil
  "Hyalo module logger (nil if elog not loaded).")

(defun hyalo-module-log (msg &rest args)
  "Log MSG with ARGS using elog if available, otherwise `message'."
  (if (and hyalo-module-elog (fboundp 'elog-info))
      (apply #'elog-info hyalo-module-elog (concat "[hyalo] " msg) args)
    (let ((formatted (apply #'format (concat "[hyalo] " msg) args)))
      (message "%s" formatted))))


(defun hyalo-module-log-init ()
  "Initialize hyalo-module logger if elog is available."
  (when (fboundp 'elog-logger)
    (setq hyalo-module-elog
          (elog-logger
           :name "hyalo"
           :level 'info
           :buffer "*elog*"
           :handlers '(buffer)))))

;; Initialize logger when elog becomes available
(with-eval-after-load 'elog
  (hyalo-module-log-init))

;;; Module State

(defvar hyalo-module--loaded nil
  "Non-nil when the dynamic module has been successfully loaded.")

(defvar hyalo-module--base-dir
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Base directory where Sources/ and .build/ are located (parent of lisp/).")

;;; Build Support

(defun hyalo-module--source-files ()
  "Return a list of all Swift source files for the module."
  (let ((sources-dir (expand-file-name "Sources" hyalo-module--base-dir)))
    (when (file-directory-p sources-dir)
      (directory-files-recursively sources-dir "\\.swift$"))))

(defun hyalo-module--package-file ()
  "Return the path to Package.swift if it exists."
  (let ((pkg (expand-file-name "Package.swift" hyalo-module--base-dir)))
    (when (file-exists-p pkg) pkg)))

(defun hyalo-module--needs-rebuild-p ()
  "Return non-nil if the module needs to be rebuilt.
The module needs rebuilding if:
- The dylib doesn't exist
- Package.swift is newer than the dylib
- Any Swift source file is newer than the dylib"
  (let ((dylib (hyalo-module--find-dylib)))
    (if (null dylib)
        t  ; No dylib exists, need to build
      (let ((dylib-time (file-attribute-modification-time
                         (file-attributes dylib)))
            (pkg-file (hyalo-module--package-file))
            (source-files (hyalo-module--source-files)))
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

(defun hyalo-module-build (&optional release)
  "Build the hyalo-module using Swift Package Manager.
With prefix arg RELEASE (or if called with non-nil), build in release mode.
Otherwise, build in release mode by default for production use.
Returns non-nil on success."
  (interactive "P")
  (let* ((default-directory hyalo-module--base-dir)
         (config (if release "release" "release"))  ; Always release for now
         (buffer-name "*hyalo-module-build*")
         (cmd (format "swift build -c %s" config)))
    (hyalo-module-log "Building (%s)..." config)
    (let ((result (call-process-shell-command cmd nil buffer-name t)))
      (if (= result 0)
          (progn
            (hyalo-module-log "Build successful")
            t)
        (pop-to-buffer buffer-name)
        (hyalo-module-log "Build failed (see %s)" buffer-name)
        nil))))

(defun hyalo-module-rebuild-and-reload ()
  "Rebuild the module and reload it into Emacs.
This is useful during development to pick up Swift code changes."
  (interactive)
  (when (hyalo-module-build t)
    (let ((dylib-path (hyalo-module--find-dylib)))
      (if dylib-path
          (condition-case err
              (progn
                (module-load dylib-path)
                (setq hyalo-module--loaded t)
                (hyalo-module-log "Rebuilt and reloaded from %s" dylib-path)
                t)
            (error
             (hyalo-module-log "Reload failed: %s" (error-message-string err))
             nil))
        (hyalo-module-log "No dylib found after build")
        nil))))

;;; Module Path Discovery

(defun hyalo-module--find-dylib ()
  "Find the path to the libHyalo.dylib dynamic module.
Searches in standard Swift build output locations."
  (let* ((release (expand-file-name ".build/release/libHyalo.dylib" hyalo-module--base-dir))
         (debug (expand-file-name ".build/debug/libHyalo.dylib" hyalo-module--base-dir)))
    (cond
     ((file-exists-p release) release)
     ((file-exists-p debug) debug)
     (t nil))))

;;; Module Loading

(defun hyalo-module-load ()
  "Load the hyalo-module dynamic module.
If `hyalo-module-auto-build' is non-nil and the module needs rebuilding,
it will be built automatically before loading.
Returns non-nil on success, nil on failure."
  (interactive)
  (cond
   (hyalo-module--loaded
    (hyalo-module-log "Already loaded")
    t)
   ;; Auto-build if enabled and needed
   ((and hyalo-module-auto-build
         (hyalo-module--needs-rebuild-p))
    (hyalo-module-log "Module needs building, auto-building...")
    (if (hyalo-module-build t)
        (hyalo-module--do-load)
      (hyalo-module-log "Auto-build failed")
      nil))
   (t
    (hyalo-module--do-load))))

(defun hyalo-module--do-load ()
  "Internal function to load the dylib.
Returns non-nil on success."
  (let ((dylib-path (hyalo-module--find-dylib)))
    (if (null dylib-path)
        (progn
          (hyalo-module-log "Dynamic module not found. Run: swift build -c release")
          nil)
      (condition-case err
          (progn
            (module-load dylib-path)
            (setq hyalo-module--loaded t)
            (hyalo-module-log "Loaded from %s" dylib-path)
            t)
        (error
         (hyalo-module-log "Failed to load module: %s" (error-message-string err))
         nil)))))

;;; Module Availability

(defun hyalo-module-available-p ()
  "Return non-nil if the hyalo-module is loaded and available."
  hyalo-module--loaded)

(defun hyalo-module-ensure ()
  "Ensure the module is loaded. Signal error if loading fails."
  (unless (or hyalo-module--loaded (hyalo-module-load))
    (user-error "hyalo-module: Module not available. Run: swift build -c release")))

;;; Module Version (delegated to Swift)

(defun hyalo-module-version ()
  "Return the version string of the loaded module.
Returns nil if module is not loaded."
  (when (and hyalo-module--loaded (fboundp 'hyalo-version))
    (hyalo-version)))

(defun hyalo-module-corner-radius ()
  "Return the window corner radius configured in the module.
Returns nil if module is not loaded."
  (when (and hyalo-module--loaded (fboundp 'hyalo-corner-radius))
    (hyalo-corner-radius)))

(provide 'hyalo)
;;; hyalo-module.el ends here
