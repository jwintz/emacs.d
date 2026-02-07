;;; hyalo-sidebar.el --- Hyalo sidebar configuration -*- lexical-binding: t -*- no-byte-compile: t -*-

;; Copyright (C) 2025
;; Author: Julien Wintz <julien.wintz@inria.fr>
;; Keywords: frames, sidebar

;;; Commentary:

;; Simplified sidebar integration for Hyalo.
;; The sidebar content is now rendered by native SwiftUI views.
;; This file provides:
;; - Toggle commands for sidebar and inspector panels
;; - Project root detection for file tree
;; - Focus management between panels and main frame

;;; Code:

(require 'hyalo)
(require 'hyalo-channels)
(require 'project)

(defgroup hyalo-sidebar nil
  "Hyalo sidebar configuration."
  :group 'hyalo
  :prefix "hyalo-sidebar-")

(defcustom hyalo-sidebar-use-project-root t
  "When non-nil, use project.el root as sidebar directory.
Falls back to `default-directory' if no project is found."
  :type 'boolean
  :group 'hyalo-sidebar)

;;; State Variables

(defvar hyalo-sidebar--main-frame nil
  "The main Emacs frame (parent of sidebars).")

(defvar hyalo-sidebar--callbacks-installed nil
  "Non-nil when Swift visibility callbacks have been installed.")

;;; Visibility change hooks (for integrations)

(defvar hyalo-on-sidebar-visibility-changed nil
  "Hook run when left sidebar visibility changes.
Called with two arguments: SIDE (\"left\") and VISIBLE (t or nil).")

(defvar hyalo-on-detail-visibility-changed nil
  "Hook run when right detail panel visibility changes.
Called with two arguments: SIDE (\"right\") and VISIBLE (t or nil).")

;;; Project.el integration

(defun hyalo-sidebar--get-root-directory ()
  "Get the root directory for sidebar.
Uses project.el if `hyalo-sidebar-use-project-root' is non-nil,
falls back to `default-directory'."
  (expand-file-name
   (if hyalo-sidebar-use-project-root
       (or (when-let* ((proj (project-current)))
             (if (fboundp 'project-root)
                 (project-root proj)
               (car (project-roots proj))))
           default-directory)
     default-directory)))

;;; Visibility Callbacks

(defun hyalo-on-sidebar-visibility-changed (panel visible)
  "Callback from Swift when sidebar visibility changes.
PANEL is 'left'. VISIBLE is a boolean."
  (run-hook-with-args 'hyalo-on-sidebar-visibility-changed panel visible))

(defun hyalo-on-detail-visibility-changed (panel visible)
  "Callback from Swift when inspector visibility changes.
PANEL is 'right'. VISIBLE is a boolean."
  (run-hook-with-args 'hyalo-on-detail-visibility-changed panel visible))

;;; Callback Setup

(defun hyalo-sidebar--setup-visibility-callbacks ()
  "Setup Swift-to-Elisp visibility callbacks."
  (when (and (hyalo-available-p)
             (not hyalo-sidebar--callbacks-installed))
    (when (fboundp 'hyalo-setup-visibility-callbacks)
      (condition-case err
          (progn
            (hyalo-setup-visibility-callbacks)
            (setq hyalo-sidebar--callbacks-installed t)
            (hyalo-log 'sidebar "Visibility callbacks installed"))
        (error
         (hyalo-log 'sidebar "Failed to install callbacks: %s" err))))))

(defun hyalo-sidebar--teardown-visibility-callbacks ()
  "Teardown Swift-to-Elisp visibility callbacks."
  (setq hyalo-sidebar--callbacks-installed nil))

;;; Mode Definition

(defvar hyalo-sidebar-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `hyalo-sidebar-mode'.")

;;;###autoload
(define-minor-mode hyalo-sidebar-mode
  "Global minor mode for Hyalo sidebar integration.
With native SwiftUI sidebar, this mode just manages visibility callbacks."
  :global t
  :group 'hyalo-sidebar
  (if hyalo-sidebar-mode
      (progn
        (hyalo-log 'sidebar "Enabled")
        ;; Ensure Hyalo module is loaded
        (ignore-errors (hyalo-ensure))
        ;; Store main frame reference
        (setq hyalo-sidebar--main-frame (selected-frame))
        ;; Setup visibility callbacks
        (hyalo-sidebar--setup-visibility-callbacks)
        ;; Enable channel-based data flow
        (when (fboundp 'hyalo-channels-mode)
          (hyalo-channels-mode 1)))
    (progn
      (hyalo-log 'sidebar "Disabled")
      ;; Teardown
      (hyalo-sidebar--teardown-visibility-callbacks)
      (when (fboundp 'hyalo-channels-mode)
        (hyalo-channels-mode -1)))))

;;; Interactive Commands

;;;###autoload
(defun hyalo-sidebar-toggle-left ()
  "Toggle the left sidebar."
  (interactive)
  (when (and (hyalo-available-p) (fboundp 'hyalo-sidebar-toggle))
    (hyalo-sidebar-toggle)))

;;;###autoload
(defun hyalo-sidebar-toggle-right ()
  "Toggle the right sidebar (inspector)."
  (interactive)
  (when (and (hyalo-available-p) (fboundp 'hyalo-detail-toggle))
    (hyalo-detail-toggle)))

;;;###autoload
(defun hyalo-sidebar-show-left ()
  "Show the left sidebar."
  (interactive)
  (when (and (hyalo-available-p) (fboundp 'hyalo-sidebar-show))
    (hyalo-sidebar-show)))

;;;###autoload
(defun hyalo-sidebar-show-right ()
  "Show the right sidebar (inspector)."
  (interactive)
  (when (and (hyalo-available-p) (fboundp 'hyalo-detail-show))
    (hyalo-detail-show)))

;;;###autoload
(defun hyalo-sidebar-hide-left ()
  "Hide the left sidebar."
  (interactive)
  (when (and (hyalo-available-p) (fboundp 'hyalo-sidebar-hide))
    (hyalo-sidebar-hide)))

;;;###autoload
(defun hyalo-sidebar-hide-right ()
  "Hide the right sidebar (inspector)."
  (interactive)
  (when (and (hyalo-available-p) (fboundp 'hyalo-detail-hide))
    (hyalo-detail-hide)))

;;;###autoload
(defun hyalo-sidebar-focus-center ()
  "Focus the main Emacs frame."
  (interactive)
  (when (and hyalo-sidebar--main-frame
             (frame-live-p hyalo-sidebar--main-frame))
    (select-frame-set-input-focus hyalo-sidebar--main-frame)))

;;; Legacy API (for compatibility)

(defun hyalo-sidebar-setup ()
  "Legacy: Configure sidebar display (no-op with SwiftUI sidebar)."
  (interactive)
  (hyalo-log 'sidebar "hyalo-sidebar-setup: Using native SwiftUI sidebar"))

(defun hyalo-sidebar-setup-left ()
  "Legacy: Setup left sidebar (no-op with SwiftUI sidebar)."
  (interactive)
  (hyalo-sidebar-show-left))

(defun hyalo-sidebar-setup-right ()
  "Legacy: Setup right sidebar (no-op with SwiftUI sidebar)."
  (interactive)
  (hyalo-sidebar-show-right))

(defun hyalo-sidebar-teardown ()
  "Clean up sidebar state."
  (interactive)
  (hyalo-sidebar--teardown-visibility-callbacks))

(defun hyalo-sidebar-focus-left ()
  "Focus left sidebar (Swift handles focus)."
  (interactive)
  (hyalo-sidebar-show-left))

(defun hyalo-sidebar-focus-right ()
  "Focus right sidebar (Swift handles focus)."
  (interactive)
  (hyalo-sidebar-show-right))

(defun hyalo-sidebar-update-to-project ()
  "Update sidebar to current project (triggers file tree update)."
  (interactive)
  (when (and (fboundp 'hyalo-channels--update-file-tree)
             (boundp 'hyalo-channels--channel-active)
             hyalo-channels--channel-active)
    (hyalo-channels--update-file-tree)))

(provide 'hyalo-sidebar)
;;; hyalo-sidebar.el ends here
