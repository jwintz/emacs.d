;;; hyalo-module-viewport.el --- Buffer viewport offset for hyalo-module -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: Julien Wintz <julien.wintz@inria.fr>
;; Version: 1.0.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: frames, faces, macos

;;; Commentary:

;; Buffer viewport management for hyalo-module.
;; Handles the visual offset needed when buffer content intersects with
;; the header view overlay.
;;
;; When a buffer is scrolled to the top, the first few lines would be
;; hidden under the header overlay. This module creates a window-specific
;; overlay that pushes content down, creating visual space.
;;
;; Uses deterministic hooks ONLY (no timers):
;; - window-scroll-functions: Called when window scroll position changes
;; - post-command-hook: Called after each command for cursor tracking
;; - window-configuration-change-hook: Called when window layout changes
;; - window-buffer-change-functions: Called when buffer in window changes
;;
;; The overlay height is set INSTANTLY (no animation) to eliminate jitter.
;;
;; Usage:
;;   (require 'hyalo-module-viewport)
;;   (hyalo-module-viewport-mode 1)

;;; Code:

(require 'cl-lib)
(require 'hyalo-module)

(defgroup hyalo-module-viewport nil
  "Viewport offset settings for hyalo-module."
  :group 'hyalo-module
  :prefix "hyalo-module-viewport-")

(defcustom hyalo-module-viewport-debug nil
  "When non-nil, print debug messages."
  :type 'boolean
  :group 'hyalo-module-viewport)

(defcustom hyalo-module-viewport-excluded-modes '(agent-shell-mode
                                                   agent-shell-viewport-view-mode
                                                   agent-shell-viewport-edit-mode)
  "List of major modes where viewport offset should NOT be applied."
  :type '(repeat symbol)
  :group 'hyalo-module-viewport)

;;; Internal State

(defvar hyalo-module-viewport--overlays (make-hash-table :test 'eq :weakness 'key)
  "Map from window to its offset overlay.")

(defvar hyalo-module-viewport--overlay-buffers (make-hash-table :test 'eq :weakness 'key)
  "Map from window to the buffer its overlay was created in.")

(defvar hyalo-module-viewport--last-window-starts (make-hash-table :test 'eq :weakness 'key)
  "Map from window to its last known window-start position.
Used to avoid redundant overlay updates.")

(defvar hyalo-module-viewport--margin 12
  "Margin in pixels added to header height for offset calculation.")

;;; Header Height

(defun hyalo-module-viewport--header-height ()
  "Return the current header height in pixels.
Queries the Swift module for the authoritative value."
  (if (and (hyalo-module-available-p) (fboundp 'hyalo-header-height))
      (+ (hyalo-header-height) hyalo-module-viewport--margin)
    47))  ; Fallback

;;; Window Analysis

(defun hyalo-module-viewport--window-top-pixels (window)
  "Return WINDOW's top edge position in pixels."
  (nth 1 (window-pixel-edges window)))

(defun hyalo-module-viewport--window-intersects-header-p (window)
  "Return non-nil if WINDOW's top edge intersects the header zone."
  (when (window-live-p window)
    (let ((window-top (hyalo-module-viewport--window-top-pixels window))
          (header-height (hyalo-module-viewport--header-height)))
      (< window-top (+ header-height 5)))))

(defun hyalo-module-viewport--window-sidebar-p (window)
  "Return non-nil if WINDOW is a sidebar window (treemacs, hyalo-explorer, etc.)."
  (or (window-parameter window 'window-side)
      (and (fboundp 'hyalo-explorer-sidebar-window-p)
           (hyalo-explorer-sidebar-window-p window))))

(defun hyalo-module-viewport--window-eligible-p (window)
  "Return non-nil if WINDOW should have viewport offset applied."
  (let ((graphic (display-graphic-p))
        (live (window-live-p window))
        (minibuf (window-minibuffer-p window))
        (eq-mini (eq window (minibuffer-window)))
        (sidebar (hyalo-module-viewport--window-sidebar-p window))
        (mode (buffer-local-value 'major-mode (window-buffer window)))
        (excluded (memq (buffer-local-value 'major-mode (window-buffer window))
                        hyalo-module-viewport-excluded-modes))
        (intersects (hyalo-module-viewport--window-intersects-header-p window)))
    (when hyalo-module-viewport-debug
      (hyalo-module-log "Eligible check win=%s: graphic=%s live=%s minibuf=%s eq-mini=%s sidebar=%s mode=%s excluded=%s intersects=%s"
                        window graphic live minibuf eq-mini sidebar mode excluded intersects))
    (and graphic live (not minibuf) (not eq-mini) (not sidebar) (not excluded) intersects)))

(defun hyalo-module-viewport--calculate-offset (window)
  "Calculate the offset pixels needed for WINDOW.
Returns the number of pixels the content should be pushed down."
  (let* ((window-top (hyalo-module-viewport--window-top-pixels window))
         (header-height (hyalo-module-viewport--header-height)))
    (max 0 (- header-height window-top))))

(defun hyalo-module-viewport--at-buffer-top-p (window)
  "Return non-nil if WINDOW is scrolled to the top of its buffer."
  (= (window-start window) (point-min)))

;;; Overlay Management

(defun hyalo-module-viewport--get-or-create-overlay (window)
  "Get or create the offset overlay for WINDOW.
Recreates overlay if window's buffer changed."
  (let* ((ov (gethash window hyalo-module-viewport--overlays))
         (current-buffer (window-buffer window))
         (saved-buffer (gethash window hyalo-module-viewport--overlay-buffers)))
    (if (and ov
             (overlay-buffer ov)
             (eq (overlay-buffer ov) current-buffer)
             (eq saved-buffer current-buffer))
        (progn
          ;; Ensure overlay is always at point-min (it might have drifted if text was inserted)
          (unless (= (overlay-start ov) (point-min))
            (move-overlay ov (point-min) (point-min)))
          ov)
      ;; Delete old overlay if exists
      (when (and ov (overlay-buffer ov))
        (delete-overlay ov))
      ;; Create new overlay
      (with-current-buffer current-buffer
        (let ((new-ov (make-overlay (point-min) (point-min) nil nil nil)))
          (overlay-put new-ov 'window window)
          (overlay-put new-ov 'hyalo-module-viewport t)
          (overlay-put new-ov 'priority 1000)
          (overlay-put new-ov 'evaporate nil)
          (puthash window new-ov hyalo-module-viewport--overlays)
          (puthash window current-buffer hyalo-module-viewport--overlay-buffers)
          new-ov)))))

(defun hyalo-module-viewport--set-overlay-height (window height)
  "Set the overlay for WINDOW to HEIGHT pixels.
If HEIGHT is 0, clears the overlay display."
  (let ((ov (gethash window hyalo-module-viewport--overlays)))
    (when (and ov (overlay-buffer ov))
      (if (> height 0)
          (overlay-put ov 'before-string (propertize "\n" 'line-height height))
        (overlay-put ov 'before-string nil))
      (when hyalo-module-viewport-debug
        (hyalo-module-log "Viewport: window=%s height=%spx" window height)))))

(defun hyalo-module-viewport--remove-overlay (window)
  "Remove the offset overlay for WINDOW."
  (let ((ov (gethash window hyalo-module-viewport--overlays)))
    (when ov
      (when (overlay-buffer ov)
        (delete-overlay ov))
      (remhash window hyalo-module-viewport--overlays)
      (remhash window hyalo-module-viewport--overlay-buffers)
      (remhash window hyalo-module-viewport--last-window-starts))))

(defun hyalo-module-viewport--remove-all-overlays ()
  "Remove all viewport overlays."
  (maphash (lambda (_win ov)
             (when (overlay-buffer ov)
               (delete-overlay ov)))
           hyalo-module-viewport--overlays)
  (clrhash hyalo-module-viewport--overlays)
  (clrhash hyalo-module-viewport--overlay-buffers)
  (clrhash hyalo-module-viewport--last-window-starts))

;;; Core Update Logic

(defun hyalo-module-viewport--update-window (window)
  "Update the viewport overlay for WINDOW.
If window is eligible, update overlay based on scroll position.
If window is NOT eligible, ensure overlay is hidden."
  (let ((eligible (hyalo-module-viewport--window-eligible-p window)))
    (when hyalo-module-viewport-debug
      (hyalo-module-log
       (format "Viewport Update: win=%s buf=%s mode=%s eligible=%s"
               window
               (window-buffer window)
               (buffer-local-value 'major-mode (window-buffer window))
               eligible)))
    (if eligible
      (let* ((current-start (window-start window))
             (last-start (gethash window hyalo-module-viewport--last-window-starts))
             (at-top (hyalo-module-viewport--at-buffer-top-p window)))
        ;; Only update if scroll position changed OR this is first check
        (when (or (null last-start) (not (= current-start last-start)))
          (puthash window current-start hyalo-module-viewport--last-window-starts)
          (if at-top
              ;; At buffer top: show overlay with calculated offset
              (let* ((ov (hyalo-module-viewport--get-or-create-overlay window))
                     (offset (hyalo-module-viewport--calculate-offset window)))
                (when ov
                  (hyalo-module-viewport--set-overlay-height window offset)))
            ;; Not at top: hide overlay
            (hyalo-module-viewport--set-overlay-height window 0))))
    ;; Window is NOT eligible (e.g. excluded mode) - ensure overlay is removed
    (hyalo-module-viewport--remove-overlay window))))

;;; Hook Functions

(defun hyalo-module-viewport--on-scroll (window _new-start)
  "Handle scroll event in WINDOW.
Called from `window-scroll-functions'."
  (hyalo-module-viewport--update-window window))

(defun hyalo-module-viewport--on-post-command ()
  "Handle post-command event.
Updates the selected window's overlay."
  (when (display-graphic-p)
    (hyalo-module-viewport--update-window (selected-window))))

(defun hyalo-module-viewport--on-window-config-change ()
  "Handle window configuration changes.
Cleans up overlays for dead windows and updates all eligible windows."
  ;; Clean up dead windows
  (let ((to-remove nil))
    (maphash (lambda (window _ov)
               (unless (window-live-p window)
                 (push window to-remove)))
             hyalo-module-viewport--overlays)
    (dolist (window to-remove)
      (hyalo-module-viewport--remove-overlay window)))
  ;; Update all eligible windows
  (dolist (window (window-list))
    (if (hyalo-module-viewport--window-eligible-p window)
        (hyalo-module-viewport--update-window window)
      ;; Window no longer intersects header - hide overlay
      (let ((ov (gethash window hyalo-module-viewport--overlays)))
        (when ov
          (hyalo-module-viewport--set-overlay-height window 0))))))

(defun hyalo-module-viewport--on-buffer-change (_frame)
  "Handle buffer change in windows.
Called from `window-buffer-change-functions'."
  ;; Force overlay recreation on next update by clearing last-start cache
  (dolist (window (window-list))
    (remhash window hyalo-module-viewport--last-window-starts)
    (hyalo-module-viewport--update-window window)))

;;; Mode Definition

(defun hyalo-module-viewport--enable ()
  "Enable viewport offset management."
  ;; Install hooks
  (add-hook 'window-scroll-functions #'hyalo-module-viewport--on-scroll)
  (add-hook 'post-command-hook #'hyalo-module-viewport--on-post-command)
  (add-hook 'window-configuration-change-hook #'hyalo-module-viewport--on-window-config-change)
  (when (boundp 'window-buffer-change-functions)
    (add-hook 'window-buffer-change-functions #'hyalo-module-viewport--on-buffer-change))
  ;; Enable smooth scrolling support
  (when (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode 1))
  (setq scroll-preserve-screen-position 'always)
  ;; Initial update for all windows
  (dolist (window (window-list))
    (hyalo-module-viewport--update-window window))
  (hyalo-module-log "Viewport: Enabled"))

(defun hyalo-module-viewport--disable ()
  "Disable viewport offset management."
  ;; Remove hooks
  (remove-hook 'window-scroll-functions #'hyalo-module-viewport--on-scroll)
  (remove-hook 'post-command-hook #'hyalo-module-viewport--on-post-command)
  (remove-hook 'window-configuration-change-hook #'hyalo-module-viewport--on-window-config-change)
  (when (boundp 'window-buffer-change-functions)
    (remove-hook 'window-buffer-change-functions #'hyalo-module-viewport--on-buffer-change))
  ;; Remove all overlays
  (hyalo-module-viewport--remove-all-overlays)
  (hyalo-module-log "Viewport: Disabled"))

;;;###autoload
(define-minor-mode hyalo-module-viewport-mode
  "Minor mode for buffer viewport offset with hyalo-module header.
When enabled, creates window-specific overlays to push content down
when the buffer is scrolled to the top, keeping content visible
below the header overlay."
  :global t
  :lighter " ηViewport"
  :group 'hyalo-module-viewport
  (if hyalo-module-viewport-mode
      (hyalo-module-viewport--enable)
    (hyalo-module-viewport--disable)))

(provide 'hyalo-module-viewport)
;;; hyalo-module-viewport.el ends here
