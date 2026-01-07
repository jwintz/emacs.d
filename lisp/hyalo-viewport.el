;;; hyalo-viewport.el --- Buffer viewport offset for hyalo -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: Julien Wintz <julien.wintz@inria.fr>
;; Version: 1.0.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: frames, faces, macos

;;; Commentary:

;; Buffer viewport management for hyalo.
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
;;   (require 'hyalo-scroll)
;;   (hyalo-viewport-mode 1)

;;; Code:

(require 'cl-lib)
(require 'hyalo)

(defgroup hyalo-viewport nil
  "Viewport offset settings for hyalo."
  :group 'hyalo
  :prefix "hyalo-viewport-")

(defcustom hyalo-viewport-debug nil
  "When non-nil, print debug messages."
  :type 'boolean
  :group 'hyalo-viewport)

(defcustom hyalo-viewport-excluded-modes '(agent-shell-mode
                                           agent-shell-viewport-view-mode
                                           agent-shell-viewport-edit-mode
                                           eat-mode)
  "List of major modes where viewport offset should NOT be applied."
  :type '(repeat symbol)
  :group 'hyalo-viewport)

;;; Internal State

(defvar hyalo-viewport--overlays (make-hash-table :test 'eq :weakness 'key)
  "Map from window to its offset overlay.")

(defvar hyalo-viewport--overlay-buffers (make-hash-table :test 'eq :weakness 'key)
  "Map from window to the buffer its overlay was created in.")

(defvar hyalo-viewport--last-window-starts (make-hash-table :test 'eq :weakness 'key)
  "Map from window to its last known window-start position.
Used to avoid redundant overlay updates.")

(defvar hyalo-viewport--margin 6
  "Margin in pixels added to header height for offset calculation.")

;;; Header Height

(defun hyalo-viewport--header-height ()
  "Return the current header height in pixels.
Queries the Swift module for the authoritative value."
  (if (and (hyalo-available-p) (fboundp 'hyalo-header-height))
      (+ (hyalo-header-height) hyalo-viewport--margin)
    47))  ; Fallback

;;; Window Analysis

(defun hyalo-viewport--window-top-pixels (window)
  "Return WINDOW's top edge position in pixels."
  (nth 1 (window-pixel-edges window)))

(defun hyalo-viewport--window-intersects-header-p (window)
  "Return non-nil if WINDOW's top edge intersects the header zone."
  (when (window-live-p window)
    (let ((window-top (hyalo-viewport--window-top-pixels window))
          (header-height (hyalo-viewport--header-height)))
      (< window-top (+ header-height 5)))))

(defun hyalo-viewport--window-sidebar-p (window)
  "Return non-nil if WINDOW is a sidebar window (treemacs, hyalo-explorer, etc.)."
  (or (window-parameter window 'window-side)
      (frame-parameter (window-frame window) 'hyalo-embedded)
      (and (fboundp 'hyalo-explorer-sidebar-window-p)
           (hyalo-explorer-sidebar-window-p window))))

(defun hyalo-viewport--window-eligible-p (window)
  "Return non-nil if WINDOW should have viewport offset applied."
  (let* ((graphic (display-graphic-p))
         (live (window-live-p window))
         (minibuf (window-minibuffer-p window))
         (eq-mini (eq window (minibuffer-window)))
         (sidebar (hyalo-viewport--window-sidebar-p window))
         (mode (buffer-local-value 'major-mode (window-buffer window)))
         (excluded (or (apply #'provided-mode-derived-p
                              mode
                              hyalo-viewport-excluded-modes)
                       (provided-mode-derived-p mode 'eat-mode)))
         (intersects (hyalo-viewport--window-intersects-header-p window)))
    (when hyalo-viewport-debug
      (hyalo-log 'viewport "Eligible check win=%s: graphic=%s live=%s minibuf=%s eq-mini=%s sidebar=%s mode=%s excluded=%s intersects=%s"
                        window graphic live minibuf eq-mini sidebar mode excluded intersects))
    (and graphic live (not minibuf) (not eq-mini) (not sidebar) (not excluded) intersects)))

(defun hyalo-viewport--calculate-offset (window)
  "Calculate the offset pixels needed for WINDOW.
Returns the number of pixels the content should be pushed down."
  (let* ((window-top (hyalo-viewport--window-top-pixels window))
         (header-height (hyalo-viewport--header-height)))
    (max 0 (- header-height window-top))))

(defun hyalo-viewport--at-buffer-top-p (window)
  "Return non-nil if WINDOW is scrolled to the top of its buffer."
  (= (window-start window) (point-min)))

;;; Overlay Management

(defun hyalo-viewport--get-or-create-overlay (window)
  "Get or create the offset overlay for WINDOW.
Recreates overlay if window's buffer changed."
  (let* ((ov (gethash window hyalo-viewport--overlays))
         (current-buffer (window-buffer window))
         (saved-buffer (gethash window hyalo-viewport--overlay-buffers)))
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
        (let ((new-ov (make-overlay (point-min) (point-min) nil t nil)))
          (overlay-put new-ov 'window window)
          (overlay-put new-ov 'hyalo-viewport t)
          ;; High priority to appear above magit section overlays
          (overlay-put new-ov 'priority 10000)
          (overlay-put new-ov 'evaporate nil)
          ;; Front-advance ensures overlay stays at buffer start
          (overlay-put new-ov 'front-advance nil)
          (overlay-put new-ov 'rear-advance nil)
          (puthash window new-ov hyalo-viewport--overlays)
          (puthash window current-buffer hyalo-viewport--overlay-buffers)
          new-ov)))))

(defun hyalo-viewport--set-overlay-height (window height)
  "Set the overlay for WINDOW to HEIGHT pixels.
If HEIGHT is 0, clears the overlay display."
  (let ((ov (gethash window hyalo-viewport--overlays)))
    (when (and ov (overlay-buffer ov))
      (if (> height 0)
          ;; Use display space spec for better compatibility with magit overlays.
          ;; We must add the line height to the space height and align to bottom (ascent 100)
          ;; so that the text on the first line (sharing the line with the space)
          ;; is pushed down by 'height' pixels relative to the top of the line.
          ;; The header covers the top 'height' pixels.
          (let ((adjusted-height (+ height (frame-char-height (window-frame window)))))
            (overlay-put ov 'before-string
                         (propertize " "
                                     'display `(space :width 0 :height (,adjusted-height) :ascent 100)
                                     'face 'default)))
        (overlay-put ov 'before-string nil))
      (when hyalo-viewport-debug
        (hyalo-log 'viewport "window=%s height=%spx" window height)))))

(defun hyalo-viewport--remove-overlay (window)
  "Remove the offset overlay for WINDOW."
  (let ((ov (gethash window hyalo-viewport--overlays)))
    (when ov
      (when (overlay-buffer ov)
        (delete-overlay ov))
      (remhash window hyalo-viewport--overlays)
      (remhash window hyalo-viewport--overlay-buffers)
      (remhash window hyalo-viewport--last-window-starts))))

(defun hyalo-viewport--remove-all-overlays ()
  "Remove all viewport overlays."
  (maphash (lambda (_win ov)
             (when (overlay-buffer ov)
               (delete-overlay ov)))
           hyalo-viewport--overlays)
  (clrhash hyalo-viewport--overlays)
  (clrhash hyalo-viewport--overlay-buffers)
  (clrhash hyalo-viewport--last-window-starts))

;;; Core Update Logic

(defun hyalo-viewport--update-window (window)
  "Update the viewport overlay for WINDOW.
If window is eligible, update overlay based on scroll position.
If window is NOT eligible, ensure overlay is hidden."
  (let ((eligible (hyalo-viewport--window-eligible-p window)))
    (when hyalo-viewport-debug
      (hyalo-log 'viewport "Update: win=%s buf=%s mode=%s eligible=%s"
                 window
                 (window-buffer window)
                 (buffer-local-value 'major-mode (window-buffer window))
                 eligible))
    (if eligible
      (let* ((current-start (window-start window))
             (last-start (gethash window hyalo-viewport--last-window-starts))
             (at-top (hyalo-viewport--at-buffer-top-p window)))
        ;; Only update if scroll position changed OR this is first check
        (when (or (null last-start) (not (= current-start last-start)))
          (puthash window current-start hyalo-viewport--last-window-starts)
          (if at-top
              ;; At buffer top: show overlay with calculated offset
              (let* ((ov (hyalo-viewport--get-or-create-overlay window))
                     (offset (hyalo-viewport--calculate-offset window)))
                (when ov
                  (hyalo-viewport--set-overlay-height window offset)))
            ;; Not at top: hide overlay
            (hyalo-viewport--set-overlay-height window 0))))
    ;; Window is NOT eligible (e.g. excluded mode) - ensure overlay is removed
    (hyalo-viewport--remove-overlay window))))

(defun hyalo-viewport--update-all-windows ()
  "Update viewport for all windows."
  (dolist (window (window-list))
    (hyalo-viewport--update-window window)))

;;; Hook Functions

(defun hyalo-viewport--on-scroll (window _new-start)
  "Handle scroll event in WINDOW.
Called from `window-scroll-functions'."
  (hyalo-viewport--update-window window))

(defun hyalo-viewport--on-post-command ()
  "Handle post-command event.
Updates the selected window's overlay."
  (when (display-graphic-p)
    (hyalo-viewport--update-window (selected-window))))

(defun hyalo-viewport--on-window-config-change ()
  "Handle window configuration changes.
Cleans up overlays for dead windows and updates all eligible windows."
  ;; Clean up dead windows
  (let ((to-remove nil))
    (maphash (lambda (window _ov)
               (unless (window-live-p window)
                 (push window to-remove)))
             hyalo-viewport--overlays)
    (dolist (window to-remove)
      (hyalo-viewport--remove-overlay window)))
  ;; Update all eligible windows
  (dolist (window (window-list))
    (if (hyalo-viewport--window-eligible-p window)
        (hyalo-viewport--update-window window)
      ;; Window no longer intersects header - hide overlay
      (let ((ov (gethash window hyalo-viewport--overlays)))
        (when ov
          (hyalo-viewport--set-overlay-height window 0))))))

(defun hyalo-viewport--on-buffer-change (_frame)
  "Handle buffer change in windows.
Called from `window-buffer-change-functions'."
  ;; Force overlay recreation on next update by clearing last-start cache
  (dolist (window (window-list))
    (remhash window hyalo-viewport--last-window-starts)
    (hyalo-viewport--update-window window)))

;;; Magit Support

(defun hyalo-viewport--magit-update (&rest _args)
  "Update viewport for magit buffers.
Forces overlay recreation to handle magit's complex overlay structure.
Ignores any arguments passed by hooks."
  (dolist (window (window-list))
    (when (window-live-p window)
      (let ((mode (buffer-local-value 'major-mode (window-buffer window))))
        (when (or (derived-mode-p 'magit-mode)
                  (provided-mode-derived-p mode 'magit-mode))
          ;; Force overlay recreation by clearing cache
          (remhash window hyalo-viewport--last-window-starts)
          (hyalo-viewport--update-window window))))))

(defun hyalo-viewport--setup-magit ()
  "Enable Magit support by adding hooks."
  (with-eval-after-load 'magit
    ;; magit-refresh-buffer-hook runs AFTER buffer content is populated
    (add-hook 'magit-refresh-buffer-hook #'hyalo-viewport--magit-update)
    ;; magit-post-refresh-hook for after full refresh cycle
    (add-hook 'magit-post-refresh-hook #'hyalo-viewport--magit-update)
    ;; Mode hooks for initial buffer creation (deferred via post-command)
    (add-hook 'magit-status-mode-hook #'hyalo-viewport--magit-update)
    (add-hook 'magit-log-mode-hook #'hyalo-viewport--magit-update)
    (add-hook 'magit-diff-mode-hook #'hyalo-viewport--magit-update)
    (add-hook 'magit-revision-mode-hook #'hyalo-viewport--magit-update)
    (add-hook 'magit-stash-mode-hook #'hyalo-viewport--magit-update)
    ;; Section movement/highlight
    (add-hook 'magit-section-movement-hook #'hyalo-viewport--magit-update)))

(defun hyalo-viewport--teardown-magit ()
  "Disable Magit support."
  (with-eval-after-load 'magit
    (remove-hook 'magit-refresh-buffer-hook #'hyalo-viewport--magit-update)
    (remove-hook 'magit-post-refresh-hook #'hyalo-viewport--magit-update)
    (remove-hook 'magit-status-mode-hook #'hyalo-viewport--magit-update)
    (remove-hook 'magit-log-mode-hook #'hyalo-viewport--magit-update)
    (remove-hook 'magit-diff-mode-hook #'hyalo-viewport--magit-update)
    (remove-hook 'magit-revision-mode-hook #'hyalo-viewport--magit-update)
    (remove-hook 'magit-stash-mode-hook #'hyalo-viewport--magit-update)
    (remove-hook 'magit-section-movement-hook #'hyalo-viewport--magit-update)))

;;; Mode Definition

(defvar hyalo-viewport--last-selected-window nil
  "Last selected window, for unified update change detection.")

(defun hyalo-viewport--unified-update ()
  "Update handler for unified dispatcher.
Combines post-command and window-config-change behavior."
  (when (display-graphic-p)
    (let* ((current-window (selected-window))
           (window-changed (not (eq current-window hyalo-viewport--last-selected-window))))
      ;; Always update selected window (has internal early-exit via window-start check)
      (hyalo-viewport--update-window current-window)
      ;; Clean up dead windows only on window change (not every command)
      (when window-changed
        (setq hyalo-viewport--last-selected-window current-window)
        (let ((to-remove nil))
          (maphash (lambda (window _ov)
                     (unless (window-live-p window)
                       (push window to-remove)))
                   hyalo-viewport--overlays)
          (dolist (window to-remove)
            (hyalo-viewport--remove-overlay window)))))))

(defun hyalo-viewport--enable ()
  "Enable viewport offset management."
  ;; Register with unified update dispatcher
  (hyalo-update-dispatcher-enable)
  (hyalo-register-update-handler 'viewport #'hyalo-viewport--unified-update)
  ;; Keep scroll hook separate (receives window/position args)
  (add-hook 'window-scroll-functions #'hyalo-viewport--on-scroll)
  ;; Keep buffer-change hook separate (receives frame arg)
  (when (boundp 'window-buffer-change-functions)
    (add-hook 'window-buffer-change-functions #'hyalo-viewport--on-buffer-change))
  ;; Enable smooth scrolling support
  (when (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode 1))
  (setq scroll-preserve-screen-position 'always)
  ;; Setup Magit support
  (hyalo-viewport--setup-magit)
  ;; Initial update for all windows
  (hyalo-viewport--update-all-windows)
  (hyalo-log 'viewport "Enabled"))

(defun hyalo-viewport--disable ()
  "Disable viewport offset management."
  ;; Unregister from unified update dispatcher
  (hyalo-unregister-update-handler 'viewport)
  ;; Remove remaining hooks
  (remove-hook 'window-scroll-functions #'hyalo-viewport--on-scroll)
  (when (boundp 'window-buffer-change-functions)
    (remove-hook 'window-buffer-change-functions #'hyalo-viewport--on-buffer-change))
  ;; Teardown Magit support
  (hyalo-viewport--teardown-magit)
  ;; Remove all overlays
  (hyalo-viewport--remove-all-overlays)
  (hyalo-log 'viewport "Disabled"))

;;;###autoload
(define-minor-mode hyalo-viewport-mode
  "Minor mode for buffer viewport offset with hyalo header.
When enabled, creates window-specific overlays to push content down
when the buffer is scrolled to the top, keeping content visible
below the header overlay."
  :global t
  :lighter " ηViewport"
  :group 'hyalo-viewport
  (if hyalo-viewport-mode
      (hyalo-viewport--enable)
    (hyalo-viewport--disable)))

(provide 'hyalo-viewport)
;;; hyalo-viewport.el ends here
