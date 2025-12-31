;;; hyalo-header.el --- Header view management for hyalo -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: Julien Wintz <julien.wintz@inria.fr>
;; Version: 1.0.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: frames, faces, macos

;;; Commentary:

;; Header view management for hyalo.
;; Handles:
;; - Formatting and sending mode-line content to Swift header view
;; - Formatting and sending header-line content to Swift header view
;; - Hiding native Emacs mode-line and header-line
;;
;; Uses deterministic hooks (no timers):
;; - post-command-hook for immediate updates
;; - window-configuration-change-hook for window changes
;; - after-change-major-mode-hook for mode changes
;;
;; Usage:
;;   (require 'hyalo-header)
;;   (hyalo-header-mode 1)

;;; Code:

(require 'hyalo)

(defgroup hyalo-header nil
  "Header view settings for hyalo."
  :group 'hyalo
  :prefix "hyalo-header-")

(defcustom hyalo-header-top-padding 12
  "Top padding for header view in pixels."
  :type 'integer
  :group 'hyalo-header)

(defcustom hyalo-header-left-padding 12
  "Left padding for header view in pixels."
  :type 'integer
  :group 'hyalo-header)

(defcustom hyalo-header-right-padding 12
  "Right padding for header view in pixels."
  :type 'integer
  :group 'hyalo-header)

(defcustom hyalo-header-right-padding 12
  "Right padding for header view in pixels."
  :type 'integer
  :group 'hyalo-header)

;;; Internal State

(defvar hyalo-header--saved-mode-line-format nil
  "Saved default mode-line-format to restore when disabling.")

(defvar hyalo-header--saved-header-line-format nil
  "Saved default header-line-format to restore when disabling.")

(defvar hyalo-header--buffer-header-lines (make-hash-table :test 'eq :weakness 'key)
  "Hash table mapping buffers to their original header-line-format.")

(defvar hyalo-header--last-mode-line ""
  "Last mode-line string sent to Swift, for change detection.")

(defvar hyalo-header--last-header-line ""
  "Last header-line string sent to Swift, for change detection.")

(defvar hyalo-header--last-window nil
  "Last window that was updated, for change detection.")

(defvar hyalo-header--saved-frame-title-format nil
  "Saved frame-title-format to restore when disabling.")

;;; Mode-line Formatting

(defun hyalo-header--save-buffer-header-line ()
  "Save current buffer's header-line-format before hiding."
  (when header-line-format
    (puthash (current-buffer) header-line-format hyalo-header--buffer-header-lines)))

(defun hyalo-header--get-buffer-header-line ()
  "Get the saved header-line-format for current buffer."
  (gethash (current-buffer) hyalo-header--buffer-header-lines))

(defun hyalo-header--format-mode-line ()
  "Get the formatted mode-line string for the current buffer."
  (when hyalo-header--saved-mode-line-format
    (format-mode-line hyalo-header--saved-mode-line-format)))

(defun hyalo-header--format-header-line ()
  "Get the formatted header-line string for the current buffer."
  (when-let* ((hl (hyalo-header--get-buffer-header-line)))
    (format-mode-line hl)))

;;; Header Update (Hook Function)

(defun hyalo-header--update ()
  "Send current buffer info to Swift header view and toolbar.
Called from post-command-hook and window-configuration-change-hook.
Skips updates from embedded child-frames (hyalo-embedded parameter)."
  (when (and (hyalo-available-p)
             (display-graphic-p)
             ;; Skip updates from embedded child-frames (sidebars)
             ;; The toolbar modeline should only reflect main content
             (not (frame-parameter nil 'hyalo-embedded)))
    ;; Enforce hidden mode-line/header-line if they reappeared
    (when (or mode-line-format header-line-format)
      (hyalo-header--enforce-hidden))
    (let* ((current-window (selected-window))
           (mode-line-str (or (hyalo-header--format-mode-line) ""))
           (header-line-str (or (hyalo-header--format-header-line) ""))
           ;; Only update if content or window changed
           (mode-line-changed (not (string= mode-line-str hyalo-header--last-mode-line)))
           (header-line-changed (not (string= header-line-str hyalo-header--last-header-line)))
           (window-changed (not (eq current-window hyalo-header--last-window))))
      ;; Update mode-line if changed
      (when (or mode-line-changed window-changed)
        (setq hyalo-header--last-mode-line mode-line-str)
        ;; Update NavigationSplitView toolbar mode-line
        (when (fboundp 'hyalo-sidebar-update-mode-line)
          (hyalo-sidebar-update-mode-line mode-line-str)))
      ;; Update header-line if changed (currently not used in toolbar)
      (when (or header-line-changed window-changed)
        (setq hyalo-header--last-header-line header-line-str))
      ;; Track window
      (setq hyalo-header--last-window current-window))))

;;; Native Mode-line Control

(defun hyalo-header--hide-native ()
  "Hide native Emacs mode-line and header-line in all buffers."
  ;; Save current formats
  (setq hyalo-header--saved-mode-line-format (default-value 'mode-line-format))
  (setq hyalo-header--saved-header-line-format (default-value 'header-line-format))
  ;; Save and hide in all existing buffers
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (hyalo-header--save-buffer-header-line)
      (setq mode-line-format nil)
      (setq header-line-format nil)))
  ;; Hide globally
  (setq-default mode-line-format nil)
  (setq-default header-line-format nil)
  ;; Hook to hide in new buffers
  (add-hook 'after-change-major-mode-hook #'hyalo-header--enforce-hidden)
  ;; Add window dividers to compensate for hidden mode-line
  (setq window-divider-default-places 'bottom-only)
  (setq window-divider-default-bottom-width 1)
  (window-divider-mode 1))

(defun hyalo-header--enforce-hidden ()
  "Enforce hidden mode-line in current buffer.
Called by hooks to override modes that set mode-line-format buffer-locally."
  (when hyalo-header--saved-mode-line-format
    ;; Save the header-line before hiding (modes like info set it)
    (when header-line-format
      (hyalo-header--save-buffer-header-line)))
  ;; Always hide, regardless of whether we saved a format
  (setq mode-line-format nil)
  (setq header-line-format nil))

(defun hyalo-header--restore-native ()
  "Restore native Emacs mode-line and header-line."
  ;; Remove hooks
  (remove-hook 'after-change-major-mode-hook #'hyalo-header--enforce-hidden)
  ;; Restore defaults
  (when hyalo-header--saved-mode-line-format
    (setq-default mode-line-format hyalo-header--saved-mode-line-format))
  (when hyalo-header--saved-header-line-format
    (setq-default header-line-format hyalo-header--saved-header-line-format))
  ;; Restore in all buffers
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (kill-local-variable 'mode-line-format)
      (kill-local-variable 'header-line-format)))
  ;; Clear saved header-line hash
  (clrhash hyalo-header--buffer-header-lines)
  (window-divider-mode -1))

;;; Header Height

(defun hyalo-header-get-height ()
  "Return the header view height in pixels from the Swift module."
  (if (and (hyalo-available-p) (fboundp 'hyalo-header-height))
      (hyalo-header-height)
    47))  ; Fallback: 12 + 24 + 1 + 22 - 12 = 47 (approx)

;;; Magit Support

(defun hyalo-header--magit-setup-advice (buffer)
  "Advice to enforce hidden mode-line after Magit setup.
Run as :filter-return on `magit-setup-buffer-internal'."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (hyalo-header--enforce-hidden)
      (when (and (boundp 'hyalo-viewport-mode)
                 hyalo-viewport-mode
                 (fboundp 'hyalo-viewport--update-window))
        ;; Update viewport with a delay to ensure window is displayed
        (run-at-time 2.5 nil
                     (lambda (buf)
                       (when (buffer-live-p buf)
                         (let ((wins (get-buffer-window-list buf nil t)))
                           (dolist (w wins)
                             (with-selected-window w
                               (hyalo-log "Magit Setup (delayed): Updating window %s" w)
                               (goto-char (point-min))
                               (hyalo-viewport--update-window w))))))
                     buffer))))
  buffer)


(defun hyalo-header--magit-refresh-advice (&rest _)
  "Advice to enforce hidden mode-line after Magit refresh.
Run as :after on `magit-refresh-buffer'."
  (hyalo-header--enforce-hidden)
  (when (and (boundp 'hyalo-viewport-mode)
             hyalo-viewport-mode
             (fboundp 'hyalo-viewport--update-window))
    (dolist (win (get-buffer-window-list (current-buffer) nil t))
      (hyalo-viewport--update-window win))))



(defun hyalo-header--setup-magit ()
  "Setup Magit integration using Advice."
  ;; Use filter-return for setup to access the correct buffer context
  (advice-add 'magit-setup-buffer-internal :filter-return #'hyalo-header--magit-setup-advice)
  (advice-add 'magit-refresh-buffer :after #'hyalo-header--magit-refresh-advice))

(defun hyalo-header--teardown-magit ()
  "Remove Magit integration."
  (advice-remove 'magit-setup-buffer-internal #'hyalo-header--magit-setup-advice)
  (advice-remove 'magit-refresh-buffer #'hyalo-header--magit-refresh-advice))

;;; Setup/Teardown

(defun hyalo-header--setup-frame (&optional frame)
  "Setup Hyalo for FRAME (or current frame if nil)."
  (let ((f (or frame (selected-frame))))
    (when (and (display-graphic-p f) (eq (framep f) 'ns))
      (with-selected-frame f
        ;; Tell Emacs the titlebar is transparent
        (modify-frame-parameters f '((ns-transparent-titlebar . t)
                                      (ns-appearance . nil)))
        ;; Clear the frame title so Emacs doesn't try to display dimensions
        (modify-frame-parameters f '((title . " ")))
        ;; Disable internal border (reduces title bar height)
        (modify-frame-parameters f '((internal-border-width . 0)))
        ;; Disable Emacs native toolbar (SwiftUI toolbar is authoritative)
        (when (fboundp 'tool-bar-mode)
          (tool-bar-mode -1))
        ;; Setup NavigationSplitView with toolbar (replaces legacy HeaderView)
        (when (fboundp 'hyalo-navigation-setup)
          (hyalo-navigation-setup))
        ;; Apply appearance settings AFTER navigation is ready
        ;; This ensures saved vibrancy/opacity values are applied to Swift
        (when (and (boundp 'hyalo-appearance-mode)
                   hyalo-appearance-mode
                   (fboundp 'hyalo-appearance--apply-vibrancy))
          (hyalo-appearance--apply-vibrancy))
        ;; Restore Emacs keyboard focus after setup
        (when (fboundp 'hyalo-restore-focus)
          (run-with-timer 0.5 nil #'hyalo-restore-focus))))))

(defun hyalo-header--teardown-frame (&optional frame)
  "Teardown Hyalo for FRAME (or current frame if nil)."
  (with-selected-frame (or frame (selected-frame))
    (when (fboundp 'hyalo-navigation-teardown)
      (hyalo-navigation-teardown))))

(defun hyalo-header--keep-title-empty ()
  "Keep frame title empty to prevent Emacs from rendering in titlebar."
  (when (and (display-graphic-p) (eq (framep (selected-frame)) 'ns))
    (let ((current-title (frame-parameter nil 'title)))
      (unless (and current-title (string= current-title " "))
        (set-frame-parameter nil 'title " ")))))

(defun hyalo-header--enable ()
  "Enable header view management."
  (hyalo-ensure)
  ;; Save and clear frame-title-format to prevent dimension display in titlebar
  (setq hyalo-header--saved-frame-title-format frame-title-format)
  (setq frame-title-format " ")
  ;; Setup all existing frames
  (dolist (frame (frame-list))
    (hyalo-header--setup-frame frame))
  ;; Setup hook for new frames
  (add-hook 'after-make-frame-functions #'hyalo-header--setup-frame)
  (add-hook 'delete-frame-functions #'hyalo-header--teardown-frame)
  ;; Hide native mode-line
  (hyalo-header--hide-native)
  ;; Install hooks for updates
  (add-hook 'post-command-hook #'hyalo-header--update)
  (add-hook 'window-configuration-change-hook #'hyalo-header--update)
  ;; Keep title empty (prevents dimension display in titlebar)
  (add-hook 'post-command-hook #'hyalo-header--keep-title-empty)
  ;; Setup Magit support
  (with-eval-after-load 'magit
    (hyalo-header--setup-magit))
  ;; Initial update
  (hyalo-header--update)
  (hyalo-log "Header: Enabled"))

(defun hyalo-header--disable ()
  "Disable header view management."
  ;; Remove frame hooks
  (remove-hook 'after-make-frame-functions #'hyalo-header--setup-frame)
  (remove-hook 'delete-frame-functions #'hyalo-header--teardown-frame)
  ;; Remove update hooks
  (remove-hook 'post-command-hook #'hyalo-header--update)
  (remove-hook 'window-configuration-change-hook #'hyalo-header--update)
  (remove-hook 'post-command-hook #'hyalo-header--keep-title-empty)
  ;; Teardown Magit support
  (with-eval-after-load 'magit
    (hyalo-header--teardown-magit))
  ;; Teardown all frames
  (dolist (frame (frame-list))
    (hyalo-header--teardown-frame frame))
  ;; Restore native mode-line
  (hyalo-header--restore-native)
  ;; Restore frame-title-format
  (when hyalo-header--saved-frame-title-format
    (setq frame-title-format hyalo-header--saved-frame-title-format))
  ;; Clear state
  (setq hyalo-header--last-mode-line ""
        hyalo-header--last-header-line ""
        hyalo-header--last-window nil
        hyalo-header--saved-frame-title-format nil)
  (hyalo-log "Header: Disabled"))

;;;###autoload
(define-minor-mode hyalo-header-mode
  "Minor mode for Hyalo header view.
When enabled, the native mode-line and header-line are hidden
and their content is displayed in a floating SwiftUI header."
  :global t
  :lighter " ηHeader"
  :group 'hyalo-header
  (if hyalo-header-mode
      (hyalo-header--enable)
    (hyalo-header--disable)))

;; Interactive wrappers for Swift-defined functions
;; Swift env.defun creates non-interactive functions, so we need wrappers

;;;###autoload
(defun hyalo-toggle-decorations-command ()
  "Toggle visibility of toolbar and traffic lights.
When hidden, provides a minimal chrome experience."
  (interactive)
  (if (fboundp 'hyalo-toggle-decorations)
      (progn
        (hyalo-toggle-decorations)
        (hyalo-log "Decorations %s" (if (hyalo-decorations-visible-p) "shown" "hidden")))
    (hyalo-log "Hyalo decorations toggle not available - module not loaded")))

;; Create an alias so M-x hyalo-toggle-decorations works
(defalias 'hyalo-toggle-chrome 'hyalo-toggle-decorations-command
  "Alias for `hyalo-toggle-decorations-command'.")

;; Handle case where Magit is loaded after this module is enabled
(with-eval-after-load 'magit
  (when hyalo-header-mode
    (hyalo-header--setup-magit)))

(provide 'hyalo-header)
;;; hyalo-header.el ends here
