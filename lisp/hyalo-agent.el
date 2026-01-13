;;; hyalo-agent.el --- Visual styling for pi-coding-agent -*- lexical-binding: t -*-

;; Copyright (C) 2025-2026
;; Author: Julien Wintz <julien.wintz@inria.fr>
;; Version: 1.0.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: tools, ai

;;; Commentary:

;; Provides visual styling for pi-coding-agent tool blocks that works with
;; Hyalo's transparency settings (ns-alpha-glyphs makes backgrounds transparent).
;;
;; Solution: Use line-prefix with colored block characters (▌) as left indicators.
;; The indicator appears at the start of each line in tool blocks.
;;
;; Colors are derived from theme faces:
;; - Pending: `shadow` face (neutral)
;; - Success: `success` face (green)
;; - Error: `error` face (red)
;;
;; Usage:
;;   (require 'hyalo-agent)
;;   ;; Automatically enables in pi-coding-agent-chat-mode

;;; Code:

(require 'cl-lib)

(defgroup hyalo-agent nil
  "Visual styling for pi-coding-agent."
  :group 'hyalo
  :prefix "hyalo-agent-")

(defcustom hyalo-agent-indicator-char ?▌
  "Character used for the left indicator.
Good options: ?▌ (left half block), ?▎ (left quarter block), ?│ (box vertical)."
  :type 'character
  :group 'hyalo-agent)

(defcustom hyalo-agent-indicator-padding " "
  "Padding after the indicator character."
  :type 'string
  :group 'hyalo-agent)

;;; Color Helpers

(defun hyalo-agent--get-indicator-color (state)
  "Get indicator color for block STATE from current theme.
STATE is one of: pending, success, error."
  (face-foreground
   (pcase state
     ('pending 'shadow)
     ('success 'success)
     ('error 'error)
     (_ 'shadow))
   nil 'default))

;;; Indicator Creation

(defun hyalo-agent--make-indicator (state)
  "Create indicator string for STATE."
  (let ((color (hyalo-agent--get-indicator-color state))
        (char (char-to-string hyalo-agent-indicator-char)))
    (concat (propertize char 'face `(:foreground ,color))
            hyalo-agent-indicator-padding)))

;;; State Detection

(defun hyalo-agent--face-to-state (face)
  "Convert pi-coding-agent FACE symbol to state symbol."
  (pcase face
    ('pi-coding-agent-tool-block-pending 'pending)
    ('pi-coding-agent-tool-block-success 'success)
    ('pi-coding-agent-tool-block-error 'error)
    (_ nil)))

;;; Overlay Management

(defun hyalo-agent--apply-indicator (ov state)
  "Apply left indicator to overlay OV for STATE."
  (when (and ov (overlay-buffer ov))
    (let ((indicator (hyalo-agent--make-indicator state)))
      ;; Use line-prefix for indicator at start of each line
      (overlay-put ov 'line-prefix indicator)
      ;; Also wrap-prefix for wrapped lines
      (overlay-put ov 'wrap-prefix indicator)
      ;; Store state for updates
      (overlay-put ov 'hyalo-agent-state state)
      ;; Mark as processed
      (overlay-put ov 'hyalo-agent-processed t)
      ;; Remove background face (transparent anyway)
      (overlay-put ov 'face nil))))

(defun hyalo-agent--remove-indicator (ov)
  "Remove indicator from overlay OV, restoring original face."
  (when (and ov (overlay-buffer ov))
    (let ((state (overlay-get ov 'hyalo-agent-state)))
      ;; Restore original face
      (overlay-put ov 'face
                   (pcase state
                     ('pending 'pi-coding-agent-tool-block-pending)
                     ('success 'pi-coding-agent-tool-block-success)
                     ('error 'pi-coding-agent-tool-block-error)
                     (_ nil)))
      ;; Remove our properties
      (overlay-put ov 'line-prefix nil)
      (overlay-put ov 'wrap-prefix nil)
      (overlay-put ov 'hyalo-agent-state nil)
      (overlay-put ov 'hyalo-agent-processed nil))))

(defun hyalo-agent--update-block-overlays ()
  "Update all pi-coding-agent tool-block overlays in current buffer."
  (when (and (derived-mode-p 'pi-coding-agent-chat-mode)
             (bound-and-true-p hyalo-agent-mode))
    (save-excursion
      (dolist (ov (overlays-in (point-min) (point-max)))
        (when (overlay-get ov 'pi-coding-agent-tool-block)
          ;; Determine state from face or stored state
          (let* ((current-face (overlay-get ov 'face))
                 (stored-state (overlay-get ov 'hyalo-agent-state))
                 (state (or (hyalo-agent--face-to-state current-face)
                            stored-state
                            'pending)))
            (hyalo-agent--apply-indicator ov state)))))))

(defun hyalo-agent--remove-all-indicators ()
  "Remove indicators from all tool-block overlays in current buffer."
  (save-excursion
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'hyalo-agent-processed)
        (hyalo-agent--remove-indicator ov)))))

;;; Change Tracking

(defvar-local hyalo-agent--update-pending nil
  "Non-nil when an overlay update is pending.")

(defun hyalo-agent--schedule-update ()
  "Schedule an overlay update for next idle time."
  (unless hyalo-agent--update-pending
    (setq hyalo-agent--update-pending t)
    (run-with-idle-timer 0.05 nil
                         (lambda (buf)
                           (when (buffer-live-p buf)
                             (with-current-buffer buf
                               (setq hyalo-agent--update-pending nil)
                               (hyalo-agent--update-block-overlays))))
                         (current-buffer))))

(defun hyalo-agent--after-change (_beg _end _len)
  "Handle buffer changes by scheduling overlay update."
  (when (bound-and-true-p hyalo-agent-mode)
    (hyalo-agent--schedule-update)))

;;; Theme Change Handling

(defun hyalo-agent--on-theme-change (&rest _)
  "Handle theme change: refresh indicators in all pi-coding-agent buffers."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (derived-mode-p 'pi-coding-agent-chat-mode)
                 (bound-and-true-p hyalo-agent-mode))
        (hyalo-agent--update-block-overlays)))))

;;; Mode Definition

(defvar hyalo-agent--global-hooks-enabled nil
  "Non-nil when global hooks are registered.")

(defun hyalo-agent--enable-global-hooks ()
  "Enable global hooks for theme changes."
  (unless hyalo-agent--global-hooks-enabled
    (add-hook 'enable-theme-functions #'hyalo-agent--on-theme-change)
    ;; Support modus-themes custom hook
    (when (boundp 'modus-themes-after-load-theme-hook)
      (add-hook 'modus-themes-after-load-theme-hook
                #'hyalo-agent--on-theme-change))
    (setq hyalo-agent--global-hooks-enabled t)))

(defun hyalo-agent--disable-global-hooks ()
  "Disable global hooks for theme changes."
  (remove-hook 'enable-theme-functions #'hyalo-agent--on-theme-change)
  (when (boundp 'modus-themes-after-load-theme-hook)
    (remove-hook 'modus-themes-after-load-theme-hook
                 #'hyalo-agent--on-theme-change))
  (setq hyalo-agent--global-hooks-enabled nil))

;;;###autoload
(define-minor-mode hyalo-agent-mode
  "Minor mode for visual styling in pi-coding-agent chat buffers.
Adds colored left indicators to tool blocks since backgrounds
are transparent with Hyalo's ns-alpha-glyphs setting."
  :lighter nil
  :group 'hyalo-agent
  (if hyalo-agent-mode
      (progn
        (hyalo-agent--enable-global-hooks)
        ;; Apply indicators
        (hyalo-agent--update-block-overlays)
        ;; Watch for new overlays
        (add-hook 'after-change-functions
                  #'hyalo-agent--after-change nil t))
    ;; Disable
    (hyalo-agent--remove-all-indicators)
    (remove-hook 'after-change-functions
                 #'hyalo-agent--after-change t)))

;;;###autoload
(defun hyalo-agent-setup ()
  "Set up hyalo-agent integration.
Call this after pi-coding-agent is loaded."
  (add-hook 'pi-coding-agent-chat-mode-hook #'hyalo-agent-mode))

(provide 'hyalo-agent)
;;; hyalo-agent.el ends here
