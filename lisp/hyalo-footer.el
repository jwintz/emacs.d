;;; hyalo-footer.el --- Footer pattern for echo area -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: Julien Wintz <julien.wintz@inria.fr>
;; Version: 1.0.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: frames, faces, macos

;;; Commentary:

;; Provides a decorative pattern layer at the bottom of the Hyalo window,
;; covering the echo area/minibuffer region.  The pattern is drawn using
;; the theme's background color, adjusted for visibility.
;;
;; The footer has two layers:
;; - Background tint: makes dark themes darker, light themes lighter
;; - Pattern foreground: draws the decorative pattern
;;
;; The footer height is dynamically tracked via hooks and advices (no timers).
;;
;; Available patterns from heropatterns.com:
;; - hideout, hexagons, death-star, bathroom-floor, tiny-checkers
;; - plus, cage, diagonal-stripes, stripes, diagonal-lines
;; - polka-dots, signal, wallpaper
;;
;; Usage:
;;   (require 'hyalo-footer)
;;   (hyalo-footer-mode 1)
;;   (hyalo-footer-set-pattern "hexagons")

;;; Code:

(require 'hyalo)

(defgroup hyalo-footer nil
  "Footer pattern settings for hyalo."
  :group 'hyalo
  :prefix "hyalo-footer-")

(defcustom hyalo-footer-pattern "none"
  "Pattern to display in the footer (echo area overlay).
Available patterns:
  - \"none\" (disabled)
  - \"hideout\" (X marks)
  - \"hexagons\"
  - \"death-star\" (circles)
  - \"bathroom-floor\" (tiles)
  - \"tiny-checkers\"
  - \"plus\" (plus signs)
  - \"cage\" (grid)
  - \"diagonal-stripes\"
  - \"stripes\" (horizontal)
  - \"diagonal-lines\"
  - \"polka-dots\"
  - \"signal\" (waves)
  - \"wallpaper\" (floral)"
  :type '(choice (const :tag "None (disabled)" "none")
                 (const :tag "Hideout (X marks)" "hideout")
                 (const :tag "Hexagons" "hexagons")
                 (const :tag "Death Star (circles)" "death-star")
                 (const :tag "Bathroom Floor (tiles)" "bathroom-floor")
                 (const :tag "Tiny Checkers" "tiny-checkers")
                 (const :tag "Plus (+ signs)" "plus")
                 (const :tag "Cage (grid)" "cage")
                 (const :tag "Diagonal Stripes" "diagonal-stripes")
                 (const :tag "Stripes (horizontal)" "stripes")
                 (const :tag "Diagonal Lines" "diagonal-lines")
                 (const :tag "Polka Dots" "polka-dots")
                 (const :tag "Signal (waves)" "signal")
                 (const :tag "Wallpaper (floral)" "wallpaper"))
  :group 'hyalo-footer
  :set (lambda (sym val)
         (set-default sym val)
         (when (fboundp 'hyalo-footer--apply-pattern)
           (hyalo-footer--apply-pattern))))

(defcustom hyalo-footer-background-alpha 0.2
  "Alpha for the footer background tint (0.0-1.0).
Makes dark themes darker, light themes lighter."
  :type 'number
  :group 'hyalo-footer
  :set (lambda (sym val)
         (set-default sym val)
         (when (fboundp 'hyalo-footer--apply-background-alpha)
           (hyalo-footer--apply-background-alpha))))

(defcustom hyalo-footer-pattern-alpha 0.025
  "Alpha for the footer pattern foreground (0.0-1.0).
Controls the visibility of the decorative pattern."
  :type 'number
  :group 'hyalo-footer
  :set (lambda (sym val)
         (set-default sym val)
         (when (fboundp 'hyalo-footer--apply-pattern-alpha)
           (hyalo-footer--apply-pattern-alpha))))

;;; API Functions

(defun hyalo-footer-set-pattern (pattern)
  "Set the footer PATTERN and apply it immediately.
Use this instead of `setq' to ensure the pattern is applied."
  (interactive
   (list (completing-read "Pattern: "
                          '("none" "hideout" "hexagons" "death-star"
                            "bathroom-floor" "tiny-checkers" "plus" "cage"
                            "diagonal-stripes" "stripes" "diagonal-lines"
                            "polka-dots" "signal" "wallpaper")
                          nil t)))
  (setq hyalo-footer-pattern pattern)
  (hyalo-footer--apply-pattern))

(defun hyalo-footer-set-background-alpha (alpha)
  "Set the footer background ALPHA (0.0-1.0) and apply it immediately."
  (interactive "nBackground Alpha (0.0-1.0): ")
  (setq hyalo-footer-background-alpha alpha)
  (hyalo-footer--apply-background-alpha))

(defun hyalo-footer-set-pattern-alpha (alpha)
  "Set the footer pattern ALPHA (0.0-1.0) and apply it immediately."
  (interactive "nPattern Alpha (0.0-1.0): ")
  (setq hyalo-footer-pattern-alpha alpha)
  (hyalo-footer--apply-pattern-alpha))

(defun hyalo-footer--apply-pattern ()
  "Apply the current footer pattern to Swift."
  (when (and (hyalo-available-p)
             (fboundp 'hyalo-set-footer-pattern)
             (boundp 'hyalo-footer-mode)
             hyalo-footer-mode)
    (hyalo-log "Footer: Setting pattern to '%s'" hyalo-footer-pattern)
    (hyalo-set-footer-pattern hyalo-footer-pattern)))

(defun hyalo-footer--apply-background-alpha ()
  "Apply the current footer background alpha to Swift."
  (when (and (hyalo-available-p)
             (fboundp 'hyalo-set-footer-background-alpha)
             (boundp 'hyalo-footer-mode)
             hyalo-footer-mode)
    (hyalo-log "Footer: Setting background alpha to %s" hyalo-footer-background-alpha)
    (hyalo-set-footer-background-alpha hyalo-footer-background-alpha)))

(defun hyalo-footer--apply-pattern-alpha ()
  "Apply the current footer pattern alpha to Swift."
  (when (and (hyalo-available-p)
             (fboundp 'hyalo-set-footer-pattern-alpha)
             (boundp 'hyalo-footer-mode)
             hyalo-footer-mode)
    (hyalo-log "Footer: Setting pattern alpha to %s" hyalo-footer-pattern-alpha)
    (hyalo-set-footer-pattern-alpha hyalo-footer-pattern-alpha)))

;;; Internal State

(defvar hyalo-footer--last-height 0
  "Last known echo area height in pixels.")

(defvar hyalo-footer--resize-timer nil
  "Timer for debouncing resize updates.")

;;; Echo Area Height Tracking

(defun hyalo-footer--echo-area-height ()
  "Return the height of the minibuffer/echo area window in pixels.
This dynamically adapts to the current minibuffer height."
  (let ((mb-win (minibuffer-window)))
    (if (and mb-win (window-live-p mb-win))
        (window-pixel-height mb-win)
      0)))

(defun hyalo-footer--update-height ()
  "Update the footer height based on current echo area size."
  (when (and (hyalo-available-p)
             (fboundp 'hyalo-set-footer-height))
    (let ((height (hyalo-footer--echo-area-height)))
      (unless (= height hyalo-footer--last-height)
        (setq hyalo-footer--last-height height)
        (hyalo-set-footer-height height)))))

(defun hyalo-footer--schedule-update ()
  "Schedule a height update (debounced to avoid excessive calls)."
  ;; Cancel any pending timer
  (when hyalo-footer--resize-timer
    (cancel-timer hyalo-footer--resize-timer))
  ;; Schedule immediate update on next event loop
  (setq hyalo-footer--resize-timer
        (run-at-time 0 nil #'hyalo-footer--update-height)))

;;; Hook Functions

(defun hyalo-footer--on-minibuffer-setup ()
  "Called when minibuffer is activated."
  (hyalo-footer--update-height))

(defun hyalo-footer--on-minibuffer-exit ()
  "Called when minibuffer is deactivated."
  (hyalo-footer--update-height))

(defun hyalo-footer--on-window-config-change ()
  "Called when window configuration changes."
  (hyalo-footer--update-height))

(defun hyalo-footer--on-echo-area-clear ()
  "Called when echo area is cleared."
  (hyalo-footer--update-height))

(defun hyalo-footer--on-post-command ()
  "Called after every command to track minibuffer size changes."
  (when (minibufferp)
    (hyalo-footer--update-height)))

;;; Advice Functions

(defun hyalo-footer--after-resize-mini-window (&rest _args)
  "Advice after `resize-mini-window' to track echo area size changes."
  (hyalo-footer--schedule-update))

(defun hyalo-footer--after-message (&rest _args)
  "Advice after `message' to track echo area content changes."
  (hyalo-footer--schedule-update))

;;; Mode Definition

(defun hyalo-footer--enable ()
  "Enable footer pattern mode."
  (when (hyalo-available-p)
    ;; Apply current settings
    (when (fboundp 'hyalo-set-footer-pattern)
      (hyalo-set-footer-pattern hyalo-footer-pattern))
    (when (fboundp 'hyalo-set-footer-background-alpha)
      (hyalo-set-footer-background-alpha hyalo-footer-background-alpha))
    (when (fboundp 'hyalo-set-footer-pattern-alpha)
      (hyalo-set-footer-pattern-alpha hyalo-footer-pattern-alpha))

    ;; Install hooks for height tracking
    (add-hook 'minibuffer-setup-hook #'hyalo-footer--on-minibuffer-setup)
    (add-hook 'minibuffer-exit-hook #'hyalo-footer--on-minibuffer-exit)
    (add-hook 'window-configuration-change-hook #'hyalo-footer--on-window-config-change)
    (add-hook 'echo-area-clear-hook #'hyalo-footer--on-echo-area-clear)
    (add-hook 'post-command-hook #'hyalo-footer--on-post-command)

    ;; Install advice for resize tracking
    (advice-add 'resize-mini-window :after #'hyalo-footer--after-resize-mini-window)
    (advice-add 'message :after #'hyalo-footer--after-message)

    ;; Initial height update
    (hyalo-footer--update-height)
    (hyalo-log "Footer: Enabled with pattern '%s'" hyalo-footer-pattern)))

(defun hyalo-footer--disable ()
  "Disable footer pattern mode."
  ;; Remove hooks
  (remove-hook 'minibuffer-setup-hook #'hyalo-footer--on-minibuffer-setup)
  (remove-hook 'minibuffer-exit-hook #'hyalo-footer--on-minibuffer-exit)
  (remove-hook 'window-configuration-change-hook #'hyalo-footer--on-window-config-change)
  (remove-hook 'echo-area-clear-hook #'hyalo-footer--on-echo-area-clear)
  (remove-hook 'post-command-hook #'hyalo-footer--on-post-command)

  ;; Remove advice
  (advice-remove 'resize-mini-window #'hyalo-footer--after-resize-mini-window)
  (advice-remove 'message #'hyalo-footer--after-message)

  ;; Cancel any pending timer
  (when hyalo-footer--resize-timer
    (cancel-timer hyalo-footer--resize-timer)
    (setq hyalo-footer--resize-timer nil))

  ;; Clear footer by setting height to 0
  (when (and (hyalo-available-p)
             (fboundp 'hyalo-set-footer-height))
    (hyalo-set-footer-height 0))

  (hyalo-log "Footer: Disabled"))

;;;###autoload
(define-minor-mode hyalo-footer-mode
  "Minor mode for footer pattern overlay on the echo area.
When enabled, displays a decorative pattern at the bottom of the
Hyalo window, covering the echo area region."
  :global t
  :lighter " ηFooter"
  :group 'hyalo-footer
  (if hyalo-footer-mode
      (hyalo-footer--enable)
    (hyalo-footer--disable)))

(provide 'hyalo-footer)
;;; hyalo-footer.el ends here
