;;; hyalo-module-appearance.el --- Appearance management for hyalo-module -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: Julien Wintz <julien.wintz@inria.fr>
;; Version: 1.0.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: frames, faces, macos

;;; Commentary:

;; Appearance management for hyalo-module.
;; Handles:
;; - System appearance detection (light/dark)
;; - Theme switching based on appearance
;; - Frame transparency (alpha-background)
;; - Window vibrancy appearance synchronization
;;
;; Uses deterministic hooks (no timers):
;; - ns-system-appearance-change-functions for macOS appearance changes
;; - enable-theme-functions for theme load events
;;
;; Usage:
;;   (require 'hyalo-module-appearance)
;;   (hyalo-module-appearance-mode 1)

;;; Code:

(require 'hyalo-module)

(defgroup hyalo-module-appearance nil
  "Appearance settings for hyalo-module."
  :group 'hyalo-module
  :prefix "hyalo-module-appearance-")

(defcustom hyalo-module-appearance-mode-setting 'auto
  "Window and theme appearance mode.
Set to `auto' to follow system appearance, or `light'/`dark' to override."
  :type '(choice (const :tag "Follow system" auto)
                 (const :tag "Light" light)
                 (const :tag "Dark" dark))
  :group 'hyalo-module-appearance)

(defcustom hyalo-module-appearance-theme-light nil
  "Theme to use for light appearance.
If nil, no theme change is made."
  :type 'symbol
  :group 'hyalo-module-appearance)

(defcustom hyalo-module-appearance-theme-dark nil
  "Theme to use for dark appearance.
If nil, no theme change is made."
  :type 'symbol
  :group 'hyalo-module-appearance)

(defcustom hyalo-module-appearance-alpha-light 0.75
  "Background transparency for light appearance.
Range: 0.0 (fully transparent) to 1.0 (fully opaque)."
  :type 'number
  :group 'hyalo-module-appearance)

(defcustom hyalo-module-appearance-alpha-dark 0.75
  "Background transparency for dark appearance.
Range: 0.0 (fully transparent) to 1.0 (fully opaque)."
  :type 'number
  :group 'hyalo-module-appearance)

(defcustom hyalo-module-appearance-echo-tint-opacity 0.25
  "Echo area tint overlay opacity.
Range: 0.0 (invisible) to 1.0 (fully opaque).
Light themes get a white tint, dark themes get a black tint.
This is independent of alpha-background."
  :type 'number
  :group 'hyalo-module-appearance)

;;; Appearance Detection

(defun hyalo-module-appearance-current ()
  "Return the effective appearance as `light' or `dark'.
Based on `hyalo-module-appearance-mode-setting'.
If mode is `auto', queries the system appearance."
  (pcase hyalo-module-appearance-mode-setting
    (`light 'light)
    (`dark 'dark)
    (`auto
     (if (and (hyalo-module-available-p) (fboundp 'hyalo-get-system-appearance))
         (let ((sys (hyalo-get-system-appearance)))
           (if (string= sys "dark") 'dark 'light))
       'light))
    (_ 'light)))

(defun hyalo-module-appearance-dark-p ()
  "Return non-nil if current appearance is dark."
  (eq (hyalo-module-appearance-current) 'dark))

;;; Theme and Transparency Application

(defun hyalo-module-appearance--apply ()
  "Apply current appearance settings to all frames and windows."
  (let* ((appearance (hyalo-module-appearance-current))
         (is-dark (eq appearance 'dark))
         (alpha (if is-dark
                    hyalo-module-appearance-alpha-dark
                  hyalo-module-appearance-alpha-light)))
    ;; Apply theme FIRST (so background color is correct)
    (hyalo-module-appearance--apply-theme appearance)
    ;; Apply alpha to all Emacs frames
    (dolist (f (frame-list))
      (when (and (display-graphic-p f) (eq (framep f) 'ns))
        (set-frame-parameter f 'alpha-background alpha)))
    ;; Apply to ALL Swift windows at once
    (when (hyalo-module-available-p)
      ;; Window appearance (vibrancy)
      (when (fboundp 'hyalo-set-window-appearance-all)
        (hyalo-set-window-appearance-all (symbol-name appearance)))
      ;; Background color for gradient (now correct after theme applied)
      (when (fboundp 'hyalo-set-background-color-all)
        (let ((bg (face-background 'default nil 'default)))
          (when bg
            (hyalo-set-background-color-all bg))))
      ;; Echo area dark theme (affects tint color)
      (when (fboundp 'hyalo-set-echo-area-dark-theme-all)
        (hyalo-set-echo-area-dark-theme-all is-dark))
      ;; Echo area height
      (when (fboundp 'hyalo-set-echo-area-height-all)
        (let ((height (window-pixel-height (minibuffer-window))))
          (hyalo-set-echo-area-height-all height)))
      ;; Echo area tint opacity
      (when (fboundp 'hyalo-set-echo-area-tint-opacity-all)
        (hyalo-set-echo-area-tint-opacity-all hyalo-module-appearance-echo-tint-opacity)))))

(defun hyalo-module-appearance--apply-to-frame (&optional frame)
  "Apply current appearance settings to FRAME only."
  (let* ((f (or frame (selected-frame)))
         (appearance (hyalo-module-appearance-current))
         (is-dark (eq appearance 'dark))
         (alpha (if is-dark
                    hyalo-module-appearance-alpha-dark
                  hyalo-module-appearance-alpha-light)))
    (when (and (display-graphic-p f) (eq (framep f) 'ns))
      (set-frame-parameter f 'alpha-background alpha)
      ;; Sync window appearance for vibrancy
      (when (and (hyalo-module-available-p) (fboundp 'hyalo-set-window-appearance))
        (hyalo-set-window-appearance (symbol-name appearance)))
      ;; Update background color for gradient overlay
      (hyalo-module-appearance--sync-background-color))))

(defun hyalo-module-appearance--apply-theme (appearance)
  "Apply theme for APPEARANCE.
Switches to the configured theme if set."
  (let* ((is-dark (eq appearance 'dark))
         (theme (if is-dark
                    hyalo-module-appearance-theme-dark
                  hyalo-module-appearance-theme-light)))
    (when theme
      (mapc #'disable-theme custom-enabled-themes)
      (load-theme theme t))))

(defun hyalo-module-appearance--sync-background-color ()
  "Send the current frame background color to Swift for gradient overlay."
  (when (and (hyalo-module-available-p) (fboundp 'hyalo-set-background-color))
    (let ((bg (face-background 'default nil 'default)))
      (when bg
        (hyalo-set-background-color bg)))))

(defun hyalo-module-appearance--sync-to-window ()
  "Sync all appearance settings to the current frame's Swift window.
Called when a new frame is created to ensure overlays match appearance."
  (when (hyalo-module-available-p)
    (let ((appearance (hyalo-module-appearance-current)))
      ;; Sync window appearance for vibrancy
      (when (fboundp 'hyalo-set-window-appearance)
        (hyalo-set-window-appearance (symbol-name appearance)))
      ;; Sync background color for gradient overlay
      (hyalo-module-appearance--sync-background-color)
      ;; Sync echo area
      (hyalo-module-appearance--update-echo-area))))

;;; Face Background Cleanup

(defun hyalo-module-appearance--clear-backgrounds (&optional _theme)
  "Clear face backgrounds that should be transparent.
Can be used as a hook function (ignores THEME argument)."
  ;; Fringe
  (set-face-background 'fringe nil)
  ;; Line numbers
  (when (facep 'line-number)
    (set-face-background 'line-number nil))
  (when (facep 'line-number-current-line)
    (set-face-background 'line-number-current-line nil)))

;;; Hook Functions

(defun hyalo-module-appearance--on-system-change (_appearance)
  "Handle system appearance change.
Only reacts if `hyalo-module-appearance-mode-setting' is `auto'."
  (when (eq hyalo-module-appearance-mode-setting 'auto)
    (hyalo-module-appearance--apply)))

(defun hyalo-module-appearance--on-theme-load (_theme)
  "Handle theme load event. Clear backgrounds and sync colors."
  (hyalo-module-appearance--clear-backgrounds)
  (hyalo-module-appearance--sync-background-color))

;;; Echo Area Tracking

(defun hyalo-module-appearance--update-echo-area ()
  "Send echo area height and tint opacity to Swift module."
  (when (hyalo-module-available-p)
    ;; Update height
    (when (fboundp 'hyalo-set-echo-area-height)
      (let ((height (window-pixel-height (minibuffer-window))))
        (hyalo-set-echo-area-height height)))
    ;; Update tint opacity
    (when (fboundp 'hyalo-set-echo-area-tint-opacity)
      (hyalo-set-echo-area-tint-opacity hyalo-module-appearance-echo-tint-opacity))))

;; Backwards compat alias
(defalias 'hyalo-module-appearance--update-echo-area-height
  #'hyalo-module-appearance--update-echo-area)

;;; Public API

(defun hyalo-module-appearance-set (appearance)
  "Set appearance to APPEARANCE globally.
APPEARANCE should be `light', `dark', or `auto'.
Applies to all frames and switches theme if configured."
  (interactive
   (list (intern (completing-read "Appearance: " '("light" "dark" "auto") nil t))))
  (setq hyalo-module-appearance-mode-setting appearance)
  (hyalo-module-appearance--apply)
  (hyalo-module-log "Appearance set to %s" appearance))

(defun hyalo-module-appearance-set-alpha (alpha)
  "Set current appearance's transparency to ALPHA (0.0-1.0).
Applies to all frames."
  (interactive
   (let ((current (if (hyalo-module-appearance-dark-p)
                      hyalo-module-appearance-alpha-dark
                    hyalo-module-appearance-alpha-light)))
     (list (read-number (format "Alpha (0.0-1.0) [current: %.2f]: " current) current))))
  (let ((clamped (max 0.0 (min 1.0 alpha))))
    ;; Update the stored value
    (if (hyalo-module-appearance-dark-p)
        (setq hyalo-module-appearance-alpha-dark clamped)
      (setq hyalo-module-appearance-alpha-light clamped))
    ;; Apply to all frames
    (dolist (f (frame-list))
      (when (and (display-graphic-p f) (eq (framep f) 'ns))
        (set-frame-parameter f 'alpha-background clamped)))
    (hyalo-module-log "Alpha set to %.2f" clamped)))

;;; Mode Definition

(defun hyalo-module-appearance--setup-frame (&optional frame)
  "Apply current appearance settings to FRAME (or current frame).
Uses the global `hyalo-module-appearance-mode-setting'."
  (let ((f (or frame (selected-frame))))
    (when (and (display-graphic-p f) (eq (framep f) 'ns))
      (hyalo-module-appearance--apply-to-frame f)
      (hyalo-module-appearance--update-echo-area))))

(defun hyalo-module-appearance--enable ()
  "Enable appearance management."
  (condition-case err
      (progn
        ;; Apply to all existing frames
        (hyalo-module-appearance--apply)
        ;; Clear backgrounds
        (hyalo-module-appearance--clear-backgrounds)
        ;; Register hooks
        (when (boundp 'ns-system-appearance-change-functions)
          (setq ns-use-system-appearance t)
          (add-hook 'ns-system-appearance-change-functions
                    #'hyalo-module-appearance--on-system-change))
        (when (boundp 'enable-theme-functions)
          (add-hook 'enable-theme-functions #'hyalo-module-appearance--on-theme-load))
        ;; Multi-frame support - new frames get current settings
        (add-hook 'after-make-frame-functions #'hyalo-module-appearance--setup-frame)
        ;; Echo area tracking
        (add-hook 'minibuffer-setup-hook #'hyalo-module-appearance--update-echo-area)
        (add-hook 'minibuffer-exit-hook #'hyalo-module-appearance--update-echo-area)
        (add-hook 'window-configuration-change-hook #'hyalo-module-appearance--update-echo-area)
        (hyalo-module-appearance--update-echo-area)
        (hyalo-module-log "Appearance: Enabled (appearance: %s, alpha: %.2f)"
                 (hyalo-module-appearance-current)
                 (if (hyalo-module-appearance-dark-p)
                     hyalo-module-appearance-alpha-dark
                   hyalo-module-appearance-alpha-light)))
    (error (hyalo-module-log "Appearance: Failed to enable: %s" err))))

(defun hyalo-module-appearance--disable ()
  "Disable appearance management."
  ;; Remove hooks
  (when (boundp 'ns-system-appearance-change-functions)
    (remove-hook 'ns-system-appearance-change-functions
                 #'hyalo-module-appearance--on-system-change))
  (when (boundp 'enable-theme-functions)
    (remove-hook 'enable-theme-functions #'hyalo-module-appearance--on-theme-load))
  ;; Remove frame hook
  (remove-hook 'after-make-frame-functions #'hyalo-module-appearance--setup-frame)
  ;; Remove echo area hooks
  (remove-hook 'minibuffer-setup-hook #'hyalo-module-appearance--update-echo-area)
  (remove-hook 'minibuffer-exit-hook #'hyalo-module-appearance--update-echo-area)
  (remove-hook 'window-configuration-change-hook #'hyalo-module-appearance--update-echo-area)
  ;; Restore full opacity on all frames
  (dolist (frame (frame-list))
    (set-frame-parameter frame 'alpha-background 1.0))
  (hyalo-module-log "Appearance: Disabled"))

;;;###autoload
(define-minor-mode hyalo-module-appearance-mode
  "Minor mode for hyalo-module appearance management.
When enabled, manages theme and transparency based on system appearance."
  :global t
  :lighter " ηAppearance"
  :group 'hyalo-module-appearance
  (if hyalo-module-appearance-mode
      (hyalo-module-appearance--enable)
    (hyalo-module-appearance--disable)))

(provide 'hyalo-module-appearance)
;;; hyalo-module-appearance.el ends here
