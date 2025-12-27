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
;; - Theme switching based on appearance (only in auto mode)
;; - Frame transparency (alpha-background)
;; - Window vibrancy and opacity synchronization
;;
;; Terminology:
;; - Vibrancy: The blur material (ultraThin, thin, regular, thick, ultraThick, none)
;; - Opacity: How much the theme color shows (0.0 = full vibrancy, 1.0 = solid color)
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
  "Theme to use for light appearance (only used in auto mode).
If nil, no theme change is made."
  :type 'symbol
  :group 'hyalo-module-appearance)

(defcustom hyalo-module-appearance-theme-dark nil
  "Theme to use for dark appearance (only used in auto mode).
If nil, no theme change is made."
  :type 'symbol
  :group 'hyalo-module-appearance)

(defcustom hyalo-module-appearance-vibrancy-material "ultraThin"
  "Vibrancy material style for the blur effect.
Available options:
- \"ultraThin\": Maximum see-through effect (recommended)
- \"thin\": Slight blur with good transparency
- \"regular\": Balanced blur and transparency
- \"thick\": More blur, less transparency
- \"ultraThick\": Maximum blur, minimal transparency
- \"none\": No vibrancy (solid background)"
  :type '(choice (const :tag "Ultra Thin (most transparent)" "ultraThin")
                 (const :tag "Thin" "thin")
                 (const :tag "Regular" "regular")
                 (const :tag "Thick" "thick")
                 (const :tag "Ultra Thick (least transparent)" "ultraThick")
                 (const :tag "None (no vibrancy)" "none"))
  :group 'hyalo-module-appearance
  :set (lambda (sym val)
         (set-default sym val)
         (when (and (boundp 'hyalo-module-appearance-mode)
                    hyalo-module-appearance-mode)
           (hyalo-module-appearance--apply-vibrancy))))

(defcustom hyalo-module-appearance-opacity 0.5
  "Tint opacity (0.0-1.0).
0.0 = full vibrancy (no tint), 1.0 = solid theme color.
This value is shared across light and dark appearances."
  :type 'number
  :group 'hyalo-module-appearance
  :set (lambda (sym val)
         (set-default sym val)
         (when (and (boundp 'hyalo-module-appearance-mode)
                    hyalo-module-appearance-mode)
           (hyalo-module-appearance--apply-vibrancy))))

;;; Appearance Detection

(defun hyalo-module-appearance--get-system-appearance ()
  "Query the current system appearance.
Returns `light' or `dark'."
  (condition-case nil
      (let ((result (ns-do-applescript
                     "tell application \"System Events\"
                        tell appearance preferences
                          if dark mode then return \"dark\"
                          else return \"light\"
                        end if
                      end tell
                    end tell")))
        (if (string= (string-trim result) "dark") 'dark 'light))
    (error 'light)))

(defun hyalo-module-appearance-current ()
  "Return the effective appearance as `light' or `dark'.
Based on `hyalo-module-appearance-mode-setting'.
If mode is `auto', queries the system appearance."
  (pcase hyalo-module-appearance-mode-setting
    ('light 'light)
    ('dark 'dark)
    ('auto (hyalo-module-appearance--get-system-appearance))
    (_ 'light)))

(defun hyalo-module-appearance-dark-p ()
  "Return non-nil if current appearance is dark."
  (eq (hyalo-module-appearance-current) 'dark))

;;; Core Application Functions

(defun hyalo-module-appearance--apply-vibrancy ()
  "Apply vibrancy and opacity settings to Swift.
This is the core function for applying visual settings."
  (when (hyalo-module-available-p)
    ;; Apply vibrancy material (blur)
    (when (fboundp 'hyalo-sidebar-set-vibrancy-material)
      (hyalo-sidebar-set-vibrancy-material hyalo-module-appearance-vibrancy-material))
    ;; Apply opacity (tint layer) with current background color
    (let* ((is-dark (hyalo-module-appearance-dark-p))
           (bg (or (face-background 'default nil 'default)
                   (if is-dark "#282c34" "#fafafa")))
           (hex-color (if (string-prefix-p "#" bg)
                          bg
                        (apply #'format "#%02x%02x%02x"
                               (mapcar (lambda (c) (/ c 256))
                                       (color-values bg))))))
      (when (fboundp 'hyalo-sidebar-set-background-color)
        (hyalo-sidebar-set-background-color hex-color (float hyalo-module-appearance-opacity))))))

(defun hyalo-module-appearance--apply-window-appearance (appearance)
  "Apply window APPEARANCE to Swift side."
  (when (hyalo-module-available-p)
    (when (fboundp 'hyalo-sidebar-set-window-appearance)
      (hyalo-sidebar-set-window-appearance (symbol-name appearance)))))

(defun hyalo-module-appearance--apply-frame-settings ()
  "Apply frame-level settings (alpha-background)."
  (dolist (f (frame-list))
    (when (and (display-graphic-p f) (eq (framep f) 'ns))
      (set-frame-parameter f 'alpha-background 0.0))))

;;; Face Background Cleanup

(defun hyalo-module-appearance--clear-backgrounds (&optional _theme)
  "Clear face backgrounds that should be transparent.
Can be used as a hook function (ignores THEME argument)."
  (set-face-background 'fringe nil)
  (when (facep 'line-number)
    (set-face-background 'line-number nil))
  (when (facep 'line-number-current-line)
    (set-face-background 'line-number-current-line nil)))

;;; Hook Functions

(defun hyalo-module-appearance--on-system-change (appearance)
  "Handle system appearance change to APPEARANCE.
APPEARANCE is `light' or `dark' as provided by `ns-system-appearance-change-functions'.
Only reacts if `hyalo-module-appearance-mode-setting' is `auto'."
  (when (eq hyalo-module-appearance-mode-setting 'auto)
    ;; Load appropriate theme (only in auto mode)
    (let ((theme (if (eq appearance 'dark)
                     hyalo-module-appearance-theme-dark
                   hyalo-module-appearance-theme-light)))
      (when theme
        (mapc #'disable-theme custom-enabled-themes)
        (load-theme theme t)))
    ;; Update window appearance on Swift side
    (hyalo-module-appearance--apply-window-appearance appearance)
    ;; Reapply vibrancy with new theme colors
    (hyalo-module-appearance--apply-vibrancy)))

(defun hyalo-module-appearance--on-theme-load (_theme)
  "Handle theme load event. Clear backgrounds and sync colors."
  (hyalo-module-appearance--clear-backgrounds)
  ;; Sync vibrancy with new theme colors
  (hyalo-module-appearance--apply-vibrancy))

;;; Public API

(defun hyalo-module-appearance-set (appearance)
  "Set appearance to APPEARANCE globally.
APPEARANCE should be `light', `dark', or `auto'.
Only changes window appearance mode. Does NOT change vibrancy/opacity.
Theme switching only occurs in `auto' mode."
  (interactive
   (list (intern (completing-read "Appearance: " '("light" "dark" "auto") nil t))))
  (setq hyalo-module-appearance-mode-setting appearance)
  ;; Apply frame settings
  (hyalo-module-appearance--apply-frame-settings)
  ;; Get effective appearance
  (let ((effective (hyalo-module-appearance-current)))
    ;; Apply theme only in auto mode
    (when (eq appearance 'auto)
      (let ((theme (if (eq effective 'dark)
                       hyalo-module-appearance-theme-dark
                     hyalo-module-appearance-theme-light)))
        (when theme
          (mapc #'disable-theme custom-enabled-themes)
          (load-theme theme t))))
    ;; Update Swift window appearance
    (hyalo-module-appearance--apply-window-appearance effective)
    ;; Sync vibrancy with current theme colors (but same opacity/material)
    (hyalo-module-appearance--apply-vibrancy)
    ;; Sync appearance mode to Swift panel
    (when (and (hyalo-module-available-p) (fboundp 'hyalo-set-panel-appearance-mode))
      (hyalo-set-panel-appearance-mode (symbol-name appearance))))
  (hyalo-module-log "Appearance set to %s" appearance))

(defun hyalo-module-appearance-sync-from-panel ()
  "Sync appearance mode from Swift panel to Emacs.
Call this to pull the current panel setting into Emacs."
  (interactive)
  (when (and (hyalo-module-available-p) (fboundp 'hyalo-get-panel-appearance-mode))
    (let ((mode (intern (hyalo-get-panel-appearance-mode))))
      (unless (eq mode hyalo-module-appearance-mode-setting)
        (hyalo-module-appearance-set mode)))))

(defun hyalo-module-appearance-set-opacity (opacity)
  "Set tint OPACITY (0.0-1.0).
0.0 = full vibrancy (no tint), 1.0 = solid theme color.
Applies to all frames without changing the theme."
  (interactive
   (list (read-number (format "Opacity (0.0-1.0) [current: %.2f]: "
                              hyalo-module-appearance-opacity)
                      hyalo-module-appearance-opacity)))
  (let ((clamped (max 0.0 (min 1.0 opacity))))
    (setq hyalo-module-appearance-opacity clamped)
    (hyalo-module-appearance--apply-vibrancy)
    (when (and (hyalo-module-available-p) (fboundp 'hyalo-refresh-appearance-panel))
      (hyalo-refresh-appearance-panel))
    (hyalo-module-log "Opacity set to %.2f" clamped)))

(defun hyalo-module-appearance-set-vibrancy (material)
  "Set vibrancy MATERIAL style.
MATERIAL is one of: ultraThin, thin, regular, thick, ultraThick, none.
Use ultraThin for maximum see-through effect."
  (interactive
   (list (completing-read "Vibrancy material: "
                          '("ultraThin" "thin" "regular" "thick" "ultraThick" "none")
                          nil t nil nil hyalo-module-appearance-vibrancy-material)))
  (setq hyalo-module-appearance-vibrancy-material material)
  (hyalo-module-appearance--apply-vibrancy)
  (when (and (hyalo-module-available-p) (fboundp 'hyalo-refresh-appearance-panel))
    (hyalo-refresh-appearance-panel))
  (customize-save-variable 'hyalo-module-appearance-vibrancy-material material)
  (hyalo-module-log "Vibrancy material set to %s" material))

(defun hyalo-module-appearance-show-panel ()
  "Show the Hyalo appearance panel with sliders for vibrancy control."
  (interactive)
  (when (hyalo-module-available-p)
    (hyalo-show-appearance-panel)))

(defun hyalo-module-appearance-toggle-panel ()
  "Toggle the Hyalo appearance panel visibility."
  (interactive)
  (when (hyalo-module-available-p)
    (hyalo-toggle-appearance-panel)))

(defun hyalo-module-appearance-hide-panel ()
  "Hide the Hyalo appearance panel."
  (interactive)
  (when (hyalo-module-available-p)
    (hyalo-hide-appearance-panel)))

;;; Panel Sync Timer

(defvar hyalo-module-appearance--sync-timer nil
  "Timer for syncing appearance mode from Swift panel.")

(defun hyalo-module-appearance--start-sync-timer ()
  "Start the timer to sync appearance mode from Swift panel."
  (when hyalo-module-appearance--sync-timer
    (cancel-timer hyalo-module-appearance--sync-timer))
  (setq hyalo-module-appearance--sync-timer
        (run-with-idle-timer 0.5 t #'hyalo-module-appearance-sync-from-panel)))

(defun hyalo-module-appearance--stop-sync-timer ()
  "Stop the panel sync timer."
  (when hyalo-module-appearance--sync-timer
    (cancel-timer hyalo-module-appearance--sync-timer)
    (setq hyalo-module-appearance--sync-timer nil)))

;;; Mode Definition

(defun hyalo-module-appearance--setup-frame (&optional frame)
  "Apply current appearance settings to FRAME (or current frame)."
  (let ((f (or frame (selected-frame))))
    (when (and (display-graphic-p f) (eq (framep f) 'ns))
      (set-frame-parameter f 'alpha-background 0.0)
      (when (hyalo-module-available-p)
        (hyalo-module-appearance--apply-window-appearance
         (hyalo-module-appearance-current))))))

(defun hyalo-module-appearance--enable ()
  "Enable appearance management."
  (condition-case err
      (progn
        ;; Apply frame settings
        (hyalo-module-appearance--apply-frame-settings)
        ;; Get current appearance
        (let ((appearance (hyalo-module-appearance-current)))
          ;; Load theme if in auto mode
          (when (eq hyalo-module-appearance-mode-setting 'auto)
            (let ((theme (if (eq appearance 'dark)
                             hyalo-module-appearance-theme-dark
                           hyalo-module-appearance-theme-light)))
              (when theme
                (mapc #'disable-theme custom-enabled-themes)
                (load-theme theme t))))
          ;; Apply window appearance to Swift
          (hyalo-module-appearance--apply-window-appearance appearance)
          ;; Sync appearance mode to Swift panel
          (when (and (hyalo-module-available-p) (fboundp 'hyalo-set-panel-appearance-mode))
            (hyalo-set-panel-appearance-mode
             (symbol-name hyalo-module-appearance-mode-setting))))
        ;; Apply vibrancy/opacity settings
        (hyalo-module-appearance--apply-vibrancy)
        ;; Clear backgrounds
        (hyalo-module-appearance--clear-backgrounds)
        ;; Register hooks
        (when (boundp 'ns-system-appearance-change-functions)
          (add-hook 'ns-system-appearance-change-functions
                    #'hyalo-module-appearance--on-system-change))
        (when (boundp 'enable-theme-functions)
          (add-hook 'enable-theme-functions #'hyalo-module-appearance--on-theme-load))
        ;; Multi-frame support
        (add-hook 'after-make-frame-functions #'hyalo-module-appearance--setup-frame)
        ;; Start panel sync timer
        (hyalo-module-appearance--start-sync-timer)
        (hyalo-module-log "Appearance: Enabled (appearance: %s, opacity: %.2f, vibrancy: %s)"
                 (hyalo-module-appearance-current)
                 hyalo-module-appearance-opacity
                 hyalo-module-appearance-vibrancy-material))
    (error (hyalo-module-log "Appearance: Failed to enable: %s" err))))

(defun hyalo-module-appearance--disable ()
  "Disable appearance management."
  ;; Stop panel sync timer
  (hyalo-module-appearance--stop-sync-timer)
  ;; Remove hooks
  (when (boundp 'ns-system-appearance-change-functions)
    (remove-hook 'ns-system-appearance-change-functions
                 #'hyalo-module-appearance--on-system-change))
  (when (boundp 'enable-theme-functions)
    (remove-hook 'enable-theme-functions #'hyalo-module-appearance--on-theme-load))
  (remove-hook 'after-make-frame-functions #'hyalo-module-appearance--setup-frame)
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
