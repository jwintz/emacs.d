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
  "Theme to use for light appearance.
If nil, no theme change is made."
  :type 'symbol
  :group 'hyalo-module-appearance)

(defcustom hyalo-module-appearance-theme-dark nil
  "Theme to use for dark appearance.
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

(defcustom hyalo-module-appearance-opacity-light 0.5
  "Tint opacity for light appearance (0.0-1.0).
0.0 = full vibrancy (no tint), 1.0 = solid theme color."
  :type 'number
  :group 'hyalo-module-appearance
  :set (lambda (sym val)
         (set-default sym val)
         (when (and (boundp 'hyalo-module-appearance-mode)
                    hyalo-module-appearance-mode)
           (hyalo-module-appearance--apply-vibrancy))))

(defcustom hyalo-module-appearance-opacity-dark 0.5
  "Tint opacity for dark appearance (0.0-1.0).
0.0 = full vibrancy (no tint), 1.0 = solid theme color."
  :type 'number
  :group 'hyalo-module-appearance
  :set (lambda (sym val)
         (set-default sym val)
         (when (and (boundp 'hyalo-module-appearance-mode)
                    hyalo-module-appearance-mode)
           (hyalo-module-appearance--apply-vibrancy))))

;;; Appearance Detection

(defun hyalo-module-appearance-current ()
  "Return the effective appearance as `light' or `dark'.
Based on `hyalo-module-appearance-mode-setting'.
If mode is `auto', queries the system appearance."
  (pcase hyalo-module-appearance-mode-setting
    ('light 'light)
    ('dark 'dark)
    ('auto
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
         (opacity (if is-dark
                      hyalo-module-appearance-opacity-dark
                    hyalo-module-appearance-opacity-light)))
    ;; Apply theme FIRST (so background color is correct)
    (hyalo-module-appearance--apply-theme appearance)
    ;; CRITICAL: Emacs alpha-background is ALWAYS 0 (fully transparent)
    ;; The Swift side handles the actual vibrancy and color tinting
    (dolist (f (frame-list))
      (when (and (display-graphic-p f) (eq (framep f) 'ns))
        (set-frame-parameter f 'alpha-background 0.0)))
    ;; Apply to ALL Swift windows at once
    (when (hyalo-module-available-p)
      ;; Window appearance (vibrancy)
      (when (fboundp 'hyalo-set-window-appearance-all)
        (hyalo-set-window-appearance-all (symbol-name appearance)))
      ;; NavigationSplitView appearance (for sidebar vibrancy update)
      (when (fboundp 'hyalo-sidebar-set-window-appearance)
        (hyalo-sidebar-set-window-appearance (symbol-name appearance)))
      ;; Vibrancy material style
      (when (fboundp 'hyalo-sidebar-set-vibrancy-material)
        (hyalo-sidebar-set-vibrancy-material hyalo-module-appearance-vibrancy-material))
      ;; Background color for NavigationSplitView
      ;; Pass opacity to Swift for tint layer control
      (let* ((bg (or (face-background 'default nil 'default)
                     (if is-dark "#282c34" "#fafafa")))
             (hex-color (if (string-prefix-p "#" bg)
                            bg
                          (apply #'format "#%02x%02x%02x"
                                 (mapcar (lambda (c) (/ c 256))
                                         (color-values bg))))))
        ;; Pass opacity to Swift (Swift uses this as backgroundAlpha for tint layer)
        (when (fboundp 'hyalo-sidebar-set-background-color)
          (hyalo-sidebar-set-background-color hex-color (float opacity)))))))

(defun hyalo-module-appearance--apply-to-frame (&optional frame)
  "Apply current appearance settings to FRAME only."
  (let* ((f (or frame (selected-frame)))
         (appearance (hyalo-module-appearance-current))
         (is-dark (eq appearance 'dark))
         (opacity (if is-dark
                      hyalo-module-appearance-opacity-dark
                    hyalo-module-appearance-opacity-light)))
    (when (and (display-graphic-p f) (eq (framep f) 'ns))
      ;; Emacs always renders fully transparent
      (set-frame-parameter f 'alpha-background 0.0)
      ;; Sync window appearance for vibrancy
      (when (and (hyalo-module-available-p) (fboundp 'hyalo-set-window-appearance))
        (hyalo-set-window-appearance (symbol-name appearance)))
      ;; Update background color for tint overlay
      (hyalo-module-appearance--sync-background-color opacity))))

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

(defun hyalo-module-appearance--sync-background-color (&optional opacity)
  "Send the current frame background color to Swift for tint overlay and sidebar.
OPACITY is the tint opacity to pass to Swift (Emacs uses 0)."
  (when (hyalo-module-available-p)
    (let* ((is-dark (hyalo-module-appearance-dark-p))
           (bg (or (face-background 'default nil 'default)
                   (if is-dark "#282c34" "#fafafa")))
           ;; Use passed opacity or calculate from settings
           (swift-opacity (or opacity
                              (if is-dark
                                  hyalo-module-appearance-opacity-dark
                                hyalo-module-appearance-opacity-light)))
           ;; Convert color to hex if it is a name
           (hex-color (if (string-prefix-p "#" bg)
                          bg
                        (apply #'format "#%02x%02x%02x"
                               (mapcar (lambda (c) (/ c 256))
                                       (color-values bg))))))
      ;; Pass opacity to Swift for tint layer control
      (when (fboundp 'hyalo-sidebar-set-background-color)
        (hyalo-sidebar-set-background-color hex-color (float swift-opacity))))))

(defun hyalo-module-appearance--sync-to-window ()
  "Sync all appearance settings to the current frame's Swift window.
Called when a new frame is created to ensure overlays match appearance."
  (when (hyalo-module-available-p)
    (let ((appearance (hyalo-module-appearance-current)))
      ;; Sync window appearance for vibrancy
      (when (fboundp 'hyalo-set-window-appearance)
        (hyalo-set-window-appearance (symbol-name appearance)))
      ;; Sync background color for tint overlay
      (hyalo-module-appearance--sync-background-color))))

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

(defun hyalo-module-appearance-set-opacity (opacity)
  "Set current appearance's tint OPACITY (0.0-1.0).
0.0 = full vibrancy (no tint), 1.0 = solid theme color.
Applies to all frames."
  (interactive
   (let ((current (if (hyalo-module-appearance-dark-p)
                      hyalo-module-appearance-opacity-dark
                    hyalo-module-appearance-opacity-light)))
     (list (read-number (format "Opacity (0.0-1.0) [current: %.2f]: " current) current))))
  (let ((clamped (max 0.0 (min 1.0 opacity))))
    ;; Update the stored value
    (if (hyalo-module-appearance-dark-p)
        (setq hyalo-module-appearance-opacity-dark clamped)
      (setq hyalo-module-appearance-opacity-light clamped))
    ;; Apply via the --apply function which handles Swift
    (hyalo-module-appearance--apply)
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
  (when (and (hyalo-module-available-p)
             (fboundp 'hyalo-sidebar-set-vibrancy-material))
    (hyalo-sidebar-set-vibrancy-material material))
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

(defun hyalo-module-appearance--apply-vibrancy ()
  "Apply vibrancy and opacity settings to Swift."
  (when (hyalo-module-available-p)
    ;; Apply vibrancy material (blur)
    (when (fboundp 'hyalo-sidebar-set-vibrancy-material)
      (hyalo-sidebar-set-vibrancy-material hyalo-module-appearance-vibrancy-material))
    ;; Apply opacity (tint layer)
    (let* ((is-dark (hyalo-module-appearance-dark-p))
           (opacity (if is-dark
                        hyalo-module-appearance-opacity-dark
                      hyalo-module-appearance-opacity-light))
           (bg (or (face-background 'default nil 'default)
                   (if is-dark "#282c34" "#fafafa")))
           (hex-color (if (string-prefix-p "#" bg)
                          bg
                        (apply #'format "#%02x%02x%02x"
                               (mapcar (lambda (c) (/ c 256))
                                       (color-values bg))))))
      (when (fboundp 'hyalo-sidebar-set-background-color)
        (hyalo-sidebar-set-background-color hex-color (float opacity))))))

;;; Mode Definition

(defun hyalo-module-appearance--setup-frame (&optional frame)
  "Apply current appearance settings to FRAME (or current frame).
Uses the global `hyalo-module-appearance-mode-setting'."
  (let ((f (or frame (selected-frame))))
    (when (and (display-graphic-p f) (eq (framep f) 'ns))
      (hyalo-module-appearance--apply-to-frame f))))

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
        (hyalo-module-log "Appearance: Enabled (appearance: %s, opacity: %.2f)"
                 (hyalo-module-appearance-current)
                 (if (hyalo-module-appearance-dark-p)
                     hyalo-module-appearance-opacity-dark
                   hyalo-module-appearance-opacity-light)))
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
