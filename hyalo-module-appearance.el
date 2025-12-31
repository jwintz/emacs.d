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

(defcustom hyalo-module-appearance-current-theme nil
  "Currently active theme, saved when theme is loaded.
Restored at startup when not in auto mode."
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

(defcustom hyalo-module-appearance-alpha-elements
  '(ns-alpha-all)
  "List of Emacs elements to render with transparency.
Available elements:
- `ns-alpha-default': Default face/background
- `ns-alpha-fringe': Fringes and internal border clears
- `ns-alpha-box': Boxed face outlines
- `ns-alpha-stipple': Stipple mask background clears
- `ns-alpha-relief': 3D relief/shadow lines
- `ns-alpha-glyphs': Glyph background fills (hl-line, region, etc.)
- `ns-alpha-all': All elements (shortcut for all above)

Default is `ns-alpha-all' for full transparency."
  :type '(repeat symbol)
  :group 'hyalo-module-appearance
  :set (lambda (sym val)
         (set-default sym val)
         (when (and (boundp 'hyalo-module-appearance-mode)
                    hyalo-module-appearance-mode)
           (hyalo-module-appearance--apply-frame-settings))))

;;; State Variables

(defvar hyalo-module-appearance--initialized nil
  "Non-nil after initial appearance setup is complete.
Used to prevent hooks from overwriting saved values at startup.")

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
      (hyalo-sidebar-set-window-appearance (symbol-name appearance)))
    ;; Also update mini-frame glass effect appearance
    (when (fboundp 'hyalo-minibuffer-update-appearance)
      (hyalo-minibuffer-update-appearance))))

(defun hyalo-module-appearance--apply-frame-settings ()
  "Apply frame-level transparency settings.
Sets `alpha-background' to 0 for vibrancy pass-through and
configures `ns-alpha-elements' for fine-grained transparency control."
  (dolist (f (frame-list))
    (when (and (display-graphic-p f) (eq (framep f) 'ns))
      ;; Set alpha-background for vibrancy layer pass-through
      (set-frame-parameter f 'alpha-background 0.0)
      ;; Set which elements should be transparent
      (set-frame-parameter f 'ns-alpha-elements
                           hyalo-module-appearance-alpha-elements)
      ;; Fringes: 4px each
      (set-frame-parameter f 'left-fringe 4)
      (set-frame-parameter f 'right-fringe 4))))


;;; Face Background Cleanup

(defun hyalo-module-appearance--clear-backgrounds (&optional _theme)
  "Clear face backgrounds that should be transparent.
Can be used as a hook function (ignores THEME argument)."
  ;; (set-face-background 'fringe nil)
  (when (facep 'line-number)
    (set-face-background 'line-number nil))
  (when (facep 'line-number-current-line)
    (set-face-background 'line-number-current-line nil)))

;;; Hook Functions

(defun hyalo-module-appearance--on-system-change (appearance)
  "Handle system appearance change to APPEARANCE.
APPEARANCE is `light' or `dark' as provided by `ns-system-appearance-change-functions'.
Only reacts if appearance mode is `auto'."
  ;; Only proceed if mode is auto
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
    ;; Reapply vibrancy with current Emacs settings (NOT synced from Swift)
    (hyalo-module-appearance--apply-vibrancy)))

(defun hyalo-module-appearance--on-theme-load (theme)
  "Handle theme load event for THEME.
Saves the loaded theme as current-theme and applies tint.
Does NOT alter vibrancy settings."
  ;; Debug: log frame size before/after (check Console.app for [THEME-DEBUG])
  (let ((frame-before (when (frame-live-p (selected-frame))
                        (frame-parameter (selected-frame) 'width))))
    (hyalo-module-log "[THEME-DEBUG] on-theme-load START: theme=%s frame-width=%s"
                      theme frame-before))
  (hyalo-module-appearance--clear-backgrounds)
  (when (hyalo-module-available-p)
    ;; Save the loaded theme as current-theme (if initialized)
    (when (and hyalo-module-appearance--initialized
               theme (symbolp theme))
      (setq hyalo-module-appearance-current-theme theme)
      (customize-save-variable 'hyalo-module-appearance-current-theme theme))
    ;; Apply tint (background color with current opacity) - NOT vibrancy
    (let* ((is-dark (hyalo-module-appearance-dark-p))
           (bg (or (face-background 'default nil 'default)
                   (if is-dark "#282c34" "#fafafa")))
           (hex-color (if (string-prefix-p "#" bg)
                          bg
                        (apply #'format "#%02x%02x%02x"
                               (mapcar (lambda (c) (/ c 256))
                                       (color-values bg))))))
      (hyalo-module-log "[THEME-DEBUG] on-theme-load: calling set-background-color with %s %.2f"
                        hex-color hyalo-module-appearance-opacity)
      (when (fboundp 'hyalo-sidebar-set-background-color)
        (hyalo-sidebar-set-background-color hex-color (float hyalo-module-appearance-opacity)))))
  ;; Debug: log frame size after
  (run-at-time 0.2 nil
               (lambda ()
                 (when (frame-live-p (selected-frame))
                   (hyalo-module-log "[THEME-DEBUG] on-theme-load END (200ms): frame-width=%s"
                                     (frame-parameter (selected-frame) 'width))))))

;;; Public API

(defun hyalo-module-appearance-set (appearance &optional no-save)
  "Set appearance to APPEARANCE globally.
APPEARANCE should be `light', `dark', or `auto'.
Only changes window appearance mode. Does NOT change vibrancy/opacity.
Theme switching only occurs in `auto' mode.
Always saves to custom.el unless NO-SAVE is non-nil."
  (interactive
   (list (intern (completing-read "Appearance: " '("light" "dark" "auto") nil t))))
  (setq hyalo-module-appearance-mode-setting appearance)
  ;; Persist to custom.el by default
  (unless no-save
    (customize-save-variable 'hyalo-module-appearance-mode-setting appearance))
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
  (hyalo-module-log "Appearance set to %s%s" appearance (if no-save "" " (saved)")))

(defun hyalo-module-appearance-sync-from-panel (&optional save)
  "Sync all appearance settings from Swift panel to Emacs.
Call this to pull current panel settings into Emacs variables.
If SAVE is non-nil, also persist the settings."
  (interactive "P")
  (when (hyalo-module-available-p)
    ;; Sync appearance mode
    (when (fboundp 'hyalo-get-panel-appearance-mode)
      (let ((mode (intern (hyalo-get-panel-appearance-mode))))
        (setq hyalo-module-appearance-mode-setting mode)
        (when save
          (customize-save-variable 'hyalo-module-appearance-mode-setting mode))))
    ;; Sync vibrancy material
    (when (fboundp 'hyalo-get-panel-vibrancy-material)
      (let ((material (hyalo-get-panel-vibrancy-material)))
        (setq hyalo-module-appearance-vibrancy-material material)
        (when save
          (customize-save-variable 'hyalo-module-appearance-vibrancy-material material))))
    ;; Sync opacity
    (when (fboundp 'hyalo-get-panel-opacity)
      (let ((opacity (hyalo-get-panel-opacity)))
        (setq hyalo-module-appearance-opacity opacity)
        (when save
          (customize-save-variable 'hyalo-module-appearance-opacity opacity))))))

(defun hyalo-module-appearance-sync-to-panel ()
  "Sync all Emacs appearance settings to the Swift panel.
Pushes vibrancy, opacity, and appearance mode to Swift, then refreshes panel."
  (interactive)
  (when (hyalo-module-available-p)
    ;; Push vibrancy material to Swift controller
    (when (fboundp 'hyalo-sidebar-set-vibrancy-material)
      (hyalo-sidebar-set-vibrancy-material hyalo-module-appearance-vibrancy-material))
    ;; Push opacity to Swift controller (with current background color)
    (let* ((bg (or (face-background 'default nil 'default)
                   (if (hyalo-module-appearance-dark-p) "#282c34" "#fafafa")))
           (hex-color (if (string-prefix-p "#" bg)
                          bg
                        (apply #'format "#%02x%02x%02x"
                               (mapcar (lambda (c) (/ c 256))
                                       (color-values bg))))))
      (when (fboundp 'hyalo-sidebar-set-background-color)
        (hyalo-sidebar-set-background-color hex-color (float hyalo-module-appearance-opacity))))
    ;; Push appearance mode
    (when (fboundp 'hyalo-set-panel-appearance-mode)
      (hyalo-set-panel-appearance-mode (symbol-name hyalo-module-appearance-mode-setting)))
    ;; Refresh panel to display updated values
    (when (fboundp 'hyalo-refresh-appearance-panel)
      (hyalo-refresh-appearance-panel))))

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
    (customize-save-variable 'hyalo-module-appearance-opacity clamped)
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

;;; Panel Timer (active only while panel is open)

(defvar hyalo-module-appearance--panel-timer nil
  "Timer for syncing panel settings while panel is open.")

(defun hyalo-module-appearance--start-panel-timer ()
  "Start timer to sync from panel while it's open."
  (hyalo-module-appearance--stop-panel-timer)
  (setq hyalo-module-appearance--panel-timer
        (run-with-timer 0.3 0.3 #'hyalo-module-appearance--panel-tick)))

(defun hyalo-module-appearance--stop-panel-timer ()
  "Stop the panel sync timer."
  (when (timerp hyalo-module-appearance--panel-timer)
    (cancel-timer hyalo-module-appearance--panel-timer)
    (setq hyalo-module-appearance--panel-timer nil)))

(defun hyalo-module-appearance--panel-tick ()
  "Timer callback - sync from panel if still visible.
Syncs to Emacs variables and saves if values differ."
  (if (and (hyalo-module-available-p)
           (fboundp 'hyalo-appearance-panel-visible-p)
           (hyalo-appearance-panel-visible-p))
      ;; Panel is open - sync from Swift and save if changed
      (hyalo-module-appearance--sync-and-save-if-changed)
    ;; Panel was closed externally - stop timer
    (hyalo-module-appearance--stop-panel-timer)))

(defun hyalo-module-appearance--sync-and-save-if-changed ()
  "Sync from panel and save only values that have changed."
  (when (hyalo-module-available-p)
    ;; Sync and save appearance mode if changed
    (when (fboundp 'hyalo-get-panel-appearance-mode)
      (let ((mode (intern (hyalo-get-panel-appearance-mode))))
        (unless (eq mode hyalo-module-appearance-mode-setting)
          (setq hyalo-module-appearance-mode-setting mode)
          (customize-save-variable 'hyalo-module-appearance-mode-setting mode))))
    ;; Sync and save vibrancy material if changed
    (when (fboundp 'hyalo-get-panel-vibrancy-material)
      (let ((material (hyalo-get-panel-vibrancy-material)))
        (unless (equal material hyalo-module-appearance-vibrancy-material)
          (setq hyalo-module-appearance-vibrancy-material material)
          (customize-save-variable 'hyalo-module-appearance-vibrancy-material material))))
    ;; Sync and save opacity if changed
    (when (fboundp 'hyalo-get-panel-opacity)
      (let ((opacity (hyalo-get-panel-opacity)))
        (unless (equal opacity hyalo-module-appearance-opacity)
          (setq hyalo-module-appearance-opacity opacity)
          (customize-save-variable 'hyalo-module-appearance-opacity opacity))))))

(defun hyalo-module-appearance-show-panel ()
  "Show the Hyalo appearance panel and start sync timer."
  (interactive)
  (when (hyalo-module-available-p)
    (hyalo-module-appearance-sync-to-panel)
    (hyalo-show-appearance-panel)
    (hyalo-module-appearance--start-panel-timer)))

(defun hyalo-module-appearance-toggle-panel ()
  "Toggle the Hyalo appearance panel visibility."
  (interactive)
  (when (hyalo-module-available-p)
    (let ((was-visible (and (fboundp 'hyalo-appearance-panel-visible-p)
                            (hyalo-appearance-panel-visible-p))))
      (if was-visible
          ;; Closing: stop timer, sync and save
          (progn
            (hyalo-module-appearance--stop-panel-timer)
            (hyalo-module-appearance-sync-from-panel t)
            (hyalo-hide-appearance-panel))
        ;; Opening: sync to panel, show, start timer
        (hyalo-module-appearance-sync-to-panel)
        (hyalo-show-appearance-panel)
        (hyalo-module-appearance--start-panel-timer)))))

(defun hyalo-module-appearance-hide-panel ()
  "Hide the Hyalo appearance panel and save settings."
  (interactive)
  (when (hyalo-module-available-p)
    (hyalo-module-appearance--stop-panel-timer)
    (hyalo-module-appearance-sync-from-panel t)
    (hyalo-hide-appearance-panel)))

(defun hyalo-module-appearance-save ()
  "Save current appearance settings from Swift to Emacs custom file.
Use this to persist settings adjusted in the panel."
  (interactive)
  (hyalo-module-appearance-sync-from-panel t)
  (hyalo-module-log "Appearance settings saved"))

(defun hyalo-module-appearance-save-from-panel ()
  "Called from Swift when panel settings change.
Syncs current Swift state to Emacs and saves to custom.el.
This is the callback function that Swift calls via the pending actions mechanism."
  (when (hyalo-module-available-p)
    ;; Sync appearance mode
    (when (fboundp 'hyalo-get-panel-appearance-mode)
      (let ((mode (intern (hyalo-get-panel-appearance-mode))))
        (unless (eq mode hyalo-module-appearance-mode-setting)
          (setq hyalo-module-appearance-mode-setting mode)
          (customize-save-variable 'hyalo-module-appearance-mode-setting mode))))
    ;; Sync vibrancy material
    (when (fboundp 'hyalo-get-panel-vibrancy-material)
      (let ((material (hyalo-get-panel-vibrancy-material)))
        (unless (equal material hyalo-module-appearance-vibrancy-material)
          (setq hyalo-module-appearance-vibrancy-material material)
          (customize-save-variable 'hyalo-module-appearance-vibrancy-material material))))
    ;; Sync opacity
    (when (fboundp 'hyalo-get-panel-opacity)
      (let ((opacity (hyalo-get-panel-opacity)))
        (unless (equal opacity hyalo-module-appearance-opacity)
          (setq hyalo-module-appearance-opacity opacity)
          (customize-save-variable 'hyalo-module-appearance-opacity opacity))))
    ;; Apply the new settings (for appearance mode changes)
    (let ((appearance (hyalo-module-appearance-current)))
      (hyalo-module-appearance--apply-window-appearance appearance)
      (hyalo-module-appearance--apply-vibrancy))
    (hyalo-module-log "Appearance settings saved from panel")))


;;; Mode Definition

(defun hyalo-module-appearance--setup-frame (&optional frame)
  "Apply current appearance settings to FRAME (or current frame)."
  (let ((f (or frame (selected-frame))))
    (when (and (display-graphic-p f) (eq (framep f) 'ns))
      (set-frame-parameter f 'alpha-background 0.0)
      (set-frame-parameter f 'left-fringe 4)
      (set-frame-parameter f 'right-fringe 4)
      (when (hyalo-module-available-p)
        (hyalo-module-appearance--apply-window-appearance
         (hyalo-module-appearance-current))))))

(defun hyalo-module-appearance--enable ()
  "Enable appearance management.
Syncs SAVED Emacs values TO Swift - does not override them."
  (condition-case err
      (progn
        ;; Apply frame settings
        (hyalo-module-appearance--apply-frame-settings)
        ;; Get current appearance based on saved mode setting
        (let ((appearance (hyalo-module-appearance-current)))
          ;; Load theme based on saved mode
          (if (eq hyalo-module-appearance-mode-setting 'auto)
              ;; Auto mode: use light/dark theme settings
              (let ((theme (if (eq appearance 'dark)
                               hyalo-module-appearance-theme-dark
                             hyalo-module-appearance-theme-light)))
                (when theme
                  (mapc #'disable-theme custom-enabled-themes)
                  (load-theme theme t)))
            ;; Manual mode: restore saved current theme
            (when hyalo-module-appearance-current-theme
              (mapc #'disable-theme custom-enabled-themes)
              (load-theme hyalo-module-appearance-current-theme t)))
          ;; Apply window appearance to Swift (from Emacs saved setting)
          (hyalo-module-appearance--apply-window-appearance appearance))
        ;; IMPORTANT: Sync saved Emacs values TO Swift panel
        ;; This ensures Swift has the correct saved values, not defaults
        (hyalo-module-appearance-sync-to-panel)
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
        ;; Mark initialization complete - hooks can now sync from panel
        (setq hyalo-module-appearance--initialized t)
        (hyalo-module-log "Appearance: Enabled (appearance: %s, opacity: %.2f, vibrancy: %s)"
                 (hyalo-module-appearance-current)
                 hyalo-module-appearance-opacity
                 hyalo-module-appearance-vibrancy-material))
    (error (hyalo-module-log "Appearance: Failed to enable: %s" err))))

(defun hyalo-module-appearance--disable ()
  "Disable appearance management."
  ;; Mark as not initialized
  (setq hyalo-module-appearance--initialized nil)
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
