;;; hyalo-module-minibuffer.el --- Glass effect for mini-frame -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: Julien Wintz <julien.wintz@inria.fr>
;; Version: 1.0.0
;; Package-Requires: ((emacs "30.1") (mini-frame "20210521"))
;; Keywords: frames, faces, macos

;;; Commentary:

;; Provides a glass effect for the mini-frame minibuffer.
;; This module:
;; 1. Strips background colors from the mini-frame to allow transparency.
;; 2. Calls the Hyalo Swift module to attach a visual effect view (GlassEffectView).

;;; Code:

(require 'hyalo-module)
(require 'mini-frame)

(defgroup hyalo-module-minibuffer nil
  "Hyalo module for minibuffer glass effects."
  :group 'hyalo-module
  :prefix "hyalo-module-minibuffer-")

(defun hyalo-module-minibuffer--strip-faces (frame)
  "Strip background faces from FRAME to allow glass effect to show."
  ;; 1. Set frame parameters for transparency and clean layout
  ;; ns-alpha-elements controls which elements are rendered with transparency:
  ;; - ns-alpha-default: default face/background
  ;; - ns-alpha-fringe: fringes + internal border clears
  ;; - ns-alpha-box: boxed face outlines
  ;; - ns-alpha-stipple: stipple mask background clears
  ;; - ns-alpha-relief: 3D relief/shadow lines
  ;; - ns-alpha-glyphs: glyph background fills (hl-line, region, vertico-current, etc.)
  (modify-frame-parameters
   frame
   '((background-color . "white")  ; Prevent mini-frame from calculating opaque color
     (alpha-background . 0)  ; Fully transparent background
     (ns-alpha-elements . (ns-alpha-all))  ; All elements transparent
     (ns-background-blur . 0)  ; No blur (NSGlassEffectView provides glass)
     ;; Internal padding around content
     (internal-border-width . 12)
     (child-frame-border-width . 0)
     (left-fringe . 8)
     (right-fringe . 8)
     (undecorated . t)
     (ns-appearance . nil)))
  
  ;; 2. Clear face backgrounds specifically for this frame
  ;; Include completion framework faces (vertico, marginalia, orderless, consult)
  (let ((faces '(default
                  fringe
                  internal-border
                  child-frame-border
                  window-divider
                  vertical-border
                  minibuffer-prompt
                  ;; Echo area
                  echo-area
                  ;; Header/mode line
                  header-line
                  mode-line
                  mode-line-inactive
                  ;; Basic highlighting faces
                  highlight
                  region
                  secondary-selection
                  shadow
                  match
                  lazy-highlight
                  ;; Completion framework faces
                  completions-annotations
                  completions-common-part
                  completions-first-difference
                  completions-highlight
                  ;; Vertico faces
                  vertico-current
                  vertico-group-title
                  vertico-group-separator
                  vertico-multiline
                  ;; Marginalia faces
                  marginalia-key
                  marginalia-documentation
                  marginalia-value
                  marginalia-size
                  marginalia-date
                  marginalia-file-priv-dir
                  marginalia-file-priv-exec
                  marginalia-file-priv-link
                  marginalia-file-priv-read
                  marginalia-file-priv-write
                  marginalia-file-priv-other
                  marginalia-type
                  marginalia-char
                  marginalia-list
                  marginalia-mode
                  marginalia-on
                  marginalia-off
                  marginalia-number
                  marginalia-null
                  marginalia-string
                  marginalia-symbol
                  marginalia-modified
                  marginalia-file-name
                  marginalia-file-owner
                  marginalia-lighter
                  marginalia-installed
                  marginalia-archive
                  marginalia-version
                  ;; Orderless faces
                  orderless-match-face-0
                  orderless-match-face-1
                  orderless-match-face-2
                  orderless-match-face-3
                  ;; Consult faces
                  consult-preview-match
                  consult-preview-cursor
                  consult-preview-line
                  consult-highlight-match
                  consult-highlight-mark
                  consult-file
                  consult-buffer
                  consult-line-number
                  consult-line-number-prefix)))
    (dolist (face faces)
      (when (facep face)
        ;; Use 'unspecified to inherit transparency from frame
        (set-face-attribute face frame :background 'unspecified :box nil))))
  
  ;; 3. Force frame redisplay to apply transparency
  (force-mode-line-update t)
  (redisplay t))

(defun hyalo-module-minibuffer-setup (frame)
  "Apply glass effect to FRAME if it is a mini-frame."
  (let ((is-mini (or (frame-parameter frame 'mini-frame)
                     (frame-parameter frame 'parent-frame))))
    (when (and (hyalo-module-available-p) is-mini)
      ;; 1. Prepare Emacs side (transparency)
      (hyalo-module-minibuffer--strip-faces frame)
      
      ;; 2. Activate Swift side
      (let ((window-id (frame-parameter frame 'window-id)))
        (when window-id
          (hyalo-minibuffer-enable window-id))))))

(defun hyalo-module-minibuffer-ensure-focus ()
  "Ensure the mini-frame has focus when minibuffer is active."
  (let ((frame (window-frame (minibuffer-window))))
    ;; Check if it's a child frame (mini-frame uses child frames)
    (let ((is-mini (or (frame-parameter frame 'mini-frame)
                       (frame-parameter frame 'parent-frame)))) ;; mini-frame usually has a parent
      (when (and is-mini (hyalo-module-available-p))
        ;; Ensure glass effect is applied if it wasn't caught by make-frame hook
        ;; (e.g. frame was created before mode was enabled or reused)
        (hyalo-module-minibuffer-setup frame)
        
        (let ((window-id (frame-parameter frame 'window-id)))
          (when window-id
            (hyalo-window-focus window-id)))))))

;;;###autoload
(define-minor-mode hyalo-module-minibuffer-mode
  "Global minor mode to enable glass effect for mini-frame."
  :global t
  :group 'hyalo-module-minibuffer
  (if hyalo-module-minibuffer-mode
      (progn
        (add-hook 'after-make-frame-functions #'hyalo-module-minibuffer-setup)
        (add-hook 'minibuffer-setup-hook #'hyalo-module-minibuffer-ensure-focus)
        ;; Deactivate modal tracking when minibuffer exits
        (add-hook 'minibuffer-exit-hook #'hyalo-module-minibuffer--on-exit)
        ;; Also apply face stripping on every minibuffer activation
        ;; (mini-frame reuses frames but may not reapply our parameters)
        (add-hook 'minibuffer-setup-hook #'hyalo-module-minibuffer--apply-transparency)
        ;; Sync theme changes to mini-frames
        (when (boundp 'enable-theme-functions)
          (add-hook 'enable-theme-functions #'hyalo-module-minibuffer--on-theme-change))
        ;; Sync system appearance changes to mini-frames
        (when (boundp 'ns-system-appearance-change-functions)
          (add-hook 'ns-system-appearance-change-functions #'hyalo-module-minibuffer--on-appearance-change))
        ;; Try to setup existing frames if any
        (dolist (f (frame-list))
          (hyalo-module-minibuffer-setup f)))
    (remove-hook 'after-make-frame-functions #'hyalo-module-minibuffer-setup)
    (remove-hook 'minibuffer-setup-hook #'hyalo-module-minibuffer-ensure-focus)
    (remove-hook 'minibuffer-exit-hook #'hyalo-module-minibuffer--on-exit)
    (remove-hook 'minibuffer-setup-hook #'hyalo-module-minibuffer--apply-transparency)
    (when (boundp 'enable-theme-functions)
      (remove-hook 'enable-theme-functions #'hyalo-module-minibuffer--on-theme-change))
    (when (boundp 'ns-system-appearance-change-functions)
      (remove-hook 'ns-system-appearance-change-functions #'hyalo-module-minibuffer--on-appearance-change))))

(defun hyalo-module-minibuffer--on-theme-change (_theme)
  "Update mini-frame appearance when theme changes."
  ;; Re-strip faces on all mini-frames to match new theme
  (dolist (frame (frame-list))
    (when (frame-parameter frame 'parent-frame)
      (hyalo-module-minibuffer--strip-faces frame)
      (redraw-frame frame)))
  ;; Update Swift glass effect appearance
  (when (and (hyalo-module-available-p)
             (fboundp 'hyalo-minibuffer-update-appearance))
    (hyalo-minibuffer-update-appearance)))

(defun hyalo-module-minibuffer--on-appearance-change (_appearance)
  "Update mini-frame glass effect when system appearance changes.
APPEARANCE is `light' or `dark'."
  ;; Update Swift glass effect appearance
  (when (and (hyalo-module-available-p)
             (fboundp 'hyalo-minibuffer-update-appearance))
    (hyalo-minibuffer-update-appearance)))

(defun hyalo-module-minibuffer--apply-transparency ()
  "Apply transparency parameters to the current minibuffer frame."
  (let ((frame (selected-frame)))
    (when (frame-parameter frame 'parent-frame)
      ;; This is a child frame (mini-frame) - apply transparency
      (hyalo-module-minibuffer--strip-faces frame)
      ;; Force redisplay
      (redraw-frame frame))))

(defun hyalo-module-minibuffer--on-exit ()
  "Handle minibuffer exit - deactivate modal tracking."
  (when (and (hyalo-module-available-p)
             (fboundp 'hyalo-minibuffer-deactivate))
    (hyalo-minibuffer-deactivate)))

(provide 'hyalo-module-minibuffer)
;;; hyalo-module-minibuffer.el ends here
