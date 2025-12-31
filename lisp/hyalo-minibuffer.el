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

(require 'hyalo)
(require 'mini-frame)

(defgroup hyalo-module-minibuffer nil
  "Hyalo module for minibuffer glass effects."
  :group 'hyalo-module
  :prefix "hyalo-module-minibuffer-")

(defun hyalo-module-minibuffer--get-theme-bg ()
  "Get the current theme's background color."
  (or (face-background 'default)
      "#1a1a2e"))  ; Fallback dark color

(defun hyalo-module-minibuffer--strip-faces (frame)
  "Set up FRAME for glass effect - fully transparent, Swift provides container."
  ;; Set frame parameters for full transparency
  (modify-frame-parameters
   frame
   `((background-color . "white")  ; Prevent mini-frame color calculation
     (alpha-background . 0)  ; Fully transparent - glass shows through
     (ns-alpha-elements . (ns-alpha-all))  ; All elements transparent
     (ns-background-blur . 0)  ; No blur (NSGlassEffectView provides glass)
     ;; No internal border - Swift handles all margins
     (internal-border-width . 0)
     (child-frame-border-width . 0)
     (left-fringe . 0)
     (right-fringe . 0)
     (undecorated . t)
     (ns-appearance . nil)))
  
  ;; Force frame redisplay
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
        ;; Enable pixel-wise resizing globally to avoid character-cell snapping
        ;; This helps Swift container sizing work correctly
        (setq frame-resize-pixelwise t)
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

(provide 'hyalo-minibuffer)
;;; hyalo-module-minibuffer.el ends here
