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
  (hyalo-module-log "Stripping faces for frame: %s" frame)
  ;; 1. Set frame parameters for transparency and clean layout
  (modify-frame-parameters 
   frame
   '((alpha-background . 0)
     (internal-border-width . 0)
     (child-frame-border-width . 0)
     (left-fringe . 10)
     (right-fringe . 10)
     (undecorated . t)
     (ns-appearance . nil)))
  
  ;; 2. Clear face backgrounds specifically for this frame
  (let ((faces '(default 
                  fringe 
                  internal-border 
                  child-frame-border
                  window-divider 
                  vertical-border 
                  minibuffer-prompt
                  echo-area)))
    (dolist (face faces)
      (when (facep face)
        ;; Using 'unspecified to ensure it inherits transparency from the frame
        (set-face-attribute face frame :background 'unspecified :box nil)))))

(defun hyalo-module-minibuffer-setup (frame)
  "Apply glass effect to FRAME if it is a mini-frame."
  (let ((is-mini (or (frame-parameter frame 'mini-frame)
                     (frame-parameter frame 'parent-frame))))
    (when (and (hyalo-module-available-p) is-mini)
      (hyalo-module-log "Setting up glass effect for mini-frame: %s" frame)
      
      ;; 1. Prepare Emacs side (transparency)
      (hyalo-module-minibuffer--strip-faces frame)
      
      ;; 2. Activate Swift side
      (let ((window-id (frame-parameter frame 'window-id)))
        (hyalo-module-log "Found window-id: %s" window-id)
        (when window-id
          (let ((success (hyalo-minibuffer-enable window-id)))
            (hyalo-module-log "Swift minibuffer-enable status: %s" success)))))))

(defun hyalo-module-minibuffer-ensure-focus ()
  "Ensure the mini-frame has focus when minibuffer is active."
  (let ((frame (window-frame (minibuffer-window))))
    (hyalo-module-log "Minibuffer setup hook triggered. Frame: %s" frame)
    ;; Debug: Log all parameters to find the identifier
    ;; (hyalo-module-log "Frame parameters: %s" (frame-parameters frame))
    
    ;; Check if it's a child frame (mini-frame uses child frames)
    (let ((is-mini (or (frame-parameter frame 'mini-frame)
                       (frame-parameter frame 'parent-frame)))) ;; mini-frame usually has a parent
      (hyalo-module-log "Is mini-frame? %s (param: %s, parent: %s)" 
                       (if is-mini "yes" "no")
                       (frame-parameter frame 'mini-frame)
                       (frame-parameter frame 'parent-frame))
      
      (when (and is-mini (hyalo-module-available-p))
        ;; Ensure glass effect is applied if it wasn't caught by make-frame hook
        ;; (e.g. frame was created before mode was enabled or reused)
        (hyalo-module-minibuffer-setup frame)
        
        (let ((window-id (frame-parameter frame 'window-id)))
          (hyalo-module-log "Requesting focus for window-id: %s" window-id)
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
        ;; Try to setup existing frames if any
        (dolist (f (frame-list))
          (hyalo-module-minibuffer-setup f)))
    (remove-hook 'after-make-frame-functions #'hyalo-module-minibuffer-setup)
    (remove-hook 'minibuffer-setup-hook #'hyalo-module-minibuffer-ensure-focus)))

(provide 'hyalo-module-minibuffer)
;;; hyalo-module-minibuffer.el ends here
