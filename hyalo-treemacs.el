;;; hyalo-treemacs.el --- Hyalo sidebar treemacs configuration -*- lexical-binding: t -*- no-byte-compile: t -*-

;; Copyright (C) 2025
;; Author: Julien Wintz <julien.wintz@inria.fr>
;; Keywords: frames, sidebar, treemacs

;;; Commentary:

;; Custom treemacs configuration for the Hyalo sidebar.
;; - Proper indentation
;; - Clean display without git status
;; - Custom face/font support

;;; Code:

(require 'treemacs nil t)

(defgroup hyalo-treemacs nil
  "Hyalo sidebar treemacs configuration."
  :group 'hyalo-module-sidebar
  :prefix "hyalo-treemacs-")

(defcustom hyalo-treemacs-indentation 2
  "Indentation width for treemacs in sidebar."
  :type 'integer
  :group 'hyalo-treemacs)

(defcustom hyalo-treemacs-show-git-status nil
  "Whether to show git status indicators in sidebar treemacs."
  :type 'boolean
  :group 'hyalo-treemacs)

(defcustom hyalo-treemacs-show-file-size nil
  "Whether to show file sizes in sidebar treemacs."
  :type 'boolean
  :group 'hyalo-treemacs)

(defcustom hyalo-treemacs-show-root-header t
  "Whether to show root header in sidebar treemacs."
  :type 'boolean
  :group 'hyalo-treemacs)

;;; Parent frame reference

(defvar hyalo-treemacs--parent-frame nil
  "The parent frame for the embedded treemacs child-frame.")

;;; Setup function

(defun hyalo-treemacs-sidebar-setup ()
  "Configure treemacs for sidebar display."
  (interactive)
  (when (featurep 'treemacs)
    ;; Indentation
    (setq treemacs-indentation hyalo-treemacs-indentation)
    (setq treemacs-indentation-string " ")

    ;; Disable features for cleaner display
    (unless hyalo-treemacs-show-git-status
      (setq treemacs-git-mode nil)
      (when (fboundp 'treemacs-git-mode)
        (treemacs-git-mode -1)))

    ;; Hide filewatch mode (reduces clutter)
    (when (fboundp 'treemacs-filewatch-mode)
      (treemacs-filewatch-mode -1))

    ;; Width settings (will be controlled by SwiftUI)
    (setq treemacs-width 40)
    (setq treemacs-width-is-initially-locked nil)

    ;; Display settings
    (setq treemacs-show-hidden-files nil)
    (setq treemacs-sorting 'alphabetic-asc)

    ;; No background colors
    (setq treemacs-fringe-indicator-mode nil)

    ;; Use hyalo-explorer icons theme if available
    (when (and (featurep 'hyalo-explorer-icons)
               (fboundp 'hyalo-explorer-icons-config))
      (hyalo-explorer-icons-config))
    
    ;; CRITICAL: Override the default visit action to use parent frame
    ;; This works globally because treemacs uses these configs
    (when (frame-parameter (selected-frame) 'hyalo-embedded)
      (setq-local treemacs-default-visit-action
                  #'hyalo-treemacs--visit-in-parent)
      ;; Also override the RET actions config locally
      (setq-local treemacs-RET-actions-config
                  '((root-node-open   . treemacs-toggle-node)
                    (root-node-closed . treemacs-toggle-node)
                    (dir-node-open    . treemacs-toggle-node)
                    (dir-node-closed  . treemacs-toggle-node)
                    (file-node-open   . hyalo-treemacs--visit-in-parent)
                    (file-node-closed . hyalo-treemacs--visit-in-parent)
                    (tag-node-open    . treemacs-toggle-node-prefer-tag-visit)
                    (tag-node-closed  . treemacs-toggle-node-prefer-tag-visit)
                    (tag-node         . hyalo-treemacs--visit-in-parent)))
      (message "[hyalo-treemacs] RET-actions-config overridden for embedded mode"))))

;;; Visit files in parent frame

(defun hyalo-treemacs--visit-in-parent (&optional _arg)
  "Visit treemacs node in parent frame.
This bypasses treemacs's window selection and opens directly in parent."
  (interactive "P")
  (let* ((btn (treemacs-current-button))
         (parent (or hyalo-treemacs--parent-frame
                     (frame-parameter (selected-frame) 'parent-frame)
                     ;; Fallback to main frame from sidebar module
                     (and (boundp 'hyalo-sidebar--main-frame)
                          hyalo-sidebar--main-frame))))
    (message "[hyalo-treemacs] visit-in-parent: btn=%s parent=%s" btn parent)
    (if (and btn parent)
        (let ((path (treemacs-safe-button-get btn :path)))
          (message "[hyalo-treemacs] path=%s exists=%s isdir=%s"
                   path
                   (when path (file-exists-p path))
                   (when path (file-directory-p path)))
          (cond
           ;; File - open in parent frame using noselect to avoid any advice
           ((and path (file-exists-p path) (not (file-directory-p path)))
            (message "[hyalo-treemacs] Opening file %s in parent frame" path)
            (let ((buf (find-file-noselect path)))
              (message "[hyalo-treemacs] Got buffer %s, switching to parent" buf)
              (select-frame-set-input-focus parent)
              (switch-to-buffer buf)))
           ;; Directory - toggle node
           ((and path (file-directory-p path))
            (message "[hyalo-treemacs] Toggling directory node")
            (treemacs-toggle-node))
           ;; Other - toggle
           (t
            (message "[hyalo-treemacs] Other node, toggling")
            (treemacs-toggle-node))))
      ;; No button or parent - fallback
      (message "[hyalo-treemacs] No button or parent, falling back to treemacs-RET-action")
      (treemacs-RET-action))))

(defun hyalo-treemacs--mouse-visit-in-parent (event)
  "Handle mouse click on treemacs node - visit file in parent frame.
EVENT is the mouse event."
  (interactive "e")
  ;; First, let treemacs handle the click to select the node
  (mouse-set-point event)
  ;; Then visit in parent frame if it's a file
  (hyalo-treemacs--visit-in-parent))

;;; Mode hook for sidebar

(defvar hyalo-treemacs--sidebar-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'hyalo-treemacs--visit-in-parent)
    (define-key map (kbd "<return>") #'hyalo-treemacs--visit-in-parent)
    (define-key map (kbd "o") #'hyalo-treemacs--visit-in-parent)
    (define-key map (kbd "<double-mouse-1>") #'hyalo-treemacs--mouse-visit-in-parent)
    map)
  "Keymap for treemacs in embedded sidebar mode.")

(defun hyalo-treemacs--setup-if-sidebar ()
  "Set up treemacs for sidebar if this is a sidebar buffer."
  (when (and (derived-mode-p 'treemacs-mode)
             (frame-parameter (selected-frame) 'hyalo-embedded))
    ;; Store parent frame reference
    (setq hyalo-treemacs--parent-frame
          (frame-parameter (selected-frame) 'parent-frame))
    (message "[hyalo-treemacs] Sidebar setup, parent-frame=%s" hyalo-treemacs--parent-frame)
    ;; Configure treemacs display
    (hyalo-treemacs-sidebar-setup)
    ;; Remove mode-line in treemacs sidebar
    (setq-local mode-line-format nil)
    (setq-local header-line-format nil)
    ;; Use minor-mode style override - add our keymap with highest priority
    (push `(hyalo-treemacs-sidebar . ,hyalo-treemacs--sidebar-mode-map)
          minor-mode-overriding-map-alist)
    (message "[hyalo-treemacs] Keybindings configured via overriding-map-alist")))

;; Setup when treemacs loads
(with-eval-after-load 'treemacs
  ;; Hook for mode setup
  (add-hook 'treemacs-mode-hook #'hyalo-treemacs--setup-if-sidebar))

(provide 'hyalo-treemacs)
;;; hyalo-treemacs.el ends here
