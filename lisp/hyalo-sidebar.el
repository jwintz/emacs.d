;;; hyalo-sidebar.el --- Hyalo sidebar configuration -*- lexical-binding: t -*- no-byte-compile: t -*-

;; Copyright (C) 2025
;; Author: Julien Wintz <julien.wintz@inria.fr>
;; Keywords: frames, sidebar, dired

;;; Commentary:

;; Custom dired-sidebar configuration for the Hyalo sidebar.
;; - Lightweight file tree based on built-in Dired
;; - Icons via nerd-icons-dired (monochromatic)
;; - Opens files in parent frame (main Emacs window)
;; - RET: open file in parent frame current window
;; - o: open file in parent frame other window

;;; Code:

(require 'dired)
(require 'hyalo)

(defgroup hyalo-sidebar nil
  "Hyalo sidebar configuration."
  :group 'hyalo
  :prefix "hyalo-sidebar-")

(defcustom hyalo-sidebar-width 35
  "Width of sidebar in characters."
  :type 'integer
  :group 'hyalo-sidebar)

(defcustom hyalo-sidebar-use-project-root t
  "When non-nil, use project.el root as sidebar directory.
Falls back to `default-directory' if no project is found."
  :type 'boolean
  :group 'hyalo-sidebar)

;;; Parent frame reference

(defvar hyalo-sidebar--parent-frame nil
  "The parent frame for the embedded sidebar child-frame.")

;;; Project.el integration

(defun hyalo-sidebar--get-root-directory ()
  "Get the root directory for sidebar.
Uses project.el if `hyalo-sidebar-use-project-root' is non-nil,
falls back to `default-directory'."
  (expand-file-name
   (if hyalo-sidebar-use-project-root
       (or (when-let* ((proj (project-current)))
             (if (fboundp 'project-root)
                 (project-root proj)
               ;; Fallback for older Emacs
               (car (project-roots proj))))
           default-directory)
     default-directory)))

;;; Display in Child Frame

(defun hyalo-sidebar-display-in-child-frame (buffer _alist)
  "Display BUFFER in a hidden child frame for embedding.
Sets `hyalo-embedded' parameter and positions off-screen."
  (let* ((parent (selected-frame))
         (frame (make-frame
                 `((parent-frame . ,parent)
                   (window-system . ns)
                   (hyalo-embedded . t)
                   (name . "Hyalo Sidebar")
                   (minibuffer . nil)
                   (undecorated . t)
                   (internal-border-width . 0)
                   (left-fringe . 0)
                   (right-fringe . 0)
                   (tool-bar-lines . 0)
                   (menu-bar-lines . 0)
                   (vertical-scroll-bars . nil)
                   (width . 40)
                   (height . 50)
                   (left . -10000)  ; Off-screen for Swift detection
                   (top . -10000)
                   (visibility . t)))))  ; Must be visible (but off-screen)
    (let ((window (frame-selected-window frame)))
      (display-buffer-record-window 'frame window buffer)
      (set-window-buffer window buffer)
      (set-window-dedicated-p window t)
      window)))

;;; Setup function

(defun hyalo-sidebar-setup ()
  "Configure dired-sidebar for sidebar display."
  (interactive)
  (when (featurep 'dired-sidebar)
    ;; Use nerd-icons theme
    (setq dired-sidebar-theme 'nerd-icons)
    ;; Follow current buffer
    (setq dired-sidebar-follow-mode-line-file t)
    ;; Don't use special refresh
    (setq dired-sidebar-special-refresh-commands nil)
    ;; Width
    (setq dired-sidebar-width hyalo-sidebar-width)
    ;; Don't show in other-window
    (setq dired-sidebar-should-follow-file t)
    ;; Subtree indentation
    (setq dired-sidebar-subtree-indent 2)))

;;; Get parent frame helper

(defun hyalo-sidebar--get-parent-frame ()
  "Get the parent frame for opening files."
  (or hyalo-sidebar--parent-frame
      (frame-parameter (selected-frame) 'parent-frame)
      ;; Fallback to main frame from sidebar module
      (and (boundp 'hyalo-sidebar--main-frame)
           hyalo-sidebar--main-frame)))

;;; Visit files in parent frame

(defun hyalo-sidebar--visit-in-parent ()
  "Visit dired file/directory in parent frame's current window.
Files open in parent frame, directories toggle expand/collapse."
  (interactive)
  (let* ((file (dired-get-file-for-visit))
         (parent (hyalo-sidebar--get-parent-frame)))
    (cond
     ;; File - open in parent frame's current window
     ((and file parent (file-regular-p file))
      (let ((buf (find-file-noselect file)))
        (select-frame-set-input-focus parent)
        (switch-to-buffer buf)))
     ;; Directory - toggle subtree if dired-subtree available, else enter
     ((and file (file-directory-p file))
      (if (and (featurep 'dired-subtree)
               (fboundp 'dired-subtree-toggle))
          (dired-subtree-toggle)
        (dired-sidebar-find-file)))
     ;; Fallback
     (t
      (dired-sidebar-find-file)))))

(defun hyalo-sidebar--visit-in-parent-other-window ()
  "Visit dired file in parent frame's other window.
Files open in parent frame's other window (if possible), directories toggle expand/collapse."
  (interactive)
  (let* ((file (dired-get-file-for-visit))
         (parent (hyalo-sidebar--get-parent-frame)))
    (cond
     ;; File - open in parent frame's other window (or current if only one window)
     ((and file parent (file-regular-p file))
      (let ((buf (find-file-noselect file)))
        (select-frame-set-input-focus parent)
        ;; Try other window, fall back to current if splitting fails
        (condition-case nil
            (switch-to-buffer-other-window buf)
          (error (switch-to-buffer buf)))))
     ;; Directory - toggle subtree if dired-subtree available, else enter
     ((and file (file-directory-p file))
      (if (and (featurep 'dired-subtree)
               (fboundp 'dired-subtree-toggle))
          (dired-subtree-toggle)
        (dired-sidebar-find-file)))
     ;; Fallback
     (t
      (dired-sidebar-find-file)))))

(defun hyalo-sidebar--mouse-visit-in-parent (event)
  "Handle mouse click on dired item - visit file in parent frame.
EVENT is the mouse event."
  (interactive "e")
  (mouse-set-point event)
  (hyalo-sidebar--visit-in-parent))

;;; Keymap for sidebar mode

(defvar hyalo-sidebar--keymap
  (let ((map (make-sparse-keymap)))
    ;; RET - open in current window of parent frame
    (define-key map (kbd "RET") #'hyalo-sidebar--visit-in-parent)
    (define-key map (kbd "<return>") #'hyalo-sidebar--visit-in-parent)
    ;; o - open in other window of parent frame
    (define-key map (kbd "o") #'hyalo-sidebar--visit-in-parent-other-window)
    ;; Mouse - open in current window (cover all mouse buttons)
    (define-key map (kbd "<mouse-1>") #'hyalo-sidebar--mouse-visit-in-parent)
    (define-key map (kbd "<mouse-2>") #'hyalo-sidebar--mouse-visit-in-parent)
    (define-key map (kbd "<double-mouse-1>") #'hyalo-sidebar--mouse-visit-in-parent)
    (define-key map (kbd "<double-mouse-2>") #'hyalo-sidebar--mouse-visit-in-parent)
    ;; Prevent dired-sidebar's default split behavior
    (define-key map [remap dired-sidebar-find-file] #'hyalo-sidebar--visit-in-parent)
    (define-key map [remap dired-sidebar-mouse-subtree-cycle-or-find-file] #'hyalo-sidebar--mouse-visit-in-parent)
    map)
  "Keymap for dired-sidebar in embedded sidebar mode.")

;;; Monochrome icons for embedded sidebar

(defvar hyalo-sidebar--monochrome-active nil
  "Non-nil when monochrome icon advice is globally active.")

(defun hyalo-sidebar--strip-icon-face (icon)
  "Strip face properties from ICON for monochrome display."
  (when (stringp icon)
    (let ((clean (copy-sequence icon)))
      (remove-text-properties 0 (length clean) '(face nil font-lock-face nil) clean)
      clean)))

(defun hyalo-sidebar--in-embedded-p ()
  "Return non-nil if current frame is an embedded hyalo frame."
  (frame-parameter (selected-frame) 'hyalo-embedded))

(defun hyalo-sidebar--monochrome-advice (orig-fun &rest args)
  "Advice to strip foreground from nerd-icons ONLY in embedded frames.
In non-embedded frames, returns original result unchanged."
  (let ((result (apply orig-fun args)))
    (if (hyalo-sidebar--in-embedded-p)
        ;; In embedded frame: strip face and ensure non-nil
        (if (and result (stringp result))
            (hyalo-sidebar--strip-icon-face result)
          "")
      ;; Not in embedded frame: return original unchanged
      result)))

(defun hyalo-sidebar--enable-monochrome ()
  "Enable monochrome icons globally (but only affects embedded frames)."
  (unless hyalo-sidebar--monochrome-active
    (advice-add 'nerd-icons-icon-for-file :around #'hyalo-sidebar--monochrome-advice)
    (advice-add 'nerd-icons-icon-for-dir :around #'hyalo-sidebar--monochrome-advice)
    (advice-add 'nerd-icons-icon-for-extension :around #'hyalo-sidebar--monochrome-advice)
    (setq hyalo-sidebar--monochrome-active t)))

;;; Mode hook for sidebar

(defun hyalo-sidebar--setup-if-embedded ()
  "Set up dired-sidebar for sidebar if this is an embedded frame."
  (when (and (derived-mode-p 'dired-sidebar-mode)
             (frame-parameter (selected-frame) 'hyalo-embedded))
    ;; Store parent frame reference
    (setq hyalo-sidebar--parent-frame
          (frame-parameter (selected-frame) 'parent-frame))
    ;; Configure display
    (hyalo-sidebar-setup)
    ;; Enable monochrome icons
    (hyalo-sidebar--enable-monochrome)
    ;; Remove mode-line in sidebar
    (setq-local mode-line-format nil)
    (setq-local header-line-format nil)
    ;; Enable our keymap override - MUST set the variable to t for the keymap to be active
    (setq-local hyalo-sidebar-embedded t)
    (push `(hyalo-sidebar-embedded . ,hyalo-sidebar--keymap)
          minor-mode-overriding-map-alist)))

;;; Project switching support

(defun hyalo-sidebar--find-sidebar-buffer ()
  "Find the active dired-sidebar buffer.
Prioritizes a buffer that is currently displayed in an embedded frame."
  (or
   ;; Try to find a visible sidebar buffer in an embedded frame
   (cl-find-if (lambda (buf)
                 (when (buffer-live-p buf)
                   (with-current-buffer buf
                     (and (derived-mode-p 'dired-sidebar-mode)
                          (let ((win (get-buffer-window buf t)))
                            (and win
                                 (frame-parameter (window-frame win) 'hyalo-embedded)))))))
               (buffer-list))
   ;; Fallback: any dired-sidebar buffer (e.g. during initial setup)
   (cl-find-if (lambda (buf)
                 (with-current-buffer buf
                   (derived-mode-p 'dired-sidebar-mode)))
               (buffer-list))))

(defun hyalo-sidebar-update-to-project ()
  "Update dired-sidebar in embedded child-frame to show the current project root.
Call this after switching projects."
  (interactive)
  (condition-case err
      (when-let* ((sidebar-buf (hyalo-sidebar--find-sidebar-buffer))
                  (sidebar-win (get-buffer-window sidebar-buf t))  ; t = search all frames
                  (sidebar-frame (window-frame sidebar-win))
                  (proj (project-current))
                  (root (expand-file-name
                         (if (fboundp 'project-root)
                             (project-root proj)
                           (car (project-roots proj))))))
        (when (and (window-live-p sidebar-win)
                   (frame-live-p sidebar-frame))
          (hyalo-log "[dired-sidebar] Updating sidebar to: %s (frame: %s)" root sidebar-frame)
          ;; Capture parent frame from existing context
          (let* ((parent-frame hyalo-sidebar--parent-frame)
                 ;; Check if a sidebar buffer for this root already exists
                 (expected-name (dired-sidebar-buffer-name root))
                 (existing-buf (get-buffer expected-name))
                 (new-buf (or existing-buf (dired-noselect root))))
            ;; Configure the new buffer
            (with-current-buffer new-buf
              ;; Ensure dired-sidebar mode is active
              (unless (derived-mode-p 'dired-sidebar-mode)
                (dired-sidebar-mode))
              ;; Restore parent frame reference
              (setq hyalo-sidebar--parent-frame parent-frame)
              ;; Apply sidebar setup
              (hyalo-sidebar-setup)
              ;; Enable monochrome advice (global but frame-aware)
              (hyalo-sidebar--enable-monochrome)
              ;; Ensure nerd-icons-dired mode is active
              (when (and (fboundp 'nerd-icons-dired-mode)
                         (not nerd-icons-dired-mode))
                (nerd-icons-dired-mode 1))
              ;; Enable keymap override
              (setq-local hyalo-sidebar-embedded t)
              (when (boundp 'hyalo-sidebar--keymap)
                (push `(hyalo-sidebar-embedded . ,hyalo-sidebar--keymap)
                      minor-mode-overriding-map-alist))
              (setq-local mode-line-format nil)
              (setq-local header-line-format nil))

            ;; Forcefully display in the sidebar window
            (set-window-dedicated-p sidebar-win nil) ; Unlock window
            (set-window-buffer sidebar-win new-buf)
            (set-window-dedicated-p sidebar-win t)   ; Re-lock window

            ;; CRITICAL: Refresh icons with embedded frame selected
            ;; The monochrome advice checks (selected-frame) to determine if we're
            ;; in an embedded frame. Without this, the timer callback runs in the
            ;; main frame context, and icons lose their monochrome handling.
            (with-selected-frame sidebar-frame
              (with-current-buffer new-buf
                (when (fboundp 'nerd-icons-dired--refresh)
                  (nerd-icons-dired--refresh)))))))
    (error
     (hyalo-log "[dired-sidebar] Error updating sidebar: %s" err))))

(defun hyalo-sidebar--after-project-switch (&rest _)
  "Update sidebar after project switch completes."
  (hyalo-log "[dired-sidebar] Project switch detected, scheduling update...")
  (run-with-timer 0.3 nil #'hyalo-sidebar-update-to-project))

;; Setup when dired-sidebar loads
(with-eval-after-load 'dired-sidebar
  (add-hook 'dired-sidebar-mode-hook #'hyalo-sidebar--setup-if-embedded))

;; Advise project-switch-project to update sidebar
(with-eval-after-load 'project
  (advice-add 'project-switch-project :after #'hyalo-sidebar--after-project-switch))

;;; Visibility Callbacks

(defun hyalo-on-sidebar-visibility-changed (panel visible)
  "Callback from Swift when sidebar visibility changes.
PANEL is 'left' or 'right'. VISIBLE is a boolean."
  (hyalo-log "Sidebar: Visibility changed: %s %s" panel visible)
  (when (string= panel "left")
    (let ((dired-open (and (featurep 'dired-sidebar)
                           (dired-sidebar-showing-sidebar-p))))
      (cond
       ;; Swift says show, but dired closed -> open dired
       ((and visible (not dired-open))
        (dired-sidebar-toggle-sidebar))
       ;; Swift says hide, but dired open -> close dired
       ((and (not visible) dired-open)
        (dired-sidebar-toggle-sidebar))))))

(defun hyalo-on-detail-visibility-changed (panel visible)
  "Callback from Swift when inspector visibility changes.
PANEL is 'right'. VISIBLE is a boolean."
  (hyalo-log "Sidebar: Detail visibility changed: %s %s" panel visible))

;;; Mode Definition

(defvar hyalo-sidebar-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `hyalo-sidebar-mode'.")

;;;###autoload
(define-minor-mode hyalo-sidebar-mode
  "Global minor mode for Hyalo sidebar integration."
  :global t
  :group 'hyalo-sidebar
  (if hyalo-sidebar-mode
      (progn
        (hyalo-log "Sidebar: Enabled")
        ;; Force dired-sidebar to use our child frame
        (add-to-list 'display-buffer-alist
                     '("^\\*dired-sidebar.*" . (hyalo-sidebar-display-in-child-frame)))
        
        ;; Setup Swift visibility callbacks
        (when (and (hyalo-available-p)
                   (fboundp 'hyalo-setup-visibility-callbacks))
          (hyalo-setup-visibility-callbacks))

        ;; Ensure dired-sidebar is configured
        (when (featurep 'dired-sidebar)
          (hyalo-sidebar-setup)))
    (progn
      (hyalo-log "Sidebar: Disabled")
      ;; Remove display buffer configuration
      (setq display-buffer-alist
            (cl-remove-if (lambda (x) (string-match-p "dired-sidebar" (car x)))
                          display-buffer-alist)))))

;;; Interactive Commands

;;;###autoload
(defun hyalo-sidebar-toggle-left ()
  "Toggle the sidebar (using dired-sidebar) and sync with Swift."
  (interactive)
  ;; 1. Toggle Swift sidebar
  (when (and (hyalo-available-p) (fboundp 'hyalo-sidebar-toggle))
    (hyalo-sidebar-toggle))
  ;; 2. Toggle Elisp sidebar (only if we need to sync state immediately)
  ;; Note: The callback will also fire, but standard toggling here ensures responsiveness
  ;; and handles cases where Swift might be slow or disabled.
  ;; We check if the state is already consistent to avoid double-toggling in the callback.
  (if (fboundp 'dired-sidebar-toggle-sidebar)
      (dired-sidebar-toggle-sidebar)
    (user-error "dired-sidebar not available")))

;;;###autoload
(defun hyalo-sidebar-focus-left ()
  "Focus the sidebar window."
  (interactive)
  (when-let* ((buf (hyalo-sidebar--find-sidebar-buffer))
              (win (get-buffer-window buf t))) ; Search all frames
    (select-frame-set-input-focus (window-frame win))
    (select-window win)))

;;;###autoload
(defun hyalo-sidebar-toggle-right ()
  "Toggle the inspector/right sidebar (placeholder)."
  (interactive)
  (when (and (hyalo-available-p) (fboundp 'hyalo-detail-toggle))
    (hyalo-detail-toggle))
  (message "Inspector sidebar not implemented"))

;;;###autoload
(defun hyalo-sidebar-focus-right ()
  "Focus the inspector/right sidebar (placeholder)."
  (interactive)
  (message "Inspector sidebar not implemented"))

(provide 'hyalo-sidebar)
;;; hyalo-sidebar.el ends here
