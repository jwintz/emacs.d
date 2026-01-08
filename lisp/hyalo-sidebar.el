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

(defcustom hyalo-sidebar-internal-border-width 12
  "Internal border width for embedded sidebar frames.
This provides margin matching SwiftUI design guidelines.
Set to 0 for no margin, 8 for compact, 12 for standard, 16 for spacious."
  :type 'integer
  :group 'hyalo-sidebar)

(defcustom hyalo-sidebar-font nil
  "Font family to use for sidebar buffers.
When non-nil, should be a font family string like \"SF Pro Display\".
When nil, uses the default frame font family."
  :type '(choice (const nil) string)
  :group 'hyalo-sidebar)

(defcustom hyalo-sidebar-font-height 100
  "Font height for sidebar buffers (100 = 10pt).
This ensures proper layout calculation for embedded frames."
  :type 'integer
  :group 'hyalo-sidebar)

;;; Frame tracking

(defvar hyalo-sidebar--parent-frame nil
  "The parent frame for the embedded sidebar child-frame.")

(defvar hyalo-sidebar--left-top-frame nil
  "Reference to the left sidebar top frame (ibuffer).")

(defvar hyalo-sidebar--left-bottom-frame nil
  "Reference to the left sidebar bottom frame (dired-sidebar).")

(defvar hyalo-sidebar--right-frame nil
  "Reference to the right sidebar frame (agent-shell).")

(defvar hyalo-sidebar--main-frame nil
  "The main Emacs frame (parent of all embedded sidebars).
Used by sidebar buffers to open files in the correct frame.")

(defvar hyalo-sidebar--pending-focus-right nil
  "Non-nil when we need to focus right sidebar after agent-shell is ready.")

(defvar hyalo-sidebar--visibility-timer nil
  "Timer for checking panel visibility changes.")

(defvar hyalo-sidebar--callbacks-installed nil
  "Non-nil when Swift visibility callbacks have been installed.")

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

;;; Appearance for embedded frames

(defun hyalo-sidebar--strip-faces (frame)
  "Set up FRAME for glass effect - fully transparent.
Preserves internal-border-width for SwiftUI-style margins."
  (modify-frame-parameters
   frame
   `((background-color . "white")
     (alpha-background . 0)
     (ns-alpha-elements . (ns-alpha-all))
     (ns-background-blur . 0)
     (child-frame-border-width . 0)
     (left-fringe . 0)
     (right-fringe . 0)
     (undecorated . t)
     (ns-appearance . nil)))
  (force-mode-line-update t)
  (redisplay t))

;;; Display in Child Frame

(defun hyalo-sidebar--make-embedded-frame (buffer slot &optional params)
  "Create a child-frame for BUFFER to be embedded in SLOT.
SLOT is the embed slot identifier (\"left\", \"left-top\", \"left-bottom\", \"right\").
PARAMS are additional frame parameters.
Returns the created frame."
  (hyalo-log 'sidebar "Creating embedded frame for slot %s" slot)
  (let* ((parent (selected-frame))
         ;; Create font-spec safely - catch any errors from malformed face specs
         (safe-font (condition-case nil
                        (font-spec :family (or hyalo-sidebar-font "Menlo")
                                   :height hyalo-sidebar-font-height
                                   :weight 'regular)
                      (error nil)))
         (frame-params
          `((parent-frame . ,parent)
            (window-system . ns)
            (hyalo-embedded . t)
            (hyalo-embed-slot . ,slot)
            (name . ,(format "Hyalo Sidebar %s" slot))
            (minibuffer . nil)
            (undecorated . t)
            (alpha-background . 0)
            (internal-border-width . ,hyalo-sidebar-internal-border-width)
            (child-frame-border-width . 0)
            (left-fringe . 0)
            (right-fringe . 0)
            (tool-bar-lines . 0)
            (menu-bar-lines . 0)
            (vertical-scroll-bars . nil)
            (unsplittable . t)
            (no-accept-focus . nil)
            (width . 40)
            (height . 20)
            (left . -10000)
            (top . -10000)
            (visibility . t)
            ,@(when safe-font `((font . ,safe-font)))
            ,@params))
         (frame (make-frame frame-params)))
    ;; Display buffer in the frame
    (with-selected-frame frame
      (switch-to-buffer buffer)
      (setq-local mode-line-format nil)
      (setq-local header-line-format nil)
      ;; Force apply font to ensure Swift layout calculation is correct
      (when safe-font
        (condition-case nil
            (set-frame-font safe-font nil (list frame))
          (error nil))))
    ;; Force a redisplay to ensure the frame is fully created
    (redisplay t)
    ;; Small delay to ensure NSWindow is created before registration
    (sit-for 0.05)
    ;; Get the window-id for this frame and register with Swift
    (let* ((window-id (frame-parameter frame 'window-id))
           (outer-window-id (frame-parameter frame 'outer-window-id)))
      (hyalo-log 'sidebar "Frame window-id: %s, outer-window-id: %s" window-id outer-window-id)
      ;; On NS, outer-window-id is the NSWindow number
      (setq window-id (or outer-window-id window-id))
      ;; Register with Swift for embedding (pass window-id)
      (if (and (hyalo-available-p)
               (fboundp 'hyalo-sidebar-register-frame)
               window-id)
          (progn
            (hyalo-log 'sidebar "Registering frame for '%s' with window-id '%s'" slot window-id)
            (hyalo-sidebar-register-frame slot window-id)
            (hyalo-log 'sidebar "Registration complete for '%s'" slot))
        (hyalo-log 'sidebar "WARNING - Swift functions not available or no window-id!")))
    ;; Store frame reference for right sidebar
    (when (string= slot "right")
      (setq hyalo-sidebar--right-frame frame))
    frame))

(defun hyalo-sidebar-display-in-child-frame (buffer _alist)
  "Display BUFFER in a hidden child frame for embedding.
Sets `hyalo-embedded' parameter and positions off-screen."
  (let* ((frame (hyalo-sidebar--make-embedded-frame buffer "left")))
    ;; Store frame reference
    (setq hyalo-sidebar--left-frame frame)
    ;; Store main frame reference if not set
    (unless hyalo-sidebar--main-frame
      (setq hyalo-sidebar--main-frame
            (or (frame-parameter frame 'parent-frame)
                (car (filtered-frame-list
                      (lambda (f) (not (frame-parameter f 'hyalo-embedded))))))))
    ;; Configure dired-sidebar specifically
    (with-selected-frame frame
      (with-current-buffer buffer
        (hyalo-sidebar--configure-embedded)))
    ;; Trigger embedding after registration
    (when (and (hyalo-available-p)
               (fboundp 'hyalo-sidebar-embed-frames))
      (hyalo-log 'sidebar "Scheduling embed-frames for left")
      (run-with-timer 0.1 nil #'hyalo-sidebar-embed-frames "left"))
    (frame-selected-window frame)))

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
    (setq dired-sidebar-should-follow-file t)))

(defun hyalo-sidebar-setup-left ()
  "Setup the left sidebar (dired-sidebar).
Restored for compatibility."
  (interactive)
  (hyalo-sidebar-toggle-left))

(defun hyalo-sidebar-setup-right ()
  "Setup the right sidebar with agent-shell."
  (interactive)
  (condition-case err
      (progn
        (hyalo-log 'sidebar "Setting up right sidebar (agent-shell)...")
        ;; Clear any existing registration
        (when (and (hyalo-available-p)
                   (fboundp 'hyalo-sidebar-clear-registration))
          (hyalo-sidebar-clear-registration "right"))
        (if (require 'agent-shell nil t)
      (let ((existing-buf (cl-find-if (lambda (buf)
                                        (with-current-buffer buf
                                          (derived-mode-p 'agent-shell-mode)))
                                      (buffer-list))))
        ;; Create buffer if needed
        (unless existing-buf
          (hyalo-log 'sidebar "Creating new agent-shell...")
          (save-window-excursion
            (condition-case err
                (agent-shell)
              (error (hyalo-log 'sidebar "ERROR creating agent-shell: %s" err)))))
        ;; Find agent-shell buffer
        (let ((agent-buf (or existing-buf
                             (cl-find-if (lambda (buf)
                                           (with-current-buffer buf
                                             (derived-mode-p 'agent-shell-mode)))
                                         (buffer-list)))))
          (when (and agent-buf (buffer-live-p agent-buf))
            ;; Ensure cursor visibility for embedded frame
            (with-current-buffer agent-buf
              (setq-local cursor-type '(bar . 2))
              (setq-local cursor-in-non-selected-windows t))
            ;; Create the child frame
            (setq hyalo-sidebar--right-frame
                  (hyalo-sidebar--make-embedded-frame agent-buf "right"
                                                      '((cursor-type . (bar . 2))
                                                        (cursor-in-non-selected-windows . t))))
            ;; Apply glass effect appearance
            (hyalo-sidebar--strip-faces hyalo-sidebar--right-frame)
            ;; Trigger embedding
            (when (and (hyalo-available-p)
                       (fboundp 'hyalo-sidebar-embed-frames))
              (hyalo-log 'sidebar "Calling embed-frames for right")
              (hyalo-sidebar-embed-frames "right"))
            ;; Focus if buffer already existed (hook won't fire)
            (when (and existing-buf hyalo-sidebar--pending-focus-right)
              (hyalo-log 'sidebar "Buffer existed, using fallback focus timer")
              (setq hyalo-sidebar--pending-focus-right nil)
              (run-at-time 0.4 nil #'hyalo-sidebar--do-focus-right))
            (hyalo-log 'sidebar "Right sidebar setup complete."))))
          (hyalo-log 'sidebar "agent-shell not available")))
    (error
     (hyalo-log 'sidebar "ERROR in setup-right: %s" (error-message-string err)))))

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

(defun hyalo-sidebar--configure-embedded ()
  "Apply configuration for embedded sidebar buffers."
  ;; Store parent frame reference if valid
  (when-let* ((parent (frame-parameter (selected-frame) 'parent-frame)))
    (setq hyalo-sidebar--parent-frame parent))
  ;; Configure display
  (hyalo-sidebar-setup)
  ;; Enable monochrome icons
  (hyalo-sidebar--enable-monochrome)
  ;; Remove mode-line in sidebar
  (setq-local mode-line-format nil)
  (setq-local header-line-format nil)
  ;; Disable interfering modes
  (whitespace-mode -1)
  (display-line-numbers-mode -1)
  ;; Hide dired details (permissions, size, date) for cleaner display
  (when (derived-mode-p 'dired-mode 'dired-sidebar-mode)
    (dired-hide-details-mode 1)
    ;; Remove all margins
    (setq-local left-margin-width 0)
    (setq-local right-margin-width 0)
    (set-window-margins (selected-window) 0 0)
    ;; Hide the leading 2-char mark/flag column completely
    (setq-local dired-marker-char ?\s)
    ;; Use display property to hide first 2 chars (mark + type indicator)
    (font-lock-add-keywords
     nil
     '(("^\\(.\\).\\( *\\)"
        (1 '(face nil display ""))
        (2 '(face nil display "")))) ; No space before icon
     'append)
    (font-lock-flush))
  ;; Enable our keymap override - MUST set the variable to t for the keymap to be active
  (setq-local hyalo-sidebar-embedded t)
  (push `(hyalo-sidebar-embedded . ,hyalo-sidebar--keymap)
        minor-mode-overriding-map-alist))

(defun hyalo-sidebar--setup-if-embedded ()
  "Set up dired-sidebar for sidebar if this is an embedded frame."
  (when (and (derived-mode-p 'dired-sidebar-mode)
             (frame-parameter (selected-frame) 'hyalo-embedded))
    (hyalo-sidebar--configure-embedded)))

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
          (hyalo-log 'sidebar "Updating sidebar to: %s (frame: %s)" root sidebar-frame)
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
              (hyalo-sidebar--configure-embedded)
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
     (hyalo-log 'sidebar "Error updating sidebar: %s" err))))

(defun hyalo-sidebar--after-project-switch (&rest _)
  "Update sidebar after project switch completes."
  (hyalo-log 'sidebar "Project switch detected, scheduling update...")
  (run-with-timer 0.3 nil #'hyalo-sidebar-update-to-project))

;; Setup when dired-sidebar loads
(with-eval-after-load 'dired-sidebar
  (add-hook 'dired-sidebar-mode-hook #'hyalo-sidebar--setup-if-embedded))

;; Advise project-switch-project to update sidebar
(with-eval-after-load 'project
  (advice-add 'project-switch-project :after #'hyalo-sidebar--after-project-switch))

;;; Visibility Callbacks

(defun hyalo-sidebar--safe-delete-frame (frame name)
  "Safely delete FRAME if it's a valid sidebar frame.
NAME is used for logging. Returns t if deleted, nil otherwise."
  (when (and frame (frame-live-p frame))
    ;; Safety check: never delete the main frame
    (let ((parent (frame-parameter frame 'parent-frame)))
      (if parent
          (progn
            (delete-frame frame)
            t)
        (hyalo-log 'sidebar "WARNING: Refusing to delete %s - no parent-frame!" name)
        nil))))

(defun hyalo-on-sidebar-visibility-changed (panel visible)
  "Callback from Swift when sidebar visibility changes.
PANEL is 'left' or 'right'. VISIBLE is a boolean (t or nil).
Note: Uses run-at-time to defer frame operations since this is called
from a channel callback which may not be safe for frame manipulation."
  ;; Ensure visible is a proper boolean (Swift false becomes nil)
  (let ((visible-bool (not (null visible)))
        (panel-str (if (stringp panel) panel (format "%s" panel))))
    ;; Defer ALL processing to next event loop - channel callbacks are not safe
    ;; for frame operations or heavy Elisp processing
    (run-at-time 0 nil
                 (lambda ()
                   (when (string= panel-str "left")
                     (if visible-bool
                         ;; Panel visible - setup frames if they don't exist
                         (unless (and hyalo-sidebar--left-top-frame
                                      (frame-live-p hyalo-sidebar--left-top-frame))
                           (hyalo-sidebar--do-left-setup))
                       ;; Panel hidden - delete frames
                       (when (or hyalo-sidebar--left-top-frame hyalo-sidebar--left-bottom-frame)
                         (let ((top-frame hyalo-sidebar--left-top-frame)
                               (bottom-frame hyalo-sidebar--left-bottom-frame))
                           ;; Clear references immediately to prevent double-deletion
                           (setq hyalo-sidebar--left-top-frame nil)
                           (setq hyalo-sidebar--left-bottom-frame nil)
                           ;; Delete the frames
                           (hyalo-sidebar--safe-delete-frame top-frame "left-top-frame")
                           (hyalo-sidebar--safe-delete-frame bottom-frame "left-bottom-frame")))))))))

(defun hyalo-on-detail-visibility-changed (panel visible)
  "Callback from Swift when inspector visibility changes.
PANEL is 'right'. VISIBLE is a boolean.
Note: Uses run-at-time to defer frame operations since this is called
from a channel callback which may not be safe for frame manipulation."
  ;; Defer ALL processing to next event loop
  (run-at-time 0 nil
               (lambda ()
                 (when (string= panel "right")
                   (if visible
                       ;; Panel visible - setup frames if they don't exist
                       (unless (and hyalo-sidebar--right-frame
                                    (frame-live-p hyalo-sidebar--right-frame))
                         (hyalo-sidebar-setup-right))
                     ;; Panel hidden - delete frames
                     (when hyalo-sidebar--right-frame
                       (let ((right-frame hyalo-sidebar--right-frame))
                         ;; Clear reference immediately to prevent double-deletion
                         (setq hyalo-sidebar--right-frame nil)
                         ;; Delete the frame
                         (hyalo-sidebar--safe-delete-frame right-frame "right-frame"))))))))

;;; Visibility Watcher (for toolbar button integration)

(defun hyalo-sidebar--check-visibility ()
  "Check if any panel needs embedded content setup.
Called periodically when sidebar mode is active.
This handles toolbar button clicks that show panels."
  (when (and (hyalo-available-p)
             (fboundp 'hyalo-panel-needs-setup))
    (let ((needs-setup (hyalo-panel-needs-setup)))
      (when needs-setup
        (hyalo-log 'sidebar "Panel needs setup: %s" needs-setup)
        (cond
         ((and (equal needs-setup "right")
               (not (and hyalo-sidebar--right-frame
                         (frame-live-p hyalo-sidebar--right-frame))))
          (hyalo-log 'sidebar "Starting right-setup from visibility watcher")
          (hyalo-sidebar-setup-right))
         ((and (equal needs-setup "left")
               (not (and hyalo-sidebar--left-top-frame
                         (frame-live-p hyalo-sidebar--left-top-frame)))
               (not (and hyalo-sidebar--left-bottom-frame
                         (frame-live-p hyalo-sidebar--left-bottom-frame))))
          (hyalo-log 'sidebar "Starting left-setup from visibility watcher")
          (hyalo-sidebar--do-left-setup)))))))

(defun hyalo-sidebar--start-visibility-watcher ()
  "Start the visibility watcher timer."
  (unless hyalo-sidebar--visibility-timer
    (setq hyalo-sidebar--visibility-timer
          (run-with-timer 0.5 0.5 #'hyalo-sidebar--check-visibility))
    (hyalo-log 'sidebar "Visibility watcher started")))

(defun hyalo-sidebar--stop-visibility-watcher ()
  "Stop the visibility watcher timer."
  (when hyalo-sidebar--visibility-timer
    (cancel-timer hyalo-sidebar--visibility-timer)
    (setq hyalo-sidebar--visibility-timer nil)
    (hyalo-log 'sidebar "Visibility watcher stopped")))

;;; Agent-shell focus hook

(defun hyalo-sidebar--agent-shell-mode-hook ()
  "Hook to focus agent-shell after it's ready."
  (when hyalo-sidebar--pending-focus-right
    (setq hyalo-sidebar--pending-focus-right nil)
    (hyalo-log 'sidebar "agent-shell-mode-hook: focusing right sidebar")
    ;; Small delay to ensure embedding is complete
    (run-at-time 0.3 nil #'hyalo-sidebar--do-focus-right)))

;; Add hook for agent-shell focus
(with-eval-after-load 'agent-shell
  (add-hook 'agent-shell-mode-hook #'hyalo-sidebar--agent-shell-mode-hook))

;;; Callback Setup

(defun hyalo-sidebar--setup-visibility-callbacks ()
  "Setup Swift-to-Elisp visibility callbacks.
This enables immediate response to toolbar button clicks."
  (when (and (hyalo-available-p)
             (not hyalo-sidebar--callbacks-installed))
    (when (fboundp 'hyalo-setup-visibility-callbacks)
      (condition-case err
          (progn
            (hyalo-setup-visibility-callbacks)
            (setq hyalo-sidebar--callbacks-installed t)
            (hyalo-log 'sidebar "Visibility callbacks installed"))
        (error
         (hyalo-log 'sidebar "Failed to install callbacks: %s" err))))
    ;; ALWAYS start the watcher as a backup - callbacks may not be reliable
    (hyalo-sidebar--start-visibility-watcher)))

(defun hyalo-sidebar--teardown-visibility-callbacks ()
  "Teardown Swift-to-Elisp visibility callbacks."
  (setq hyalo-sidebar--callbacks-installed nil)
  (hyalo-sidebar--stop-visibility-watcher))

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
        (hyalo-log 'sidebar "Enabled")
        ;; Ensure Hyalo module is loaded (handles init order issues)
        (ignore-errors (hyalo-ensure))
        ;; Store main frame reference
        (setq hyalo-sidebar--main-frame (selected-frame))
        ;; Force dired-sidebar to use our child frame
        (add-to-list 'display-buffer-alist
                     '("^\\*dired-sidebar.*" . (hyalo-sidebar-display-in-child-frame)))
        ;; Setup Swift visibility callbacks (or fallback to watcher)
        (hyalo-sidebar--setup-visibility-callbacks)
        ;; Ensure dired-sidebar is configured
        (when (featurep 'dired-sidebar)
          (hyalo-sidebar-setup)))
    (progn
      (hyalo-log 'sidebar "Disabled")
      ;; Teardown callbacks and watcher
      (hyalo-sidebar--teardown-visibility-callbacks)
      ;; Remove display buffer configuration
      (setq display-buffer-alist
            (cl-remove-if (lambda (x) (string-match-p "dired-sidebar" (car x)))
                          display-buffer-alist))
      ;; Clean up frames
      (hyalo-sidebar-teardown))))

;;; Resize handling (called from Swift)

(defun hyalo-sidebar-resize-slot (slot width height)
  "Resize the frame in SLOT to WIDTH x HEIGHT pixels.
Called from Swift when sidebar geometry changes."
  (let ((frame (pcase slot
                 ("left" hyalo-sidebar--left-bottom-frame)
                 ("left-top" hyalo-sidebar--left-top-frame)
                 ("left-bottom" hyalo-sidebar--left-bottom-frame)
                 ("right" hyalo-sidebar--right-frame))))
    (when (and frame (frame-live-p frame))
      (let* ((char-width (frame-char-width frame))
             (char-height (frame-char-height frame))
             (cols (max 10 (/ width char-width)))
             (rows (max 3 (/ height char-height))))
        (set-frame-size frame cols rows)))))

;;; Frame slot query (called from Swift)

(defun hyalo-sidebar-get-frame-slot (window-id)
  "Get the embed slot for frame with WINDOW-ID.
Returns the slot string or nil if not an embedded frame."
  (catch 'found
    (dolist (frame (frame-list))
      (when (and (frame-parameter frame 'hyalo-embedded)
                 (equal (frame-parameter frame 'outer-window-id) window-id))
        (throw 'found (frame-parameter frame 'hyalo-embed-slot))))
    nil))

;;; Teardown

(defun hyalo-sidebar-teardown ()
  "Clean up all sidebar frames."
  (interactive)
  ;; Stop visibility watcher
  (hyalo-sidebar--stop-visibility-watcher)
  ;; Notify Swift to detach
  (when (and (hyalo-available-p)
             (fboundp 'hyalo-sidebar-detach-frames))
    (hyalo-sidebar-detach-frames "left")
    (hyalo-sidebar-detach-frames "right"))
  ;; Delete tracked frames
  (when (and hyalo-sidebar--left-top-frame
             (frame-live-p hyalo-sidebar--left-top-frame))
    (delete-frame hyalo-sidebar--left-top-frame))
  (when (and hyalo-sidebar--left-bottom-frame
             (frame-live-p hyalo-sidebar--left-bottom-frame))
    (delete-frame hyalo-sidebar--left-bottom-frame))
  (when (and hyalo-sidebar--right-frame
             (frame-live-p hyalo-sidebar--right-frame))
    (delete-frame hyalo-sidebar--right-frame))
  ;; Clear references
  (setq hyalo-sidebar--left-top-frame nil
        hyalo-sidebar--left-bottom-frame nil
        hyalo-sidebar--right-frame nil))

;;; Interactive Commands

;;;###autoload
(defun hyalo-sidebar-toggle-left ()
  "Toggle the left sidebar. Sets up frames on first use."
  (interactive)
  (if (and hyalo-sidebar--left-top-frame
           (frame-live-p hyalo-sidebar--left-top-frame))
      ;; Frames exist - just toggle Swift visibility
      (when (and (hyalo-available-p) (fboundp 'hyalo-sidebar-toggle))
        (hyalo-sidebar-toggle))
    ;; Frames don't exist - show panel, setup, then focus
    (when (and (hyalo-available-p) (fboundp 'hyalo-sidebar-show))
      (hyalo-sidebar-show))
    (run-at-time 0.1 nil #'hyalo-sidebar--do-left-setup)
    (run-at-time 0.5 nil #'hyalo-sidebar--do-focus-left)))

(defun hyalo-sidebar--do-left-setup ()
  "Internal: Set up the left sidebar frames (ibuffer + dired-sidebar)."
  (hyalo-log 'sidebar "Setting up left sidebar...")
  ;; IMPORTANT: Capture the main frame FIRST before any child frames are created
  (let ((main-frame (selected-frame)))
    (setq hyalo-sidebar--main-frame main-frame)
    (hyalo-log 'sidebar "Left setup starting, main-frame=%s" main-frame)

    ;; Clear any existing registration
    (when (and (hyalo-available-p)
               (fboundp 'hyalo-sidebar-clear-registration))
      (hyalo-sidebar-clear-registration "left"))

    ;; Create ibuffer frame (top)
    (require 'ibuffer)
    (require 'hyalo-ibuffer)
    (let ((ibuf-buffer (get-buffer-create "*Hyalo-Ibuffer*")))
      (with-current-buffer ibuf-buffer
        (unless (eq major-mode 'ibuffer-mode)
          (ibuffer-mode))
        ;; Apply sidebar-specific configuration
        (when (fboundp 'hyalo-ibuffer-sidebar-setup)
          (hyalo-ibuffer-sidebar-setup)))
      (setq hyalo-sidebar--left-top-frame
            (hyalo-sidebar--make-embedded-frame ibuf-buffer "left-top"))
      (hyalo-sidebar--strip-faces hyalo-sidebar--left-top-frame)
      (hyalo-log 'sidebar "Created ibuffer frame"))

    ;; Create dired-sidebar frame (bottom)
    (when (require 'dired-sidebar nil t)
      (hyalo-log 'sidebar "Setting up dired-sidebar...")
      (let* ((project-root (hyalo-sidebar--get-root-directory)))
        (hyalo-log 'sidebar "Creating dired-sidebar for: %s" project-root)
        (let ((dired-buf (save-window-excursion
                           (dired-sidebar-get-or-create-buffer project-root))))
          (hyalo-log 'sidebar "dired-buf = %s" dired-buf)
          (if dired-buf
              (progn
                ;; Ensure dired-sidebar-mode is active
                (with-current-buffer dired-buf
                  (unless (derived-mode-p 'dired-sidebar-mode)
                    (dired-sidebar-mode)))
                ;; Create the child frame
                (setq hyalo-sidebar--left-bottom-frame
                      (hyalo-sidebar--make-embedded-frame dired-buf "left-bottom"))
                ;; Apply configuration in the child frame context
                (with-selected-frame hyalo-sidebar--left-bottom-frame
                  (with-current-buffer dired-buf
                    ;; Set the parent frame reference
                    (setq hyalo-sidebar--parent-frame main-frame)
                    ;; Apply dired-sidebar display settings
                    (hyalo-sidebar-setup)
                    ;; Set keybindings using overriding map
                    (setq-local hyalo-sidebar-embedded t)
                    (push `(hyalo-sidebar-embedded . ,hyalo-sidebar--keymap)
                          minor-mode-overriding-map-alist)
                    ;; Remove mode-line
                    (setq-local mode-line-format nil)
                    (setq-local header-line-format nil)))
                (hyalo-sidebar--strip-faces hyalo-sidebar--left-bottom-frame)
                (hyalo-log 'sidebar "Created dired-sidebar frame"))
            (hyalo-log 'sidebar "WARNING - Could not get dired-sidebar buffer")))))

    ;; Notify Swift to embed these frames
    (hyalo-log 'sidebar "Calling embed-frames for 'left'")
    (when (and (hyalo-available-p)
               (fboundp 'hyalo-sidebar-embed-frames))
      (hyalo-sidebar-embed-frames "left")
      (hyalo-log 'sidebar "embed-frames 'left' complete")))
  (hyalo-log 'sidebar "Left setup complete"))

(defun hyalo-sidebar--do-focus-left ()
  "Internal: Focus the left sidebar frame (dired-sidebar)."
  (when (and hyalo-sidebar--left-bottom-frame
             (frame-live-p hyalo-sidebar--left-bottom-frame))
    (select-frame-set-input-focus hyalo-sidebar--left-bottom-frame)
    (let ((win (frame-first-window hyalo-sidebar--left-bottom-frame)))
      (when win
        (select-window win)))
    (hyalo-log 'sidebar "Focused left frame")))

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
  "Toggle the right sidebar (inspector). Sets up frames on first use."
  (interactive)
  (if (and hyalo-sidebar--right-frame
           (frame-live-p hyalo-sidebar--right-frame))
      ;; Frame exists - just toggle Swift visibility
      (when (and (hyalo-available-p) (fboundp 'hyalo-detail-toggle))
        (hyalo-detail-toggle))
    ;; Frame doesn't exist - show panel, setup, then focus
    (when (and (hyalo-available-p) (fboundp 'hyalo-detail-show))
      (hyalo-detail-show))
    (run-at-time 0.1 nil #'hyalo-sidebar-setup-right)
    (run-at-time 0.5 nil #'hyalo-sidebar--do-focus-right)))

(defun hyalo-sidebar--do-focus-right ()
  "Internal: Focus the right sidebar frame."
  (when (and hyalo-sidebar--right-frame
             (frame-live-p hyalo-sidebar--right-frame))
    (select-frame-set-input-focus hyalo-sidebar--right-frame)
    (let ((win (frame-first-window hyalo-sidebar--right-frame)))
      (when win
        (select-window win)))
    (hyalo-log 'sidebar "Focused right frame")))

;;;###autoload
(defun hyalo-sidebar-focus-right ()
  "Focus the inspector/right sidebar."
  (interactive)
  (cond
   ;; Frame exists and is live - focus it
   ((and hyalo-sidebar--right-frame
         (frame-live-p hyalo-sidebar--right-frame))
    (hyalo-sidebar--do-focus-right))
   ;; Inspector not visible - show it first, then setup and focus
   ((and (hyalo-available-p)
         (fboundp 'hyalo-detail-visible-p)
         (not (hyalo-detail-visible-p)))
    (hyalo-log 'sidebar "Inspector not visible, showing first")
    (when (fboundp 'hyalo-detail-show)
      (hyalo-detail-show))
    (run-at-time 0.2 nil #'hyalo-sidebar-setup-right)
    (run-at-time 0.5 nil #'hyalo-sidebar--do-focus-right))
   ;; Frame doesn't exist but inspector is visible - create it
   (t
    (hyalo-log 'sidebar "Frame doesn't exist, creating...")
    (hyalo-sidebar-setup-right)
    (run-at-time 0.3 nil #'hyalo-sidebar--do-focus-right))))

;;;###autoload
(defun hyalo-sidebar-focus-center ()
  "Focus the main Emacs frame (center window)."
  (interactive)
  (let ((frame (or (and hyalo-sidebar--main-frame
                        (frame-live-p hyalo-sidebar--main-frame)
                        hyalo-sidebar--main-frame)
                   (hyalo-sidebar--get-parent-frame))))
    (when (and frame (frame-live-p frame))
      (select-frame-set-input-focus frame)
      (hyalo-log 'sidebar "Focused main frame"))))

(provide 'hyalo-sidebar)
;;; hyalo-sidebar.el ends here
