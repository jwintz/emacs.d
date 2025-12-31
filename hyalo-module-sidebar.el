;;; hyalo-module-sidebar.el --- Embedded child-frame sidebars -*- lexical-binding: t -*- no-byte-compile: t -*-

;; Copyright (C) 2025
;; Author: Julien Wintz <julien.wintz@inria.fr>
;; Version: 1.0.0
;; Package-Requires: ((emacs "30.1") (dired-sidebar "0.0"))
;; Keywords: frames, sidebar, macos

;;; Commentary:

;; Provides embedded child-frame sidebars for NavigationSplitView.
;; This module:
;; 1. Creates child-frames for dired-sidebar, ibuffer, and agent-shell
;; 2. Marks frames with (hyalo-embedded . t) for Swift detection
;; 3. Handles resize notifications from Swift side
;; 4. Manages frame lifecycle and focus
;; 5. Uses event forwarding to route events back to original Emacs windows

;;; Code:

(require 'hyalo-module)
(require 'hyalo-ibuffer)
(require 'hyalo-dired-sidebar)  ; Required for dired-sidebar keybindings

(defgroup hyalo-module-sidebar nil
  "Hyalo module for embedded sidebar child-frames."
  :group 'hyalo-module
  :prefix "hyalo-module-sidebar-")

(defcustom hyalo-sidebar-internal-border-width 12
  "Internal border width for embedded sidebar frames.
This provides margin matching SwiftUI design guidelines.
Set to 0 for no margin, 8 for compact, 12 for standard, 16 for spacious."
  :type 'integer
  :group 'hyalo-module-sidebar)

(defcustom hyalo-sidebar-font nil
  "Font to use for sidebar buffers.
When non-nil, should be a font spec string like \"SF Pro-12\".
When nil, uses the default frame font."
  :type '(choice (const nil) string)
  :group 'hyalo-module-sidebar)

(defvar hyalo-sidebar-debug nil
  "Enable debug logging for sidebar embedding.")

(defun hyalo-sidebar--log (format-string &rest args)
  "Log debug message if `hyalo-sidebar-debug' is non-nil."
  (when hyalo-sidebar-debug
    (apply #'message (concat "[hyalo-sidebar] " format-string) args)))

;;; Visibility change hooks (called by Swift via Channel)

(defvar hyalo-on-sidebar-visibility-changed nil
  "Hook run when the sidebar visibility changes.
Each function is called with two arguments:
SIDE - a string, \"left\" for the sidebar
VISIBLE - a boolean, t if the panel became visible, nil if hidden")

(defvar hyalo-on-detail-visibility-changed nil
  "Hook run when the inspector (detail) visibility changes.
Each function is called with two arguments:
SIDE - a string, \"right\" for the inspector
VISIBLE - a boolean, t if the panel became visible, nil if hidden")

(defun hyalo-sidebar--on-sidebar-visibility-changed (side visible)
  "Handle sidebar visibility change from Swift.
SIDE is \"left\", VISIBLE is t if now visible.
This is added to `hyalo-on-sidebar-visibility-changed-functions'."
  (hyalo-sidebar--log "Sidebar visibility changed: side=%s visible=%s" side visible)
  (when (and visible (string= side "left"))
    ;; Only setup if frames don't exist
    (unless (or hyalo-sidebar--left-top-frame
                hyalo-sidebar--left-bottom-frame)
      (hyalo-sidebar--log "Triggering left-setup from callback")
      (hyalo-sidebar-left-setup))))

(defun hyalo-sidebar--on-detail-visibility-changed (side visible)
  "Handle inspector/detail visibility change from Swift.
SIDE is \"right\", VISIBLE is t if now visible.
This is added to `hyalo-on-detail-visibility-changed-functions'."
  (hyalo-sidebar--log "Detail visibility changed: side=%s visible=%s" side visible)
  (when (and visible (string= side "right"))
    ;; Only setup if frame doesn't exist
    (unless hyalo-sidebar--right-frame
      (hyalo-sidebar--log "Triggering right-setup from callback")
      (hyalo-sidebar-right-setup))))


;;; Frame storage

(defvar hyalo-sidebar--left-top-frame nil
  "Child frame for left sidebar top section (ibuffer).")

(defvar hyalo-sidebar--left-bottom-frame nil
  "Child frame for left sidebar bottom section (treemacs).")

(defvar hyalo-sidebar--right-frame nil
  "Child frame for right sidebar (agent-shell).")

(defvar hyalo-sidebar--active nil
  "Non-nil when sidebar frames are active.")

(defvar hyalo-sidebar--pending-focus-right nil
  "Non-nil when we need to focus right sidebar after agent-shell is ready.")

;;; Child-frame creation

(defun hyalo-sidebar--prevent-split (&rest _)
  "Prevent window splitting in embedded sidebar frames."
  (when (frame-parameter (selected-frame) 'hyalo-embedded)
    (user-error "Cannot split windows in sidebar")))

(defun hyalo-sidebar--make-embedded-frame (buffer slot &optional params)
  "Create a child-frame for BUFFER designed to be embedded in SwiftUI.
SLOT is the embed slot identifier (\"left-top\", \"left-bottom\", \"right\").
PARAMS are additional frame parameters.
Returns the created frame."
  (let* ((parent (selected-frame))
         (frame-params
          `((parent-frame . ,parent)
            ;; Mark as embedded so Swift knows to detach NSView
            (hyalo-embedded . t)
            (hyalo-embed-slot . ,slot)
            ;; Fully transparent - SwiftUI provides glass
            (alpha-background . 0)
            ;; No decorations
            (undecorated . t)
            (no-accept-focus . nil)  ; Allow focus for editing
            ;; Prevent window splitting
            (unsplittable . t)
            ;; Internal border for SwiftUI-style margins
            (internal-border-width . ,hyalo-sidebar-internal-border-width)
            (child-frame-border-width . 0)
            (left-fringe . 0)
            (right-fringe . 0)
            ;; No mode-line or header-line
            (minibuffer . nil)
            ;; Initial position off-screen (Swift will embed)
            (left . -10000)
            (top . -10000)
            ;; Initial size - will be set by Swift
            (width . 40)
            (height . 20)
            ;; Font for sidebar
            ,@(when hyalo-sidebar-font
                `((font . ,hyalo-sidebar-font)))
            ,@params)))
    (let ((frame (make-frame frame-params)))
      (hyalo-sidebar--log "Created frame %s for slot '%s'" frame slot)
      ;; Display buffer in the frame
      (with-selected-frame frame
        (switch-to-buffer buffer)
        ;; Hide mode-line in this frame
        (setq-local mode-line-format nil)
        (setq-local header-line-format nil)
        ;; Set font if configured
        (when hyalo-sidebar-font
          (set-frame-font hyalo-sidebar-font nil (list frame))))
      ;; Force a redisplay to ensure the frame is fully created
      (redisplay t)
      ;; Small delay to ensure NSWindow is created before registration
      (sit-for 0.05)
      ;; Get the window-id for this frame
      ;; On macOS NS, outer-window-id contains the NSWindow number
      ;; ns-window-number returns internal Emacs frame number (useless)
      (let* ((window-id (frame-parameter frame 'window-id))
             (outer-window-id (frame-parameter frame 'outer-window-id))
             (ns-win-num (and (fboundp 'ns-window-number)
                              (ns-window-number frame))))
        (hyalo-sidebar--log "Frame window-id: %s, outer-window-id: %s, ns-window-number: %s"
                            window-id outer-window-id ns-win-num)
        ;; On NS, outer-window-id is the NSWindow number (large int like 44382)
        ;; ns-window-number returns internal frame number (small int like 2), don't use it
        (setq window-id (or outer-window-id window-id))
        ;; Register with Swift for embedding (pass window-id)
        (if (and (hyalo-module-available-p)
                 (fboundp 'hyalo-sidebar-register-frame)
                 window-id)
            (progn
              (hyalo-sidebar--log "Calling hyalo-sidebar-register-frame for '%s' with window-id '%s'" slot window-id)
              (hyalo-sidebar-register-frame slot window-id)
              (hyalo-sidebar--log "Registration complete for '%s'" slot))
          (hyalo-sidebar--log "WARNING: Swift functions not available or no window-id!")))
      frame)))

(defun hyalo-sidebar--strip-faces (frame)
  "Set up FRAME for glass effect - fully transparent.
Preserves internal-border-width for SwiftUI-style margins."
  (modify-frame-parameters
   frame
   `((background-color . "white")
     (alpha-background . 0)
     (ns-alpha-elements . (ns-alpha-all))
     (ns-background-blur . 0)
     ;; Keep internal-border-width for margins (set in make-embedded-frame)
     (child-frame-border-width . 0)
     (left-fringe . 0)
     (right-fringe . 0)
     (undecorated . t)
     (ns-appearance . nil)))
  (force-mode-line-update t)
  (redisplay t))

;;; Left sidebar setup (ibuffer + dired-sidebar)

(defun hyalo-sidebar-left-setup ()
  "Setup left sidebar with ibuffer (top) and dired-sidebar (bottom)."
  (interactive)
  ;; IMPORTANT: Capture the main frame FIRST before any child frames are created
  (let ((main-frame (selected-frame)))
    (hyalo-sidebar--log "Left setup starting, main-frame=%s" main-frame)

    ;; Clear any existing registration
    (when (and (hyalo-module-available-p)
               (fboundp 'hyalo-sidebar-clear-registration))
      (hyalo-sidebar-clear-registration "left"))

    ;; Create ibuffer frame (top)
    (require 'ibuffer)
    (let ((ibuf-buffer (get-buffer-create "*Hyalo-Ibuffer*")))
      (with-current-buffer ibuf-buffer
        (unless (eq major-mode 'ibuffer-mode)
          (ibuffer-mode))
        ;; Apply sidebar-specific configuration
        (hyalo-ibuffer-sidebar-setup))
      (setq hyalo-sidebar--left-top-frame
            (hyalo-sidebar--make-embedded-frame
             ibuf-buffer "left-top"))
      (hyalo-sidebar--strip-faces hyalo-sidebar--left-top-frame))

    ;; Create dired-sidebar frame (bottom)
    (when (require 'dired-sidebar nil t)
      (hyalo-sidebar--log "Setting up dired-sidebar...")
      ;; Get project root using hyalo-dired-sidebar function
      (let* ((project-root (if (fboundp 'hyalo-dired-sidebar--get-root-directory)
                               (hyalo-dired-sidebar--get-root-directory)
                             (expand-file-name default-directory))))
        (hyalo-sidebar--log "Creating dired-sidebar for: %s" project-root)
        ;; Create dired-sidebar buffer using proper API
        (let ((dired-buf (save-window-excursion
                           (dired-sidebar-get-or-create-buffer project-root))))
          (hyalo-sidebar--log "dired-buf = %s, using main-frame = %s" dired-buf main-frame)
          (if dired-buf
              (progn
                ;; Ensure dired-sidebar-mode is active
                (with-current-buffer dired-buf
                  (unless (derived-mode-p 'dired-sidebar-mode)
                    (dired-sidebar-mode)))
                ;; Create the child frame
                (setq hyalo-sidebar--left-bottom-frame
                      (hyalo-sidebar--make-embedded-frame
                       dired-buf "left-bottom"))
                ;; Apply configuration in the child frame context
                (with-selected-frame hyalo-sidebar--left-bottom-frame
                  (with-current-buffer dired-buf
                    ;; Set the parent frame reference
                    (when (boundp 'hyalo-dired-sidebar--parent-frame)
                      (setq hyalo-dired-sidebar--parent-frame main-frame)
                      (hyalo-sidebar--log "Set hyalo-dired-sidebar--parent-frame = %s" main-frame))
                    ;; Apply dired-sidebar display settings
                    (when (fboundp 'hyalo-dired-sidebar-setup)
                      (hyalo-dired-sidebar-setup))
                    ;; Set keybindings using overriding map for highest priority
                    ;; MUST set the variable to t for the keymap to be active
                    (setq-local hyalo-dired-sidebar-embedded t)
                    (when (boundp 'hyalo-dired-sidebar--keymap)
                      (push `(hyalo-dired-sidebar-embedded . ,hyalo-dired-sidebar--keymap)
                            minor-mode-overriding-map-alist)
                      (hyalo-sidebar--log "Dired-sidebar keybindings set via overriding-map-alist"))
                    ;; Remove mode-line
                    (setq-local mode-line-format nil)
                    (setq-local header-line-format nil)))
                (hyalo-sidebar--strip-faces hyalo-sidebar--left-bottom-frame))
            (hyalo-sidebar--log "WARNING: Could not get dired-sidebar buffer")))))

    ;; Notify Swift to embed these frames
    (hyalo-sidebar--log "Calling embed-frames for 'left'")
    (if (and (hyalo-module-available-p)
             (fboundp 'hyalo-sidebar-embed-frames))
        (progn
          (hyalo-sidebar-embed-frames "left")
          (hyalo-sidebar--log "embed-frames 'left' complete"))
      (hyalo-sidebar--log "WARNING: embed-frames not available"))))

;;; Right sidebar setup (agent-shell)

(defun hyalo-sidebar-right-setup ()
  "Setup right sidebar with agent-shell."
  (interactive)
  (hyalo-sidebar--log "=== hyalo-sidebar-right-setup called ===")
  ;; Clear any existing registration
  (when (and (hyalo-module-available-p)
             (fboundp 'hyalo-sidebar-clear-registration))
    (hyalo-sidebar--log "Clearing registration for 'right'")
    (hyalo-sidebar-clear-registration "right"))

  (if (require 'agent-shell nil t)
      (progn
        (hyalo-sidebar--log "agent-shell loaded")
        ;; Check if buffer already exists (by major mode, name is dynamic)
        (let ((existing-buf (car (seq-filter
                                  (lambda (buf)
                                    (with-current-buffer buf
                                      (derived-mode-p 'agent-shell-mode)))
                                  (buffer-list)))))
          (hyalo-sidebar--log "existing agent-shell buffer: %s" existing-buf)
          ;; Create buffer if needed
          (unless existing-buf
            (hyalo-sidebar--log "Creating new agent-shell...")
            (save-window-excursion
              (condition-case err
                  (agent-shell)
                (error (hyalo-sidebar--log "ERROR creating agent-shell: %s" err)))))
          ;; Find agent-shell buffer by major mode (buffer name is dynamic)
          (let ((agent-buf (car (seq-filter
                                 (lambda (buf)
                                   (with-current-buffer buf
                                     (derived-mode-p 'agent-shell-mode)))
                                 (buffer-list)))))
            (hyalo-sidebar--log "agent-buf = %s" agent-buf)
            (if agent-buf
                (progn
                  ;; Ensure cursor visibility and style for embedded frame
                  (with-current-buffer agent-buf
                    (setq-local cursor-type '(bar . 2))
                    (setq-local cursor-in-non-selected-windows t))

                  (setq hyalo-sidebar--right-frame
                        (hyalo-sidebar--make-embedded-frame
                         agent-buf "right"
                         '((cursor-type . (bar . 2))
                           (cursor-in-non-selected-windows . t))))
                  (hyalo-sidebar--log "Created right frame: %s" hyalo-sidebar--right-frame)
                  (hyalo-sidebar--strip-faces hyalo-sidebar--right-frame)

                  ;; Notify Swift to embed
                  (hyalo-sidebar--log "Calling embed-frames for 'right'")
                  (if (and (hyalo-module-available-p)
                           (fboundp 'hyalo-sidebar-embed-frames))
                      (progn
                        (hyalo-sidebar-embed-frames "right")
                        (hyalo-sidebar--log "embed-frames 'right' complete")
                        ;; If buffer already existed, hook won't fire - use fallback timer
                        (when (and existing-buf hyalo-sidebar--pending-focus-right)
                          (hyalo-sidebar--log "Buffer existed, using fallback focus timer")
                          (setq hyalo-sidebar--pending-focus-right nil)
                          (run-at-time 0.4 nil #'hyalo-sidebar--do-focus-right)))
                    (hyalo-sidebar--log "WARNING: embed-frames not available")))
              (hyalo-sidebar--log "ERROR: agent-buf is nil after creation attempt")))))
    (hyalo-sidebar--log "WARNING: agent-shell not available")))

;;; Resize handling (called from Swift)

(defun hyalo-sidebar-resize-slot (slot width height)
  "Resize the frame in SLOT to WIDTH x HEIGHT pixels.
Called from Swift when sidebar geometry changes."
  (let ((frame (pcase slot
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
                 (equal (frame-parameter frame 'window-id) window-id))
        (throw 'found (frame-parameter frame 'hyalo-embed-slot))))
    nil))

;;; Teardown

(defun hyalo-sidebar--cleanup-orphaned-frames ()
  "Delete any orphaned embedded frames.
Orphaned frames are those marked with `hyalo-embedded' that are not
tracked in the sidebar frame variables."
  (let ((tracked (list hyalo-sidebar--left-top-frame
                       hyalo-sidebar--left-bottom-frame
                       hyalo-sidebar--right-frame))
        (orphans 0))
    (dolist (frame (frame-list))
      (when (and (frame-parameter frame 'hyalo-embedded)
                 (not (memq frame tracked))
                 (frame-live-p frame))
        (hyalo-sidebar--log "Deleting orphaned frame: %s" frame)
        (delete-frame frame)
        (setq orphans (1+ orphans))))
    (when (> orphans 0)
      (hyalo-sidebar--log "Cleaned up %d orphaned frames" orphans))))

(defun hyalo-sidebar-teardown ()
  "Clean up all sidebar frames."
  (interactive)
  ;; Stop visibility watcher
  (hyalo-sidebar--stop-visibility-watcher)

  ;; Notify Swift to detach
  (when (and (hyalo-module-available-p)
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
        hyalo-sidebar--right-frame nil
        hyalo-sidebar--active nil)

  ;; Clean up any orphaned frames
  (hyalo-sidebar--cleanup-orphaned-frames))

;;; Main frame reference (for sidebar file opens)

(defvar hyalo-sidebar--main-frame nil
  "The main Emacs frame (parent of all embedded sidebars).
Used by sidebar buffers to open files in the correct frame.")

;;; Mode definition

;;;###autoload
(define-minor-mode hyalo-module-sidebar-mode
  "Global minor mode to enable embedded sidebar child-frames."
  :global t
  :group 'hyalo-module-sidebar
  (if hyalo-module-sidebar-mode
      (progn
        (setq frame-resize-pixelwise t)
        (setq hyalo-sidebar--active t)
        ;; Store main frame reference for sidebar file opens
        (setq hyalo-sidebar--main-frame (selected-frame))
        (hyalo-sidebar--log "Sidebar mode enabled, main-frame=%s" hyalo-sidebar--main-frame))
    (setq hyalo-sidebar--main-frame nil)
    (hyalo-sidebar-teardown)))

;;; Public API

(defun hyalo-sidebar-show-left ()
  "Show the left sidebar with ibuffer and treemacs."
  (interactive)
  (unless hyalo-sidebar--left-top-frame
    (hyalo-sidebar-left-setup))
  (when (and (hyalo-module-available-p)
             (fboundp 'hyalo-sidebar-show))
    (hyalo-sidebar-show)))

(defun hyalo-sidebar-show-right ()
  "Show the right sidebar with agent-shell."
  (interactive)
  (unless hyalo-sidebar--right-frame
    (hyalo-sidebar-right-setup))
  (when (and (hyalo-module-available-p)
             (fboundp 'hyalo-detail-show))
    (hyalo-detail-show)))

(defun hyalo-sidebar-hide-left ()
  "Hide the left sidebar."
  (interactive)
  (when (and (hyalo-module-available-p)
             (fboundp 'hyalo-sidebar-hide))
    (hyalo-sidebar-hide)))

(defun hyalo-sidebar-hide-right ()
  "Hide the right sidebar."
  (interactive)
  (when (and (hyalo-module-available-p)
             (fboundp 'hyalo-detail-hide))
    (hyalo-detail-hide)))

(defun hyalo-sidebar--do-focus-left ()
  "Internal: Actually focus the left sidebar frame."
  (when (and hyalo-sidebar--left-bottom-frame
             (frame-live-p hyalo-sidebar--left-bottom-frame))
    (select-frame-set-input-focus hyalo-sidebar--left-bottom-frame)
    (let ((win (frame-first-window hyalo-sidebar--left-bottom-frame)))
      (when win
        (select-window win)))
    (hyalo-sidebar--log "Focused left frame")))

(defun hyalo-sidebar--do-focus-right ()
  "Internal: Actually focus the right sidebar frame."
  (when (and hyalo-sidebar--right-frame
             (frame-live-p hyalo-sidebar--right-frame))
    (select-frame-set-input-focus hyalo-sidebar--right-frame)
    (let ((win (frame-first-window hyalo-sidebar--right-frame)))
      (when win
        (select-window win)))
    (hyalo-sidebar--log "Focused right frame")))

(defun hyalo-sidebar-toggle-left ()
  "Toggle the left sidebar. Focuses after initial setup."
  (interactive)
  (if hyalo-sidebar--left-top-frame
      ;; Frame exists - just toggle visibility
      (when (and (hyalo-module-available-p)
                 (fboundp 'hyalo-sidebar-toggle))
        (hyalo-sidebar-toggle))
    ;; Frame doesn't exist - show panel, setup, then focus
    (when (and (hyalo-module-available-p)
               (fboundp 'hyalo-sidebar-show))
      (hyalo-sidebar-show))
    (run-at-time 0.1 nil #'hyalo-sidebar-left-setup)
    (run-at-time 0.5 nil #'hyalo-sidebar--do-focus-left)))

(defun hyalo-sidebar-toggle-right ()
  "Toggle the right sidebar (inspector). Focuses after initial setup."
  (interactive)
  (if hyalo-sidebar--right-frame
      ;; Frame exists - just toggle visibility
      (when (and (hyalo-module-available-p)
                 (fboundp 'hyalo-detail-toggle))
        (hyalo-detail-toggle))
    ;; Frame doesn't exist - show panel, setup, then focus via hook
    (setq hyalo-sidebar--pending-focus-right t)  ; Hook will focus after mode init
    (when (and (hyalo-module-available-p)
               (fboundp 'hyalo-detail-show))
      (hyalo-detail-show))
    (run-at-time 0.1 nil #'hyalo-sidebar-right-setup)))

(defun hyalo-sidebar-focus-right ()
  "Focus the right sidebar (inspector) frame."
  (interactive)
  (hyalo-sidebar--log "focus-right called, frame: %s, live: %s"
                      hyalo-sidebar--right-frame
                      (when hyalo-sidebar--right-frame
                        (frame-live-p hyalo-sidebar--right-frame)))
  (cond
   ;; Frame exists and is live - focus it
   ((and hyalo-sidebar--right-frame
         (frame-live-p hyalo-sidebar--right-frame))
    (hyalo-sidebar--do-focus-right))
   ;; Inspector not visible - show it first, then setup and focus via hook
   ((and (hyalo-module-available-p)
         (fboundp 'hyalo-detail-visible-p)
         (not (hyalo-detail-visible-p)))
    (hyalo-sidebar--log "Inspector not visible, showing first")
    (setq hyalo-sidebar--pending-focus-right t)  ; Hook will focus after mode init
    (when (fboundp 'hyalo-detail-show)
      (hyalo-detail-show))
    (run-at-time 0.2 nil #'hyalo-sidebar-right-setup))
   ;; Frame doesn't exist but inspector is visible - create it, focus via hook
   (t
    (hyalo-sidebar--log "Frame doesn't exist, creating...")
    (setq hyalo-sidebar--pending-focus-right t)  ; Hook will focus after mode init
    (hyalo-sidebar-right-setup))))

(defun hyalo-sidebar-focus-left ()
  "Focus the left sidebar (dired-sidebar) frame."
  (interactive)
  (cond
   ;; Frame exists and is live - focus it
   ((and hyalo-sidebar--left-bottom-frame
         (frame-live-p hyalo-sidebar--left-bottom-frame))
    (hyalo-sidebar--do-focus-left))
   ;; Sidebar not visible - show it first, then setup and focus
   ((and (hyalo-module-available-p)
         (fboundp 'hyalo-sidebar-visible-p)
         (not (hyalo-sidebar-visible-p)))
    (hyalo-sidebar--log "Sidebar not visible, showing first")
    (when (fboundp 'hyalo-sidebar-show)
      (hyalo-sidebar-show))
    (run-at-time 0.2 nil #'hyalo-sidebar-left-setup)
    (run-at-time 0.5 nil #'hyalo-sidebar--do-focus-left))
   ;; Frame doesn't exist but sidebar is visible - create it
   (t
    (hyalo-sidebar--log "Frame doesn't exist, creating...")
    (hyalo-sidebar-left-setup)
    (run-at-time 0.3 nil #'hyalo-sidebar--do-focus-left))))

;;; Panel visibility watcher (for toolbar button integration)

(defvar hyalo-sidebar--visibility-timer nil
  "Timer for checking panel visibility changes.")

(defvar hyalo-sidebar--last-checked nil
  "Last panel that was checked for setup to avoid duplicate calls.")

(defun hyalo-sidebar--check-visibility ()
  "Check if any panel needs embedded content setup.
Called periodically when sidebar mode is active.
This is called from the Swift side via `hyalo-panel-needs-setup'."
  (when (and (hyalo-module-available-p)
             (fboundp 'hyalo-panel-needs-setup))
    (let ((needs-setup (hyalo-panel-needs-setup)))
      (when needs-setup
        (hyalo-sidebar--log "Panel needs setup: %s (last-checked: %s)"
                            needs-setup hyalo-sidebar--last-checked)
        ;; Always try to setup if needed, but avoid rapid re-setup of same panel
        ;; by checking if frames are already pending creation
        (cond
         ((and (equal needs-setup "right")
               (not hyalo-sidebar--right-frame))
          (hyalo-sidebar--log "Starting right-setup from visibility watcher")
          (hyalo-sidebar-right-setup))
         ((and (equal needs-setup "left")
               (not hyalo-sidebar--left-top-frame)
               (not hyalo-sidebar--left-bottom-frame))
          (hyalo-sidebar--log "Starting left-setup from visibility watcher")
          (hyalo-sidebar-left-setup)))))))

(defun hyalo-sidebar--start-visibility-watcher ()
  "Start the visibility watcher timer."
  (unless hyalo-sidebar--visibility-timer
    (setq hyalo-sidebar--visibility-timer
          (run-with-timer 0.5 0.5 #'hyalo-sidebar--check-visibility))
    (hyalo-sidebar--log "Visibility watcher started")))

(defun hyalo-sidebar--stop-visibility-watcher ()
  "Stop the visibility watcher timer."
  (when hyalo-sidebar--visibility-timer
    (cancel-timer hyalo-sidebar--visibility-timer)
    (setq hyalo-sidebar--visibility-timer nil)
    (hyalo-sidebar--log "Visibility watcher stopped")))

;;; Agent-shell focus hook

(defun hyalo-sidebar--agent-shell-mode-hook ()
  "Hook to focus agent-shell after it's ready."
  (when hyalo-sidebar--pending-focus-right
    (setq hyalo-sidebar--pending-focus-right nil)
    (hyalo-sidebar--log "agent-shell-mode-hook: focusing right sidebar")
    ;; Small delay to ensure embedding is complete
    (run-at-time 0.3 nil #'hyalo-sidebar--do-focus-right)))

;; Add hook for agent-shell focus
(with-eval-after-load 'agent-shell
  (add-hook 'agent-shell-mode-hook #'hyalo-sidebar--agent-shell-mode-hook))

(defvar hyalo-sidebar--callbacks-installed nil
  "Non-nil when Swift visibility callbacks have been installed.")

(defun hyalo-sidebar--setup-visibility-callbacks ()
  "Setup Swift-to-Elisp visibility callbacks.
This enables immediate response to toolbar button clicks."
  (when (and (hyalo-module-available-p)
             (not hyalo-sidebar--callbacks-installed))
    (add-hook 'hyalo-on-sidebar-visibility-changed
              #'hyalo-sidebar--on-sidebar-visibility-changed)
    (add-hook 'hyalo-on-detail-visibility-changed
              #'hyalo-sidebar--on-detail-visibility-changed)
    (if (fboundp 'hyalo-setup-visibility-callbacks)
        (condition-case err
            (progn
              (hyalo-setup-visibility-callbacks)
              (setq hyalo-sidebar--callbacks-installed t)
              (hyalo-sidebar--log "Visibility callbacks installed"))
          (error
           (hyalo-sidebar--log "Failed to install callbacks: %s" err)
           (hyalo-sidebar--start-visibility-watcher)))
      (hyalo-sidebar--start-visibility-watcher))))



(defun hyalo-sidebar--teardown-visibility-callbacks ()
  "Teardown Swift-to-Elisp visibility callbacks."
  (remove-hook 'hyalo-on-sidebar-visibility-changed
               #'hyalo-sidebar--on-sidebar-visibility-changed)
  (remove-hook 'hyalo-on-detail-visibility-changed
               #'hyalo-sidebar--on-detail-visibility-changed)
  (setq hyalo-sidebar--callbacks-installed nil))

;; Setup callbacks when mode is enabled
(add-hook 'hyalo-module-sidebar-mode-hook
          (lambda ()
            (if hyalo-module-sidebar-mode
                (hyalo-sidebar--setup-visibility-callbacks)
              (progn
                (hyalo-sidebar--teardown-visibility-callbacks)
                (hyalo-sidebar--stop-visibility-watcher)))))


(provide 'hyalo-module-sidebar)
;;; hyalo-module-sidebar.el ends here
