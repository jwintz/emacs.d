;;; hyalo-header.el --- Header view management for hyalo -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: Julien Wintz <julien.wintz@inria.fr>
;; Version: 1.0.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: frames, faces, macos

;;; Commentary:

;; Header view management for hyalo.
;; Handles:
;; - Formatting and sending mode-line content to Swift header view
;; - Formatting and sending header-line content to Swift header view
;; - Hiding native Emacs mode-line and header-line
;;
;; Uses deterministic hooks (no timers):
;; - post-command-hook for immediate updates
;; - window-configuration-change-hook for window changes
;; - after-change-major-mode-hook for mode changes
;;
;; Usage:
;;   (require 'hyalo-header)
;;   (hyalo-header-mode 1)

;;; Code:

(require 'cl-lib)
(require 'hyalo)

(defgroup hyalo-header nil
  "Header view settings for hyalo."
  :group 'hyalo
  :prefix "hyalo-header-")

(defcustom hyalo-header-top-padding 12
  "Top padding for header view in pixels."
  :type 'integer
  :group 'hyalo-header)

(defcustom hyalo-header-left-padding 12
  "Left padding for header view in pixels."
  :type 'integer
  :group 'hyalo-header)

(defcustom hyalo-header-right-padding 12
  "Right padding for header view in pixels."
  :type 'integer
  :group 'hyalo-header)

(defcustom hyalo-header-right-padding 12
  "Right padding for header view in pixels."
  :type 'integer
  :group 'hyalo-header)

;;; Internal State

(defvar hyalo-header--saved-mode-line-format nil
  "Saved default mode-line-format to restore when disabling.")

(defvar hyalo-header--saved-header-line-format nil
  "Saved default header-line-format to restore when disabling.")

(defvar hyalo-header--buffer-header-lines (make-hash-table :test 'eq :weakness 'key)
  "Hash table mapping buffers to their original header-line-format.")

(defvar hyalo-header--last-mode-line ""
  "Last mode-line string sent to Swift, for change detection.")

(defvar hyalo-header--last-header-line ""
  "Last header-line string sent to Swift, for change detection.")

(defvar hyalo-header--last-window nil
  "Last window that was updated, for change detection.")

(defvar hyalo-header--saved-frame-title-format nil
  "Saved frame-title-format to restore when disabling.")

(defvar hyalo-header--last-buffer nil
  "Last buffer that was updated, for change detection.")

;;; Mode-line Formatting

(defun hyalo-header--save-buffer-header-line ()
  "Save current buffer's header-line-format before hiding."
  (when header-line-format
    (puthash (current-buffer) header-line-format hyalo-header--buffer-header-lines)))

(defun hyalo-header--get-buffer-header-line ()
  "Get the saved header-line-format for current buffer."
  (gethash (current-buffer) hyalo-header--buffer-header-lines))

(defun hyalo-header--format-mode-line ()
  "Get the formatted mode-line string for the current buffer."
  (when hyalo-header--saved-mode-line-format
    (format-mode-line hyalo-header--saved-mode-line-format)))

(defun hyalo-header--format-header-line ()
  "Get the formatted header-line string for the current buffer."
  (when-let* ((hl (hyalo-header--get-buffer-header-line)))
    (format-mode-line hl)))

;;; Header Update (Hook Function)

(defun hyalo-header--update ()
  "Send current buffer info to Swift header view and toolbar.
Called via unified update dispatcher.
Skips updates from embedded child-frames (hyalo-embedded parameter)."
  (when (and (hyalo-available-p)
             (display-graphic-p)
             ;; Skip updates from embedded child-frames (sidebars)
             ;; The toolbar modeline should only reflect main content
             (not (frame-parameter nil 'hyalo-embedded)))
    ;; Enforce hidden mode-line/header-line if they reappeared
    (when (or mode-line-format header-line-format)
      (hyalo-header--enforce-hidden))
    (let* ((current-window (selected-window))
           (window-changed (not (eq current-window hyalo-header--last-window)))
           ;; Always format - modeline changes on navigation (line/col), keycast, etc.
           (mode-line-str (or (hyalo-header--format-mode-line) ""))
           (header-line-str (or (hyalo-header--format-header-line) ""))
           ;; Only send to Swift if content actually changed
           (mode-line-changed (not (string= mode-line-str hyalo-header--last-mode-line)))
           (header-line-changed (not (string= header-line-str hyalo-header--last-header-line))))
      ;; Update mode-line if changed
      (when (or mode-line-changed window-changed)
        (setq hyalo-header--last-mode-line mode-line-str)
        ;; Update NavigationSplitView toolbar mode-line
        (when (fboundp 'hyalo-sidebar-update-mode-line)
          (hyalo-sidebar-update-mode-line mode-line-str))
        ;; Also send structured segment data for menus/tooltips
        (when (fboundp 'hyalo-sidebar-update-mode-line-segments)
          (let ((segments-json (hyalo-header--extract-structured-segments)))
            (when hyalo-header--debug-extraction
              (hyalo-debug 'header "segments-json length=%d" (length (or segments-json ""))))
            (when segments-json
              (hyalo-sidebar-update-mode-line-segments segments-json)))))
      ;; Update header-line if changed (currently not used in toolbar)
      (when (or header-line-changed window-changed)
        (setq hyalo-header--last-header-line header-line-str))
      ;; Track window for next call
      (setq hyalo-header--last-window current-window))))

;;; Native Mode-line Control

(defun hyalo-header--hide-native ()
  "Hide native Emacs mode-line and header-line in all buffers."
  ;; Save current formats
  (setq hyalo-header--saved-mode-line-format (default-value 'mode-line-format))
  (setq hyalo-header--saved-header-line-format (default-value 'header-line-format))
  ;; Save and hide in all existing buffers
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (hyalo-header--save-buffer-header-line)
      (setq mode-line-format nil)
      (setq header-line-format nil)))
  ;; Hide globally
  (setq-default mode-line-format nil)
  (setq-default header-line-format nil)
  ;; Hook to hide in new buffers
  (add-hook 'after-change-major-mode-hook #'hyalo-header--enforce-hidden)
  ;; Add window dividers to compensate for hidden mode-line
  (setq window-divider-default-places 'bottom-only)
  (setq window-divider-default-bottom-width 1)
  (window-divider-mode 1))

(defun hyalo-header--enforce-hidden ()
  "Enforce hidden mode-line in current buffer.
Called by hooks to override modes that set mode-line-format buffer-locally."
  (when hyalo-header--saved-mode-line-format
    ;; Save the header-line before hiding (modes like info set it)
    (when header-line-format
      (hyalo-header--save-buffer-header-line)))
  ;; Always hide, regardless of whether we saved a format
  (setq mode-line-format nil)
  (setq header-line-format nil))

(defun hyalo-header--restore-native ()
  "Restore native Emacs mode-line and header-line."
  ;; Remove hooks
  (remove-hook 'after-change-major-mode-hook #'hyalo-header--enforce-hidden)
  ;; Restore defaults
  (when hyalo-header--saved-mode-line-format
    (setq-default mode-line-format hyalo-header--saved-mode-line-format))
  (when hyalo-header--saved-header-line-format
    (setq-default header-line-format hyalo-header--saved-header-line-format))
  ;; Restore in all buffers
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (kill-local-variable 'mode-line-format)
      (kill-local-variable 'header-line-format)))
  ;; Clear saved header-line hash
  (clrhash hyalo-header--buffer-header-lines)
  (window-divider-mode -1))

;;; Header Height

(defun hyalo-header-get-height ()
  "Return the header view height in pixels from the Swift module."
  (if (and (hyalo-available-p) (fboundp 'hyalo-header-height))
      (hyalo-header-height)
    47))  ; Fallback: 12 + 24 + 1 + 22 - 12 = 47 (approx)

;;; Magit Support

(defun hyalo-header--magit-setup-advice (buffer)
  "Advice to enforce hidden mode-line after Magit setup.
Run as :filter-return on `magit-setup-buffer-internal'."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (hyalo-header--enforce-hidden)
      (when (and (boundp 'hyalo-viewport-mode)
                 hyalo-viewport-mode
                 (fboundp 'hyalo-viewport--update-window))
        ;; Update viewport with a delay to ensure window is displayed
        (run-at-time 0.5 nil
                     (lambda (buf)
                       (when (buffer-live-p buf)
                         (let ((wins (get-buffer-window-list buf nil t)))
                           (dolist (w wins)
                             (with-selected-window w
                               (hyalo-debug 'header "Magit Setup (delayed): Updating window %s" w)
                               (goto-char (point-min))
                               (hyalo-viewport--update-window w))))))
                     buffer))))
  buffer)


(defun hyalo-header--magit-refresh-advice (&rest _)
  "Advice to enforce hidden mode-line after Magit refresh.
Run as :after on `magit-refresh-buffer'."
  (hyalo-header--enforce-hidden)
  (when (and (boundp 'hyalo-viewport-mode)
             hyalo-viewport-mode
             (fboundp 'hyalo-viewport--update-window))
    (dolist (win (get-buffer-window-list (current-buffer) nil t))
      (hyalo-viewport--update-window win))))



(defun hyalo-header--setup-magit ()
  "Setup Magit integration using Advice."
  ;; Use filter-return for setup to access the correct buffer context
  (advice-add 'magit-setup-buffer-internal :filter-return #'hyalo-header--magit-setup-advice)
  (advice-add 'magit-refresh-buffer :after #'hyalo-header--magit-refresh-advice))

(defun hyalo-header--teardown-magit ()
  "Remove Magit integration."
  (advice-remove 'magit-setup-buffer-internal #'hyalo-header--magit-setup-advice)
  (advice-remove 'magit-refresh-buffer #'hyalo-header--magit-refresh-advice))

;;; Setup/Teardown

(defun hyalo-header--setup-frame (&optional frame)
  "Setup Hyalo for FRAME (or current frame if nil)."
  (let ((f (or frame (selected-frame))))
    (when (and (display-graphic-p f) (eq (framep f) 'ns))
      (with-selected-frame f
        ;; Tell Emacs the titlebar is transparent
        (modify-frame-parameters f '((ns-transparent-titlebar . t)
                                      (ns-appearance . nil)))
        ;; Clear the frame title so Emacs doesn't try to display dimensions
        (modify-frame-parameters f '((title . " ")))
        ;; Disable internal border (reduces title bar height)
        (modify-frame-parameters f '((internal-border-width . 0)))
        ;; Disable Emacs native toolbar (SwiftUI toolbar is authoritative)
        (when (fboundp 'tool-bar-mode)
          (tool-bar-mode -1))
        ;; Setup NavigationSplitView with toolbar (replaces legacy HeaderView)
        (when (fboundp 'hyalo-navigation-setup)
          (hyalo-navigation-setup))
        ;; Apply appearance settings AFTER navigation is ready
        ;; This ensures saved vibrancy/opacity values are applied to Swift
        (when (and (boundp 'hyalo-appearance-mode)
                   hyalo-appearance-mode
                   (fboundp 'hyalo-appearance--apply-vibrancy))
          (hyalo-appearance--apply-vibrancy))
        ;; Restore Emacs keyboard focus after setup
        (when (fboundp 'hyalo-restore-focus)
          (run-with-timer 0.5 nil #'hyalo-restore-focus))))))

(defun hyalo-header--teardown-frame (&optional frame)
  "Teardown Hyalo for FRAME (or current frame if nil)."
  (with-selected-frame (or frame (selected-frame))
    (when (fboundp 'hyalo-navigation-teardown)
      (hyalo-navigation-teardown))))

(defun hyalo-header--keep-title-empty ()
  "Keep frame title empty to prevent Emacs from rendering in titlebar."
  (when (and (display-graphic-p) (eq (framep (selected-frame)) 'ns))
    (let ((current-title (frame-parameter nil 'title)))
      (unless (and current-title (string= current-title " "))
        (set-frame-parameter nil 'title " ")))))

(defun hyalo-header--enable ()
  "Enable header view management."
  (hyalo-ensure)
  ;; Save and clear frame-title-format to prevent dimension display in titlebar
  (setq hyalo-header--saved-frame-title-format frame-title-format)
  (setq frame-title-format " ")
  ;; Setup all existing frames
  (dolist (frame (frame-list))
    (hyalo-header--setup-frame frame))
  ;; Setup hook for new frames
  (add-hook 'after-make-frame-functions #'hyalo-header--setup-frame)
  (add-hook 'delete-frame-functions #'hyalo-header--teardown-frame)
  ;; Hide native mode-line
  (hyalo-header--hide-native)
  ;; Register with unified update dispatcher
  (hyalo-update-dispatcher-enable)
  (hyalo-register-update-handler 'header #'hyalo-header--update)
  ;; Keep title empty (prevents dimension display in titlebar)
  (add-hook 'post-command-hook #'hyalo-header--keep-title-empty)
  ;; Setup Magit support
  (with-eval-after-load 'magit
    (hyalo-header--setup-magit))
  ;; Setup mode-line click callback
  (hyalo-header--setup-modeline-click)
  ;; Initial update
  (hyalo-header--update)
  (hyalo-info 'header "Enabled"))

(defun hyalo-header--disable ()
  "Disable header view management."
  ;; Remove frame hooks
  (remove-hook 'after-make-frame-functions #'hyalo-header--setup-frame)
  (remove-hook 'delete-frame-functions #'hyalo-header--teardown-frame)
  ;; Unregister from unified update dispatcher
  (hyalo-unregister-update-handler 'header)
  (remove-hook 'post-command-hook #'hyalo-header--keep-title-empty)
  ;; Teardown Magit support
  (with-eval-after-load 'magit
    (hyalo-header--teardown-magit))
  ;; Teardown all frames
  (dolist (frame (frame-list))
    (hyalo-header--teardown-frame frame))
  ;; Restore native mode-line
  (hyalo-header--restore-native)
  ;; Restore frame-title-format
  (when hyalo-header--saved-frame-title-format
    (setq frame-title-format hyalo-header--saved-frame-title-format))
  ;; Remove modeline click hook
  (remove-hook 'post-command-hook #'hyalo-header--check-modeline-click)
  ;; Clear state
  (setq hyalo-header--last-mode-line ""
        hyalo-header--last-header-line ""
        hyalo-header--last-window nil
        hyalo-header--saved-frame-title-format nil)
  (hyalo-info 'header "Disabled"))

;;;###autoload
(define-minor-mode hyalo-header-mode
  "Minor mode for Hyalo header view.
When enabled, the native mode-line and header-line are hidden
and their content is displayed in a floating SwiftUI header."
  :global t
  :lighter " ηHeader"
  :group 'hyalo-header
  (if hyalo-header-mode
      (hyalo-header--enable)
    (hyalo-header--disable)))

;; Interactive wrappers for Swift-defined functions
;; Swift env.defun creates non-interactive functions, so we need wrappers

;;;###autoload
(defun hyalo-toggle-decorations-command ()
  "Toggle visibility of toolbar and traffic lights.
When hidden, provides a minimal chrome experience."
  (interactive)
  (if (fboundp 'hyalo-toggle-decorations)
      (progn
        (hyalo-toggle-decorations)
        (hyalo-info 'header "Decorations %s" (if (hyalo-decorations-visible-p) "shown" "hidden")))
    (hyalo-warn 'header "Hyalo decorations toggle not available - module not loaded")))

;; Create an alias so M-x hyalo-toggle-decorations works
(defalias 'hyalo-toggle-chrome 'hyalo-toggle-decorations-command
  "Alias for `hyalo-toggle-decorations-command'.")

;; Handle case where Magit is loaded after this module is enabled
(with-eval-after-load 'magit
  (when hyalo-header-mode
    (hyalo-header--setup-magit)))

;;; Mode-line Click Handling

(defun hyalo-on-modeline-click (segment position)
  "Handle mode-line click from Swift.
SEGMENT is \"lhs\" or \"rhs\", POSITION is 0.0-1.0 relative position.
This simulates a mouse-1 click on the mode-line to trigger native popups."
  (hyalo-debug 'header "on-modeline-click: segment=%s position=%s" segment position)
  ;; Schedule the action in Emacs event loop for proper popup display
  (run-at-time 0 nil
               (lambda (seg pos)
                 (let* ((mode-line-str (or (hyalo-header--format-mode-line) ""))
                        (segments (hyalo-header--parse-mode-line-segments mode-line-str))
                        (segment-str (if (string= seg "lhs") (car segments) (cdr segments)))
                        (char-pos (floor (* pos (length segment-str))))
                        (abs-pos (if (string= seg "lhs")
                                     char-pos
                                   (+ (length (car segments)) char-pos))))
                   (hyalo-trace 'header "on-modeline-click (scheduled): mode-line-str=%s" mode-line-str)
                   (hyalo-trace 'header "on-modeline-click (scheduled): segments=%s" segments)
                   (hyalo-trace 'header "on-modeline-click (scheduled): char-pos=%s abs-pos=%s" char-pos abs-pos)
                   ;; Find clickable spans and match click position to the correct one
                   (hyalo-header--trigger-mode-line-action-by-span seg pos mode-line-str)))
               segment position))


(defun hyalo--safe-mode-line-command (cmd)
  "Map mode-line mouse commands to regular commands."
  (cond
   ((eq cmd 'mode-line-previous-buffer) 'previous-buffer)
   ((eq cmd 'mode-line-next-buffer) 'next-buffer)
   ((eq cmd 'mode-line-toggle-read-only) 'read-only-mode)
   ((eq cmd 'mode-line-bury-buffer) 'bury-buffer)
   ((eq cmd 'mode-line-unbury-buffer) 'unbury-buffer)
   (t cmd)))

(defun hyalo-execute-string-command (command-name)
  "Execute command from Swift by name.
Called by HyaloExecuteCommand notification from Swift.
Triggers modeline refresh immediately after command execution.
Can also be called interactively for testing."
  (interactive "sCommand name: ")
  (let* ((sym (intern-soft command-name))
         (cmd (hyalo--safe-mode-line-command sym)))
    (if (and cmd (commandp cmd))
        ;; Run in timer to ensure we are in valid event context
        (run-at-time 0 nil
                     (lambda (c)
                       (with-demoted-errors "Mode-line command error: %S"
                         (call-interactively c))
                       ;; Clear cache and refresh modeline
                       (setq hyalo-header--last-mode-line nil)
                       (hyalo-header--update)
                       (force-mode-line-update t)
                       (redisplay t))
                     cmd)
      (hyalo-error 'header "execute-string-command: not a valid command: %s" command-name))))

(defun hyalo-header--invoke-mode-line-binding (map)
  "Invoke mode-line binding from MAP.
Tries multiple mouse events to find working binding."
  ;; Log available bindings in the keymap for debugging
  (when (keymapp map)
    )
  ;; Try different mouse events in order of preference
  (let* ((events '([down-mouse-1] [mouse-1] [down-mouse-3] [mouse-3]
                   [mode-line down-mouse-1] [mode-line mouse-1]
                   [mode-line down-mouse-3] [mode-line mouse-3]))
         (found nil))
    (dolist (event events)
      (unless found
        (let ((binding (lookup-key map event)))
          (when binding
            (setq found t)
            (cond
             ;; Keymap = popup menu
             ((keymapp binding)
              (let ((choice (x-popup-menu t binding)))
                (when choice
                  (let* ((res (lookup-key binding (vconcat choice)))
                         (cmd (hyalo--safe-mode-line-command res)))
                    (when (commandp cmd)
                      (call-interactively cmd))))))
             ;; menu-item with potential :filter
             ((and (consp binding) (eq (car binding) 'menu-item))
              (let* ((menu (nth 2 binding))
                     (plist (nthcdr 3 binding))
                     (filter (plist-get plist :filter))
                     (final-menu (if filter
                                     (condition-case nil
                                         (funcall filter menu)
                                       (error menu))
                                   menu)))
                (cond
                 ((keymapp final-menu)
                  (let ((choice (x-popup-menu t final-menu)))
                    (when choice
                      (let* ((res (lookup-key final-menu (vconcat choice)))
                             (cmd (hyalo--safe-mode-line-command res)))
                        (when (commandp cmd)
                          (call-interactively cmd))))))
                 ((commandp final-menu)
                  (call-interactively (hyalo--safe-mode-line-command final-menu))))))
             ;; Direct command
             ((commandp binding)
              (call-interactively (hyalo--safe-mode-line-command binding)))
             ;; Function
             ((functionp binding)
              (funcall binding))
             ;; Something else - keep looking
             (t
              (setq found nil)))))))
    (unless found
      (hyalo-warn 'header "invoke-mode-line-binding: no usable binding found"))))

(defun hyalo-header--parse-mode-line-segments (str)
  "Parse mode-line STR into (LHS . RHS) cons cell.
Splits at the first occurrence of two or more consecutive spaces."
  (if (string-match "\\([^ ].*?\\)  +\\(.*[^ ]\\)" str)
      (cons (match-string 1 str) (match-string 2 str))
    (cons str "")))

(defun hyalo-header--extract-menu-items (keymap)
  "Extract menu items from KEYMAP for Swift popup menu.
Returns list of alists with title, command, checked, enabled keys."
  (let ((items nil))
    (when (keymapp keymap)
      (map-keymap
       (lambda (key def)
         (cond
          ;; menu-item entries
          ((and (consp def) (eq (car def) 'menu-item))
           (let* ((title (nth 1 def))
                  (cmd (nth 2 def))
                  (plist (nthcdr 3 def))
                  (button (plist-get plist :button))
                  (checked (and button
                                (eq (car button) :toggle)
                                (eval (cdr button))))
                  (enabled (let ((en (plist-get plist :enable)))
                             (if en
                                 (not (null (ignore-errors (eval en))))
                               t))))
             (when (and title (not (eq cmd 'ignore)))
               (push `((title . ,(if (stringp title) title (format "%s" title)))
                       (command . ,(if (symbolp cmd) (symbol-name cmd) ""))
                       (checked . ,checked)
                       (enabled . ,enabled))
                     items))))
          ;; Direct command bindings (not nested keymaps)
          ((and (symbolp key) (commandp def))
           (push `((title . ,(symbol-name key))
                   (command . ,(symbol-name def))
                   (checked . nil)
                   (enabled . t))
                 items))))
       keymap))
    (nreverse items)))

(defvar hyalo-header--debug-extraction nil
  "When non-nil, log extraction debug info.")

(defun hyalo-header-toggle-debug ()
  "Toggle debug logging for mode-line extraction."
  (interactive)
  (setq hyalo-header--debug-extraction (not hyalo-header--debug-extraction))
  (message "Mode-line extraction debug: %s" (if hyalo-header--debug-extraction "ON" "OFF")))

(defun hyalo-header--find-lhs-rhs-separator (formatted)
  "Find the LHS/RHS separator position in FORMATTED mode-line string.
Returns the position where RHS CONTENT begins (after the separator), or length if not found.
Handles doom-modeline style (display property with align-to) and standard (multi-space)."
  (let ((len (length formatted))
        (sep-pos nil))
    ;; Method 1: Look for display property with (space :align-to ...) specifically
    ;; This is the doom-modeline separator - NOT just any (space ...) display property
    (let ((pos 0))
      (while (< pos len)
        (let ((disp (get-text-property pos 'display formatted)))
          (when (and disp
                     (consp disp)
                     (eq (car disp) 'space)
                     (plist-get (cdr disp) :align-to))
            ;; Found the doom-modeline separator!
            ;; Skip past this character to where RHS content begins
            (setq sep-pos (1+ pos))
            (when hyalo-header--debug-extraction
              (hyalo-debug 'header "Found align-to separator at pos %d, RHS starts at %d" pos sep-pos))))
        (setq pos (1+ pos))))
    ;; Method 2: Look for the largest multi-space gap in the second half
    ;; The doom-modeline separator creates a large gap
    (unless sep-pos
      (let ((gaps nil)
            (start 0)
            (min-start-pct 0.3))  ; Separator is typically after 30% of the string
        ;; Collect all gaps
        (while (string-match "  +" formatted start)
          (let ((gap-start (match-beginning 0))
                (gap-end (match-end 0))
                (gap-length (- (match-end 0) (match-beginning 0))))
            (push (list gap-start gap-end gap-length) gaps)
            (setq start gap-end)))
        ;; Find the largest gap after min-start-pct - RHS starts at gap-end
        (when gaps
          (let ((best-length 0)
                (best-gap nil))
            (dolist (gap gaps)
              (let ((gap-start (car gap))
                    (gap-length (caddr gap)))
                (when (and (> gap-start (* len min-start-pct))
                           (> gap-length best-length))
                  (setq best-length gap-length
                        best-gap gap))))
            (when best-gap
              ;; RHS starts at the END of the gap (gap-end), not the start
              (setq sep-pos (cadr best-gap))
              (when hyalo-header--debug-extraction
                (hyalo-debug 'header "Found gap separator, RHS starts at pos %d (gap length %d)"
                             sep-pos (caddr best-gap))))))))
    ;; Return separator position or end of string (everything is LHS)
    (when hyalo-header--debug-extraction
      (hyalo-debug 'header "Final: RHS starts at %d, total length %d" (or sep-pos len) len))
    (or sep-pos len)))

(defun hyalo-header--extract-structured-segments ()
  "Extract structured segment data from mode-line for Swift.
Returns JSON string with segment array.
Extracts ALL text, including non-interactive spans (for icons)."
  (when hyalo-header--debug-extraction
    (hyalo-debug 'header "extract-structured-segments: saved-format=%S"
                 (and hyalo-header--saved-mode-line-format t)))
  (when hyalo-header--saved-mode-line-format
    (let* ((formatted (format-mode-line hyalo-header--saved-mode-line-format nil nil (current-buffer)))
           (len (length formatted))
           ;; Find LHS/RHS separator using improved detection
           (sep-start (hyalo-header--find-lhs-rhs-separator formatted))
           (segments nil))
      ;; Scan ALL text, grouping by changes in keymap OR help-echo
      ;; This ensures icons (which may lack keymaps) are still captured
      (let ((pos 0)
            (current-start 0)
            (current-map nil)
            (current-help nil))
        (while (< pos len)
          (let ((km (or (get-text-property pos 'keymap formatted)
                        (get-text-property pos 'local-map formatted)))
                (help (get-text-property pos 'help-echo formatted))
                ;; Also split at separator boundary
                (at-separator (and (= pos sep-start) (> pos current-start))))
            ;; When properties change OR we reach the separator, close current span
            (when (or at-separator
                      (not (eq km current-map))
                      (not (equal help current-help)))
              ;; Close previous span if it has content
              (when (> pos current-start)
                (let* ((text (substring formatted current-start pos))
                       ;; Segment is LHS if it ENDS before sep-start (strictly)
                       (side (if (< current-start sep-start) "lhs" "rhs"))
                       (menu-binding (when current-map
                                       (or (lookup-key current-map [mode-line down-mouse-1])
                                           (lookup-key current-map [mode-line mouse-1])
                                           (lookup-key current-map [down-mouse-1])
                                           (lookup-key current-map [mouse-1]))))
                       (menu-items (cond
                                    ((keymapp menu-binding)
                                     (hyalo-header--extract-menu-items menu-binding))
                                    ((and (consp menu-binding)
                                          (eq (car menu-binding) 'menu-item))
                                     (let* ((filter (plist-get (nthcdr 3 menu-binding) :filter))
                                            (menu (if filter
                                                      (condition-case nil
                                                          (funcall filter (nth 2 menu-binding))
                                                        (error nil))
                                                    (nth 2 menu-binding))))
                                       (when (keymapp menu)
                                         (hyalo-header--extract-menu-items menu))))
                                    (t nil)))
                       (command (when (and (not menu-items) (commandp menu-binding))
                                  (if (symbolp menu-binding) (symbol-name menu-binding) ""))))
                  ;; Only add non-empty, non-whitespace-only segments
                  (when (string-match-p "[^ \t]" text)
                    ;; Trim leading/trailing whitespace for clean Swift display
                    (let ((trimmed-text (string-trim text)))
                      (push `((text . ,trimmed-text)
                              (relStart . ,(/ (float current-start) len))
                              (relEnd . ,(/ (float pos) len))
                              (helpEcho . ,(when (stringp current-help) current-help))
                              (side . ,side)
                              (menuItems . ,menu-items)
                              (command . ,command))
                            segments)))))
              ;; Start new span (but only if we're actually moving forward)
              (setq current-start pos
                    current-map km
                    current-help help)))
          (cl-incf pos))
        ;; Close final span
        (when (> pos current-start)
          (let* ((text (substring formatted current-start pos))
                 (side (if (< current-start sep-start) "lhs" "rhs"))
                 (menu-binding (when current-map
                                 (or (lookup-key current-map [mode-line down-mouse-1])
                                     (lookup-key current-map [mode-line mouse-1])
                                     (lookup-key current-map [down-mouse-1])
                                     (lookup-key current-map [mouse-1]))))
                 (menu-items (when (keymapp menu-binding)
                               (hyalo-header--extract-menu-items menu-binding)))
                 (command (when (and (not menu-items) (commandp menu-binding))
                            (if (symbolp menu-binding) (symbol-name menu-binding) ""))))
            (when (string-match-p "[^ \t]" text)
              ;; Trim leading/trailing whitespace for clean Swift display
              (let ((trimmed-text (string-trim text)))
                (push `((text . ,trimmed-text)
                        (relStart . ,(/ (float current-start) len))
                        (relEnd . ,(/ (float pos) len))
                        (helpEcho . ,(when (stringp current-help) current-help))
                        (side . ,side)
                        (menuItems . ,menu-items)
                        (command . ,command))
                      segments))))))
      ;; Debug output
      (when hyalo-header--debug-extraction
        (hyalo-debug 'header "Extracted %d segments" (length segments))
        (dolist (seg segments)
          (let ((text (alist-get 'text seg))
                (items (alist-get 'menuItems seg)))
            (hyalo-debug 'header "  Segment '%s': menuItems=%d"
                         (substring text 0 (min 20 (length text)))
                         (length (or items '()))))))
      ;; Return JSON
      (json-encode (nreverse segments)))))

(defun hyalo-header--find-clickable-spans (formatted)
  "Find all clickable spans in FORMATTED mode-line string.
Returns list of (START END KEYMAP REL-START REL-END) where REL-* are 0.0-1.0."
  (let ((len (length formatted))
        (pos 0)
        (spans nil)
        (current-start nil)
        (current-map nil))
    (while (< pos len)
      (let ((km (or (get-text-property pos 'keymap formatted)
                    (get-text-property pos 'local-map formatted))))
        (cond
         ;; Start of new clickable span
         ((and km (not (eq km current-map)))
          (when (and current-start current-map)
            (push (list current-start pos current-map) spans))
          (setq current-start pos
                current-map km))
         ;; End of clickable span
         ((and current-map (not km))
          (push (list current-start pos current-map) spans)
          (setq current-start nil
                current-map nil))))
      (cl-incf pos))
    ;; Close final span
    (when (and current-start current-map)
      (push (list current-start pos current-map) spans))
    ;; Add relative positions
    (mapcar (lambda (span)
              (list (nth 0 span) (nth 1 span) (nth 2 span)
                    (/ (float (nth 0 span)) len)
                    (/ (float (nth 1 span)) len)))
            (nreverse spans))))

(defun hyalo-header--trigger-mode-line-action-by-span (segment position mode-line-str)
  "Trigger mode-line action by matching POSITION to clickable spans.
SEGMENT is \"lhs\" or \"rhs\", POSITION is 0.0-1.0 relative.
MODE-LINE-STR is the visual mode-line string."
  (when (and hyalo-header--saved-mode-line-format mode-line-str)
    (let* ((formatted (format-mode-line hyalo-header--saved-mode-line-format nil nil (current-buffer)))
           (spans (hyalo-header--find-clickable-spans formatted))
           ;; Parse FORMATTED string for separator (not visual string)
           ;; Find the multi-space gap in the formatted string
           (sep-match (string-match "  +" formatted))
           (formatted-lhs-len (or sep-match (length formatted)))
           (formatted-rhs-start (if sep-match
                                    (match-end 0)
                                  (length formatted)))
           (formatted-rhs-len (- (length formatted) formatted-rhs-start))
           (total-len (length formatted))
           ;; Calculate separator position in formatted string
           (separator-rel (if (> total-len 0)
                              (/ (float formatted-lhs-len) total-len)
                            0.5))
           ;; Convert segment+position to global position in formatted string
           (global-pos (if (string= segment "lhs")
                           (* position separator-rel)
                         ;; RHS: position 0.0 = start of RHS, 1.0 = end
                         (+ (/ (float formatted-rhs-start) total-len)
                            (* position (/ (float formatted-rhs-len) total-len)))))
           ;; Find the span that contains this position
           (matched-span (cl-find-if
                          (lambda (span)
                            (and (>= global-pos (nth 3 span))
                                 (< global-pos (nth 4 span))))
                          spans)))
      (if matched-span
          (hyalo-header--invoke-mode-line-binding (nth 2 matched-span))
        ;; No direct match - find nearest span
        (let ((nearest (cl-reduce
                        (lambda (a b)
                          (if (< (abs (- global-pos (/ (+ (nth 3 a) (nth 4 a)) 2)))
                                 (abs (- global-pos (/ (+ (nth 3 b) (nth 4 b)) 2))))
                              a b))
                        spans)))
          (when nearest
            (hyalo-header--invoke-mode-line-binding (nth 2 nearest))))))))

(defun hyalo-header--dump-mode-line-properties ()
  "Debug function to dump all text properties in the formatted mode-line."
  (interactive)
  (when hyalo-header--saved-mode-line-format
    (let ((formatted (format-mode-line hyalo-header--saved-mode-line-format nil nil (current-buffer)))
          (pos 0)
          (last-props nil))
      (hyalo-debug 'header "=== Mode-line properties dump ===")
      (hyalo-debug 'header "Total length: %s" (length formatted))
      (while (< pos (length formatted))
        (let ((props (text-properties-at pos formatted))
              (char (aref formatted pos)))
          (unless (equal props last-props)
            (hyalo-debug 'header "pos %s (char '%c'): %s" pos char props)
            (setq last-props props)))
        (setq pos (1+ pos)))
      (hyalo-debug 'header "=== End dump ==="))))

(defun hyalo-header--trigger-mode-line-action (mode-line-str pos)
  "Trigger the mode-line action at character position POS in MODE-LINE-STR.
Try to find and execute the keymap binding at the clicked position."
  (when (and hyalo-header--saved-mode-line-format
             mode-line-str (> (length mode-line-str) 0)
             (>= pos 0) (< pos (length mode-line-str)))
    ;; Format mode-line with text properties preserved
    (let* ((formatted (format-mode-line hyalo-header--saved-mode-line-format nil nil (current-buffer))))
      (hyalo-trace 'header "trigger-mode-line-action: formatted length=%s" (length formatted))
      ;; Clamp pos to formatted string length
      (setq pos (min pos (1- (max 1 (length formatted)))))
      ;; Log text properties at position
      (let ((props (text-properties-at pos formatted)))
        (hyalo-trace 'header "trigger-mode-line-action: props at pos=%s: %s" pos props))
      ;; Find keymap - try current position first, then scan
      (let* ((map (or (get-text-property pos 'keymap formatted)
                      (get-text-property pos 'local-map formatted)
                      (hyalo-header--find-nearest-keymap formatted pos))))
        (if map
            (hyalo-header--invoke-mode-line-binding map)
          (hyalo-warn 'header "trigger-mode-line-action: no keymap found"))))))

(defun hyalo-header--find-nearest-keymap (formatted pos)
  "Find nearest keymap in FORMATTED string around POS.
Scans both backwards and forwards, preferring the closest match."
  (let ((len (length formatted))
        (found nil)
        (found-distance most-positive-fixnum))
    ;; Scan backwards
    (let ((scan-pos pos))
      (while (and (not found) (>= scan-pos 0))
        (let ((km (or (get-text-property scan-pos 'keymap formatted)
                      (get-text-property scan-pos 'local-map formatted))))
          (when km
            (setq found km
                  found-distance (- pos scan-pos))))
        (cl-decf scan-pos)))
    ;; Scan forwards (within a reasonable range)
    (let ((scan-pos (1+ pos))
          (max-scan (min len (+ pos 10))))
      (while (< scan-pos max-scan)
        (let ((km (or (get-text-property scan-pos 'keymap formatted)
                      (get-text-property scan-pos 'local-map formatted))))
          (when (and km (< (- scan-pos pos) found-distance))
            (setq found km
                  found-distance (- scan-pos pos))))
        (cl-incf scan-pos)))
    found))


(defun hyalo-header--check-modeline-click ()
  "Check for pending mode-line clicks from Swift.
Called from `post-command-hook' to process clicks without polling."
  (when (fboundp 'hyalo-poll-modeline-click)
    (when-let* ((click-str (hyalo-poll-modeline-click)))
      (hyalo-debug 'header "check-modeline-click: received click=%s" click-str)
      (when (string-match "\\`\\([^:]+\\):\\(.+\\)\\'" click-str)
        (let ((segment (match-string 1 click-str))
              (position (string-to-number (match-string 2 click-str))))
          (hyalo-on-modeline-click segment position))))))

;; NOTE: Command execution uses channel hooks directly from Swift
;; hyalo-execute-string-command is called via channel.hook() mechanism

(defun hyalo-header--setup-modeline-click ()
  "Setup the mode-line click callback.
Uses channel hooks for command execution - no timers or polling."
  (hyalo-debug 'header "setup-modeline-click: checking if hyalo-setup-modeline-click-callback exists")
  (if (fboundp 'hyalo-setup-modeline-click-callback)
      (progn
        (hyalo-debug 'header "setup-modeline-click: calling hyalo-setup-modeline-click-callback")
        (let ((result (hyalo-setup-modeline-click-callback)))
          (hyalo-debug 'header "setup-modeline-click: result=%s" result))
        (hyalo-debug 'header "setup-modeline-click: channel hooks installed"))
    (hyalo-warn 'header "setup-modeline-click: function not available")))

(provide 'hyalo-header)
