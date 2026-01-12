;;; hyalo-splash.el --- Hyalo splash screen -*- lexical-binding: t -*-

;; Copyright (C) 2025-2026
;; Author: Julien Wintz <julien.wintz@inria.fr>
;; Version: 1.0.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: frames, startup

;;; Commentary:

;; A splash screen for Hyalo that displays:
;; - The hyalo-splash.svg logo (recolored to match theme)
;; - Title and subtitle
;; - Emacs init time
;;
;; The splash buffer is automatically killed when:
;; - Another buffer becomes visible (e.g., opening a file, switching buffers)
;; - The user presses `q`
;;
;; Keypresses and mouse events do NOT dismiss the splash, allowing
;; command sequences like `M-x` or `C-x C-f` to work normally.
;;
;; Based on nano-splash by Nicolas Rougier.

;;; Code:

(require 'cl-lib)

(defgroup hyalo-splash nil
  "Hyalo splash screen."
  :group 'hyalo
  :prefix "hyalo-splash-")

(defcustom hyalo-splash-title "Hyalo"
  "Splash screen title."
  :type 'string
  :group 'hyalo-splash)

(defcustom hyalo-splash-subtitle "Emacs. Made simple."
  "Splash screen subtitle."
  :type 'string
  :group 'hyalo-splash)

(defcustom hyalo-splash-logo-height 200
  "Height of the logo in pixels."
  :type 'integer
  :group 'hyalo-splash)

(defvar hyalo-splash--buffer-name "*splash*"
  "Name of the splash buffer.")

(defvar hyalo-splash--svg-path
  (expand-file-name "hyalo-splash.svg"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the splash screen SVG file.")

;;; Faces

(defface hyalo-splash-title
  '((t :inherit variable-pitch :weight bold :height 2.0))
  "Face for splash screen title."
  :group 'hyalo-splash)

(defface hyalo-splash-subtitle
  '((t :inherit variable-pitch :weight light :height 1.3))
  "Face for splash screen subtitle."
  :group 'hyalo-splash)

(defface hyalo-splash-info
  '((t :inherit shadow :height 0.9))
  "Face for splash screen info text."
  :group 'hyalo-splash)

;;; SVG Processing

(defun hyalo-splash--recolor-svg (svg-content foreground-color)
  "Replace white fills in SVG-CONTENT with FOREGROUND-COLOR.
The SVG has shapes filled with #f9f9f9 which need to be replaced
with the theme's foreground color for proper visibility."
  (let ((result svg-content))
    ;; Replace the specific white color used in the SVG
    (setq result (replace-regexp-in-string
                  "fill=\"#f9f9f9\""
                  (format "fill=\"%s\"" foreground-color)
                  result t t))
    ;; Also handle potential variations
    (setq result (replace-regexp-in-string
                  "fill=\"#F9F9F9\""
                  (format "fill=\"%s\"" foreground-color)
                  result t t))
    result))

(defun hyalo-splash--create-logo-image ()
  "Create the logo image from SVG, recolored for current theme.
Uses `:mask heuristic' to enable true transparency when the frame
uses `alpha-background' for transparent backgrounds."
  (when (and (display-graphic-p)
             (file-exists-p hyalo-splash--svg-path))
    (let* ((svg-content (with-temp-buffer
                          (insert-file-contents hyalo-splash--svg-path)
                          (buffer-string)))
           ;; Get foreground color from default face
           (fg-color (face-foreground 'default nil t))
           ;; Recolor the SVG shapes
           (recolored-svg (hyalo-splash--recolor-svg svg-content fg-color)))
      (create-image recolored-svg 'svg t
                    :height hyalo-splash-logo-height
                    :ascent 'center
                    :mask 'heuristic))))

;;; Display Helpers

(defun hyalo-splash--measure-text-pixel-width (text)
  "Measure the pixel width of TEXT in current window."
  (let* ((window (selected-window))
         (orig-buffer (window-buffer window)))
    (unwind-protect
        (with-temp-buffer
          (set-window-buffer window (current-buffer))
          (insert text)
          (car (window-text-pixel-size window nil nil nil nil)))
      (set-window-buffer window orig-buffer))))

(defun hyalo-splash--insert-centered (text &optional face)
  "Insert TEXT centered in the window using pixel-based measurement.
If FACE is provided, apply it to the text."
  (let* ((propertized-text (if face
                               (propertize text 'face face)
                             text))
         (text-pixel-width (hyalo-splash--measure-text-pixel-width
                            propertized-text))
         (window-pixel-width (window-body-width nil t))
         (left-padding-pixels (max 0 (/ (- window-pixel-width text-pixel-width) 2)))
         (padding-spec `(space :width (,left-padding-pixels))))
    (insert (propertize " " 'display padding-spec))
    (insert propertized-text)
    (insert "\n")))

(defun hyalo-splash--insert-centered-image (image)
  "Insert IMAGE centered in the window."
  (when image
    (let* ((image-width (car (image-size image t)))
           (window-pixel-width (window-body-width nil t))
           (left-padding-pixels (max 0 (/ (- window-pixel-width image-width) 2)))
           (padding-spec `(space :width (,left-padding-pixels))))
      (insert (propertize " " 'display padding-spec))
      (insert-image image)
      (insert "\n"))))

(defun hyalo-splash--vertical-space (lines)
  "Insert LINES of vertical spacing."
  (insert (make-string lines ?\n)))

;;; Buffer Creation

(defun hyalo-splash--create-buffer ()
  "Create and populate the splash buffer."
  (let ((splash-buffer (get-buffer-create hyalo-splash--buffer-name)))
    (with-current-buffer splash-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (hyalo-splash-mode)

        ;; Calculate vertical centering
        ;; Estimate content height: logo + spacing + title + subtitle + info
        ;; Rough estimate: logo ~8 lines, title ~2, subtitle ~1, info ~1, spacing ~6 = 18 lines
        (let* ((window-height (window-body-height nil))
               (content-height 18)
               (top-padding (max 2 (/ (- window-height content-height) 3))))

          ;; Top padding
          (hyalo-splash--vertical-space top-padding)

          ;; Logo
          (let ((logo (hyalo-splash--create-logo-image)))
            (when logo
              (hyalo-splash--insert-centered-image logo)
              (hyalo-splash--vertical-space 3)))

          ;; Title
          (hyalo-splash--insert-centered hyalo-splash-title 'hyalo-splash-title)
          (hyalo-splash--vertical-space 1)

          ;; Subtitle
          (hyalo-splash--insert-centered hyalo-splash-subtitle 'hyalo-splash-subtitle)
          (hyalo-splash--vertical-space 3)

          ;; Init time
          (let ((init-time (if (fboundp 'emacs-init-time)
                               (emacs-init-time "%.2f seconds")
                             "N/A")))
            (hyalo-splash--insert-centered
             (format "Loaded in %s" init-time)
             'hyalo-splash-info))))

      (goto-char (point-min))
      (setq buffer-read-only t))
    splash-buffer))

;;; Major Mode

(defvar hyalo-splash-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Only q explicitly kills - otherwise dismissed by opening another buffer
    (define-key map (kbd "q") #'hyalo-splash-kill)
    map)
  "Keymap for `hyalo-splash-mode'.")

(define-derived-mode hyalo-splash-mode special-mode "Splash"
  "Major mode for the Hyalo splash screen."
  :group 'hyalo-splash
  (setq-local mode-line-format nil)
  (setq-local header-line-format nil)
  (setq-local cursor-type nil)
  (setq-local truncate-lines t)
  (setq-local word-wrap nil)
  (setq-local buffer-read-only t)
  (buffer-disable-undo)
  ;; Add window resize hook for re-centering
  (add-hook 'window-size-change-functions
            #'hyalo-splash--on-window-resize nil t))

(defun hyalo-splash--on-window-resize (frame)
  "Refresh splash when window is resized.
FRAME is the frame that changed."
  (when-let* ((buf (get-buffer hyalo-splash--buffer-name))
              (win (get-buffer-window buf frame)))
    (with-current-buffer buf
      (when (eq major-mode 'hyalo-splash-mode)
        ;; Recreate the buffer content with new centering
        (hyalo-splash--create-buffer)))))

;;; Lifecycle Management

(defun hyalo-splash--should-display-p ()
  "Return non-nil if splash should be displayed.
Splash is not shown if:
- File buffers already exist
- Command line arguments specify files
- Running in batch mode"
  (and (display-graphic-p)
       (not noninteractive)
       (not (member "-no-splash" command-line-args))
       (not (member "--file" command-line-args))
       (not (member "--insert" command-line-args))
       (not (member "--find-file" command-line-args))
       ;; No file buffers exist
       (zerop (length (cl-loop for buf in (buffer-list)
                               if (buffer-file-name buf)
                               collect buf)))))

(defvar hyalo-splash--kill-timer nil
  "Timer for auto-killing the splash.")

(defun hyalo-splash-kill ()
  "Kill the splash screen buffer immediately."
  (interactive)
  ;; Cancel any pending timer
  (when hyalo-splash--kill-timer
    (cancel-timer hyalo-splash--kill-timer)
    (setq hyalo-splash--kill-timer nil))
  ;; Remove hooks
  (remove-hook 'window-configuration-change-hook #'hyalo-splash--maybe-kill)
  ;; Kill the buffer
  (when-let* ((buf (get-buffer hyalo-splash--buffer-name)))
    (kill-buffer buf))
  ;; Clear any message
  (message nil))

(defun hyalo-splash--maybe-kill ()
  "Kill splash if a real buffer is now visible."
  (when (get-buffer hyalo-splash--buffer-name)
    (let ((dominated nil))
      ;; Check if splash is still the only visible buffer
      (walk-windows
       (lambda (win)
         (let ((buf (window-buffer win)))
           (unless (or (eq buf (get-buffer hyalo-splash--buffer-name))
                       (minibufferp buf)
                       (string-prefix-p " " (buffer-name buf)))
             (setq dominated t))))
       nil t)
      (when dominated
        (hyalo-splash-kill)))))

;;;###autoload
(defun hyalo-splash ()
  "Display the Hyalo splash screen."
  (interactive)
  (when (hyalo-splash--should-display-p)
    (let ((splash-buffer (hyalo-splash--create-buffer)))
      ;; Display in current window
      (switch-to-buffer splash-buffer)
      ;; Clear any startup messages
      (run-with-idle-timer 0.05 nil (lambda () (message nil)))
      ;; Set up auto-kill on window config change
      (add-hook 'window-configuration-change-hook #'hyalo-splash--maybe-kill))))

;;;###autoload
(defun hyalo-splash-setup ()
  "Set up splash screen to display on startup.
Call this from your init file, after setting up the theme."
  (when (hyalo-splash--should-display-p)
    ;; Inhibit default startup screen
    (setq inhibit-startup-screen t
          inhibit-startup-message t
          inhibit-startup-echo-area-message (user-login-name))
    ;; Display splash after frame is ready
    (add-hook 'window-setup-hook #'hyalo-splash)))

(provide 'hyalo-splash)
;;; hyalo-splash.el ends here
