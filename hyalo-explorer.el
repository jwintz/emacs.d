;;; hyalo-explorer.el --- NavigationSplitView sidebar with treemacs -*- lexical-binding: t -*-

;; Copyright (C) 2025 Julien Wintz

;; Author: Julien Wintz
;; Keywords: convenience, files
;; Version: 0.4.0
;; Package-Requires: ((emacs "30.1") (treemacs "3.0") (nerd-icons "0.1.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; NavigationSplitView sidebar for Emacs:
;; - SwiftUI sidebar shows Open Editors + Explorer (from treemacs)
;; - Treemacs runs hidden, providing file tree data to Swift sidebar
;; - Native traffic lights are shown when sidebar is active
;; - Nerd font icons used for file types (same as treemacs)

;;; Code:

(require 'json)
(require 'treemacs)
(require 'nerd-icons)
(require 'hyalo-module)

(defgroup hyalo-explorer nil
  "NavigationSplitView sidebar for Emacs."
  :group 'convenience
  :prefix "hyalo-explorer-")

(defvar hyalo-explorer--visible nil
  "Whether the NavigationSplitView sidebar is visible.")

(defvar hyalo-explorer--refresh-timer nil
  "Timer for debounced refresh.")

(defvar hyalo-explorer--treemacs-was-visible nil
  "Whether treemacs was visible before we hid it.")

;;; Nerd Icons Helpers

(defun hyalo-explorer--nerd-icon-for-file (filename)
  "Get nerd font icon character for FILENAME."
  (let ((icon (nerd-icons-icon-for-file filename)))
    ;; nerd-icons returns a propertized string, we need just the character
    (if (stringp icon)
        (substring-no-properties icon 0 1)
      "")))

(defun hyalo-explorer--nerd-icon-for-dir (dirname &optional closed)
  "Get nerd font icon character for directory DIRNAME.
If CLOSED is non-nil, return closed folder icon."
  (let ((icon (if closed
                  (nerd-icons-icon-for-dir dirname)
                (nerd-icons-icon-for-dir dirname))))
    (if (stringp icon)
        (substring-no-properties icon 0 1)
      "")))

(defun hyalo-explorer--nerd-icon-for-mode (mode)
  "Get nerd font icon character for major MODE."
  (let ((icon (nerd-icons-icon-for-mode mode)))
    (if (and (stringp icon) (> (length icon) 0))
        (substring-no-properties icon 0 1)
      "")))

;;; Buffer Data Collection

(defun hyalo-explorer--interesting-buffer-p (buffer)
  "Return t if BUFFER should be shown in Open Editors."
  (let ((name (buffer-name buffer)))
    (and (not (string-prefix-p " " name))
         (not (string-prefix-p "*" name))
         (not (string-prefix-p "magit" name))
         (not (eq (buffer-local-value 'major-mode buffer) 'treemacs-mode))
         (buffer-file-name buffer))))

(defun hyalo-explorer--collect-buffers ()
  "Collect buffer data as list of alists for JSON encoding."
  (let ((current-buf (current-buffer)))
    (mapcar
     (lambda (buf)
       (let* ((filename (buffer-file-name buf))
              (icon (if filename
                        (hyalo-explorer--nerd-icon-for-file filename)
                      (hyalo-explorer--nerd-icon-for-mode
                       (buffer-local-value 'major-mode buf)))))
         `((id . ,(buffer-name buf))
           (name . ,(buffer-name buf))
           (path . ,filename)
           (modified . ,(if (buffer-modified-p buf) t :false))
           (current . ,(if (eq buf current-buf) t :false))
           (icon . ,icon))))
     (seq-filter #'hyalo-explorer--interesting-buffer-p (buffer-list)))))

;;; Treemacs Data Collection

(defun hyalo-explorer--collect-treemacs-files ()
  "Collect file tree from treemacs as list of alists for JSON encoding."
  (when-let* ((proj (project-current))
              (root (project-root proj)))
    (let ((result '())
          (max-depth 3)  ; Limit depth for performance
          (max-files 200))  ; Limit total files
      ;; Walk directory tree
      (hyalo-explorer--walk-directory root 0 max-depth max-files result)
      (nreverse result))))

(defun hyalo-explorer--walk-directory (dir depth max-depth max-files result)
  "Walk DIR at DEPTH, collecting files up to MAX-DEPTH and MAX-FILES into RESULT."
  (when (and (< depth max-depth)
             (< (length result) max-files)
             (file-directory-p dir))
    (let* ((entries (directory-files dir t "^[^.]" t))  ; Skip dotfiles
           (dirs (seq-filter #'file-directory-p entries))
           (files (seq-remove #'file-directory-p entries)))
      ;; Add directories first
      (dolist (d dirs)
        (when (< (length result) max-files)
          (let ((name (file-name-nondirectory d)))
            (push `((id . ,d)
                    (name . ,name)
                    (path . ,d)
                    (directory . t)
                    (expanded . ,(< depth 1))  ; Expand first level
                    (depth . ,depth)
                    (icon . ,(hyalo-explorer--nerd-icon-for-dir name)))
                  result)
            ;; Recurse into expanded directories
            (when (< depth 1)
              (hyalo-explorer--walk-directory d (1+ depth) max-depth max-files result)))))
      ;; Add files
      (dolist (f files)
        (when (< (length result) max-files)
          (let ((name (file-name-nondirectory f)))
            (push `((id . ,f)
                    (name . ,name)
                    (path . ,f)
                    (directory . :false)
                    (expanded . :false)
                    (depth . ,depth)
                    (icon . ,(hyalo-explorer--nerd-icon-for-file name)))
                  result)))))))

;;; Mode Line

(defun hyalo-explorer--format-mode-line ()
  "Format mode-line content for the toolbar."
  (format-mode-line
   '(" "
     mode-name
     "  │  "
     (:eval (if buffer-read-only "RO" "RW"))
     "  │  "
     "L" (:eval (format "%d" (line-number-at-pos)))
     ":" "C" (:eval (format "%d" (current-column)))
     "  │  "
     (:eval (or (buffer-file-name) (buffer-name))))
   nil nil (current-buffer)))

;;; Swift Communication

(defun hyalo-explorer--send-buffers ()
  "Send buffer list to Swift sidebar."
  (when (and hyalo-explorer--visible
             (fboundp 'hyalo-sidebar-update-buffers-json))
    (let ((json-data (json-encode (hyalo-explorer--collect-buffers))))
      (hyalo-sidebar-update-buffers-json json-data))))

(defun hyalo-explorer--send-files ()
  "Send file tree to Swift sidebar."
  (when (and hyalo-explorer--visible
             (fboundp 'hyalo-sidebar-update-files-json))
    (let ((files (hyalo-explorer--collect-treemacs-files)))
      (when files
        (hyalo-sidebar-update-files-json (json-encode files))))))

(defun hyalo-explorer--send-project-name ()
  "Send project name to Swift sidebar."
  (when (and hyalo-explorer--visible
             (fboundp 'hyalo-sidebar-set-project))
    (if-let* ((proj (project-current))
              (root (project-root proj))
              (name (file-name-nondirectory (directory-file-name root))))
        (hyalo-sidebar-set-project name)
      (hyalo-sidebar-set-project "Emacs"))))

(defun hyalo-explorer--send-mode-line ()
  "Send mode-line content to Swift toolbar."
  (when (and hyalo-explorer--visible
             (fboundp 'hyalo-sidebar-update-mode-line))
    (hyalo-sidebar-update-mode-line (hyalo-explorer--format-mode-line))))

(defun hyalo-explorer--refresh ()
  "Refresh all sidebar data."
  (hyalo-explorer--send-buffers)
  (hyalo-explorer--send-files)
  (hyalo-explorer--send-project-name)
  (hyalo-explorer--send-mode-line))

(defun hyalo-explorer--auto-refresh ()
  "Auto-refresh when buffers change (debounced)."
  (when hyalo-explorer--visible
    (when hyalo-explorer--refresh-timer
      (cancel-timer hyalo-explorer--refresh-timer))
    (setq hyalo-explorer--refresh-timer
          (run-with-idle-timer 0.3 nil #'hyalo-explorer--refresh))))

(defun hyalo-explorer--sync-background-color ()
  "Sync sidebar vibrancy background color with Emacs default face + alpha."
  (when (and hyalo-explorer--visible
             (fboundp 'hyalo-sidebar-set-background-color))
    (let* ((bg-color (face-background 'default nil t))
           (alpha (or (frame-parameter nil 'alpha-background) 1.0))
           ;; Convert color to hex if it's a name
           (hex-color (if (and bg-color (string-prefix-p "#" bg-color))
                          bg-color
                        (when bg-color
                          (apply #'format "#%02x%02x%02x"
                                 (mapcar (lambda (c) (/ c 256))
                                         (color-values bg-color)))))))
      (when hex-color
        (hyalo-sidebar-set-background-color hex-color (float alpha))))))

(defun hyalo-explorer--on-theme-change (_theme)
  "Handle theme change - sync sidebar background color.
Called via `enable-theme-functions'."
  (when hyalo-explorer--visible
    ;; Delay slightly to ensure theme colors are fully applied
    (run-with-timer 0.1 nil #'hyalo-explorer--sync-background-color)))

;;; Treemacs Management

(defun hyalo-explorer--setup-treemacs ()
  "Setup treemacs in hidden mode for data collection.
This initializes treemacs silently without showing its window."
  ;; Remember if treemacs was visible
  (setq hyalo-explorer--treemacs-was-visible
        (and (treemacs-get-local-window)
             (window-live-p (treemacs-get-local-window))))
  ;; Initialize treemacs if not already done
  ;; This activates treemacs mode for proper file tree access
  (unless (treemacs-current-workspace)
    (let ((inhibit-message t))
      (treemacs--init)))
  ;; Activate treemacs to ensure mode is active and data is current
  (unless (treemacs-get-local-window)
    (let ((inhibit-message t))
      (treemacs)))
  ;; Always hide treemacs window - Swift sidebar displays the content
  (when (treemacs-get-local-window)
    (treemacs-quit)))

(defun hyalo-explorer--teardown-treemacs ()
  "Restore treemacs state."
  ;; Restore treemacs if it was visible before
  (when hyalo-explorer--treemacs-was-visible
    (treemacs)))

;;; Public Interface

;;;###autoload
(defun hyalo-explorer-show ()
  "Show the NavigationSplitView sidebar."
  (interactive)
  (hyalo-module-ensure)
  ;; Note: Do NOT disable header mode - NavigationSplitView works with it
  ;; The Swift toolbar replaces the header capsule only when sidebar is visible
  ;; Hide treemacs (we use Swift sidebar for explorer)
  (hyalo-explorer--setup-treemacs)
  ;; Show sidebar
  (when (fboundp 'hyalo-sidebar-show)
    (hyalo-sidebar-show)
    (setq hyalo-explorer--visible t)
    ;; Set sidebar background color to match Emacs default face + alpha
    (hyalo-explorer--sync-background-color)
    ;; Send initial data
    (run-with-timer 0.2 nil #'hyalo-explorer--refresh)
    ;; Setup auto-refresh
    (add-hook 'buffer-list-update-hook #'hyalo-explorer--auto-refresh)
    (add-hook 'window-configuration-change-hook #'hyalo-explorer--auto-refresh)
    (add-hook 'post-command-hook #'hyalo-explorer--send-mode-line)
    ;; Sync background color when theme changes
    (add-hook 'enable-theme-functions #'hyalo-explorer--on-theme-change)))

;;;###autoload
(defun hyalo-explorer-hide ()
  "Hide the NavigationSplitView sidebar."
  (interactive)
  ;; Remove hooks
  (remove-hook 'buffer-list-update-hook #'hyalo-explorer--auto-refresh)
  (remove-hook 'window-configuration-change-hook #'hyalo-explorer--auto-refresh)
  (remove-hook 'post-command-hook #'hyalo-explorer--send-mode-line)
  (remove-hook 'enable-theme-functions #'hyalo-explorer--on-theme-change)
  (when hyalo-explorer--refresh-timer
    (cancel-timer hyalo-explorer--refresh-timer)
    (setq hyalo-explorer--refresh-timer nil))
  ;; Hide sidebar
  (when (fboundp 'hyalo-sidebar-hide)
    (hyalo-sidebar-hide))
  (setq hyalo-explorer--visible nil)
  ;; Restore treemacs
  (hyalo-explorer--teardown-treemacs)
  ;; Note: Do NOT re-enable header mode - it was never disabled
  )

;;;###autoload
(defun hyalo-explorer-toggle ()
  "Toggle the NavigationSplitView sidebar."
  (interactive)
  (if hyalo-explorer--visible
      (hyalo-explorer-hide)
    (hyalo-explorer-show)))

;;;###autoload
(defun hyalo-explorer-refresh ()
  "Manually refresh the sidebar content."
  (interactive)
  (when hyalo-explorer--visible
    (hyalo-explorer--refresh)))

(provide 'hyalo-explorer)
;;; hyalo-explorer.el ends here
