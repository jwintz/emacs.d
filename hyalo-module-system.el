;;; hyalo-module-system.el --- macOS system integration -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: Julien Wintz <julien.wintz@inria.fr>
;; Version: 1.0.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: macos, system, finder, share

;;; Commentary:

;; macOS system integration for hyalo-module.
;; This file provides native macOS features:
;; - `hyalo-module-reveal-in-finder': Reveal file(s) in Finder
;; - `hyalo-module-share': Share file(s) via macOS share sheet
;; - `hyalo-module-show-emoji-picker': Show the macOS emoji picker
;;
;; Uses a "do what I mean" approach:
;; - In dired: use marked files or file at point
;; - In buffer with file: use buffer's file
;; - Otherwise: prompt for file
;;
;; Usage:
;;   (require 'hyalo-module-system)
;;   (global-set-key (kbd "C-c l r") #'hyalo-module-reveal-in-finder)
;;   (global-set-key (kbd "C-c l s") #'hyalo-module-share)
;;   (global-set-key (kbd "C-c l e") #'hyalo-module-show-emoji-picker)

;;; Code:

(require 'hyalo-module)

;;; File Selection

(defun hyalo-module--files-dwim ()
  "Return a list of files based on context (Do What I Mean).
- In dired: return marked files, or file at point
- In buffer with file: return buffer's file
- Otherwise: prompt for file"
  (cond
   ;; Dired mode: get marked files or file at point
   ((derived-mode-p 'dired-mode)
    (or (dired-get-marked-files nil nil nil t)
        (list (dired-get-filename nil t))))
   ;; Buffer with associated file
   (buffer-file-name
    (list buffer-file-name))
   ;; Default: prompt for file
   (t
    (list (read-file-name "File: ")))))

;;; Interactive Commands

;;;###autoload
(defun hyalo-module-reveal-in-finder ()
  "Reveal file(s) in macOS Finder.
In dired, reveals marked files or file at point.
In a file buffer, reveals the buffer's file.
Otherwise, prompts for a file."
  (interactive)
  (hyalo-module-ensure)
  (let ((files (hyalo-module--files-dwim)))
    (when files
      (if (fboundp 'hyalo-reveal-in-finder)
          (hyalo-reveal-in-finder (vconcat files))
        (user-error "hyalo-reveal-in-finder not available")))))

;;;###autoload
(defun hyalo-module-share ()
  "Share file(s) via the macOS share sheet.
In dired, shares marked files or file at point.
In a file buffer, shares the buffer's file.
Otherwise, prompts for a file.
Uses AirDrop, Mail, Messages, and other macOS sharing services."
  (interactive)
  (hyalo-module-ensure)
  (let ((files (hyalo-module--files-dwim)))
    (when files
      (if (fboundp 'hyalo-share)
          (hyalo-share (vconcat files))
        (user-error "hyalo-share not available")))))

;;;###autoload
(defun hyalo-module-show-emoji-picker ()
  "Show the macOS emoji picker.
Inserts the selected emoji at point."
  (interactive)
  (hyalo-module-ensure)
  (if (fboundp 'hyalo-show-emoji-picker)
      (hyalo-show-emoji-picker)
    (user-error "hyalo-show-emoji-picker not available")))

(provide 'hyalo-module-system)
;;; hyalo-module-system.el ends here
