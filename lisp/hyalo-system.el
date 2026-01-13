;;; hyalo-system.el --- macOS system integration -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: Julien Wintz <julien.wintz@inria.fr>
;; Version: 1.0.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: macos, system, finder, share

;;; Commentary:

;; macOS system integration for hyalo.
;; This file provides native macOS features:
;; - `hyalo-reveal-in-finder': Reveal file(s) in Finder
;; - `hyalo-share': Share file(s) via macOS share sheet
;; - `hyalo-show-emoji-picker': Show the macOS emoji picker
;;
;; Uses a "do what I mean" approach:
;; - In dired: use marked files or file at point
;; - In buffer with file: use buffer's file
;; - Otherwise: prompt for file
;;
;; Usage:
;;   (require 'hyalo-system)
;;   (global-set-key (kbd "C-c l r") #'hyalo-reveal-in-finder)
;;   (global-set-key (kbd "C-c l s") #'hyalo-share)
;;   (global-set-key (kbd "C-c l e") #'hyalo-show-emoji-picker)

;;; Code:

(require 'hyalo)

;;; File Selection

(defun hyalo-system--files-dwim ()
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
(defun hyalo-reveal-in-finder ()
  "Reveal file(s) in macOS Finder.
In dired, reveals marked files or file at point.
In a file buffer, reveals the buffer's file.
Otherwise, prompts for a file."
  (interactive)
  (hyalo-ensure)
  (let ((files (hyalo-system--files-dwim)))
    (when files
      (if (fboundp 'hyalo-reveal-in-finder)
          (hyalo-reveal-in-finder (vconcat files))
        (user-error "hyalo-reveal-in-finder not available")))))

;;;###autoload
(defun hyalo-share ()
  "Share file(s) via the macOS share sheet.
In dired, shares marked files or file at point.
In a file buffer, shares the buffer's file.
Otherwise, prompts for a file.
Uses AirDrop, Mail, Messages, and other macOS sharing services."
  (interactive)
  (hyalo-ensure)
  (let ((files (hyalo-system--files-dwim)))
    (when files
      (if (fboundp 'hyalo-share)
          (hyalo-share (vconcat files))
        (user-error "hyalo-share not available")))))

;;;###autoload
(defun hyalo-show-emoji-picker ()
  "Show the macOS emoji picker.
Inserts the selected emoji at point."
  (interactive)
  (hyalo-ensure)
  (if (fboundp 'hyalo-show-emoji-picker)
      (hyalo-show-emoji-picker)
    (user-error "hyalo-show-emoji-picker not available")))

;;;###autoload
(defun fork-emacs ()
  "Start a new Emacs instance as a sibling process.
This starts a new instance of Emacs using the same configuration,
independent of the current process."
  (interactive)
  (unless (eq system-type 'darwin)
    (user-error "fork-emacs is currently only supported on macOS"))
  (let ((app-bundle (expand-file-name "../.." invocation-directory)))
    (if (and (string-suffix-p ".app" app-bundle)
             (file-exists-p app-bundle))
        (call-process "open" nil 0 nil "-n" "-a" app-bundle)
      (user-error "Could not locate Emacs.app bundle from %s" invocation-directory))))


;;;###autoload
(defun hyalo-toggle-macos-menu-bar ()
  "Toggle macOS menu bar auto-hide."
  (interactive)
  (message "[Hyalo] Toggling macOS menu bar...")
  (let* ((script "tell application \"System Events\"\n if (get autohide menu bar of dock preferences) then\n return \"true\"\n else\n return \"false\"\n end if\n end tell")
         (status (do-applescript script))
         (status-str (format "%s" status)))
    (message "[Hyalo] AppleScript status raw: %S, formatted: %S" status status-str)
    (if (string-equal status-str "true")
        (progn
          (message "[Hyalo] Status is TRUE -> Setting autohide to FALSE (Show menu bar)")
          (do-applescript "tell application \"System Events\" to set autohide menu bar of dock preferences to false"))
      (progn
        (message "[Hyalo] Status is FALSE -> Setting autohide to TRUE (Hide menu bar)")
        (do-applescript "tell application \"System Events\" to set autohide menu bar of dock preferences to true")))))

(provide 'hyalo-system)
;;; hyalo-system.el ends here
