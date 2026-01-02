;;; iota-shell.el --- Eshell improvements (Starship-like) -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: Julien Wintz
;; Keywords: eshell, terminal, prompt

;;; Commentary:

;; Provides Starship-like experience for Eshell with a 1-line prompt.
;; Layout: [User] at [Host] in [Dir] λ [CURSOR]           [Git] via [Lang] at [Time]
;;
;; Right prompt is implemented as an overlay on the last character of the prompt.

;;; Code:

(require 'eshell)
(require 'em-ls)
(require 'em-smart)
(require 'em-tramp)
(require 'vc-git)
(require 'dom)

(defgroup iota-shell nil
  "Iota shell configuration."
  :group 'eshell)

;;; Logging

(defvar iota-shell-elog nil
  "Iota shell logger (nil if elog not loaded).")

(defun iota-shell-log (context msg &rest args)
  "Log MSG with ARGS in CONTEXT using elog."
  (let ((formatted (format "[%s] %s" context msg)))
    (if (and iota-shell-elog (fboundp 'elog-info))
        (apply #'elog-info iota-shell-elog formatted args)
      (let ((out (apply #'format formatted args)))
        (message "[iota-shell] %s" out)))))

(defun iota-shell-debug (context msg &rest args)
  "Log debug level MSG with ARGS in CONTEXT."
  (let ((formatted (format "[%s] %s" context msg)))
    (if (and iota-shell-elog (fboundp 'elog-debug))
        (apply #'elog-debug iota-shell-elog formatted args)
      (let ((out (apply #'format formatted args)))
        (message "[iota-shell] DEBUG: %s" out)))))

(defun iota-shell-log-init ()
  "Initialize iota-shell logger if elog is available."
  (when (fboundp 'elog-logger)
    (setq iota-shell-elog
          (elog-logger
           :name "iota-shell"
           :level 'info
           :buffer "*elog*"
           :handlers '(buffer)))))

;; Initialize logger when elog becomes available
(with-eval-after-load 'elog
  (iota-shell-log-init))

;;; Aliases

(defcustom iota-shell-eza-options
  '("--group-directories-first" "--header" "--git")
  "Options to pass to eza when used as ls."
  :type '(repeat string)
  :group 'iota-shell)

(defun iota-shell-ls (&rest args)
  "Use eza as ls if available, otherwise fallback to standard eshell ls."
  (if (executable-find "eza")
      (throw 'eshell-replace-command
             (eshell-parse-command "eza" (append iota-shell-eza-options args)))
    (apply #'eshell/ls-orig args)))

;;; Faces

(defface iota-shell-user-face
  '((t (:inherit font-lock-variable-name-face :weight bold)))
  "Face for username."
  :group 'iota-shell)

(defface iota-shell-host-face
  '((t (:inherit font-lock-warning-face :weight bold)))
  "Face for hostname."
  :group 'iota-shell)

(defface iota-shell-dir-face
  '((t (:inherit font-lock-function-name-face :weight bold)))
  "Face for directory."
  :group 'iota-shell)

(defface iota-shell-git-face
  '((t (:inherit font-lock-string-face)))
  "Face for git info."
  :group 'iota-shell)

(defface iota-shell-lang-face
  '((t (:inherit font-lock-constant-face)))
  "Face for language info."
  :group 'iota-shell)

(defface iota-shell-time-face
  '((t (:inherit font-lock-comment-face)))
  "Face for time."
  :group 'iota-shell)

(defface iota-shell-prompt-char-success
  '((t (:inherit font-lock-keyword-face :weight bold)))
  "Face for prompt char on success."
  :group 'iota-shell)

(defface iota-shell-prompt-char-error
  '((t (:inherit error :weight bold)))
  "Face for prompt char on error."
  :group 'iota-shell)

(defface iota-shell-dim-face
  '((t (:inherit shadow)))
  "Face for separators (at, in, on, via)."
  :group 'iota-shell)

;;; Components

(defvar iota-shell-prompt-char "λ"
  "Character used for the input prompt.")

(defun iota-shell--dim (str)
  (propertize str 'face 'iota-shell-dim-face))

(defun iota-shell--user-host ()
  "Return 'user at host'."
  (concat
   (propertize (user-login-name) 'face 'iota-shell-user-face)
   (iota-shell--dim " at ")
   (propertize (system-name) 'face 'iota-shell-host-face)))

(defun iota-shell--dir ()
  "Return 'in dir'."
  (let* ((path (abbreviate-file-name (eshell/pwd)))
         (parts (split-string path "/"))
         (len (length parts))
         (short-path (if (> len 3)
                         (concat (car parts) "/.../" (mapconcat #'identity (nthcdr (- len 2) parts) "/"))
                       path)))
    (concat (iota-shell--dim " in ")
            (propertize short-path 'face 'iota-shell-dir-face))))

(defun iota-shell--git ()
  "Return 'on git branch [status]'."
  (if-let* ((branch (car (vc-git-branches))))
      (concat (iota-shell--dim " on ")
              (propertize (concat "git " branch) 'face 'iota-shell-git-face))
    ""))

(defun iota-shell--lang ()
  "Return 'via lang'."
  (let ((lang nil))
    (cond
     ((file-exists-p "Package.swift") (setq lang "swift"))
     ((file-exists-p "Cargo.toml")    (setq lang "rust"))
     ((file-exists-p "package.json")  (setq lang "node"))
     ((file-exists-p "go.mod")        (setq lang "go"))
     ((file-exists-p "mix.exs")       (setq lang "elixir"))
     ((file-exists-p "pyproject.toml") (setq lang "python"))
     ((or (file-exists-p "Cask") (file-exists-p "Eask")) (setq lang "emacs")))
    
    (if lang
        (concat (iota-shell--dim " via ")
                (propertize lang 'face 'iota-shell-lang-face))
      "")))

(defun iota-shell--time ()
  "Return 'at HH:MM:SS'."
  (concat (iota-shell--dim " at ")
          (propertize (format-time-string "%H:%M:%S") 'face 'iota-shell-time-face)))

;;; Right Prompt

(defun iota-shell--make-right-prompt-string ()
  "Construct the right prompt string."
  (concat (iota-shell--git)
          (iota-shell--lang)
          (iota-shell--time)))

(defun iota-shell--draw-right-prompt ()
  "Draw the right prompt using an overlay at the very end."
  (message "[iota-shell] Draw Right Prompt called in buffer %s" (current-buffer))
  (save-excursion
    (goto-char (point-max))
    (let ((p (point)))
      (iota-shell-debug 'prompt "Drawing right prompt at point %d" p)
      (when (> p (point-min))
        (let* ((right-string (iota-shell--make-right-prompt-string))
               (right-len (length (substring-no-properties right-string)))
               ;; Create zero-width overlay at the very end, explicitly in current buffer
               (ov (make-overlay p p nil t nil)))
          (overlay-put ov 'after-string
                       (concat
                        (propertize " " 'display `(space :align-to (- right ,right-len)))
                        right-string))
          (overlay-put ov 'evaporate t)
          (overlay-put ov 'iota-shell-right-prompt t)
          (message "[iota-shell] Overlay created: %s at %d with string '%s'" ov p right-string))))))

;;; Left Prompt Function

(defun iota-shell-prompt ()
  "Generate the left prompt and schedule right prompt drawing."
  (message "[iota-shell] Generating prompt")
  ;; Schedule right prompt drawing immediately after this function returns
  ;; and Eshell inserts the string.
  (run-at-time 0 nil #'iota-shell--draw-right-prompt)
  
  (concat
   "\n" ;; Spacing from previous command
   (iota-shell--user-host)
   (iota-shell--dir)
   " "
   (if (and (boundp 'eshell-last-command-status) (= eshell-last-command-status 0))
       (propertize iota-shell-prompt-char 'face 'iota-shell-prompt-char-success)
     (propertize iota-shell-prompt-char 'face 'iota-shell-prompt-char-error))
   " ")) ;; Trailing space for cursor

;;; Setup

;;;###autoload
(define-minor-mode iota-shell-mode
  "Minor mode for Iota Shell enhancements."
  :global t
  :group 'iota-shell
  (if iota-shell-mode
      (progn
        (iota-shell-log 'core "Enabled")
        ;; Robust ls alias
        (unless (fboundp 'eshell/ls-orig)
          (if (fboundp 'eshell/ls)
              (defalias 'eshell/ls-orig (symbol-function 'eshell/ls))
            (require 'em-ls)
            (defalias 'eshell/ls-orig (symbol-function 'eshell/ls))))
        (defalias 'eshell/ls #'iota-shell-ls)
        
        ;; Set prompt
        (setq eshell-prompt-function #'iota-shell-prompt)
        (setq eshell-highlight-prompt nil)
        ;; Prompt starts with newline, then text, then "λ "
        (setq eshell-prompt-regexp (concat "^.*" (regexp-quote iota-shell-prompt-char) " ")))
    ;; Restore
    (iota-shell-log 'core "Disabled")
    (setq eshell-prompt-function #'eshell-default-prompt-function)))

(message "Loading iota-shell.el")
(provide 'iota-shell)
;;; iota-shell.el ends here