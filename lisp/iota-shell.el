;;; iota-shell.el --- Eshell improvements (Starship-like) -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: Julien Wintz
;; Keywords: eshell, terminal, prompt

;;; Commentary:

;; Provides Starship-like experience for Eshell with a TRUE single-line prompt.
;; Layout: [User] at [Host] in [Dir] ⦿ [CURSOR...]     [Git] [Lang] [Time]
;;
;; The right prompt stays at the right edge as you type, disappearing when
;; the command gets too long (like Starship in a real terminal).

;;; Code:

(require 'eshell)
(require 'em-ls)
(require 'vc-git)

(defgroup iota-shell nil
  "Iota shell configuration."
  :group 'eshell)

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
  '((t (:inherit success :weight bold)))
  "Face for prompt char on success."
  :group 'iota-shell)

(defface iota-shell-prompt-char-error
  '((t (:inherit error :weight bold)))
  "Face for prompt char on error."
  :group 'iota-shell)

(defface iota-shell-dim-face
  '((t (:inherit shadow)))
  "Face for separators (at, in, via)."
  :group 'iota-shell)

;;; Variables

(defvar iota-shell-prompt-char "⦿"
  "Character used for the input prompt.")

(defvar-local iota-shell--right-overlay nil
  "Overlay for the right prompt.")

(defvar-local iota-shell--cached-right-prompt nil
  "Cached right prompt string for current prompt.")

;;; Helper Functions

(defun iota-shell--dim (str)
  "Return STR with dim face."
  (propertize str 'face 'iota-shell-dim-face))

;;; Right Prompt Components

(defun iota-shell--git ()
  "Return git branch info or nil."
  (when-let ((branch (ignore-errors (car (vc-git-branches)))))
    (propertize (concat "git " branch) 'face 'iota-shell-git-face)))

(defun iota-shell--lang ()
  "Return detected language or nil."
  (let ((lang (cond
               ((file-exists-p "Package.swift") "swift")
               ((file-exists-p "Cargo.toml") "rs")
               ((file-exists-p "package.json") "nodejs")
               ((file-exists-p "go.mod") "go")
               ((file-exists-p "mix.exs") "exs")
               ((file-exists-p "pyproject.toml") "py")
               ((or (file-exists-p "Cask") (file-exists-p "Eask")) "elisp"))))
    (when lang
      (propertize lang 'face 'iota-shell-lang-face))))

(defun iota-shell--time ()
  "Return current time."
  (propertize (format-time-string "%H:%M:%S") 'face 'iota-shell-time-face))

(defun iota-shell--build-right-prompt ()
  "Build the right prompt string."
  (let ((parts (delq nil (list (iota-shell--git)
                               (iota-shell--lang)
                               (iota-shell--time)))))
    (mapconcat #'identity parts " ")))

;;; Right Prompt Overlay Management

(defun iota-shell--update-right-prompt ()
  "Update the right prompt overlay on the current input line."
  (when (and (derived-mode-p 'eshell-mode)
             (bound-and-true-p iota-shell-mode)
             (boundp 'eshell-last-output-end)
             (markerp eshell-last-output-end)
             (>= (point) eshell-last-output-end))
    ;; Remove old overlay
    (when (overlayp iota-shell--right-overlay)
      (delete-overlay iota-shell--right-overlay))

    (let* ((right-str (or iota-shell--cached-right-prompt ""))
           (right-len (length right-str))
           (line-end (line-end-position))
           (line-start (line-beginning-position))
           (current-len (- line-end line-start))
           (win-width (- (window-body-width) 1))
           (space-needed (+ current-len 1 right-len)))

      ;; Only show if there's room (at least 1 char gap)
      (when (< space-needed win-width)
        (setq iota-shell--right-overlay (make-overlay line-end line-end nil t))
        (overlay-put iota-shell--right-overlay 'iota-shell-right t)
        (overlay-put iota-shell--right-overlay 'evaporate t)
        (overlay-put iota-shell--right-overlay 'after-string
                     (concat
                      (propertize " " 'display
                                  `(space :align-to (- right-fringe ,right-len 1)))
                      right-str))))))

(defun iota-shell--clear-right-prompt ()
  "Clear the right prompt overlay."
  (when (overlayp iota-shell--right-overlay)
    (delete-overlay iota-shell--right-overlay)
    (setq iota-shell--right-overlay nil)))

;;; Left Prompt

(defun iota-shell-prompt ()
  "Generate the left prompt and cache right prompt for overlay."
  ;; Cache the right prompt at prompt generation time
  (setq iota-shell--cached-right-prompt (iota-shell--build-right-prompt))

  ;; Schedule right prompt overlay after prompt is inserted
  (run-at-time 0 nil #'iota-shell--update-right-prompt)

  ;; Build left prompt
  (let* ((user (propertize (user-login-name) 'face 'iota-shell-user-face))
         (host (propertize (car (split-string (system-name) "\\."))
                           'face 'iota-shell-host-face))
         (path (abbreviate-file-name (eshell/pwd)))
         (parts (split-string path "/" t))
         (len (length parts))
         (short-path (if (> len 3)
                         (concat ".../" (mapconcat #'identity (last parts 2) "/"))
                       path))
         (dir (propertize short-path 'face 'iota-shell-dir-face))
         (success (or (not (boundp 'eshell-last-command-status))
                      (zerop eshell-last-command-status)))
         (char-face (if success 'iota-shell-prompt-char-success 'iota-shell-prompt-char-error))
         (char (propertize iota-shell-prompt-char 'face char-face)))
    (concat user (iota-shell--dim " at ") host (iota-shell--dim " in ") dir " " char " ")))

;;; Hooks

(defun iota-shell--post-command ()
  "Update right prompt after each command in eshell."
  (when (derived-mode-p 'eshell-mode)
    (iota-shell--update-right-prompt)))

(defun iota-shell--eshell-setup ()
  "Setup iota-shell in an eshell buffer."
  (add-hook 'post-command-hook #'iota-shell--post-command nil t))

;;; Minor Mode

;;;###autoload
(define-minor-mode iota-shell-mode
  "Minor mode for Iota Shell enhancements."
  :global t
  :group 'iota-shell
  (if iota-shell-mode
      (progn
        ;; Robust ls alias
        (unless (fboundp 'eshell/ls-orig)
          (when (fboundp 'eshell/ls)
            (defalias 'eshell/ls-orig (symbol-function 'eshell/ls))))
        (defalias 'eshell/ls #'iota-shell-ls)
        ;; Set prompt
        (setq eshell-prompt-function #'iota-shell-prompt
              eshell-highlight-prompt nil
              eshell-prompt-regexp (concat "^.*" (regexp-quote iota-shell-prompt-char) " "))
        ;; Hook into eshell buffers
        (add-hook 'eshell-mode-hook #'iota-shell--eshell-setup)
        ;; Setup existing eshell buffers
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (when (derived-mode-p 'eshell-mode)
              (iota-shell--eshell-setup)))))
    ;; Disable
    (remove-hook 'eshell-mode-hook #'iota-shell--eshell-setup)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (derived-mode-p 'eshell-mode)
          (remove-hook 'post-command-hook #'iota-shell--post-command t)
          (iota-shell--clear-right-prompt))))
    (setq eshell-prompt-function #'eshell-default-prompt-function)))

(provide 'iota-shell)
;;; iota-shell.el ends here
