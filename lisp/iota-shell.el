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

(defvar iota-shell-prompt-char "λ"
  "Character used for the input prompt.")

;;; Helper Functions

(defun iota-shell--dim (str)
  "Return STR with dim face."
  (propertize str 'face 'iota-shell-dim-face))

;;; Prompt Components

(defun iota-shell--git ()
  "Return git branch info or nil."
  (when-let* ((branch (ignore-errors (car (vc-git-branches)))))
    (concat (iota-shell--dim " on ")
            (propertize branch 'face 'iota-shell-git-face))))

(defun iota-shell--lang ()
  "Return detected language or nil."
  (let ((lang (cond
               ((file-exists-p "Package.swift") "swift")
               ((file-exists-p "Cargo.toml") "rs")
               ((file-exists-p "package.json") "js")
               ((file-exists-p "go.mod") "go")
               ((file-exists-p "mix.exs") "ex")
               ((file-exists-p "pyproject.toml") "py")
               ((or (file-exists-p "Cask") (file-exists-p "Eask")) "elisp"))))
    (when lang
      (concat (iota-shell--dim " via ")
              (propertize lang 'face 'iota-shell-lang-face)))))

;;; Prompt

;;;###autoload
(defun iota-shell-prompt ()
  "Generate the prompt."
  (let* ((user (propertize (user-login-name) 'face 'iota-shell-user-face))
         (host (propertize (car (split-string (system-name) "\\."))
                           'face 'iota-shell-host-face))
         (pwd (eshell/pwd))
         (root (or (vc-root-dir) (locate-dominating-file pwd ".git")))
         (path (if root
                   (file-relative-name pwd (file-name-directory (directory-file-name root)))
                 (abbreviate-file-name pwd)))
         (dir (propertize path 'face 'iota-shell-dir-face))
         (git (iota-shell--git))
         (lang (iota-shell--lang))
         (success (or (not (boundp 'eshell-last-command-status))
                      (zerop eshell-last-command-status)))
         (char-face (if success 'iota-shell-prompt-char-success 'iota-shell-prompt-char-error))
         (char (propertize iota-shell-prompt-char 'face char-face))
         ;; Add newline unless we are at the beginning of the buffer (first prompt)
         (nl (if (and (boundp 'eshell-last-output-end)
                      (= eshell-last-output-end (point-min)))
                 ""
               "\n")))
    (concat nl user (iota-shell--dim " at ") host (iota-shell--dim " in ") dir
            (or git "") (or lang "") " " char " ")))

;;; Minor Mode

(defun iota-shell--setup ()
  "Setup iota-shell in the current eshell buffer."
  (setq-local eshell-prompt-function #'iota-shell-prompt)
  (setq-local eshell-highlight-prompt nil)
  (setq-local eshell-prompt-regexp (concat "^\\(?:\n\\)?.*" (regexp-quote iota-shell-prompt-char) " "))

  ;; Force initial prompt update and clean slate
  (when (and (eq major-mode 'eshell-mode)
             (not (bound-and-true-p iota-shell--initialized)))
    (setq-local iota-shell--initialized t)
    (let ((inhibit-read-only t))
      (erase-buffer))))

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

        ;; Set global defaults immediately
        (set-default 'eshell-prompt-function #'iota-shell-prompt)
        (set-default 'eshell-highlight-prompt nil)
        (set-default 'eshell-prompt-regexp (concat "^\\(?:\n\\)?.*" (regexp-quote iota-shell-prompt-char) " "))

        ;; Hook for eshell buffers (still needed if something overrides locals)
        (add-hook 'eshell-mode-hook #'iota-shell--setup 100)

        ;; Apply to existing buffers
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (when (derived-mode-p 'eshell-mode)
              (iota-shell--setup)
              ;; Force prompt redisplay
              (eshell-emit-prompt)))))
    ;; Disable
    (remove-hook 'eshell-mode-hook #'iota-shell--setup)
    (set-default 'eshell-prompt-function #'eshell-default-prompt-function)))

;; Force update if mode is already active (handles reload)
(when (bound-and-true-p iota-shell-mode)
  (iota-shell-mode 1))

(provide 'iota-shell)
;;; iota-shell.el ends here
