;;; iota-shell.el --- Eshell with starship prompt and eza -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: Julien Wintz
;; Keywords: eshell, terminal, prompt

;;; Commentary:

;; Eshell enhancements with starship prompt integration and eza alias.
;; Uses starship binary if available, otherwise falls back to built-in prompt.
;; Layout: user at host in dir on git via lang λ

;;; Code:

(require 'eshell)
(require 'em-ls)
(require 'em-dirs)
(require 'em-basic)
(require 'vc-git)
(require 'ansi-color)
(require 'seq)

(defgroup iota-shell nil
  "Iota shell configuration."
  :group 'eshell)

;;; Starship Integration

(defcustom iota-shell-starship-config
  (expand-file-name "config/eshprompt" (or (bound-and-true-p emacs-config-dir)
                                          (file-name-directory (or load-file-name buffer-file-name))))
  "Path to starship configuration file for eshell."
  :type 'file
  :group 'iota-shell)

(defvar iota-shell--starship-available nil
  "Cached result of starship availability check.
Use `iota-shell-reset-starship-cache' to clear.")

(defun iota-shell-reset-starship-cache ()
  "Reset the starship availability cache."
  (interactive)
  (setq iota-shell--starship-available nil))

(defun iota-shell--starship-available-p ()
  "Return non-nil if starship is available."
  (unless iota-shell--starship-available
    (setq iota-shell--starship-available
          (if (and (executable-find "starship")
                   (file-exists-p iota-shell-starship-config))
              'yes 'no)))
  (eq iota-shell--starship-available 'yes))

(defun iota-shell--make-theme-aware-color-map ()
  "Create an ansi-color-map that uses named faces instead of static colors.
This allows the prompt to adapt immediately when the theme changes."
  (let ((map (ansi-color-make-color-map)))
    ;; Override standard foreground codes (30-37) to use faces
    (aset map 30 '(font-lock-face shadow))                  ; Black
    (aset map 31 '(font-lock-face error))                   ; Red
    (aset map 32 '(font-lock-face success))                 ; Green
    (aset map 33 '(font-lock-face warning))                 ; Yellow
    (aset map 34 '(font-lock-face font-lock-keyword-face))  ; Blue
    (aset map 35 '(font-lock-face font-lock-constant-face)) ; Magenta
    (aset map 36 '(font-lock-face font-lock-string-face))   ; Cyan
    (aset map 37 '(font-lock-face default))                 ; White
    map))

(defun iota-shell--starship-prompt ()
  "Generate prompt using starship.
Returns nil if starship fails, allowing fallback."
  (if (not (iota-shell--starship-available-p))
      nil
    (let* ((status (if (boundp 'eshell-last-command-status)
                       eshell-last-command-status 0))
           (width (window-width))
           (pwd (eshell/pwd)))
      (with-temp-buffer
        (let ((process-environment (copy-sequence process-environment))
              (default-directory pwd))
          (setenv "TERM" "xterm") ;; Force standard ANSI colors
          (setenv "INSIDE_EMACS" nil)
          (setenv "CLICOLOR_FORCE" "1")
          (setenv "STARSHIP_CONFIG" iota-shell-starship-config)
          (setenv "STARSHIP_SHELL" "sh")
          
          (if (zerop (call-process "starship" nil t nil
                                   "prompt"
                                   "--status" (number-to-string status)
                                   "--terminal-width" (number-to-string width)
                                   "--path" pwd))
              (let ((output (buffer-string)))
                (cond
                 ((string-empty-p output)
                  nil)
                 ((string-match-p "TERM=dumb" output)
                  nil)
                 (t
                  ;; Apply face-based color map for dynamic theming
                  (let ((ansi-color-map (iota-shell--make-theme-aware-color-map)))
                    (ansi-color-apply output)))))
            (message "Debug: Starship process failed with exit code %d" 1)
            nil))))))

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

(defun iota-shell--fallback-prompt ()
  "Generate fallback prompt when starship is unavailable."
  (message "Debug: Using fallback prompt")
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
         (nl (if (and (boundp 'eshell-last-output-end)
                      (= eshell-last-output-end (point-min)))
                 "" "\n")))
    (concat nl user (iota-shell--dim " at ") host (iota-shell--dim " in ") dir
            (or git "") (or lang "") " " char " ")))

;;;###autoload
(defun iota-shell-prompt ()
  "Generate the eshell prompt.
Uses starship if available, otherwise falls back to built-in prompt."
  (or (iota-shell--starship-prompt)
      (iota-shell--fallback-prompt)))

;;; Minor Mode

(defun iota-shell--setup ()
  "Setup iota-shell in the current eshell buffer."
  (setq-local eshell-prompt-function #'iota-shell-prompt)
  (setq-local eshell-highlight-prompt nil) ;; Disable default highlighting to let Starship colors show
  (setq-local eshell-prompt-regexp (concat "^.*" (regexp-quote iota-shell-prompt-char) " "))

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
        (set-default 'eshell-prompt-regexp (concat "^.*" (regexp-quote iota-shell-prompt-char) " "))

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
