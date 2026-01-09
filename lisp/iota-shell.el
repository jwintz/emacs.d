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
  (expand-file-name "conf/eshprompt" emacs-config-dir)
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

(defvar-local iota-shell--right-prompt-overlay nil
  "Overlay for displaying the right-side prompt.")

(defvar-local iota-shell--pending-right-prompt nil
  "Cached content of the right-side prompt for the current input cycle.")

(defun iota-shell--starship-get-right-prompt (status width pwd)
  "Fetch the right prompt from starship."
  (with-temp-buffer
    (let ((process-environment (copy-sequence process-environment))
          (default-directory pwd))
      (setenv "TERM" "xterm")
      (setenv "INSIDE_EMACS" nil)
      (setenv "CLICOLOR_FORCE" "1")
      (setenv "STARSHIP_CONFIG" iota-shell-starship-config)
      (setenv "STARSHIP_SHELL" "sh")

      (if (zerop (call-process "starship" nil t nil
                               "prompt" "--right"
                               "--status" (number-to-string status)
                               "--terminal-width" (number-to-string width)
                               "--path" pwd))
          (let ((output (string-trim (buffer-string))))
            (if (string-empty-p output)
                nil
              (let ((ansi-color-map (iota-shell--make-theme-aware-color-map))
                    (ansi-color-context nil)) ;; Isolate from previous state
                (ansi-color-apply output))))
        nil))))



(defun iota-shell--update-right-prompt-from-property (&optional force)
  "Update right prompt using text property from the current prompt.
If FORCE is non-nil, update even if `this-command` is `eshell-send-input`."
  (when (and (or force (not (eq this-command 'eshell-send-input)))
             (eq major-mode 'eshell-mode)
             (boundp 'eshell-last-output-end)
             eshell-last-output-end
             (> eshell-last-output-end (point-min)))
    (let ((right-prompt (get-text-property (1- eshell-last-output-end) 'iota-right-prompt)))
      (when right-prompt
        (let* ((str right-prompt)
               (str-width (string-width str))
               (win-width (window-width))
               (current-col (save-excursion (goto-char eshell-last-output-end) (current-column))))

          ;; Create overlay if needed
          (unless (and iota-shell--right-prompt-overlay
                       (overlayp iota-shell--right-prompt-overlay))
            (setq iota-shell--right-prompt-overlay (make-overlay (point) (point)))
            (overlay-put iota-shell--right-prompt-overlay 'priority -1)
            (overlay-put iota-shell--right-prompt-overlay 'face 'default))

          ;; Position at end of line
          (move-overlay iota-shell--right-prompt-overlay
                        (line-end-position)
                        (line-end-position))

          ;; Update content and handle overlap
          ;; Use 4 chars margin to avoid wrapping issues
          (if (> (+ current-col str-width 4) win-width)
              (overlay-put iota-shell--right-prompt-overlay 'after-string nil)
            (let ((padding (propertize " "
                                       'display `(space :align-to (- right ,str-width))
                                       'face 'default)))
              (overlay-put iota-shell--right-prompt-overlay 'after-string (concat padding str)))))))))

(defun iota-shell--force-update-right-prompt ()
  "Force update of the right prompt."
  (iota-shell--update-right-prompt-from-property t))


(defun iota-shell--cleanup-overlay (&rest _)

  "Remove the right prompt overlay."

  (when iota-shell--right-prompt-overlay

    (delete-overlay iota-shell--right-prompt-overlay)

    (setq iota-shell--right-prompt-overlay nil)))



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
          (setenv "TERM" "xterm")
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
                 ((string-empty-p output) nil)
                 ((string-match-p "TERM=dumb" output) nil)
                 (t
                  (let ((right-prompt (iota-shell--starship-get-right-prompt status width pwd))
                        (ansi-color-map (iota-shell--make-theme-aware-color-map))
                        (ansi-color-context nil))

                    (let ((colored (ansi-color-apply output)))
                      ;; Attach properties: Read-only, field, AND right-prompt content
                      (add-text-properties 0 (length colored)
                                           `(read-only t
                                             field prompt
                                             front-sticky (read-only field)
                                             rear-nonsticky (read-only field)
                                             iota-right-prompt ,right-prompt)
                                           colored)
                      colored)))))
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


;;; Variables

(defvar iota-shell-prompt-char "λ"
  "Character used for the input prompt.")

;;;###autoload
(defun iota-shell-clear ()
  "Clear the eshell buffer and emit a new prompt."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-emit-prompt)))

;;;###autoload
(defun iota-shell-prompt ()
  "Generate the eshell prompt.
Uses starship if available, otherwise falls back to default prompt."
  (or (iota-shell--starship-prompt)
      (funcall 'eshell-default-prompt-function)))

;;; Minor Mode

(defun iota-shell--setup ()
  "Setup iota-shell in the current eshell buffer."
  (setq-local eshell-prompt-function #'iota-shell-prompt)
  (setq-local eshell-highlight-prompt nil) ;; Disable default highlighting to let Starship colors show
  (setq-local eshell-prompt-regexp (concat "^.*" (regexp-quote iota-shell-prompt-char) " "))

  ;; Add persistent right-prompt updater
  (add-hook 'post-command-hook #'iota-shell--update-right-prompt-from-property nil t)
  (add-hook 'window-configuration-change-hook #'iota-shell--update-right-prompt-from-property nil t)
  (add-hook 'eshell-input-filter-functions #'iota-shell--cleanup-overlay nil t)
  (add-hook 'eshell-after-prompt-hook #'iota-shell--force-update-right-prompt nil t)

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
