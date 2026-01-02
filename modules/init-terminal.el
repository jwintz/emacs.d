;;; init-terminal.el --- Terminal emulation: eat -*- lexical-binding: t; -*-

;;; Code:

(use-package eat
  :ensure t
  :general
  (:prefix "C-c s"
   "" '(:ignore t :which-key "shell")
   "s" 'eat
   "p" 'eat-project
   "e" 'eat-eshell-mode
   "v" 'eat-eshell-visual-command-mode)
  :custom
  (eat-enable-directory-tracking t)
  (eat-enable-shell-prompt-annotation t)
  (eat-term-name "xterm-256color")
  (eat-kill-process-on-exit t)
  :config
  (add-hook 'eat-mode-hook
            (lambda ()
              (when (fboundp 'god-local-mode)
                (god-local-mode -1))))

  (with-eval-after-load 'eat
    (when (boundp 'eat-semi-char-mode-map)
      (define-key eat-semi-char-mode-map (kbd "DEL") 'eat-self-input)
      (define-key eat-semi-char-mode-map (kbd "<backspace>") 'eat-self-input)
      (define-key eat-semi-char-mode-map (kbd "C-?") 'eat-self-input)
      (define-key eat-semi-char-mode-map (kbd "C-c C-k") 'eat-char-mode))

    (when (boundp 'eat-mode-map)
      (define-key eat-mode-map (kbd "C-c C-j") 'eat-semi-char-mode)))

  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode))

(use-package iota-shell
  :ensure nil
  :after eshell
  :hook (eshell-mode . iota-shell-mode)
  :config
  ;; Set global defaults to ensure initial prompt is correct
  (setq eshell-prompt-function #'iota-shell-prompt)
  (setq eshell-prompt-regexp "^λ "))

(provide 'init-terminal)

;;; init-terminal.el ends here
