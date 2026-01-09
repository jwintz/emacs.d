;;; init-terminal.el --- Eshell with starship prompt and eat -*- lexical-binding: t; -*-

;;; Code:

(use-package eshell
  :bind ("C-c s s" . eshell)
  :init
  (setq eshell-save-history-on-exit t)
  (setq eshell-hist-ignoredups t)
  ;; eshell-scroll-to-bottom-on-input 'all
  ;; eshell-error-if-no-glob t
  ;; eshell-prefer-lisp-functions nil
  ;; eshell-destroy-buffer-when-process-dies t
  (setq eshell-login-script (expand-file-name "conf/eshlogin" emacs-config-dir))
  (setq eshell-rc-script (expand-file-name "conf/eshrc" emacs-config-dir))
  :config
  (with-eval-after-load 'esh-mode
    (bind-key "C-c s k" #'iota-shell-clear eshell-mode-map))

  ;; Ensure em-term is loaded and add bat to visual commands
  (require 'em-term)

  (add-to-list 'eshell-visual-commands "bat")
  (add-to-list 'eshell-visual-commands "more")

  (use-package capf-autosuggest
    :hook (eshell-mode . capf-autosuggest-mode)
    :init
    (with-eval-after-load 'capf-autosuggest
      (defun hyalo-capf-autosuggest-history-capf ()
        "Wrapper for history capf that requires 1 char of input in Eshell."
        (let ((allow t))
          (when (derived-mode-p 'eshell-mode)
            (setq allow (and (boundp 'eshell-last-output-end)
                             eshell-last-output-end
                             (> (point) eshell-last-output-end))))
          (when allow
            (capf-autosuggest-history-capf))))

      (setq capf-autosuggest-capf-functions
            '(hyalo-capf-autosuggest-history-capf)))))

(use-package eat
  :ensure t
  :hook (eshell-load-hook . eat-eshell-mode)
  :config
  ;; Robustly ensure Eat integration is active in every Eshell buffer
  (defun init-terminal--setup-eat ()
    (eat-eshell-mode)
    (eat-eshell-visual-command-mode))
  (add-hook 'eshell-mode-hook #'init-terminal--setup-eat))

(use-package iota-shell
   :ensure nil
   :after eshell
   :hook (eshell-mode . iota-shell-mode))

(provide 'init-terminal)

;;; init-terminal.el ends here
