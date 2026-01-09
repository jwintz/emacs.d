;;; init-terminal.el --- Eshell with starship prompt and eat -*- lexical-binding: t; -*-

;;; Code:

(use-package eshell
  :init
  (setq eshell-scroll-to-bottom-on-input 'all
        eshell-error-if-no-glob t
        eshell-hist-ignoredups t
        eshell-save-history-on-exit t
        eshell-prefer-lisp-functions nil
        eshell-destroy-buffer-when-process-dies t
        eshell-login-script (expand-file-name "config/eshlogin" emacs-config-dir)
        eshell-rc-script (expand-file-name "config/eshrc" emacs-config-dir))
  :config
  (use-package capf-autosuggest
    :hook (eshell-mode . capf-autosuggest-mode))
  (use-package esh-help
    :config
    (setup-esh-help-eldoc))
  (use-package eshell-outline))

(use-package eat
  :ensure t
  :config
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode))

(use-package iota-shell
  :ensure nil
  :after eshell
  :hook (eshell-mode . iota-shell-mode))

(provide 'init-terminal)

;;; init-terminal.el ends here
