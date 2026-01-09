;;; init-terminal.el --- Eshell with starship prompt and eat -*- lexical-binding: t; -*-

;;; Code:

(use-package eshell
  :bind ("C-c s s" . eshell)
  :init
  ;; eshell-error-if-no-glob t
  ;; eshell-prefer-lisp-functions nil
  (setq eshell-save-history-on-exit t)
  (setq eshell-hist-ignoredups t)
  (setq eshell-scroll-to-bottom-on-input 'all)
  (setq eshell-destroy-buffer-when-process-dies t)
  (setq eshell-login-script (expand-file-name "conf/eshlogin" emacs-config-dir))
  (setq eshell-rc-script (expand-file-name "conf/eshrc" emacs-config-dir))
  (setq eshell-aliases-file (expand-file-name "conf/eshaliases" emacs-config-dir))
  :config
  (with-eval-after-load 'esh-mode
    (bind-key "C-c s k" #'iota-shell-clear eshell-mode-map))

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

(use-package iota-shell
   :ensure nil
   :after eshell
   :hook (eshell-mode . iota-shell-mode))

(provide 'init-terminal)

;;; init-terminal.el ends here
