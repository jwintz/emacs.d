;;; init-modes.el --- Major language modes -*- lexical-binding: t; -*-

;;; Code:

;; TOML mode
(use-package toml-mode)

;; Eshell configuration files
(add-to-list 'auto-mode-alist '("/conf/eshlogin\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("/conf/eshrc\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("/conf/eshaliases\'" . eshell-mode))

(provide 'init-modes)

;;; init-modes.el ends here