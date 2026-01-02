;;; init-dired.el --- Dired and file browsing -*- lexical-binding: t; -*-

;;; Code:

(use-package dired
  :ensure nil
  :custom
  (dired-use-ls-dired nil)
  ;; -A hides . and .., -g omits owner, -h human readable, -o omits group
  (dired-listing-switches "-Agho")
  (dired-auto-revert-buffer t)
  (delete-by-moving-to-trash t))

(use-package dired-sidebar
  :ensure t
  :defer t
  :commands (dired-sidebar-toggle-sidebar)
  :custom
  (dired-sidebar-theme 'nerd-icons)
  (dired-sidebar-width 35)
  (dired-sidebar-should-follow-file t)
  :config
  (add-hook 'dired-sidebar-mode-hook #'nerd-icons-dired-mode))

(use-package hyalo-sidebar
  :ensure nil
  :after hyalo
  :demand t
  :custom
  (hyalo-sidebar-font nil)
  (hyalo-sidebar-internal-border-width 0)
  :config
  (hyalo-sidebar-mode 1)
  :general
  (leader-def
    "t e" '(hyalo-sidebar-toggle-left :wk "sidebar (left)")
    "t i" '(hyalo-sidebar-toggle-right :wk "inspector (right)")
    "t E" '(hyalo-sidebar-focus-left :wk "focus sidebar")
    "t I" '(hyalo-sidebar-focus-right :wk "focus inspector")
    "t m" '(demap-toggle :wk "minimap")))

(provide 'init-dired)

;;; init-dired.el ends here
