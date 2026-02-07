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

;; Use Emacs' built-in ls emulation for portable directory-first sorting
(use-package ls-lisp
  :ensure nil
  :custom
  (ls-lisp-use-insert-directory-program nil)  ; Use ls-lisp instead of external ls
  (ls-lisp-dirs-first t)                      ; Folders on top
  (ls-lisp-use-string-collate nil))

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
  :config
  (hyalo-sidebar-mode 1)
  :general
  (leader-def
    "t e" '(hyalo-sidebar-toggle-left :wk "sidebar (left)")
    "t i" '(hyalo-sidebar-toggle-right :wk "inspector (right)")
    "t I" '(hyalo-sidebar-focus-right :wk "focus inspector")
    "t S" '(hyalo-sidebar-focus-left :wk "focus sidebar")
    "t E" '(hyalo-sidebar-focus-center :wk "focus main")
    "t m" '(demap-toggle :wk "minimap")))

(provide 'init-dired)

;;; init-dired.el ends here
