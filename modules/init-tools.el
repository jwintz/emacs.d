;;; init-tools.el --- Development tools: project, magit, diff-hl, eglot -*- lexical-binding: t; -*-

;;; Code:

(use-package project
  :ensure nil
  :general
  (leader-def
    "p f" '(project-find-file :wk "find file")
    "p b" '(project-switch-to-buffer :wk "switch buffer")
    "p d" '(project-dired :wk "dired")
    "p k" '(project-kill-buffers :wk "kill buffers")
    "p p" '(project-switch-project :wk "switch project")
    "p c" '(project-compile :wk "compile")
    "p v" '(magit-project-status :wk "magit")
    "p s" '(consult-ripgrep :wk "search"))
  :custom
  (project-switch-commands
   '((project-find-file "Find file")
     (consult-ripgrep "Search" ?s)
     (project-dired "Dired")
     (magit-project-status "Magit" ?v)
     (project-eshell "Eshell"))))

(use-package magit
  :ensure t
  :defer t
  :general
  (leader-def
    "v s" '(magit-status :wk "status")
    "v l" '(magit-log :wk "log")
    "v b" '(magit-blame :wk "blame")
    "v d" '(magit-diff :wk "diff"))
  :custom
  (magit-display-buffer-function
   (lambda (buffer) (display-buffer buffer '(display-buffer-same-window))))
  :config
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t))

(use-package diff-hl
  :ensure t
  :custom
  (diff-hl-side 'right)
  (diff-hl-margin-symbols-alist '((insert . "┃") (delete . "┃") (change . "┃")))
  :config
  (defun hyalo-diff-hl--update-faces (&rest _)
    "Update diff-hl margin colors from theme."
    (let ((insert-fg (or (face-foreground 'success nil t)
                         (face-foreground 'diff-added nil t)
                         "#50a14f"))
          (delete-fg (or (face-foreground 'error nil t)
                         (face-foreground 'diff-removed nil t)
                         "#e45649"))
          (change-fg (or (face-foreground 'warning nil t)
                         (face-foreground 'diff-changed nil t)
                         "#c18401")))
      (set-face-attribute 'diff-hl-insert nil :foreground insert-fg :background 'unspecified)
      (set-face-attribute 'diff-hl-delete nil :foreground delete-fg :background 'unspecified)
      (set-face-attribute 'diff-hl-change nil :foreground change-fg :background 'unspecified)))
  (add-hook 'enable-theme-functions #'hyalo-diff-hl--update-faces)
  (hyalo-diff-hl--update-faces)
  (global-diff-hl-mode 1)
  (diff-hl-margin-mode 1)
  (diff-hl-flydiff-mode 1)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package swift-mode
  :ensure t
  :mode "\\.swift\\'")

(use-package eglot
  :ensure nil
  :hook (swift-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '(swift-mode . ("xcrun" "sourcekit-lsp")))
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0))

(provide 'init-tools)

;;; init-tools.el ends here
