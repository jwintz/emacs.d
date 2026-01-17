;;; init.example.el --- Hyalo Minial Configuration -*- lexical-binding: t; -*-

(defvar emacs-config-dir
  (file-name-directory (or load-file-name buffer-file-name)))

(add-to-list 'load-path (expand-file-name "init" emacs-config-dir))

(use-package init-bootstrap
  :ensure nil
  :demand t)

(use-package init-core
  :ensure nil
  :demand t)

(use-package init-emacs
  :ensure nil
  :demand t)

(use-package init-hyalo
  :ensure nil
  :demand t)

;;; init.example.el ends here
