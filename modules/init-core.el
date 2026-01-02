;;; init-core.el --- Core packages: general, which-key, diminish -*- lexical-binding: t; -*-

;;; Code:

(use-package diminish
  :ensure t
  :demand t)

(use-package elog
  :ensure t
  :vc (:url "https://github.com/Kinneyzhang/elog" :rev :newest)
  :demand t
  :config
  ;; Initialize main Emacs logger
  (defvar emacs-logger
    (elog-logger
     :name "emacs"
     :level 'info
     :buffer "*elog*"
     :handlers '(buffer))))

(use-package general
  :ensure t
  :demand t
  :config
  (general-create-definer leader-def :prefix "C-c")
  (leader-def
    "b" '(:ignore t :wk "buffer")
    "f" '(:ignore t :wk "file")
    "h" '(:ignore t :wk "help")
    "l" '(:ignore t :wk "hyalo")
    "n" '(:ignore t :wk "notes")
    "p" '(:ignore t :wk "project")
    "t" '(:ignore t :wk "toggle")
    "v" '(:ignore t :wk "versionning")))

(use-package which-key
  :ensure t
  :diminish
  :custom
  (which-key-idle-delay 0.3)
  (which-key-separator " -> ")
  (which-key-prefix-prefix "")
  (which-key-inhibit-regexps '("^ESC"))
  :config
  (which-key-mode 1))

;;;; Quiet Custom Save

(defun hyalo-inhibit-custom-save-message (orig-fun &rest args)
  "Inhibit messages when saving customizations."
  (let ((inhibit-message t))
    (apply orig-fun args)))

(advice-add 'custom-save-all :around #'hyalo-inhibit-custom-save-message)

(provide 'init-core)

;;; init-core.el ends here
