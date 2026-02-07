;;; init-agents.el --- AI agents: copilot -*- lexical-binding: t; -*-

;;; Code:

(use-package copilot
  :ensure t
  :commands (copilot-mode)
  :init
  (autoload 'copilot-mode "copilot" "Copilot" t)
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main")
  :custom
  (copilot-idle-delay 0.2)
  (copilot-indent-offset-warning-disable t)
  (copilot-install-dir (locate-user-emacs-file "copilot/"))
  ;;:hook ((prog-mode text-mode) . copilot-mode)
  :config
  (diminish 'copilot-mode (concat " " (nerd-icons-codicon "nf-cod-copilot")))

  (general-def copilot-completion-map
    "<tab>" 'copilot-accept-completion
    "TAB"   'copilot-accept-completion)

  (defun emacs/copilot-suppress-warnings (orig-fun &rest args)
    (let ((inhibit-message t))
      (apply orig-fun args)))

  (advice-add 'copilot--start-agent :around #'emacs/copilot-suppress-warnings)
  (advice-add 'copilot--start-server :around #'emacs/copilot-suppress-warnings)

  (defun emacs/copilot-redirect-stderr (orig-fun &rest args)
    (let ((stderr-file (locate-user-emacs-file "copilot-stderr")))
      (if (string= (plist-get args :name) "copilot server")
          (apply orig-fun (plist-put args :stderr stderr-file))
        (apply orig-fun args))))

  (advice-add 'make-process :around #'emacs/copilot-redirect-stderr)

  ;; Suppress "Copilot server started" message
  (defun emacs/copilot-suppress-log (orig-fun type msg &rest args)
    (unless (string-match-p "Copilot server started" msg)
      (apply orig-fun type msg args)))

  (advice-add 'copilot--log :around #'emacs/copilot-suppress-log))

(provide 'init-agents)

;;; init-agents.el ends here
