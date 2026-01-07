;;; init-agents.el --- AI agents: copilot, agent-shell -*- lexical-binding: t; -*-

;;; Code:

(use-package copilot
  :ensure t
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main")
  :custom
  (copilot-idle-delay 0.2)
  (copilot-indent-offset-warning-disable t)
  (copilot-install-dir (locate-user-emacs-file "copilot/"))
  :hook ((prog-mode text-mode) . copilot-mode)
  :config
  (diminish 'copilot-mode (concat " " (nerd-icons-codicon "nf-cod-copilot")))

  (let ((server-bin (expand-file-name "bin" copilot-install-dir)))
    (unless (file-directory-p server-bin)
      (when (yes-or-no-p "Copilot language server not installed. Install now? ")
        (copilot-install-server))))

  (general-def copilot-completion-map
    "<tab>" 'copilot-accept-completion
    "TAB"   'copilot-accept-completion)

  (defun emacs/copilot-suppress-warnings (orig-fun &rest args)
    (let ((inhibit-message t))
      (apply orig-fun args)))

  (when (fboundp 'copilot--start-agent)
    (advice-add 'copilot--start-agent :around #'emacs/copilot-suppress-warnings))
  (when (fboundp 'copilot--start-server)
    (advice-add 'copilot--start-server :around #'emacs/copilot-suppress-warnings))

  (defun emacs/copilot-redirect-stderr (orig-fun &rest args)
    (let ((stderr-file (locate-user-emacs-file "copilot-stderr")))
      (if (string= (plist-get args :name) "copilot server")
          (apply orig-fun (plist-put args :stderr stderr-file))
        (apply orig-fun args))))

  (advice-add 'make-process :around #'emacs/copilot-redirect-stderr)

  ;; Suppress "Copilot server started" message
  (defun emacs/copilot-suppress-log (orig-fun type msg &rest args)
    (unless (and (eq type 'info) (string-prefix-p "Copilot server started" msg))
      (apply orig-fun type msg args)))

  (advice-add 'copilot--log :around #'emacs/copilot-suppress-log))

(defvar agent-shell-home-dir (expand-file-name ".agent-shell" "~")
  "Central directory for all agent-shell data.")

(use-package agent-shell
  :ensure t
  :vc (:url "https://github.com/xenodium/agent-shell"
            :rev :newest)
  :custom
  (agent-shell-section-functions nil)
  :hook (agent-shell-mode . agent-shell-completion-mode)
  :general
  (leader-def
    "a" '(:ignore t :wk "agents")
    "a c" '(agent-shell-anthropic-start-claude-code :wk "claude")
    "a g" '(agent-shell-google-start-gemini :wk "gemini")
    "a s" '(agent-shell-sidebar-toggle :wk "sidebar toggle")
    "a S" '(agent-shell-send-screenshot :wk "screenshot")
    "a q" '(agent-shell-queue-request :wk "queue request")
    "a f" '(agent-shell-sidebar-toggle-focus :wk "sidebar focus"))
  (:keymaps 'agent-shell-mode-map
   "C-p" 'agent-shell-previous-input
   "C-n" 'agent-shell-next-input)
  :config
  ;; Use central HOME directory for transcripts
  (defun agent-shell--home-transcript-file-path ()
    "Generate transcript path in ~/.agent-shell/transcripts/PROJECT/."
    (let* ((project-name (or (when-let* ((proj (project-current)))
                               (file-name-nondirectory
                                (directory-file-name (project-root proj))))
                             "default"))
           (dir (expand-file-name (concat "transcripts/" project-name) agent-shell-home-dir))
           (filename (format-time-string "%F-%H-%M-%S.md")))
      (expand-file-name filename dir)))

  (setq agent-shell-transcript-file-path-function #'agent-shell--home-transcript-file-path)

  ;; Override screenshot directory to use central HOME location
  (defun agent-shell--home-screenshots-dir (orig-fun &rest args)
    "Redirect screenshots to ~/.agent-shell/screenshots/."
    (let ((screenshots-dir (expand-file-name "screenshots" agent-shell-home-dir)))
      (make-directory screenshots-dir t)
      (cl-letf (((symbol-function 'agent-shell-cwd)
                 (lambda () agent-shell-home-dir)))
        (apply orig-fun args))))

  (advice-add 'agent-shell-send-screenshot :around #'agent-shell--home-screenshots-dir))

(use-package hyalo-agent-extras
  :ensure nil

  :after agent-shell
  :config
  (hyalo-agent-extras-mode 1))

(use-package agent-shell-sidebar
  :ensure t
  :vc (:url "https://github.com/cmacrae/agent-shell-sidebar"
            :rev :newest)
  :after agent-shell)

(use-package agent-shell-manager
  :ensure t
  :vc (:url "https://github.com/jethrokuan/agent-shell-manager"
            :rev :newest)
  :after agent-shell
  :general
  (leader-def
    "a m" '(agent-shell-manager-toggle :wk "manager")))

(provide 'init-agents)

;;; init-agents.el ends here
