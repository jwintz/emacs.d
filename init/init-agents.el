;;; init-agents.el --- AI agents: copilot, pi-coding-agent -*- lexical-binding: t; -*-

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

;;;; Pi Coding Agent

(use-package pi-coding-agent
  :ensure t
  :defer t
  :vc (:url "https://github.com/dnouri/pi-coding-agent"
            :rev :newest)
  :commands (pi-coding-agent pi-coding-agent-menu)
  :general
  (leader-def
    "a" '(:ignore t :wk "agents")
    "a p" '(pi-coding-agent :wk "pi")
    "a m" '(pi-coding-agent-menu :wk "menu")
    "a s" '(hyalo-sidebar-toggle-right :wk "sidebar toggle")
    "a f" '(hyalo-sidebar-focus-right :wk "sidebar focus"))
  :hook
  (pi-coding-agent-chat-mode . (lambda ()
                                 (face-remap-add-relative 'default :family "Monaspace Krypton")))
  :init
  ;; Set up SVG backgrounds for tool blocks (Hyalo transparency compatibility)
  (with-eval-after-load 'hyalo
    (require 'hyalo-agent)
    (hyalo-agent-setup))
  :config
  ;; Use a smaller input window for embedded sidebar
  (setq pi-coding-agent-input-window-height 6)

  ;; Fix: upstream bug - :content is a vector, not a string
  ;; Use :streaming-content for text delta accumulation
  (defun pi-coding-agent--handle-message-update-fixed (event)
    "Handle a message_update EVENT by accumulating text deltas.
Fixed version that uses :streaming-content instead of :content."
    (let* ((msg-event (plist-get event :assistantMessageEvent))
           (event-type (plist-get msg-event :type))
           (current (plist-get pi-coding-agent--state :current-message)))
      (when (and current (equal event-type "text_delta"))
        (let* ((delta (plist-get msg-event :delta))
               (content (or (plist-get current :streaming-content) ""))
               (new-content (concat content delta)))
          (plist-put current :streaming-content new-content)))))

  (advice-add 'pi-coding-agent--handle-message-update
              :override #'pi-coding-agent--handle-message-update-fixed))

(provide 'init-agents)

;;; init-agents.el ends here
