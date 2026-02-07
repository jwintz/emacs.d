;;; init-terminal.el --- Eat terminal configuration -*- lexical-binding: t; -*-

;;; Code:

(use-package eat
  :ensure t
  :vc (:url "https://codeberg.org/akib/emacs-eat" :rev :newest)
  :bind (("C-c s s" . eat)
         :map eat-mode-map
         ("C-z" . eat-semi-char-mode)) ; Quick toggle to Emacs-friendly mode
  :config
  (setq eat-kill-buffer-on-exit t)
  (setq eat-enable-mouse t)
  
  ;; Disable annotations and synchronization features that cause "strange characters"
  ;; when the shell/prompt is complex (like starship).
  (setq eat-enable-shell-prompt-annotation nil)
  (setq eat-enable-synchronized-output nil)
  (setq eat-term-name "xterm-256color")
  
  ;; Use char-mode by default for "intuitive" (direct) terminal use
  (setq eat-terminal-mode 'char)
  
  (add-hook 'eat-mode-hook
            (lambda ()
              (setq-local scroll-margin 0))))

(provide 'init-terminal)

;;; init-terminal.el ends here
