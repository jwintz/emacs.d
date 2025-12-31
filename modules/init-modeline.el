;;; init-modeline.el --- Modeline: doom-modeline, keycast -*- lexical-binding: t; -*-

;;; Code:

(defvar rcirc-track-minor-mode nil) ;; Fix doom-modeline error

(use-package keycast
  :ensure t
  :config
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line."
    :global t
    (if keycast-mode
        (progn
          (add-hook 'pre-command-hook 'keycast--update t)
          (add-to-list 'global-mode-string '("" keycast-mode-line " ")))
      (progn
        (remove-hook 'pre-command-hook 'keycast--update)
        (setq global-mode-string (delete '("" keycast-mode-line " ") global-mode-string))))))

(use-package doom-modeline
  :ensure t
  :after nerd-icons
  :config
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 22)
  (doom-modeline-bar-width 4)
  (doom-modeline-hud t)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-time-icon t)
  (doom-modeline-buffer-encoding-icon t)
  (doom-modeline-modal t)
  (doom-modeline-modal-icon t)
  (doom-modeline-modal-modern-icon t)
  (doom-modeline-buffer-file-name-style 'file-name-with-project)
  (doom-modeline-project-detection 'project)
  (doom-modeline-vcs-max-length 24)
  (doom-modeline-check-simple-format t)
  (doom-modeline-env-version nil)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-indent-info nil)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count nil)
  (doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))
  (doom-modeline-unicode-fallback t)
  (doom-modeline-irc nil)
  (doom-modeline-checker-simple-format t))

(provide 'init-modeline)

;;; init-modeline.el ends here
