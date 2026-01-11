;;; init-modeline.el --- Modeline: doom-modeline, keycast -*- lexical-binding: t; -*-

;;; Code:

(defvar rcirc-track-minor-mode nil) ;; Fix doom-modeline error

(use-package which-func
  :ensure nil)
  ;; :config
  ;; (which-function-mode 1))

(use-package keycast
  :ensure t
  :custom
  ;; (keycast-prefix "⎈ ")
  ;; (keycast-separator " ⦿ ")
  ;; (keycast-command-format "%s")
  (keycast-modifiers-format '((control . "C-")
			      (meta . "M-")
			      (shift . "S-")
			      (super . "s-")))
  (keycast-special-keys-format '((return . "RET")
				 (escape . "ESC")
				 (space . "SPC")
				 (tab . "TAB")
				 (backspace . "BS")))
  ;; (keycast-show-mouse-buttons nil)
  :config
  ;; Keycast uses mode-line-misc-info, which doom-modeline displays via misc-info segment
  (defun hyalo-keycast-update-safe ()
    "Update keycast only if not in a minimap buffer."
    (condition-case err
        (unless (string-match-p "Minimap" (buffer-name))
          (keycast--update))
      (error
       ;; Silently ignore errors to prevent modeline spam
       nil)))

  (defun hyalo-keycast-window-predicate ()
    "Show keycast if window is selected OR if a minimap window is selected.
This prevents keycast from flickering/disappearing when demap updates."
    (let ((sel-win (selected-window)))
      (or (eq sel-win (get-buffer-window (current-buffer)))
          ;; Check if selected window is a minimap (demap) window
          (and (window-live-p sel-win)
               (string-match-p "Minimap" (buffer-name (window-buffer sel-win)))
               ;; Don't show keycast IN the minimap itself (though it usually lacks mode-line)
               (not (string-match-p "Minimap" (buffer-name)))))))

  (setq keycast-mode-line-window-predicate #'hyalo-keycast-window-predicate)

  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line."
    :global t
    (if keycast-mode
        (progn
          (add-hook 'pre-command-hook 'hyalo-keycast-update-safe t)
          ;; Remove from global-mode-string if present (default keycast behavior)
          (setq global-mode-string (remove 'keycast-mode-line global-mode-string))
          ;; Add to mode-line-misc-info safely
          (add-to-list 'mode-line-misc-info '("" keycast-mode-line " ")))
      (progn
        (remove-hook 'pre-command-hook 'hyalo-keycast-update-safe)
        (setq mode-line-misc-info (delete '("" keycast-mode-line " ") mode-line-misc-info))))))

  ;;(keycast-mode 1))

(use-package doom-modeline
  :ensure t
  :after nerd-icons
  :config
  (doom-modeline-mode 1)

  :custom
  (doom-modeline-height 22)
  (doom-modeline-bar-width 4)
  (doom-modeline-hud t)
  ;; Set reasonable limit - segments collapse when window narrower than this
  (doom-modeline-window-width-limit nil)
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
