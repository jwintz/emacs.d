;;; init-appearance.el --- Visual appearance: fonts, icons, themes -*- lexical-binding: t; -*-

;;; Code:

;;;; Fonts

(use-package fontaine
  :ensure t
  :demand t
  :config
  (require 'hyalo-fonts))

;;;; Icons

(use-package nerd-icons
  :ensure t
  :demand t
  :config
  (require 'nerd-icons)
  ;; Use FontAwesome icons for default directories and files
  (setf (cdr (assoc ".?" nerd-icons-dir-icon-alist))
        '(nerd-icons-faicon "nf-fa-folder"))
  (setq nerd-icons-default-file-icon
        '(nerd-icons-faicon "nf-fa-file")))

(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package hyalo-icons
  :ensure nil
  :after nerd-icons
  :custom
  (hyalo-icons-monochrome t))

;;;; Highlighting

(use-package hl-line
  :ensure nil
  :demand t
  :config
  (global-hl-line-mode 1))

;;;; Ligatures

(use-package ligature
  :ensure t
  :config
  ;; Monaspace ligatures - comprehensive set for programming
  ;; Reference: https://github.com/githubnext/monaspace#ligatures
  (ligature-set-ligatures 'prog-mode
                          '(;; Arrows
                            "->" "-->" "--->" "-<" "-<<" "-~"
                            "<-" "<--" "<---" "<->" "<-->" "<-<"
                            "=>" "==>" "===>" "=>>" "=<<"
                            ">-" ">>" ">>>" ">>-" ">>="
                            "<=" "<==" "<===" "<=>" "<==>" "<=<"
                            ;; Comparisons
                            "==" "!=" "===" "!==" "=/="
                            "<=" ">=" "<>" "<=>"
                            ;; Logic and math
                            "&&" "||" "??" "?:" "?."
                            "++" "--" "**" "***"
                            "//" "///" "/*" "*/" "/="
                            ;; Pipes and composition
                            "|>" "<|" "<|>" "||>" "|>>"
                            "<:" ":>" "::" ":::" "::::"
                            ;; Brackets and tags
                            "</" "</>" "/>" "<>"
                            "<!--" "-->"
                            ;; Other common ligatures
                            "..." ".." ".=" "..<"
                            "!!" "##" "###" "####"
                            "#{" "#[" "#(" "#?" "#_" "#_("
                            "~@" "~=" "~>" "~~>" "~-"
                            ;; Assignment and binding
                            ":=" "::=" "<->" "<<-" "->>"
                            ;; Haskell/functional
                            ">>=" "=<<" ">>" "<<"
                            "<*>" "<$>" "<+>"
                            ;; F#/ML
                            "|]" "[|" "||]" "[||"))
  (global-ligature-mode t))

;;;; Themes

(use-package modus-themes
  :ensure t
  :demand t
  :custom
  (modus-themes-mixed-fonts t)
  (modus-themes-variable-pitch-ui t)
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs nil)
  (modus-themes-completions '((t . (semibold))))
  (modus-themes-prompts '(semibold))
  :config
  (defun hyalo/current-theme ()
    "Display the currently enabled themes in the echo area."
    (interactive)
    (if custom-enabled-themes
	(message "Current theme(s): %s"
		 (mapconcat #'symbol-name custom-enabled-themes ", "))
      (message "No custom themes are currently enabled.")))

  (defun hyalo-switch-to-dark (&rest _)
    (when (fboundp 'hyalo-appearance-set)
      (hyalo-appearance-set 'dark)
      (hyalo-footer-set-pattern-alpha 0.02)))

  (advice-add 'modus-themes-select-dark :after #'hyalo-switch-to-dark)
  (advice-add 'modus-themes-load-random-dark :after #'hyalo-switch-to-dark)

  (defun hyalo-switch-to-light (&rest _)
    (when (fboundp 'hyalo-appearance-set)
      (hyalo-appearance-set 'light)
      (hyalo-footer-set-pattern-alpha 0.06)))

  (advice-add 'modus-themes-select-light :after #'hyalo-switch-to-light)
  (advice-add 'modus-themes-load-random-light :after #'hyalo-switch-to-light))

(use-package ef-themes
  :ensure t
  :demand t
  :after modus-themes
  :config
  (modus-themes-include-derivatives-mode 1))

;;;; Mixed Pitch

(use-package mixed-pitch
  :ensure t
  :vc (:url "https://gitlab.com/jabranham/mixed-pitch" :rev :newest)
  :hook ((text-mode Info-mode) . (lambda ()
                                   (when (display-graphic-p)
                                     (mixed-pitch-mode 1)))))

;;;; Dimmer

(use-package iota-dimmer
  :ensure nil
  :custom
  (iota-dimmer-saturation-fraction 0.90)
  (iota-dimmer-luminance-fraction 0.30)
  :config
  (iota-dimmer-mode 1))

;;; Minimap

(use-package hide-mode-line
  :ensure t)

(use-package demap
  :ensure t
  :defer t
  :custom
  (demap-minimap-window-side 'right)
  (demap-minimap-window-width 20)
  :config
  (require 'hyalo-minimap)
  ;; Override default to include Info-mode
  (setq demap-track-window-mode-update-p-func
        (lambda (&rest _) t))
  (add-hook 'demap-minimap-construct-hook #'hyalo-minimap-setup)
  (add-hook 'demap-minimap-window-set-hook #'hyalo-minimap-setup)

  ;; Debounced demap update for smooth text-scale changes
  (defvar iota/demap-update-timer nil
    "Timer for debounced demap updates.")

  (defun iota/demap-update-debounced ()
    "Schedule a debounced demap minimap update."
    (when (and (bound-and-true-p demap-minimap-window)
               (window-live-p demap-minimap-window))
      ;; Cancel any pending update
      (when (timerp iota/demap-update-timer)
        (cancel-timer iota/demap-update-timer))
      ;; Schedule new update after user stops changing scale
      (setq iota/demap-update-timer
            (run-with-idle-timer 0.3 nil
                                 (lambda ()
                                   (when (and (bound-and-true-p demap-minimap-window)
                                              (window-live-p demap-minimap-window))
                                     (demap-minimap-update)))))))

  ;; Use text-scale-mode-hook which fires on each scale change (including CMD-+/-)
  (add-hook 'text-scale-mode-hook #'iota/demap-update-debounced))

(use-package olivetti
  :ensure t)

(provide 'init-appearance)

;;; init-appearance.el ends here
