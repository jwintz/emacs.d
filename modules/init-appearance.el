;;; init-appearance.el --- Visual appearance: fonts, icons, themes -*- lexical-binding: t; -*-

;;; Code:

;;;; Fonts

(use-package faces
  :ensure nil
  :if (display-graphic-p)
  :config
  (set-face-attribute 'default nil
                      :font "Monaspace Neon Frozen"
                      :height 110
                      :weight 'thin)
  (set-face-attribute 'fixed-pitch nil
                      :font "Monaspace Neon Frozen"
                      :height 110
                      :weight 'thin)
  (set-face-attribute 'variable-pitch nil
                      :font "Monaspace Radon Frozen"
                      :height 110
                      :weight 'thin)
  (set-fontset-font t '(#xe000 . #xffdd)
                    (font-spec :name "Symbols Nerd Font Mono"
                               :size 11) nil))

;;;; Icons

(use-package nerd-icons
  :ensure t
  :demand t
  :config
  (require 'nerd-icons))

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
  (defun hyalo-switch-to-dark (&rest _)
    (when (fboundp 'hyalo-module-appearance-set)
      (hyalo-module-appearance-set 'dark)
      (hyalo-module-footer-set-pattern-alpha 0.02)))
  (advice-add 'modus-themes-load-random-dark :after #'hyalo-switch-to-dark)

  (defun hyalo-switch-to-light (&rest _)
    (when (fboundp 'hyalo-module-appearance-set)
      (hyalo-module-appearance-set 'light)
      (hyalo-module-footer-set-pattern-alpha 0.06)))
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

;;;; Minions

(use-package minions
  :ensure t
  :config
  (minions-mode 1))

(provide 'init-appearance)

;;; init-appearance.el ends here
