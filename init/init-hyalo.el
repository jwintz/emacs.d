;;; init-hyalo.el --- Hyalo: macOS Liquid Glass integration -*- lexical-binding: t; -*-

;;; Code:

;;;; macOS Settings

(use-package ns-win
  :ensure nil
  :if (and (eq system-type 'darwin) (display-graphic-p))
  :config
  (setq frame-resize-pixelwise t)
  (setq frame-inhibit-implied-resize t)
  (set-frame-parameter nil 'internal-border-width 0)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (set-frame-parameter nil 'ns-transparent-titlebar t)
  (general-unbind "C-z" "C-x C-z"))

;;;; Better scrolling

(use-package ultra-scroll
  :ensure t
  :vc (:url "https://github.com/jdtsmith/ultra-scroll")
  :config
  (ultra-scroll-mode 1))

;;;; Hyalo Core Module

(use-package hyalo
  :ensure nil
  :if (eq window-system 'ns)
  :custom
  (hyalo-auto-build t)
  :config
  (hyalo-load)
  ;; Initialize chrome visibility based on splash state
  (require 'hyalo-splash)
  (hyalo-splash-init-chrome))

;;;; Hyalo Header

(use-package hyalo-header
  :ensure nil
  :if (eq window-system 'ns)
  :after hyalo
  :config
  (hyalo-header-mode 1))

;;;; Hyalo Appearance

(use-package hyalo-appearance
  :ensure nil
  :if (eq window-system 'ns)
  :after hyalo
  :demand t
  :custom
  (hyalo-appearance-mode-setting 'auto)
  (hyalo-appearance-vibrancy-material "ultraThin")
  (hyalo-appearance-opacity 0.6)
  :general
  (leader-def
    "l v" '(hyalo-appearance-set-vibrancy :wk "vibrancy")
    "l o" '(hyalo-appearance-set-opacity :wk "opacity")
    "l p" '((lambda () (interactive)
              (call-interactively 'hyalo-appearance-set)
              (customize-save-variable 'hyalo-appearance-mode-setting
                                       hyalo-appearance-mode-setting)
              (message "Appearance mode saved: %s" hyalo-appearance-mode-setting))
            :wk "appearance mode")
    "l P" '(hyalo-appearance-show-panel :wk "panel"))
  :config
  (hyalo-appearance-mode 1))

;;;; Hyalo Viewport/Scroll

(use-package hyalo-viewport
  :ensure nil
  :if (eq window-system 'ns)
  :after hyalo-header
  :custom
  (hyalo-viewport-debug nil)
  (hyalo-viewport-excluded-modes nil)
  :config
  (hyalo-viewport-mode 1))

;;;; Hyalo System

(use-package hyalo-system
  :ensure nil
  :if (eq window-system 'ns)
  :after hyalo
  :commands (fork-emacs)
  :general
  (leader-def
    "l r" '(hyalo-reveal-in-finder :wk "reveal")
    "l s" '(hyalo-share :wk "share")
    "l e" '(hyalo-show-emoji-picker :wk "emoji")
    "l d" '(hyalo-debug-status :wk "debug")
    ;; Toggles
    "t M" '(hyalo-toggle-macos-menu-bar :wk "macos menu bar")
    "t c" '(hyalo-toggle-chrome :wk "chrome")))

;;;; Hyalo Footer

(use-package hyalo-footer
  :ensure nil
  :if (eq window-system 'ns)
  :after hyalo
  :config
  (defun hyalo-footer-random-pattern ()
    "Set a random footer pattern."
    (interactive)
    (let* ((patterns '("hideout" "hexagons" "tiny-checkers"
		       "plus" "cage" "diagonal-stripes"
                       "stripes" "diagonal-lines" "signal"
                       "wallpaper"))
           (choice (nth (random (length patterns)) patterns)))
      (hyalo-footer-set-pattern choice)
      (message "Footer pattern set to: %s" choice)))

  (hyalo-footer-mode 1)
  (hyalo-footer-set-pattern "tiny-checkers")
  (hyalo-footer-set-background-alpha 0.1))

(provide 'init-hyalo)

;;; init-hyalo.el ends here
