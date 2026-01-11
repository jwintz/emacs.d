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
  (hyalo-load))

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
  :preface
  (defvar hyalo-profiles
    '((light . (:appearance light :theme ef-summer       :vibrancy "regular" :opacity 0.6))
      (dark  . (:appearance dark  :theme ef-melissa-dark :vibrancy "regular" :opacity 0.6))
      (shell . (:appearance dark  :theme kaolin-temple   :vibrancy "regular" :opacity 0.6))
      (notes . (:appearance dark  :theme kaolin-eclipse  :vibrancy "regular" :opacity 0.6)))
    "Alist of Hyalo appearance profiles.")

  (defun hyalo-load-profile (name)
    "Load the profile NAME and persist settings."
    (let ((profile (alist-get name hyalo-profiles)))
      (when profile
        (let ((appearance (plist-get profile :appearance))
              (theme (plist-get profile :theme))
              (vibrancy (plist-get profile :vibrancy))
              (opacity (plist-get profile :opacity)))
          ;; Load theme first
          (when theme
            (mapc #'disable-theme custom-enabled-themes)
            (load-theme theme t)
            (hyalo-fonts-set-highlights)
            (customize-save-variable 'hyalo-appearance-current-theme theme))
          ;; Apply appearance (won't reload theme since not in auto mode)
          (when appearance
            (hyalo-appearance-set appearance))
          (when vibrancy
            (hyalo-appearance-set-vibrancy vibrancy))
          (when opacity
            (hyalo-appearance-set-opacity opacity))
          (message "Loaded profile: %s" name)))))

  (defun hyalo-switch-profile ()
    "Switch to a defined profile."
    (interactive)
    (let* ((choices (mapcar (lambda (x) (symbol-name (car x))) hyalo-profiles))
           (selection (completing-read "Select Profile: " choices nil t)))
      (hyalo-load-profile (intern selection))))

  :init
  (unless (get 'hyalo-appearance-theme-light 'saved-value)
    (setq hyalo-appearance-theme-light 'modus-operandi))
  (unless (get 'hyalo-appearance-theme-dark 'saved-value)
    (setq hyalo-appearance-theme-dark 'modus-vivendi))

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
    "l P" '(hyalo-appearance-show-panel :wk "panel")
    "l ." '(hyalo-switch-profile :wk "profile"))

  :config
  (hyalo-appearance-mode 1)

  ;; Hyalo Appearance menu
  ;; Pass nil to easy-menu-define map arg so it doesn't auto-install.
  ;; We verify installation with easy-menu-add-item below.
  (easy-menu-define hyalo-appearance-menu nil
    "Hyalo Appearance Menu"
    '("Hyalo"
      ["Appearance Panel..." hyalo-appearance-show-panel
       :help "Open appearance settings panel"]
      "---"
      ["Set Vibrancy..." hyalo-appearance-set-vibrancy]
      ["Set Opacity..." hyalo-appearance-set-opacity]
      "---"
      ("Profiles"
       ["Focus" (hyalo-load-profile 'focus)]
       ["Glass" (hyalo-load-profile 'glass)]
       ["Void"  (hyalo-load-profile 'void)]
       ["Switch..." hyalo-switch-profile])
      "---"
      ("Appearance Mode"
       ["Auto" (hyalo-appearance-set 'auto)
        :style radio :selected (eq hyalo-appearance-mode-setting 'auto)]
       ["Light" (hyalo-appearance-set 'light)
        :style radio :selected (eq hyalo-appearance-mode-setting 'light)]
       ["Dark" (hyalo-appearance-set 'dark)
        :style radio :selected (eq hyalo-appearance-mode-setting 'dark)])))
  (easy-menu-add-item global-map '(menu-bar) hyalo-appearance-menu)
  (add-hook 'hyalo-appearance-mode-hook
            #'hyalo-appearance--apply-vibrancy))

;;;; Hyalo Viewport/Scroll

(use-package hyalo-viewport
  :ensure nil
  :if (eq window-system 'ns)
  :after hyalo-header
  :custom
  (hyalo-viewport-debug nil)
  (hyalo-viewport-excluded-modes '(agent-shell-mode
                                   agent-shell-viewport-view-mode
                                   agent-shell-viewport-edit-mode))
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
    "l d" '(hyalo-debug-status :wk "debug")))

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
