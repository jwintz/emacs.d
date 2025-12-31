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

;;;; Fonts

(use-package fontaine
  :ensure t
  :if (display-graphic-p))

(use-package hyalo-fonts
  :ensure nil
  :if (eq window-system 'ns)
  :after (hyalo fontaine))

;;;; Hyalo Appearance

(use-package hyalo-appearance
  :ensure nil
  :if (eq window-system 'ns)
  :after hyalo
  :demand t
  :preface
  (defvar hyalo-profiles
    '((focus . (:appearance light :theme modus-operandi :vibrancy "none"      :opacity 1.0))
      (glass . (:appearance dark  :theme modus-vivendi  :vibrancy "ultraThin" :opacity 0.85))
      (void  . (:appearance dark  :theme ef-winter      :vibrancy "thick"     :opacity 0.80)))
    "Alist of Hyalo appearance profiles.")

  (defun hyalo-set-highlights (&rest _)
    "Set highlight faces using weight differentiation."
    (let ((m 'medium)
          (b 'bold)
          (wb 'ultra-bold))
      (set-face-attribute 'region nil :weight b)
      (set-face-attribute 'isearch nil :weight wb :box '(:line-width -1 :style released-button))
      (set-face-attribute 'lazy-highlight nil :weight b)
      (set-face-attribute 'match nil :weight b)
      (set-face-attribute 'show-paren-match nil :weight b)
      (when (facep 'hl-line)
        (set-face-attribute 'hl-line nil :weight m))
      (when (facep 'highlight)
        (set-face-attribute 'highlight nil :weight m))
      (when (facep 'vertico-current)
        (set-face-attribute 'vertico-current nil :weight b))
      (dolist (face '(orderless-match-face-0
                      orderless-match-face-1
                      orderless-match-face-2
                      orderless-match-face-3
                      consult-highlight-match
                      consult-preview-match))
        (when (facep face)
          (set-face-attribute face nil :weight wb)))
      (dolist (mface '(magit-section-highlight
                       magit-diff-hunk-heading-highlight
                       magit-diff-context-highlight))
        (when (facep mface)
          (set-face-attribute mface nil :weight wb)))))

  (defun hyalo-load-profile (name)
    "Load the profile NAME and persist settings."
    (let ((profile (alist-get name hyalo-profiles)))
      (when profile
        (let ((appearance (plist-get profile :appearance))
              (theme (plist-get profile :theme))
              (vibrancy (plist-get profile :vibrancy))
              (opacity (plist-get profile :opacity)))
          (when appearance
            (hyalo-appearance-set appearance))
          (when theme
            (mapc #'disable-theme custom-enabled-themes)
            (load-theme theme t)
            (hyalo-set-highlights)
            (customize-save-variable 'hyalo-appearance-current-theme theme))
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
  (add-hook 'fontaine-set-preset-hook #'hyalo-set-highlights)
  (add-hook 'enable-theme-functions #'hyalo-set-highlights)
  (hyalo-set-highlights)
  (with-eval-after-load 'magit
    (hyalo-set-highlights))

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
    (let* ((patterns '("hideout" "hexagons" "death-star" "bathroom-floor"
                       "tiny-checkers" "plus" "cage" "diagonal-stripes"
                       "stripes" "diagonal-lines" "polka-dots" "signal"
                       "wallpaper"))
           (choice (nth (random (length patterns)) patterns)))
      (hyalo-footer-set-pattern choice)
      (message "Footer pattern set to: %s" choice)))

  (hyalo-footer-mode 1)
  (hyalo-footer-set-pattern "hexagons")
  (hyalo-footer-set-background-alpha 0.1))

(provide 'init-hyalo)

;;; init-hyalo.el ends here
