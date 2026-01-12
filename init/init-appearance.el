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
  (advice-add 'modus-themes-select-dark :after #'hyalo-switch-to-dark)
  (advice-add 'modus-themes-load-random-dark :after #'hyalo-switch-to-dark)

  (advice-add 'modus-themes-select-light :after #'hyalo-switch-to-light)
  (advice-add 'modus-themes-load-random-light :after #'hyalo-switch-to-light))
(use-package ef-themes
  :ensure t
  :after modus-themes
  :config
  (modus-themes-include-derivatives-mode 1))

(use-package doric-themes
  :ensure t)

(use-package kaolin-themes
  :ensure t
  :config
  (defvar kaolin-themes-dark-list
    '(kaolin-dark kaolin-aurora kaolin-bubblegum kaolin-eclipse kaolin-ocean
      kaolin-temple kaolin-valley-dark kaolin-blossom kaolin-mono-dark kaolin-shiva)
    "List of dark Kaolin themes.")

  (defvar kaolin-themes-light-list
    '(kaolin-light kaolin-galaxy kaolin-valley-light kaolin-breeze kaolin-mono-light)
    "List of light Kaolin themes.")

  (defun kaolin-themes-load-random-dark ()
    "Load a random dark Kaolin theme."
    (interactive)
    (let ((theme (nth (random (length kaolin-themes-dark-list)) kaolin-themes-dark-list)))
      (mapc #'disable-theme custom-enabled-themes)
      (load-theme theme t)
      (message "Loaded Kaolin theme: %s" theme)))

  (defun kaolin-themes-load-random-light ()
    "Load a random light Kaolin theme."
    (interactive)
    (let ((theme (nth (random (length kaolin-themes-light-list)) kaolin-themes-light-list)))
      (mapc #'disable-theme custom-enabled-themes)
      (load-theme theme t)
      (message "Loaded Kaolin theme: %s" theme))))

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

;;;; Minimap

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
  (add-hook 'demap-minimap-construct-hook #'hyalo-minimap-setup)
  (add-hook 'demap-minimap-window-set-hook #'hyalo-minimap-setup)

  ;; When demap is constructed while diffview is active, set the buffer
  (add-hook 'demap-minimap-construct-hook #'hyalo-demap--maybe-set-diffview-buffer))

;; Diffview-demap integration (load-order independent)
(defvar hyalo-demap--diffview-active nil
  "Non-nil when diffview is showing side-by-side diff.")

(defun hyalo-demap--maybe-set-diffview-buffer ()
  "Set demap to show side-by-side-2 buffer if in diffview mode."
  (when hyalo-demap--diffview-active
    (run-with-timer 0.1 nil #'hyalo-demap--set-diffview-buffer-now)))

(defun hyalo-demap--set-diffview-buffer-now ()
  "Actually set the demap to track side-by-side-2 window."
  (when hyalo-demap--diffview-active
    (when-let* ((buf (get-buffer "*side-by-side-2*"))
                (win (get-buffer-window buf)))
      (when-let* ((minimap-buf (get-buffer "*Minimap*"))
                  (minimap (and (fboundp 'demap-buffer-minimap)
                                (demap-buffer-minimap minimap-buf))))
        ;; Use window-set to properly trigger hooks (visible region, etc.)
        (demap-minimap-window-set minimap win)))))

(defun hyalo-demap--enter-diffview (&rest _)
  "Set up demap for diffview mode."
  (setq hyalo-demap--diffview-active t)
  (run-with-timer 0.3 nil #'hyalo-demap--set-diffview-buffer-now))

(defun hyalo-demap--exit-diffview (&rest _)
  "Clean up demap and buffers after diffview exits."
  (setq hyalo-demap--diffview-active nil)
  ;; Kill the git diff output buffer
  (when-let* ((buf (get-buffer "*git-diff-output*")))
    (kill-buffer buf)))

;; Add advice when diffview loads (works regardless of demap load state)
(with-eval-after-load 'diffview
  (advice-add 'diffview-current :before #'hyalo-demap--enter-diffview)
  (advice-add 'diffview-region :before #'hyalo-demap--enter-diffview)
  (advice-add 'diffview--quit :after #'hyalo-demap--exit-diffview))

;; Set up demap tracking function when demap loads
(with-eval-after-load 'demap
  (setq demap-track-window-mode-update-p-func
        (lambda (&optional window &rest _)
          (if hyalo-demap--diffview-active
              nil
            (let ((win (or window (selected-window))))
              (with-current-buffer (window-buffer win)
                (not (or (string-match-p "^\\*side-by-side" (buffer-name))
                         (derived-mode-p 'diffview-mode)))))))))

(use-package olivetti
  :ensure t
  :defer t)

;;;; Splash Screen

(use-package hyalo-splash
  :ensure nil
  :if (display-graphic-p)
  :defer t
  :init
  ;; Lightweight setup - just inhibit default splash and schedule ours
  (when (and (not noninteractive)
             (not (member "-no-splash" command-line-args))
             (not (member "--file" command-line-args))
             (not (member "--find-file" command-line-args)))
    (setq inhibit-startup-screen t
          inhibit-startup-message t
          inhibit-startup-echo-area-message (user-login-name))
    (add-hook 'window-setup-hook
              (lambda () (require 'hyalo-splash) (hyalo-splash)))))

(provide 'init-appearance)

;;; init-appearance.el ends here
