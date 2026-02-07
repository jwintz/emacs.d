;;; init-appearance.el --- Visual appearance: fonts, icons, themes -*- lexical-binding: t; -*-

;;; Code:

;;;; Fonts

(setq nano-font-size 11)
(set-face-attribute 'default nil :family "SF Mono" :height 110 :weight 'regular)
(set-face-attribute 'fixed-pitch nil :family "SF Mono" :height 110 :weight 'regular)
(set-face-attribute 'variable-pitch nil :family "SF Pro Display" :height 140 :weight 'regular)

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
  :preface (require 'color)
  :config
  (global-hl-line-mode 1))

;;;; Ligatures

(use-package ligature
  :ensure t
  :config
  ;; SF Mono ligatures (supported via specialized builds or patches,
  ;; or generic programming ligatures)
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
                            "|> " "<|" "<|>" "||>" "|>>"
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

;;;; Nano Theme & Layout

(require 'nano-base-colors)
(require 'nano-faces)
(require 'nano-theme)
(require 'nano-layout)
(require 'hyalo-theme)

(require 'hyalo-fonts)
(hyalo-fonts-setup)

(hyalo-theme-setup)

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
  :defer t
  :init
  (autoload 'olivetti-mode "olivetti" "Writing mode" t))

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
