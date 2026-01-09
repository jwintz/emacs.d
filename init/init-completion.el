;;; init-completion.el --- Completion framework configuration -*- lexical-binding: t; -*-

;; Description: Vertico, Consult, Marginalia, Orderless configuration
;; Part of the Hyalo Emacs configuration

;;; Code:

(use-package vertico
  :ensure t
  ;; :custom
  ;; (vertico-preselect 'prompt)
  :init
  (vertico-mode)
  ;; :bind (:map vertico-map
  ;;             ("TAB" . minibuffer-complete))
  )

(use-package marginalia
  :ensure t
  :after vertico
  :init
  (marginalia-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion orderless))))
  (orderless-component-separator #'orderless-escapable-split-on-space))

(use-package consult
  :ensure t
  :bind (("C-x b" . consult-buffer)
	 ("C-x C-r" . consult-recent-file)
         ("C-x 4 b" . consult-buffer-other-window)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line))
  :config
  (setq consult-preview-key nil))

(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect\\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(defun my/corfu-complete-and-send ()
  "Insert completion and send input if in eshell at end of line."
  (interactive)
  (corfu-insert)
  (when (and (derived-mode-p 'eshell-mode)
             (eolp))
    (eshell-send-input)))

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  (corfu-echo-mode)              ;; Show candidates in echo area
  :custom
  (corfu-cycle t)                ;; Enable cycling for "fast" completion
  (corfu-auto nil)               ;; Disable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-preview-current t)      ;; Enable inline preview
  (corfu-min-width 1)            ;; Minimal width
  ;; (corfu-popupinfo-mode nil)     ;; Disable documentation popup (default)
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)
        ("RET" . my/corfu-complete-and-send)
        ([return] . my/corfu-complete-and-send))
  :config
  ;; Suppress the popup frame entirely
  (advice-add #'corfu--popup-show :override #'ignore)
  (advice-add #'corfu--popup-hide :override #'ignore))

;; (use-package completion-preview
;;   :ensure nil  ;; Built-in to Emacs 30+
;;   :hook ((prog-mode . completion-preview-mode)
;;          (text-mode . completion-preview-mode))
;; ;;       (eshell-mode . completion-preview-mode))
;;   :custom
;;   (completion-preview-minimum-symbol-length 2)
;;   :bind (:map completion-preview-active-mode-map
;;               ("M-n" . completion-preview-next-candidate)
;;               ("M-p" . completion-preview-prev-candidate)
;;               ("M-i" . completion-preview-insert)))

(provide 'init-completion)

;;; init-completion.el ends here
