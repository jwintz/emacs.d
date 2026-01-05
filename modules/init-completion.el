;;; init-completion.el --- Completion framework configuration -*- lexical-binding: t; -*-

;; Description: Vertico, Consult, Marginalia, Orderless configuration
;; Part of the Hyalo Emacs configuration

;;; Code:

(use-package vertico
  :ensure t
  :custom
  (vertico-preselect 'prompt)
  :init
  (vertico-mode)
  :bind (:map vertico-map
              ("TAB" . minibuffer-complete)))  ; prefix expansion instead of insert

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

(provide 'init-completion)

;;; init-completion.el ends here
