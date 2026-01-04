;;; init-completion.el --- Completion framework configuration -*- lexical-binding: t; -*-

;; Description: Vertico, Consult, Marginalia, Orderless configuration
;; Part of the Hyalo Emacs configuration

;;; Code:

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package marginalia
  :ensure t
  :after vertico
  :init
  (marginalia-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

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
