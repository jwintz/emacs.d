;;; init-help.el --- Help system: helpful, elisp-refs -*- lexical-binding: t; -*-

;;; Code:

(use-package helpful
  :ensure t
  :commands (helpful-callable helpful-variable helpful-command helpful-key helpful-symbol)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol)
  :general
  (leader-def
    "h f" 'helpful-callable
    "h v" 'helpful-variable
    "h k" 'helpful-key
    "h s" 'helpful-symbol
    "h ." 'helpful-at-point))

(use-package elisp-refs
  :ensure t
  :general
  (:prefix "C-c h r"
   "" '(:ignore t :which-key "elisp-refs")
   "f" 'elisp-refs-function
   "m" 'elisp-refs-macro
   "v" 'elisp-refs-variable
   "s" 'elisp-refs-special
   "r" 'elisp-refs-symbol)
  :commands (elisp-refs-function
             elisp-refs-macro
             elisp-refs-variable
             elisp-refs-special
             elisp-refs-symbol))

(provide 'init-help)

;;; init-help.el ends here
