;;; init-tengwar.el --- Tengwar script rendering -*- lexical-binding: t; -*-

;;; Commentary:

;; Renders visible text in Tengwar script using overlays.
;; Requires:
;;   - bun runtime (https://bun.sh)
;;   - tengwar npm package: bun add tengwar
;;   - Tengwar font (e.g., Tengwar Annatar)

;;; Code:

(use-package hyalo-tengwar
  :ensure nil
  :commands (hyalo-tengwar-minor-mode hyalo-tengwar-partial-mode)
  :custom
  ;; Font configuration: select from hyalo-tengwar-fonts
  (hyalo-tengwar-font "Tengwar Annatar")
  (hyalo-tengwar-font-height 1.3)
  (hyalo-tengwar-language "english")
  (hyalo-tengwar-mode "general-use")
  (hyalo-tengwar-partial-delimiters '("@@" . "@@")))

(use-package hyalo-tengwar-tutorial
  :ensure nil
  :commands (hyalo/tengwar-tutorial
             hyalo/tengwar-tutorial-reset
             hyalo/tengwar-tutorial-lesson)
  :bind (("C-c T t" . hyalo/tengwar-tutorial)))

(provide 'init-tengwar)

;;; init-tengwar.el ends here
