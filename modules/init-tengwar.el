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
  ;; Font derived from use-csur: nil = Tengwar Annatar, t = Tengwar Telcontar
  (hyalo-tengwar-use-csur nil)
  (hyalo-tengwar-font-height 1.2)
  (hyalo-tengwar-language "english")
  (hyalo-tengwar-mode "general-use")
  (hyalo-tengwar-partial-delimiters '("@@" . "@@")))

(provide 'init-tengwar)

;;; init-tengwar.el ends here
