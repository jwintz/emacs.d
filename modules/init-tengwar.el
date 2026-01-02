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
  :commands hyalo-tengwar-minor-mode
  :custom
  (hyalo-tengwar-font-family "Tengwar Annatar")
  (hyalo-tengwar-font-height 1.0)
  (hyalo-tengwar-language "english")
  (hyalo-tengwar-mode "general-use")
  (hyalo-tengwar-use-csur nil))

(provide 'init-tengwar)

;;; init-tengwar.el ends here
