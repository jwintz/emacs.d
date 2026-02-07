;;; hyalo-fonts.el --- Font configuration for Hyalo -*- lexical-binding: t; -*-

;;; Commentary:

;; Provides font settings for Hyalo.
;; Strictly uses SF Mono and SF Pro Display.

;;; Code:

(defun hyalo-fonts-setup ()
  "Apply font settings."
  ;; Line numbers
  (when (display-graphic-p)
    (dolist (face '(line-number line-number-current-line))
      (when (facep face)
        (set-face-attribute face nil
                            :family "SF Mono"
                            :height 0.9
                            :weight 'normal)))))

(provide 'hyalo-fonts)
;;; hyalo-fonts.el ends here