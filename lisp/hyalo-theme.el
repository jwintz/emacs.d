;;; hyalo-theme.el --- Hyalo theme for N Λ N O -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Hyalo theme adaptation for Nano Emacs.
;; Dichromatic with Violet accent, based on Zinc grays.
;;
;;; Code:

(require 'nano-base-colors)
(require 'nano-faces)
(require 'nano-theme)

(defun hyalo-theme-light ()
  "Apply the Hyalo Light theme."
  (interactive)
  (setq nano-theme-var "light")
  
  ;; Zinc 50 background, Zinc 900 foreground
  (setq nano-color-background "#FAFAFA")
  (setq nano-color-foreground "#18181B")
  
  ;; Subtle: Zinc 100
  (setq nano-color-subtle "#F4F4F5")
  
  ;; Faded: Zinc 400
  (setq nano-color-faded "#A1A1AA")
  
  ;; Highlight: Zinc 200
  (setq nano-color-highlight "#E4E4E7")
  (set-face-background 'hl-line "#E4E4E7") ; Zinc 200 (more distinct at 10% alpha)
  
  ;; Salient: Violet 600 (Primary accent)
  (setq nano-color-salient "#7C3AED")
  
  ;; Popout: Violet 500 (Secondary accent)
  (setq nano-color-popout "#8B5CF6")
  
  ;; Critical: Red 500
  (setq nano-color-critical "#EF4444")
  
  ;; Strong: Same as foreground
  (setq nano-color-strong nano-color-foreground)

  (nano-refresh-theme))

(defun hyalo-theme-dark ()
  "Apply the Hyalo Dark theme."
  (interactive)
  (setq nano-theme-var "dark")
  
  ;; Zinc 900 background, Zinc 100 foreground
  (setq nano-color-background "#18181B")
  (setq nano-color-foreground "#F4F4F5")
  
  ;; Subtle: Zinc 800
  (setq nano-color-subtle "#27272A")
  
  ;; Faded: Zinc 600
  (setq nano-color-faded "#52525B")
  
  ;; Highlight: Zinc 800/700 mix
  (setq nano-color-highlight "#3F3F46")
  (set-face-background 'hl-line "#3F3F46") ; Zinc 700 (more distinct at 10% alpha)
  
  ;; Salient: Violet 400 (Primary accent)
  (setq nano-color-salient "#A58AF9")
  
  ;; Popout: Violet 300 (Secondary accent)
  (setq nano-color-popout "#C4B5FD")
  
  ;; Critical: Red 400
  (setq nano-color-critical "#F87171")
  
  ;; Strong: Same as foreground
  (setq nano-color-strong nano-color-foreground)

  (nano-refresh-theme))

(defun hyalo-theme-sync (appearance)
  "Sync theme with system APPEARANCE (ns-appearance)."
  (pcase appearance
    ('light (hyalo-theme-light))
    ('dark (hyalo-theme-dark))))

(defun hyalo-theme-setup ()
  "Setup Hyalo theme hooks."
  (if (boundp 'ns-system-appearance-change-functions)
      (add-hook 'ns-system-appearance-change-functions #'hyalo-theme-sync)
    (hyalo-theme-dark)))

(provide 'hyalo-theme)
;;; hyalo-theme.el ends here
