;;; hyalo-theme.el --- Hyalo theme for N Λ N O -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Hyalo theme adaptation for Nano Emacs.
;; Dichromatic with Violet accent, based on Zinc grays.
;;
;;; Code:

(require 'cl-lib)
(require 'json)
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

  (nano-refresh-theme)

  ;; Update hyalo appearance for light mode
  (when (featurep 'hyalo-appearance)
    (hyalo-appearance--apply-window-appearance 'light)
    (hyalo-appearance--apply-vibrancy)
    (hyalo-appearance--clear-backgrounds))

  ;; Send palette to Swift for terminal theming
  (hyalo-theme-send-palette)
  )

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

  (nano-refresh-theme)

  ;; Update hyalo appearance for dark mode
  (when (featurep 'hyalo-appearance)
    (hyalo-appearance--apply-window-appearance 'dark)
    (hyalo-appearance--apply-vibrancy)
    (hyalo-appearance--clear-backgrounds))

  ;; Send palette to Swift for terminal theming
  (hyalo-theme-send-palette)
  )

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

;; Terminal palette

(defun hyalo-theme--color-to-hex (color)
  "Convert Emacs COLOR name or spec to a hex string like \"#RRGGBB\"."
  (when color
    (apply #'format "#%02x%02x%02x"
           (mapcar (lambda (c) (round (* c 255)))
                   (color-name-to-rgb color)))))

(defun hyalo-theme--term-face-color (face attr)
  "Return ATTR (:foreground or :background) of FACE as hex, or nil.
Returns nil if FACE is not yet defined (term.el not loaded)."
  (when (facep face)
    (let ((color (if (eq attr :foreground)
                     (face-foreground face nil 'default)
                   (face-background face nil 'default))))
      (hyalo-theme--color-to-hex color))))

(defun hyalo-theme-send-palette ()
  "Send the current theme palette to the inspector terminal.
Derives ANSI colors from `term-color-*' faces and nano colors."
  (when (fboundp 'hyalo-set-terminal-palette)
    (let* ((fg (or nano-color-foreground
                   (face-foreground 'default nil t)))
           (bg (or nano-color-background
                   (face-background 'default nil t)))
           (cursor (or nano-color-salient "#7C3AED"))
           ;; Normal (0-7): foreground attribute of term-color-* faces
           (faces '(term-color-black term-color-red term-color-green
                    term-color-yellow term-color-blue term-color-magenta
                    term-color-cyan term-color-white))
           (fallback-normal (vector bg "#EF5350" "#66BB6A" "#FFEE58"
                                    "#42A5F5" "#AB47BC" "#26C6DA" fg))
           ;; Bright (8-15): background attribute of term-color-* faces
           (fallback-bright (vector (or nano-color-faded "#52525B")
                                    "#F87171" "#4ADE80" "#FDE047"
                                    "#60A5FA" "#C084FC" "#22D3EE" "#FFFFFF"))
           (normal (cl-loop for face in faces
                            for i from 0
                            collect (or (hyalo-theme--term-face-color face :foreground)
                                        (aref fallback-normal i))))
           (bright (cl-loop for face in faces
                            for i from 0
                            collect (or (hyalo-theme--term-face-color face :background)
                                        (aref fallback-bright i))))
           (ansi (vconcat normal bright))
           (palette (json-encode
                     `((foreground . ,fg)
                       (background . ,bg)
                       (cursor . ,cursor)
                       (ansi . ,ansi)))))
      (hyalo-set-terminal-palette palette))))

(provide 'hyalo-theme)
;;; hyalo-theme.el ends here
