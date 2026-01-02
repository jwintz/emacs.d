;;; hyalo-icons.el --- Hyalo monochromatic icon helpers -*- lexical-binding: t -*- no-byte-compile: t -*-

;; Copyright (C) 2025
;; Author: Julien Wintz <julien.wintz@inria.fr>
;; Keywords: faces, icons

;;; Commentary:

;; Local monochromatic icon helpers for Hyalo sidebars.
;; Does NOT use global advice to avoid breaking doom-modeline.
;; Instead, provides helper functions that sidebar code can use directly.

;;; Code:

(defgroup hyalo-icons nil
  "Hyalo monochromatic icon configuration."
  :group 'hyalo-module
  :prefix "hyalo-icons-")

(defcustom hyalo-icons-monochrome t
  "When non-nil, sidebar icon functions strip foreground colors."
  :type 'boolean
  :group 'hyalo-icons)

;;; Helper to strip foreground from icon

(defun hyalo-icons-strip-foreground (icon)
  "Strip face properties from ICON string.
Returns ICON with face removed so it inherits from context.
Returns empty string if ICON is nil."
  (cond
   ((null icon) "")
   ((not (stringp icon)) icon)
   (hyalo-icons-monochrome
    (let ((clean-icon (copy-sequence icon)))
      (remove-text-properties 0 (length clean-icon)
                              '(face nil font-lock-face nil)
                              clean-icon)
      clean-icon))
   (t icon)))

;;; Wrapper functions for sidebar use (call these instead of nerd-icons directly)

(defun hyalo-icons-for-mode (mode)
  "Get icon for MODE, stripped of foreground if monochrome enabled.
Returns empty string if no icon found or on error."
  (condition-case nil
      (if (fboundp 'nerd-icons-icon-for-mode)
          (hyalo-icons-strip-foreground (nerd-icons-icon-for-mode mode))
        "")
    (error "")))

(defun hyalo-icons-for-file (file &rest args)
  "Get icon for FILE, stripped of foreground if monochrome enabled."
  (if (fboundp 'nerd-icons-icon-for-file)
      (hyalo-icons-strip-foreground (apply #'nerd-icons-icon-for-file file args))
    ""))

(defun hyalo-icons-for-dir (dir &rest args)
  "Get icon for DIR, stripped of foreground if monochrome enabled."
  (if (fboundp 'nerd-icons-icon-for-dir)
      (hyalo-icons-strip-foreground (apply #'nerd-icons-icon-for-dir dir args))
    ""))

(defun hyalo-icons-for-buffer (&optional buffer)
  "Get icon for BUFFER, stripped of foreground if monochrome enabled."
  (if (fboundp 'nerd-icons-icon-for-buffer)
      (hyalo-icons-strip-foreground (nerd-icons-icon-for-buffer buffer))
    ""))

;;; Global protection for doom-modeline
;; nerd-icons can return nil for some modes, which breaks doom-modeline.
;; This advice ensures a fallback icon is returned instead of nil.

(defun hyalo-icons--ensure-not-nil (orig-fun &rest args)
  "Advice to ensure nerd-icons functions never return nil or non-string.
Returns a fallback icon if the original function returns nil or non-string."
  (let ((result (apply orig-fun args)))
    (if (stringp result)
        result
      "")))

(defun hyalo-icons--install-protection ()
  "Install nil-protection advice on nerd-icons functions."
  (advice-add 'nerd-icons-icon-for-mode :around #'hyalo-icons--ensure-not-nil)
  (advice-add 'nerd-icons-icon-for-file :around #'hyalo-icons--ensure-not-nil)
  (advice-add 'nerd-icons-icon-for-dir :around #'hyalo-icons--ensure-not-nil)
  (advice-add 'nerd-icons-icon-for-buffer :around #'hyalo-icons--ensure-not-nil)
  (advice-add 'nerd-icons-icon-for-extension :around #'hyalo-icons--ensure-not-nil))

;; Install immediately if nerd-icons already loaded, otherwise defer
(if (featurep 'nerd-icons)
    (hyalo-icons--install-protection)
  (with-eval-after-load 'nerd-icons
    (hyalo-icons--install-protection)))

(provide 'hyalo-icons)
;;; hyalo-icons.el ends here
