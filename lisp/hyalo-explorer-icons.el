;;; hyalo-explorer-icons.el --- Monochrome nerd-icons theme for treemacs -*- lexical-binding: t -*-

;; Copyright (C) 2025 Julien Wintz
;; Based on treemacs-nerd-icons.el by Hongyu Ding <rainstormstudio@yahoo.com>

;; Author: Julien Wintz
;; Keywords: files, icons, treemacs
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3") (nerd-icons "0.0.1") (treemacs "0.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Monochrome nerd-icons theme for treemacs.
;; A fork of treemacs-nerd-icons that uses a single face for all icons,
;; providing a clean, uniform appearance.

;;; Code:

(require 'nerd-icons)
(require 'treemacs)

(defgroup hyalo-explorer-icons nil
  "Monochrome nerd-icons theme for treemacs."
  :group 'treemacs)

(defface hyalo-explorer-icons-face
  '((t (:inherit default)))
  "Face used for all icons in hyalo-explorer-icons theme.
Inherits from default to match the current theme's foreground color."
  :group 'hyalo-explorer-icons)

(defface hyalo-explorer-icons-root-face
  '((t (:inherit default :weight bold)))
  "Face used for root icons in hyalo-explorer-icons theme."
  :group 'hyalo-explorer-icons)

(defface hyalo-explorer-icons-dim-face
  '((t (:inherit shadow)))
  "Face used for chevrons and separators."
  :group 'hyalo-explorer-icons)

(defcustom hyalo-explorer-icons-size 1.0
  "The default icon size in treemacs."
  :group 'hyalo-explorer-icons
  :type 'float)

(defvar hyalo-explorer-icons-tab
  (propertize "\t" 'face 'hyalo-explorer-icons-face))

(treemacs-create-theme "hyalo-explorer"
  :config
  (let* ((sep hyalo-explorer-icons-tab)
         (face 'hyalo-explorer-icons-face)
         (dim-face 'hyalo-explorer-icons-dim-face)
         (root-face 'hyalo-explorer-icons-root-face)
         (size hyalo-explorer-icons-size)
         (chevron-down (nerd-icons-octicon "nf-oct-chevron_down" :face dim-face :height (* 0.75 size) :v-adjust 0.1))
         (chevron-right (nerd-icons-octicon "nf-oct-chevron_right" :face dim-face :height (* 0.75 size) :v-adjust 0.1)))

    ;; File extension icons - all monochrome
    (dolist (item nerd-icons-extension-icon-alist)
      (let* ((extension (car item))
             (func (cadr item))
             (icon-name (cadr (cdr item)))
             (icon (funcall func icon-name :face face :v-adjust 0.0 :height size)))
        (let* ((icon-pair (cons (format " %s%s%s" sep icon sep) (format " %s%s%s" sep icon sep)))
               (gui-icons (treemacs-theme->gui-icons treemacs--current-theme))
               (tui-icons (treemacs-theme->tui-icons treemacs--current-theme))
               (gui-icon (car icon-pair))
               (tui-icon (cdr icon-pair)))
          (ht-set! gui-icons extension gui-icon)
          (ht-set! tui-icons extension tui-icon))))

    ;; Root icon
    (treemacs-create-icon
     :icon (format "%s%s" (nerd-icons-octicon "nf-oct-repo" :face root-face :height (* 1.2 size)) sep)
     :extensions (root-closed root-open)
     :fallback 'same-as-icon)

    ;; Directory icons
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron-down sep (nerd-icons-sucicon "nf-custom-folder_open" :face face :height size) sep)
     :extensions (dir-open)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron-right sep (nerd-icons-sucicon "nf-custom-folder_oct" :face face :height size) sep)
     :extensions (dir-closed)
     :fallback 'same-as-icon)

    ;; Special directory icons
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron-down sep (nerd-icons-octicon "nf-oct-code" :face face :height size) sep)
     :extensions ("src-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron-right sep (nerd-icons-octicon "nf-oct-code" :face face :height size) sep)
     :extensions ("src-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron-down sep (nerd-icons-sucicon "nf-custom-folder_open" :face face :height size) sep)
     :extensions ("build-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron-right sep (nerd-icons-mdicon "nf-md-folder_cog" :face face :height size) sep)
     :extensions ("build-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron-down sep (nerd-icons-sucicon "nf-custom-folder_open" :face face :height size) sep)
     :extensions ("test-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron-right sep (nerd-icons-mdicon "nf-md-folder_check" :face face :height size) sep)
     :extensions ("test-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron-down sep (nerd-icons-sucicon "nf-custom-folder_open" :face face :height size) sep)
     :extensions ("bin-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron-right sep (nerd-icons-mdicon "nf-md-folder_zip" :face face :height size) sep)
     :extensions ("bin-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron-down sep (nerd-icons-sucicon "nf-custom-folder_open" :face face :height size) sep)
     :extensions ("git-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron-right sep (nerd-icons-sucicon "nf-custom-folder_git" :face face :height size) sep)
     :extensions ("git-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron-down sep (nerd-icons-sucicon "nf-custom-folder_open" :face face :height size) sep)
     :extensions ("github-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron-right sep (nerd-icons-sucicon "nf-custom-folder_github" :face face :height size) sep)
     :extensions ("github-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron-down sep (nerd-icons-sucicon "nf-custom-folder_open" :face face :height size) sep)
     :extensions ("public-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron-right sep (nerd-icons-mdicon "nf-md-folder_eye" :face face :height size) sep)
     :extensions ("public-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron-down sep (nerd-icons-sucicon "nf-custom-folder_open" :face face :height size) sep)
     :extensions ("private-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron-right sep (nerd-icons-mdicon "nf-md-folder_lock" :face face :height size) sep)
     :extensions ("private-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron-down sep (nerd-icons-sucicon "nf-custom-folder_open" :face face :height size) sep)
     :extensions ("temp-open" "tmp-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron-right sep (nerd-icons-mdicon "nf-md-folder_question" :face face :height size) sep)
     :extensions ("temp-closed" "tmp-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron-down sep (nerd-icons-sucicon "nf-custom-folder_open" :face face :height size) sep)
     :extensions ("readme-open" "docs-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron-right sep (nerd-icons-mdicon "nf-md-folder_file" :face face :height size) sep)
     :extensions ("readme-closed" "docs-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron-down sep (nerd-icons-sucicon "nf-custom-folder_open" :face face :height size) sep)
     :extensions ("screenshots-open" "icons-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron-right sep (nerd-icons-mdicon "nf-md-folder_image" :face face :height size) sep)
     :extensions ("screenshots-closed" "icons-closed")
     :fallback 'same-as-icon)

    ;; Tag icons
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron-down sep (nerd-icons-octicon "nf-oct-package" :face face :height size) sep)
     :extensions (tag-open)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron-right sep (nerd-icons-octicon "nf-oct-package" :face face :height size) sep)
     :extensions (tag-closed)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s" (nerd-icons-octicon "nf-oct-tag" :face face :height size) sep)
     :extensions (tag-leaf)
     :fallback 'same-as-icon)

    ;; Status icons (keep subtle differentiation via face)
    (treemacs-create-icon
     :icon (format "%s%s" (nerd-icons-octicon "nf-oct-flame" :face face :height size) sep)
     :extensions (error)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s" (nerd-icons-octicon "nf-oct-stop" :face face :height size) sep)
     :extensions (warning)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s" (nerd-icons-octicon "nf-oct-info" :face face :height size) sep)
     :extensions (info)
     :fallback 'same-as-icon)

    ;; Utility icons
    (treemacs-create-icon
     :icon (format "%s%s" (nerd-icons-mdicon "nf-md-mail" :face face :height size) sep)
     :extensions (mail)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s" (nerd-icons-octicon "nf-oct-bookmark" :face face :height size) sep)
     :extensions (bookmark)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s" (nerd-icons-mdicon "nf-md-monitor" :face face :height size) sep)
     :extensions (screen)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s" (nerd-icons-mdicon "nf-md-home" :face face :height size) sep)
     :extensions (house)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s" (nerd-icons-faicon "nf-fa-list" :face face :height size) sep)
     :extensions (list)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s" (nerd-icons-mdicon "nf-md-repeat" :face face :height size) sep)
     :extensions (repeat)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s" (nerd-icons-faicon "nf-fa-suitcase" :face face :height size) sep)
     :extensions (suitcase)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s" (nerd-icons-mdicon "nf-md-close" :face face :height size) sep)
     :extensions (close)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s" (nerd-icons-octicon "nf-oct-calendar" :face face :height size) sep)
     :extensions (calendar)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s" (nerd-icons-faicon "nf-fa-briefcase" :face face :height size) sep)
     :extensions (briefcase)
     :fallback 'same-as-icon)

    ;; Fallback icon
    (treemacs-create-icon
     :icon (format " %s%s%s" sep (nerd-icons-faicon "nf-fa-file_o" :face face :height size) sep)
     :extensions (fallback)
     :fallback 'same-as-icon)))

;;;###autoload
(defun hyalo-explorer-icons-config ()
  "Install hyalo-explorer monochrome icon theme for treemacs."
  (treemacs-load-theme "hyalo-explorer"))

(provide 'hyalo-explorer-icons)
;;; hyalo-explorer-icons.el ends here
