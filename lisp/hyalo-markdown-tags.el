;;; hyalo-markdown-tags.el --- SVG tag rendering for markdown -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: Julien Wintz <julien.wintz@inria.fr>
;; Keywords: markdown, tags, svg

;;; Commentary:

;; Renders hashtag-style tags (#tag) in markdown buffers as SVG pills
;; using svg-lib (https://github.com/rougier/svg-lib).
;;
;; Uses overlays for compatibility with polymode.
;;
;; Usage:
;;   (add-hook 'markdown-mode-hook #'hyalo-markdown-tags-mode)

;;; Code:

(require 'svg-lib)

(defgroup hyalo-markdown-tags nil
  "SVG tag rendering for markdown."
  :group 'markdown
  :prefix "hyalo-markdown-tags-")

(defface hyalo-markdown-tag
  '((t :inherit font-lock-keyword-face))
  "Face for markdown tags. SVG colors are derived from this face."
  :group 'hyalo-markdown-tags)

(defface hyalo-markdown-tag-special
  '((t :inherit font-lock-builtin-face))
  "Face for special tags (todo, fixme, note, etc.)."
  :group 'hyalo-markdown-tags)

(defcustom hyalo-markdown-tags-special-patterns
  '("todo" "fixme" "note" "important" "warning" "bug" "hack" "review" "done" "doing" "wip")
  "Tag names that use `hyalo-markdown-tag-special' face.
Case-insensitive matching."
  :type '(repeat string)
  :group 'hyalo-markdown-tags)

(defun hyalo-markdown-tags--special-p (tag-name)
  "Return non-nil if TAG-NAME is a special tag."
  (let ((name (downcase (replace-regexp-in-string "^#" "" tag-name))))
    (member name hyalo-markdown-tags-special-patterns)))

(defun hyalo-markdown-tags--make-tag (tag)
  "Create SVG tag for TAG string."
  (let* ((face (if (hyalo-markdown-tags--special-p tag)
                   'hyalo-markdown-tag-special
                 'hyalo-markdown-tag))
         (fg (face-foreground face nil 'default))
         (bg (face-background 'default nil t))
	 (tg (string-trim-left tag "#")))

    (svg-lib-icon+tag "tag" tg nil
		    :font-family "Roboto Mono"
		    :font-weight 500
		    :stroke 2
		    :radius 4
		    :background bg
		    :foreground fg)
    ))

(defun hyalo-markdown-tags--remove-overlays (&optional beg end)
  "Remove tag overlays in region BEG to END, or entire buffer."
  (remove-overlays (or beg (point-min)) (or end (point-max)) 'hyalo-markdown-tag t))

(defun hyalo-markdown-tags--apply-overlays (&optional beg end)
  "Apply SVG tag overlays in region BEG to END."
  (let ((beg (or beg (point-min)))
        (end (or end (point-max))))
    (hyalo-markdown-tags--remove-overlays beg end)
    (save-excursion
      (goto-char beg)
      ;; Match #tag or #tag/subtag (not ## headers)
      (while (re-search-forward
              "\\(?:^\\|[[:space:]]\\)\\(#[[:alnum:]_-]+\\(?:/[[:alnum:]_-]+\\)*\\)"
              end t)
        (let* ((mbeg (match-beginning 1))
               (mend (match-end 1))
               (tag (match-string-no-properties 1))
               (svg (hyalo-markdown-tags--make-tag tag))
               (ov (make-overlay mbeg mend)))
          (overlay-put ov 'hyalo-markdown-tag t)
          (overlay-put ov 'display svg)
          (overlay-put ov 'evaporate t))))))

(defun hyalo-markdown-tags--after-change (beg end _len)
  "Update overlays after buffer change between BEG and END."
  (when (bound-and-true-p hyalo-markdown-tags-mode)
    ;; Extend region to line boundaries for safety
    (let ((beg (save-excursion (goto-char beg) (line-beginning-position)))
          (end (save-excursion (goto-char end) (line-end-position))))
      (hyalo-markdown-tags--apply-overlays beg end))))

(defun hyalo-markdown-tags--on-theme-change (&rest _)
  "Handle theme changes by refreshing overlays."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (bound-and-true-p hyalo-markdown-tags-mode)
        (hyalo-markdown-tags--apply-overlays)))))

;;;###autoload
(define-minor-mode hyalo-markdown-tags-mode
  "Minor mode to render hashtag tags as SVG pills in markdown.
Uses svg-lib with theme-aware colors and overlay-based display."
  :lighter " Tags"
  :group 'hyalo-markdown-tags
  (if hyalo-markdown-tags-mode
      (progn
        (hyalo-markdown-tags--apply-overlays)
        (add-hook 'after-change-functions #'hyalo-markdown-tags--after-change nil t)
        (add-hook 'enable-theme-functions #'hyalo-markdown-tags--on-theme-change))
    (hyalo-markdown-tags--remove-overlays)
    (remove-hook 'after-change-functions #'hyalo-markdown-tags--after-change t)
    (remove-hook 'enable-theme-functions #'hyalo-markdown-tags--on-theme-change)))

;;;###autoload
(defun hyalo-markdown-tags-setup ()
  "Enable SVG tags in markdown-mode buffers."
  (when (derived-mode-p 'markdown-mode 'gfm-mode)
    (hyalo-markdown-tags-mode 1)))

(provide 'hyalo-markdown-tags)
;;; hyalo-markdown-tags.el ends here
