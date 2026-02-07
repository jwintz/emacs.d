;;; hyalo-markdown-mode.el --- Hyalo decorations for markdown -*- lexical-binding: t -*--

;; Copyright (C) 2025
;; Author: Julien Wintz <julien.wintz@inria.fr>
;; Keywords: markdown, tags, svg, decoration

;;; Commentary:

;; Provides SVG decorations for markdown buffers:
;; - Hashtag-style tags (#tag) as SVG pills (relative scaling)
;; - Horizontal rules (---) as full-width lines
;;
;; Uses overlays for compatibility with polymode.
;;
;; Usage:
;;   (add-hook 'markdown-mode-hook #'hyalo-markdown-mode)

;;; Code:

(require 'svg-lib)
(require 'markdown-mode)

(defgroup hyalo-markdown nil
  "Hyalo markdown decorations."
  :group 'markdown
  :prefix "hyalo-markdown-")

;;; Faces

(defface hyalo-markdown-tag
  '((t :inherit font-lock-keyword-face))
  "Face for markdown tags. SVG colors are derived from this face."
  :group 'hyalo-markdown)

(defface hyalo-markdown-tag-special
  '((t :inherit font-lock-builtin-face))
  "Face for special tags (todo, fixme, note, etc.)."
  :group 'hyalo-markdown)

(defface hyalo-markdown-hr
  '((t :inherit shadow))
  "Face for horizontal rules."
  :group 'hyalo-markdown)

;;; Tags Configuration

(defcustom hyalo-markdown-special-tags
  '("todo" "fixme" "note" "important" "warning" "bug" "hack" "review" "done" "doing" "wip")
  "Tag names that use `hyalo-markdown-tag-special' face.
Case-insensitive matching."
  :type '(repeat string)
  :group 'hyalo-markdown)

(defcustom hyalo-markdown-tag-height 0.90
  "Height ratio for tags (multiplied by line height).
A value of 0.90 means tags are 90% of the line height."
  :type 'float
  :group 'hyalo-markdown)

;;; Tag Helpers

(defun hyalo-markdown--special-tag-p (tag-name)
  "Return non-nil if TAG-NAME is a special tag."
  (let ((name (downcase (replace-regexp-in-string "^#" "" tag-name))))
    (member name hyalo-markdown-special-tags)))

(defun hyalo-markdown--text-scale-factor ()
  "Return the current text scale factor from `text-scale-mode'."
  (if (and (boundp 'text-scale-mode-amount)
           (numberp text-scale-mode-amount)
           (not (zerop text-scale-mode-amount)))
      (let ((step (if (boundp 'text-scale-mode-step)
                      text-scale-mode-step
                    1.2)))
        (expt step text-scale-mode-amount))
    1.0))

(defun hyalo-markdown--make-tag (tag)
  "Create SVG tag for TAG.
Uses window-font-height to derive proper font-size for vertical centering.
Scales all internal dimensions with text-scale-mode."
  (let* ((face (if (hyalo-markdown--special-tag-p tag)
                   'hyalo-markdown-tag-special
                 'hyalo-markdown-tag))
         (fg (face-foreground face nil 'default))
         (bg (face-background 'default nil t))
         (tg (string-trim-left tag "#"))
         ;; Use window-font-height directly for consistent sizing
         ;; Container height = window-font-height * height-ratio
         ;; Font-size should be ~85% of container height for good fit
         (wfh (window-font-height))
         (container-height (* wfh hyalo-markdown-tag-height))
         ;; Font-size in points: approximate conversion from pixels
         ;; Typical: 1pt ≈ 1.33px at 96 DPI, so px/1.33 ≈ pt
         (font-size (max 8 (round (/ (* container-height 0.925) 1.33))))
         (radius (max 2 (round (* container-height 0.15))))
         (stroke (max 1 (round (* container-height 0.08)))))
    (svg-lib-icon+tag "tag" tg nil
                      :font-family "SF Mono"
                      :font-weight 500
                      :font-size font-size
                      :stroke stroke
                      :radius radius
                      :padding 1
                      :scale 0.75
                      :height hyalo-markdown-tag-height
                      :background bg
                      :foreground fg)))

;;; Horizontal Rule Helpers

(defun hyalo-markdown--make-hr (width)
  "Create SVG horizontal rule of WIDTH pixels.
Minimal height to reduce background visibility."
  (let* ((height 1)
         (fg (face-foreground 'hyalo-markdown-hr nil 'default))
         (bg (face-background 'default nil t))
         (svg (svg-create width height))
         (y 1))
    (svg-rectangle svg 0 0 width height :fill bg)
    (svg-line svg 0 y width y
              :stroke-color fg
              :stroke-width 1)
    (svg-image svg :ascent 'center)))

;;; Overlay Management

(defun hyalo-markdown--remove-overlays (&optional beg end)
  "Remove decorations in region BEG to END, or entire buffer."
  (remove-overlays (or beg (point-min)) (or end (point-max)) 'hyalo-markdown-overlay t))

(defun hyalo-markdown--apply-overlays (&optional beg end)
  "Apply SVG decorations in region BEG to END."
  (let ((beg (or beg (point-min)))
        (end (or end (point-max)))
        (inhibit-modification-hooks t))
    (hyalo-markdown--remove-overlays beg end)
    (save-excursion
      ;; 1. Process Tags (#tag or #tag/subtag)
      (goto-char beg)
      (while (re-search-forward
              "\\(?:^\\|[[:space:]]\\)\\(#[[:alnum:]_+-]+\\(?:/[[:alnum:]_+-]+\\)*\\)"
              end t)
        (let* ((mbeg (match-beginning 1))
               (mend (match-end 1))
               (tag (match-string-no-properties 1))
               (svg (hyalo-markdown--make-tag tag))
               (ov (make-overlay mbeg mend)))
          (overlay-put ov 'hyalo-markdown-overlay t)
          (overlay-put ov 'display svg)
          (overlay-put ov 'evaporate t)))

      ;; 2. Process Horizontal Rules (--- or more dashes on a line)
      (goto-char beg)
      (while (re-search-forward "^\\(-\\{3,\\}\\)$" end t)
        (let* ((mbeg (match-beginning 1))
               (mend (match-end 1))
               (width (window-body-width nil t))
               (svg (hyalo-markdown--make-hr width))
               (ov (make-overlay mbeg mend)))
          (overlay-put ov 'hyalo-markdown-overlay t)
          (overlay-put ov 'display svg)
          (overlay-put ov 'evaporate t))))))

(defun hyalo-markdown--after-change (beg end _len)
  "Update overlays after buffer change between BEG and END."
  (when (bound-and-true-p hyalo-markdown-mode)
    ;; Extend region to line boundaries for safety
    (let ((beg (save-excursion (goto-char beg) (line-beginning-position)))
          (end (save-excursion (goto-char end) (line-end-position))))
      (hyalo-markdown--apply-overlays beg end))))

(defun hyalo-markdown--on-window-change ()
  "Update decorations when window configuration changes (resize)."
  (dolist (win (window-list))
    (with-selected-window win
      (when (bound-and-true-p hyalo-markdown-mode)
        ;; Re-apply to update width of HRs
        (hyalo-markdown--apply-overlays (window-start) (window-end))))))

(defun hyalo-markdown--on-theme-change (&rest _)
  "Handle theme changes by refreshing overlays."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (bound-and-true-p hyalo-markdown-mode)
        (hyalo-markdown--apply-overlays)))))

(defun hyalo-markdown--on-text-scale-change (&rest _)
  "Refresh overlays after text scale change (CMD++/CMD+-)."
  (when (bound-and-true-p hyalo-markdown-mode)
    (hyalo-markdown--apply-overlays)
    (force-window-update (current-buffer))))

;;;###autoload
(define-minor-mode hyalo-markdown-mode
  "Minor mode to render Hyalo decorations (tags, HRs) in markdown.
Uses svg-lib with theme-aware colors and overlay-based display."
  :lighter " HMD"
  :group 'hyalo-markdown
  (if hyalo-markdown-mode
      (progn
        (hyalo-markdown--apply-overlays)
        (add-hook 'after-change-functions #'hyalo-markdown--after-change nil t)
        (add-hook 'enable-theme-functions #'hyalo-markdown--on-theme-change)
        (add-hook 'window-configuration-change-hook #'hyalo-markdown--on-window-change)
        ;; Advise all text-scale functions for CMD++/CMD+- support
        (advice-add 'text-scale-set :after #'hyalo-markdown--on-text-scale-change)
        (advice-add 'text-scale-increase :after #'hyalo-markdown--on-text-scale-change)
        (advice-add 'text-scale-decrease :after #'hyalo-markdown--on-text-scale-change))
    (hyalo-markdown--remove-overlays)
    (remove-hook 'after-change-functions #'hyalo-markdown--after-change t)
    (remove-hook 'enable-theme-functions #'hyalo-markdown--on-theme-change)
    (remove-hook 'window-configuration-change-hook #'hyalo-markdown--on-window-change)
    (advice-remove 'text-scale-set #'hyalo-markdown--on-text-scale-change)
    (advice-remove 'text-scale-increase #'hyalo-markdown--on-text-scale-change)
    (advice-remove 'text-scale-decrease #'hyalo-markdown--on-text-scale-change)))

(provide 'hyalo-markdown-mode)
;;; hyalo-markdown-mode.el ends here
