;;; hyalo-markdown-tags.el --- SVG tag rendering for markdown -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: Julien Wintz <julien.wintz@inria.fr>
;; Keywords: markdown, tags, svg

;;; Commentary:

;; Renders hashtag-style tags (#tag) in markdown buffers as SVG pills
;; using svg-lib. Tags are theme and font aware.
;;
;; Features:
;; - Theme-aware colors (inherits from current theme faces)
;; - Font-aware (uses buffer's default font)
;; - Supports nested tags (#project/work)
;; - Customizable appearance

;;; Code:

(require 'svg-lib)

(defgroup hyalo-markdown-tags nil
  "SVG tag rendering for markdown."
  :group 'markdown
  :prefix "hyalo-markdown-tags-")

;;; Faces

(defface hyalo-markdown-tag
  '((t :inherit font-lock-keyword-face))
  "Face for markdown tags. SVG colors are derived from this face."
  :group 'hyalo-markdown-tags)

(defface hyalo-markdown-tag-special
  '((t :inherit font-lock-builtin-face))
  "Face for special tags (todo, fixme, note, etc.)."
  :group 'hyalo-markdown-tags)

;;; Customization

(defcustom hyalo-markdown-tags-padding 2
  "Padding inside SVG tags (in characters)."
  :type 'integer
  :group 'hyalo-markdown-tags)

(defcustom hyalo-markdown-tags-radius 4
  "Corner radius of SVG tags (in pixels)."
  :type 'integer
  :group 'hyalo-markdown-tags)

(defcustom hyalo-markdown-tags-stroke 2
  "Border stroke width of SVG tags (in pixels)."
  :type 'integer
  :group 'hyalo-markdown-tags)

(defcustom hyalo-markdown-tags-special-patterns
  '("todo" "fixme" "note" "important" "warning" "bug" "hack" "review" "done" "doing" "wip")
  "Tag names that use `hyalo-markdown-tag-special' face.
Case-insensitive matching."
  :type '(repeat string)
  :group 'hyalo-markdown-tags)

;;; Internal

(defvar-local hyalo-markdown-tags--cache nil
  "Cache of SVG images keyed by (tag-text . face-colors).
Invalidated on theme change.")

(defun hyalo-markdown-tags--get-face-colors (face)
  "Get foreground color from FACE, theme-aware."
  (let ((fg (face-foreground face nil t)))
    (or fg (face-foreground 'default))))

(defun hyalo-markdown-tags--get-font-family ()
  "Get a font family that svg-lib can use for metrics.
Uses the frame's default font if it can be looked up, otherwise falls back."
  (let* ((font (face-attribute 'default :font))
         (family (when (fontp font)
                   (font-get font :family)))
         (family (or family (face-attribute 'default :family nil t))))
    ;; Verify font-info can find this font, else use fallback
    (if (and family
             (ignore-errors
               (font-info (format "%s-12" family))))
        family
      ;; Fallback to a common monospace font
      (cond
       ((ignore-errors (font-info "Menlo-12")) "Menlo")
       ((ignore-errors (font-info "Monaco-12")) "Monaco")
       ((ignore-errors (font-info "Consolas-12")) "Consolas")
       (t "Monospace")))))

(defun hyalo-markdown-tags--get-font-size ()
  "Get the current frame's font size in points, accounting for text-scale."
  (let* ((font (face-attribute 'default :font))
         (base-size (if (fontp font)
                        (font-get font :size)
                      ;; Fallback to height attribute
                      (let ((height (face-attribute 'default :height nil t)))
                        (if (and height (numberp height))
                            (/ height 10.0)
                          13))))
         ;; Account for text-scale-mode
         (scale-factor (if (and (boundp 'text-scale-mode-amount)
                                (numberp text-scale-mode-amount))
                           (expt text-scale-mode-step text-scale-mode-amount)
                         1.0)))
    (* (or base-size 13) scale-factor)))

(defun hyalo-markdown-tags--special-p (tag-name)
  "Return non-nil if TAG-NAME is a special tag."
  (let ((name (downcase tag-name)))
    (cl-some (lambda (pattern)
               (string-match-p (concat "^" (regexp-quote pattern) "$") name))
             hyalo-markdown-tags-special-patterns)))

(defun hyalo-markdown-tags--get-frame-background ()
  "Get the current frame's background color."
  (or (face-background 'default nil t)
      "#000000"))

(defun hyalo-markdown-tags--create-svg (tag-text)
  "Create SVG image for TAG-TEXT (without the # prefix)."
  (let* ((is-special (hyalo-markdown-tags--special-p
                      (car (split-string tag-text "/"))))
         (face (if is-special
                   'hyalo-markdown-tag-special
                 'hyalo-markdown-tag))
         (fg (hyalo-markdown-tags--get-face-colors face))
         (bg (hyalo-markdown-tags--get-frame-background))
         (font-family (hyalo-markdown-tags--get-font-family))
         (font-size (truncate (hyalo-markdown-tags--get-font-size)))
         (text-scale (if (boundp 'text-scale-mode-amount) text-scale-mode-amount 0))
         ;; Display text includes the # prefix
         (display-text (concat "#" tag-text))
         ;; Cache key includes scale
         (cache-key (list display-text fg bg font-size text-scale))
         (cached (assoc cache-key hyalo-markdown-tags--cache)))
    (if cached
        (cdr cached)
      (let* ((svg (svg-lib-tag display-text nil
                               :foreground fg
                               :background bg
                               :font-family font-family
                               :font-size font-size
                               :stroke hyalo-markdown-tags-stroke
                               :padding hyalo-markdown-tags-padding
                               :radius hyalo-markdown-tags-radius
                               :height 1.1)))
        (push (cons cache-key svg) hyalo-markdown-tags--cache)
        svg))))

(defun hyalo-markdown-tags--propertize (match)
  "Return display property for tag MATCH."
  ;; MATCH is the full match including #
  (let* ((tag-text (substring match 1)) ; Remove # prefix
         (svg (hyalo-markdown-tags--create-svg tag-text)))
    svg))

(defun hyalo-markdown-tags--fontify (limit)
  "Font-lock matcher for hashtag tags up to LIMIT."
  ;; Match #tag or #tag/subtag (but not ## headers or # at line start)
  (when (re-search-forward
         "\\(?:^\\|[[:space:]]\\)\\(#\\([[:alnum:]_-]+\\(?:/[[:alnum:]_-]+\\)*\\)\\)"
         limit t)
    (let* ((beg (match-beginning 1))
           (end (match-end 1))
           (tag-text (match-string 2))
           (svg (hyalo-markdown-tags--create-svg tag-text)))
      ;; Set display property on the match
      (put-text-property beg end 'display svg)
      ;; Continue searching
      t)))

(defun hyalo-markdown-tags--clear-cache ()
  "Clear the SVG cache, forcing regeneration on next display."
  (setq hyalo-markdown-tags--cache nil))

(defun hyalo-markdown-tags--on-theme-change (&rest _)
  "Handle theme changes by clearing cache and refontifying."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (bound-and-true-p hyalo-markdown-tags-mode)
        (hyalo-markdown-tags--clear-cache)
        (font-lock-flush)))))

(defun hyalo-markdown-tags--on-text-scale-change ()
  "Handle text-scale changes by clearing cache and refontifying."
  (when (bound-and-true-p hyalo-markdown-tags-mode)
    (hyalo-markdown-tags--clear-cache)
    (font-lock-flush)))

;;; Font-lock integration

(defvar hyalo-markdown-tags--font-lock-keywords
  '((hyalo-markdown-tags--fontify))
  "Font-lock keywords for SVG tag rendering.")

;;; Minor mode

;;;###autoload
(define-minor-mode hyalo-markdown-tags-mode
  "Minor mode to render hashtag tags as SVG pills in markdown."
  :lighter " Tags"
  :group 'hyalo-markdown-tags
  (if hyalo-markdown-tags-mode
      (progn
        ;; Add font-lock keywords
        (font-lock-add-keywords nil hyalo-markdown-tags--font-lock-keywords t)
        ;; Theme change hook
        (add-hook 'enable-theme-functions #'hyalo-markdown-tags--on-theme-change)
        ;; Text scale change hook (buffer-local)
        (add-hook 'text-scale-mode-hook #'hyalo-markdown-tags--on-text-scale-change nil t)
        ;; Initial fontification
        (font-lock-flush))
    ;; Disable
    (font-lock-remove-keywords nil hyalo-markdown-tags--font-lock-keywords)
    (remove-hook 'enable-theme-functions #'hyalo-markdown-tags--on-theme-change)
    (remove-hook 'text-scale-mode-hook #'hyalo-markdown-tags--on-text-scale-change t)
    ;; Remove display properties
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "#[[:alnum:]_-]+\\(?:/[[:alnum:]_-]+\\)*" nil t)
        (remove-text-properties (match-beginning 0) (match-end 0) '(display nil))))
    (font-lock-flush)))

;;;###autoload
(defun hyalo-markdown-tags-setup ()
  "Enable SVG tags in markdown-mode buffers."
  (when (derived-mode-p 'markdown-mode 'gfm-mode)
    (hyalo-markdown-tags-mode 1)))

(provide 'hyalo-markdown-tags)
;;; hyalo-markdown-tags.el ends here
