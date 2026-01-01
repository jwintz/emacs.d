;;; init-markdown.el --- Markdown and knowledge management -*- lexical-binding: t; -*-

;;; Code:

(use-package svg-lib
  :ensure t)

(use-package hyalo-markdown-tags
  :ensure nil

  :commands (hyalo-markdown-tags-mode))

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (defvar-local hyalo-markdown--code-face-cookies nil
    "Face remap cookies for markdown code faces.")

  (defun hyalo-markdown--update-code-scale ()
    "Update markdown code face scaling to match text-scale."
    (when (derived-mode-p 'markdown-mode 'gfm-mode)
      (dolist (cookie hyalo-markdown--code-face-cookies)
        (face-remap-remove-relative cookie))
      (setq hyalo-markdown--code-face-cookies nil)
      (let ((scale (if (and (boundp 'text-scale-mode-amount)
                            (boundp 'text-scale-mode-step)
                            text-scale-mode-amount)
                       (expt text-scale-mode-step text-scale-mode-amount)
                     1.0)))
        (push (face-remap-add-relative 'markdown-code-face :height scale)
              hyalo-markdown--code-face-cookies)
        (push (face-remap-add-relative 'markdown-inline-code-face :height scale)
              hyalo-markdown--code-face-cookies)
        (push (face-remap-add-relative 'markdown-pre-face :height scale)
              hyalo-markdown--code-face-cookies))))

  (defun hyalo-markdown--setup-code-scaling ()
    "Set up code face scaling for markdown buffers."
    (add-hook 'text-scale-mode-hook #'hyalo-markdown--update-code-scale nil t)
    (hyalo-markdown--update-code-scale))

  :hook ((markdown-mode . hyalo-markdown-tags-mode)
         (markdown-mode . hyalo-markdown--setup-code-scaling))
  :config
  (defun hyalo-markdown--setup-pre-face ()
    "Set subtle background for markdown pre blocks based on theme."
    (let* ((bg (face-background 'default))
           (subtle-bg (if (eq (frame-parameter nil 'background-mode) 'dark)
                          (color-lighten-name bg 5)
                        (color-darken-name bg 3))))
      (set-face-attribute 'markdown-pre-face nil
                          :background subtle-bg
                          :extend t)))
  (add-hook 'enable-theme-functions (lambda (&rest _) (hyalo-markdown--setup-pre-face)))
  (hyalo-markdown--setup-pre-face)
  :general
  (:keymaps 'markdown-mode-map
   :prefix "C-c m"
   "" '(:ignore t :which-key "markdown")
   "l" 'markdown-insert-link
   "u" 'markdown-insert-uri
   "f" 'markdown-insert-footnote
   "w" 'markdown-insert-wiki-link
   "c" 'markdown-insert-code
   "C" 'markdown-insert-gfm-code-block
   "p" 'markdown-insert-pre
   "t" 'markdown-insert-table
   "h" 'markdown-insert-header-dwim
   "b" 'markdown-insert-bold
   "i" 'markdown-insert-italic
   "s" 'markdown-insert-strike-through
   "q" 'markdown-insert-blockquote))

(use-package polymode
  :ensure t)

(use-package poly-markdown
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode)))

(use-package obsidian
  :ensure t
  :defer t
  :commands (obsidian-capture obsidian-jump obsidian-daily-note obsidian-search)
  :init
  (let ((path (expand-file-name "~/Library/Mobile Documents/iCloud~md~obsidian/Documents/Vault")))
    (setq obsidian-directory path)
    (setq obsidian--relative-path-length (length (file-name-as-directory path))))

  (with-eval-after-load 'obsidian
    (let ((setter (get 'obsidian-directory 'custom-set)))
      (when setter
        (put 'obsidian-directory 'custom-set
             (lambda (symbol value)
               (let ((inhibit-message t))
                 (funcall setter symbol value)))))))

  :config
  (global-obsidian-mode t)
  (unless (and (boundp 'obsidian-vault-cache) obsidian-vault-cache)
    (when (file-directory-p (expand-file-name obsidian-directory))
      (obsidian-update)))
  :custom
  (obsidian-inbox-directory "Inbox")
  (obsidian-wiki-link-create-file-in-inbox t)
  (markdown-enable-wiki-links t)
  :bind
  (:map obsidian-mode-map
   ("C-c C-o" . obsidian-follow-link-at-point))
  :general
  (:prefix "C-c n"
   "" '(:ignore t :which-key "notes")
   "n" 'obsidian-capture
   "j" 'obsidian-jump
   "d" 'obsidian-daily-note
   "t" 'obsidian-tag-find
   "l" 'obsidian-insert-link
   "w" 'obsidian-insert-wikilink
   "b" 'obsidian-backlink-jump
   "f" 'obsidian-follow-link-at-point
   "s" 'obsidian-search
   "m" 'obsidian-move-file
   "g" 'obsidian-grep
   "v" 'obsidian-jump))

(provide 'init-markdown)

;;; init-markdown.el ends here
