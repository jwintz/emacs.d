;;; init-emacs.el --- Core Emacs settings: cursor, startup, UI -*- lexical-binding: t; -*-

;;; Code:

(use-package emacs
  :ensure nil
  :custom
  ;; Startup
  (inhibit-startup-screen t)
  (inhibit-startup-message t)
  (inhibit-startup-echo-area-message t)
  (initial-buffer-choice nil)
  (initial-scratch-message ";; - *scratch*

;; Towards using box instead of highlight for visible region indication
;;

(require ’color)

(let* ((base-color (face-attribute 'default :background))
       (tint-color (face-attribute 'highlight :background))
       (alpha      0.9)
       (blended-color (apply 'color-rgb-to-hex
                             (color-blend (color-name-to-rgb tint-color)
                                          (color-name-to-rgb base-color)
                                          alpha))))
  ;; Apply the face attribute
  (set-face-attribute 'demap-visible-region-face nil
                      :box (list :line-width -4
                                 :color blended-color)))
")
  ;; Cursor
  (cursor-in-non-selected-windows nil)
  (cursor-type '(hbar . 2))
  (cursor-intangible-mode t)
  (x-stretch-cursor nil)
  ;; Text
  (text-scale-mode-step 1.1)
  ;; Backups
  (make-backup-files nil)
  ;; Misc
  (use-short-answers t)
  (use-dialog-box nil)
  (frame-title-format nil)
  :config
  (setq visible-bell nil)
  (setq ring-bell-function 'ignore)
  (advice-add 'display-startup-echo-area-message :override #'ignore)
  (blink-cursor-mode 0)
  (show-paren-mode 1)
  ;; UI elements
  (when (fboundp 'scroll-bar-mode) (set-scroll-bar-mode nil))
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (if (display-graphic-p) (menu-bar-mode 1) (menu-bar-mode -1))
  ;; Remove continuation indicators in fringe
  (setq-default fringe-indicator-alist
                (delq (assq 'continuation fringe-indicator-alist)
                      fringe-indicator-alist))
  ;; Coding systems
  (set-default-coding-systems 'utf-8)
  ;; Scrolling
  (general-def
    "C-M-n" 'scroll-up-line
    "C-M-p" 'scroll-down-line
    "<home>" 'scroll-up-line
    "<end>" 'scroll-down-line
    "s-<down>" 'scroll-up-line
    "s-<up>" 'scroll-down-line))

;;;; Built-in Packages

(use-package recentf
  :ensure nil
  :demand t
  :init
  (defvar recentf-mode nil)
  (require 'recentf)
  :custom
  (recentf-auto-cleanup (if (daemonp) 300 'never))
  (recentf-max-saved-items 200)
  (recentf-exclude
   `(,@(mapcar (lambda (f) (expand-file-name f user-emacs-directory))
               '("recentf" "bookmarks" ".local/"))
     ,(rx bos "/tmp/")))
  :config
  ;; Suppress "Loading .../recentf.eld...done" message
  (advice-add 'recentf-load-list :around
              (lambda (orig-fun &rest args)
                (let ((inhibit-message t))
                  (apply orig-fun args))))
  (recentf-mode 1))

(use-package saveplace
  :ensure nil
  :init
  (defvar save-place-mode nil)
  :config
  (save-place-mode 1))

(use-package savehist
  :ensure nil
  :init
  (defvar savehist-mode nil)
  :config
  (savehist-mode 1))

(use-package autorevert
  :ensure nil
  :init (global-auto-revert-mode 1)
  :custom
  (global-auto-revert-non-file-buffers nil)
  (auto-revert-interval 3)
  (auto-revert-verbose t))

(use-package eldoc
  :ensure nil
  :diminish)

(provide 'init-emacs)

;;; init-emacs.el ends here
