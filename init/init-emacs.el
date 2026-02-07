;;; init-emacs.el --- Core Emacs settings: cursor, startup, UI -*- lexical-binding: t; -*-

;;; Code:

(defcustom hyalo-initial-frame-width 600
  "Initial frame width in text pixels."
  :type 'integer
  :group 'frames)

(defcustom hyalo-initial-frame-height 800
  "Initial frame height in text pixels."
  :type 'integer
  :group 'frames)

(use-package emacs
  :ensure nil
  :custom
  ;; Startup
  (inhibit-startup-screen t)
  (inhibit-startup-message t)
  (inhibit-startup-echo-area-message t)
  (initial-buffer-choice t)
  (initial-scratch-message "")
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
  (defun hyalo-center-initial-frame ()
    "Center the initial frame using `hyalo-initial-frame-width/height`."
    (run-at-time "0.1" nil
                 (lambda ()
                   (let* ((frame (selected-frame))
                          ;; Desired text area dimensions
                          (target-w hyalo-initial-frame-width)
                          (target-h hyalo-initial-frame-height)
                          ;; Measure current decoration overhead
                          (cur-outer-w (frame-outer-width frame))
                          (cur-text-w (frame-text-width frame))
                          (deco-w (- cur-outer-w cur-text-w))
                          (cur-outer-h (frame-outer-height frame))
                          (cur-text-h (frame-text-height frame))
                          (deco-h (- cur-outer-h cur-text-h))
                          ;; Calculate target outer dimensions
                          (final-w (+ target-w deco-w))
                          (final-h (+ target-h deco-h))
                          ;; Get monitor workarea
                          (workarea (frame-monitor-workarea frame))
                          (mx (nth 0 workarea))
                          (my (nth 1 workarea))
                          (mw (nth 2 workarea))
                          (mh (nth 3 workarea))
                          ;; Calculate centered position
                          (x (+ mx (/ (- mw final-w) 2)))
                          (y (+ my (/ (- mh final-h) 2))))
                     ;; (message "Hyalo Center: Target [%d %d] Deco [%d %d] -> Pos [%d %d]"
                     ;;          final-w final-h deco-w deco-h x y)
                     ;; Enforce size (pixels) and position
                     (set-frame-size frame target-w target-h t)
                     (set-frame-position frame x y)))))

  (add-hook 'window-setup-hook #'hyalo-center-initial-frame)

  (setq initial-frame-alist
        `((width . (text-pixels . ,hyalo-initial-frame-width))
          (height . (text-pixels . ,hyalo-initial-frame-height))
          (tool-bar-lines . 0)))
  (setq visible-bell nil)
  (setq ring-bell-function 'ignore)
  (setq inhibit-compacting-font-caches t)
  (advice-add 'display-startup-echo-area-message :override #'ignore)
  (blink-cursor-mode 0)
  (show-paren-mode 1)
  ;; UI elements are now handled via default-frame-alist in init-bootstrap.el
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
  :defer 1
  :init
  (defvar recentf-mode nil)
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
                (cl-letf (((symbol-function 'load-file)
                           (lambda (file) (load file nil t))))
                  (apply orig-fun args))))
  (recentf-mode 1))

(use-package saveplace
  :ensure nil
  :defer 1
  :init
  (defvar save-place-mode nil)
  :config
  (save-place-mode 1))

(use-package savehist
  :ensure nil
  :defer 1
  :init
  (defvar savehist-mode nil)
  :config
  (savehist-mode 1))

(use-package autorevert
  :ensure nil
  :defer 1
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
