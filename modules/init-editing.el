;;; init-editing.el --- Editing: god-mode, windmove, outline, scrolling -*- lexical-binding: t; -*-

;;; Code:

(use-package display-line-numbers
  :ensure nil
  :diminish display-line-numbers-mode
  :custom
  (display-line-numbers-width 2)
  (display-line-numbers-widen t)
  (display-line-numbers-grow-only t))

(use-package god-mode
  :ensure t
  :bind
  (("C-x C-1" . delete-other-windows)
   ("C-x C-2" . split-window-below)
   ("C-x C-3" . split-window-right)
   ("C-x C-0" . delete-window)
   ("C-x C-o" . other-window))
  :config
  (defun iota/god-mode-update-cursor-type ()
    "Change cursor shape based on god-mode state."
    (let ((new-cursor (if god-local-mode 'box '(hbar . 2))))
      (setq cursor-type new-cursor)
      (when (not (display-graphic-p))
        (run-with-idle-timer 0.01 nil
          (lambda ()
            (let ((escape-seq (if (eq new-cursor 'box)
                                  "\033[2 q"
                                "\033[4 q")))
              (send-string-to-terminal escape-seq)))))))

  (defun iota/god-mode-restore-cursor ()
    "Restore cursor to underline on exit."
    (when (not (display-graphic-p))
      (send-string-to-terminal "\033[4 q")))

  (add-hook 'post-command-hook #'iota/god-mode-update-cursor-type)
  (add-hook 'buffer-list-update-hook #'iota/god-mode-update-cursor-type)
  (add-hook 'window-configuration-change-hook #'iota/god-mode-update-cursor-type)
  (add-hook 'kill-emacs-hook #'iota/god-mode-restore-cursor))

(use-package move-dup
  :ensure t
  :general
  ("M-<up>"   'move-dup-move-lines-up)
  ("M-<down>" 'move-dup-move-lines-down)
  ("M-s-<up>"   'move-dup-duplicate-up)
  ("M-s-<down>" 'move-dup-duplicate-down))

(use-package windmove
  :ensure nil
  :general
  (:prefix "C-x"
   "<left>"  'windmove-left
   "<right>" 'windmove-right
   "<up>"    'windmove-up
   "<down>"  'windmove-down))

(use-package stripspace
  :ensure t
  :diminish stripspace-local-mode
  :hook ((prog-mode text-mode conf-mode) . stripspace-local-mode)
  :custom
  (stripspace-only-if-initially-clean nil)
  (stripspace-restore-column t))

(use-package outline
  :ensure nil
  :diminish outline-minor-mode
  :hook (prog-mode . outline-minor-mode)
  :general
  (:prefix "C-c @"
   "" '(:ignore t :which-key "outline"))
  :config
  (add-hook 'outline-minor-mode-hook
            (lambda ()
              (require 'nerd-icons)
              (let* ((display-table (or buffer-display-table (make-display-table)))
                     (face-offset (* (face-id 'shadow) (ash 1 22)))
                     (chevron (concat " " (nerd-icons-codicon "nf-cod-chevron_down")))
                     (value (vconcat (mapcar (lambda (c) (+ face-offset c)) chevron))))
                (set-display-table-slot display-table 'selective-display value)
                (setq buffer-display-table display-table)))))

(use-package outline-indent
  :ensure t
  :after nerd-icons
  :custom
  (outline-indent-ellipsis (concat " " (nerd-icons-codicon "nf-cod-chevron_down")))
  :hook
  ((python-mode . outline-indent-minor-mode)
   (python-ts-mode . outline-indent-minor-mode)
   (yaml-mode . outline-indent-minor-mode)
   (yaml-ts-mode . outline-indent-minor-mode)))

(use-package ultra-scroll
  :ensure t
  :vc (:url "https://github.com/jdtsmith/ultra-scroll")
  :custom
  (scroll-conservatively 3)
  (scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

(provide 'init-editing)

;;; init-editing.el ends here
