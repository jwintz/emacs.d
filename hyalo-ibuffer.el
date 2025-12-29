;;; hyalo-ibuffer.el --- Hyalo sidebar ibuffer configuration -*- lexical-binding: t -*- no-byte-compile: t -*-

;; Copyright (C) 2025
;; Author: Julien Wintz <julien.wintz@inria.fr>
;; Keywords: frames, sidebar, ibuffer

;;; Commentary:

;; Custom ibuffer configuration for the Hyalo sidebar.
;; - Single column layout (icon + name only)
;; - Icons based on major mode
;; - Exclusion lists for buffers and modes

;;; Code:

(require 'ibuffer)

(defgroup hyalo-ibuffer nil
  "Hyalo sidebar ibuffer configuration."
  :group 'hyalo-module-sidebar
  :prefix "hyalo-ibuffer-")

(defcustom hyalo-ibuffer-excluded-buffer-names
  '("*Messages*" "*Completions*" "*Help*" "*Backtrace*"
    "*Compile-Log*" "*scratch*" "*Warnings*" "*Ibuffer*"
    " *Minibuf-" " *Echo Area" " *code-conversion-work*"
    " *Treemacs-" "*tramp/")
  "List of buffer name patterns to exclude from sidebar ibuffer.
Patterns starting with space match hidden buffers."
  :type '(repeat string)
  :group 'hyalo-ibuffer)

(defcustom hyalo-ibuffer-excluded-modes
  '(special-mode help-mode completion-list-mode
    Custom-mode messages-buffer-mode
    treemacs-mode ibuffer-mode dired-mode
    minibuffer-inactive-mode echo-area-mode)
  "List of major modes to exclude from sidebar ibuffer."
  :type '(repeat symbol)
  :group 'hyalo-ibuffer)

(defcustom hyalo-ibuffer-show-modified-only nil
  "When non-nil, only show modified file buffers."
  :type 'boolean
  :group 'hyalo-ibuffer)

(defcustom hyalo-ibuffer-auto-refresh-interval 2.0
  "Interval in seconds for auto-refreshing sidebar ibuffer.
Set to nil to disable auto-refresh."
  :type '(choice (const nil) number)
  :group 'hyalo-ibuffer)

(defvar hyalo-ibuffer--auto-refresh-timer nil
  "Timer for auto-refreshing sidebar ibuffer.")

;;; Icon support

(defvar hyalo-ibuffer--icon-cache (make-hash-table :test 'equal)
  "Cache for buffer icons.")

(defun hyalo-ibuffer--get-icon (mode)
  "Get icon for major MODE."
  (or (gethash mode hyalo-ibuffer--icon-cache)
      (let ((icon (hyalo-ibuffer--compute-icon mode)))
        (puthash mode icon hyalo-ibuffer--icon-cache)
        icon)))

(defun hyalo-ibuffer--compute-icon (mode)
  "Compute icon string for major MODE."
  (cond
   ;; Programming modes
   ((memq mode '(emacs-lisp-mode lisp-interaction-mode)) "")
   ((memq mode '(python-mode python-ts-mode)) "")
   ((memq mode '(js-mode js2-mode javascript-mode js-ts-mode)) "")
   ((memq mode '(typescript-mode typescript-ts-mode)) "")
   ((memq mode '(rust-mode rust-ts-mode)) "")
   ((memq mode '(go-mode go-ts-mode)) "")
   ((memq mode '(c-mode c-ts-mode)) "")
   ((memq mode '(c++-mode c++-ts-mode)) "")
   ((memq mode '(java-mode java-ts-mode)) "")
   ((memq mode '(ruby-mode ruby-ts-mode)) "")
   ((memq mode '(swift-mode)) "")
   ((memq mode '(shell-script-mode sh-mode bash-ts-mode)) "")
   ((memq mode '(sql-mode)) "")

   ;; Web/markup
   ((memq mode '(html-mode mhtml-mode html-ts-mode)) "")
   ((memq mode '(css-mode css-ts-mode)) "")
   ((memq mode '(json-mode json-ts-mode)) "")
   ((memq mode '(yaml-mode yaml-ts-mode)) "")
   ((memq mode '(xml-mode nxml-mode)) "")
   ((memq mode '(markdown-mode gfm-mode)) "")

   ;; Config/data
   ((memq mode '(conf-mode conf-unix-mode conf-toml-mode)) "")
   ((memq mode '(makefile-mode makefile-gmake-mode)) "")
   ((memq mode '(dockerfile-mode dockerfile-ts-mode)) "")

   ;; Org/text
   ((memq mode '(org-mode)) "")
   ((memq mode '(text-mode)) "")

   ;; Version control
   ((memq mode '(magit-status-mode magit-log-mode)) "")
   ((memq mode '(diff-mode)) "")

   ;; Shell/terminal
   ((memq mode '(term-mode vterm-mode eshell-mode shell-mode)) "")
   ((memq mode '(agent-shell-mode)) "")

   ;; Default
   (t "")))

;;; Filter predicates

(defun hyalo-ibuffer--buffer-excluded-p (buf)
  "Return non-nil if BUF should be excluded from sidebar ibuffer."
  (let ((name (buffer-name buf)))
    (or
     ;; Check excluded buffer names
     (cl-some (lambda (pattern)
                (string-match-p (regexp-quote pattern) name))
              hyalo-ibuffer-excluded-buffer-names)
     ;; Check excluded modes
     (with-current-buffer buf
       (memq major-mode hyalo-ibuffer-excluded-modes))
     ;; Hidden buffers (start with space)
     (string-prefix-p " " name))))

(defun hyalo-ibuffer--file-buffer-p (buf)
  "Return non-nil if BUF is visiting a file."
  (buffer-file-name buf))

;;; Custom format

(defvar hyalo-ibuffer-sidebar-format
  '((icon 2 2 :left :elide)
    " "
    (name 30 30 :left :elide)
    " "
    (modified 1 1 :left))
  "Ibuffer format for sidebar display.
Shows icon, name, and modified indicator only.")

(define-ibuffer-column icon
  (:name "" :inline t)  ; Empty name = no header
  (hyalo-ibuffer--get-icon major-mode))

(define-ibuffer-column modified
  (:name "" :inline t)
  (if (buffer-modified-p) "●" " "))

;; Custom name column with empty header to suppress "Name" column header
(define-ibuffer-column hyalo-name
  (:name "" :inline t)  ; Empty name = no header
  (propertize (buffer-name) 'font-lock-face 'default))

;;; Parent frame handling

(defvar hyalo-ibuffer--parent-frame nil
  "The parent frame for the embedded ibuffer child-frame.
Set when the sidebar is created.")

(defun hyalo-ibuffer--get-parent-frame ()
  "Get the parent frame for buffer opens.
First tries frame parameter, then falls back to stored value."
  (or (frame-parameter (selected-frame) 'parent-frame)
      hyalo-ibuffer--parent-frame))

;;; Setup function

(defun hyalo-ibuffer--visit-buffer-in-parent (&optional noselect)
  "Visit the buffer on this line in the parent frame.
If NOSELECT is non-nil, don't select the buffer."
  (interactive "P")
  (let ((buf (ibuffer-current-buffer t)))
    (when buf
      ;; Find the parent frame
      (let ((parent (hyalo-ibuffer--get-parent-frame)))
        (if parent
            (progn
              ;; Select parent frame and display buffer there
              (select-frame-set-input-focus parent)
              (if noselect
                  (display-buffer buf)
                (switch-to-buffer buf)))
          ;; No parent frame, just switch buffer normally
          (if noselect
              (display-buffer buf)
            (switch-to-buffer buf)))))))

(defun hyalo-ibuffer-sidebar-setup ()
  "Configure ibuffer for sidebar display."
  (interactive)

  ;; CRITICAL: Disable all filter groups BEFORE setting format
  ;; These must be set before ibuffer-update to prevent "[ Default ]"
  (setq ibuffer-saved-filter-groups nil)
  (setq ibuffer-filter-groups nil)
  (setq ibuffer-current-filter-groups nil)
  (setq ibuffer-show-empty-filter-groups nil)

  ;; Set format - use hyalo-name to suppress column header
  ;; Also removes mark column for cleaner display
  (setq-local ibuffer-formats
              '((icon " " (hyalo-name 30 30 :left :elide) " " modified)))

  ;; Use never-show predicate instead of filtering qualifiers
  ;; This avoids creating filter groups entirely
  (setq-local ibuffer-filtering-qualifiers nil)
  (setq-local ibuffer-never-show-predicates
              '((lambda (buf) (hyalo-ibuffer--buffer-excluded-p buf))))

  ;; Sort by recency (most recently used first)
  (setq-local ibuffer-default-sorting-mode 'recency)

  ;; Hide header-line (column headers go in buffer without this)
  (setq-local ibuffer-use-header-line t)

  ;; Suppress all headers and summary
  (setq-local ibuffer-display-summary nil)

  ;; Expert mode suppresses some prompts
  (setq-local ibuffer-expert t)

  ;; Bind RET and mouse-1 to visit buffer in parent frame
  (local-set-key (kbd "RET") #'hyalo-ibuffer--visit-buffer-in-parent)
  (local-set-key (kbd "<mouse-1>") #'hyalo-ibuffer--visit-buffer-in-parent)
  (local-set-key (kbd "o") (lambda () (interactive) (hyalo-ibuffer--visit-buffer-in-parent t)))

  ;; Update display silently - this regenerates the buffer
  (ibuffer-update nil t)

  ;; Move to first buffer line (skip any headers)
  (goto-char (point-min))
  (forward-line 0)
  (ibuffer-forward-line 0 t))

;;; Auto-refresh

(defun hyalo-ibuffer--suppress-header-line (&rest _)
  "Ensure header-line is hidden in Hyalo sidebar buffer."
  (when (string= (buffer-name) "*Hyalo-Ibuffer*")
    (setq header-line-format nil)))

(advice-add 'ibuffer-update :after #'hyalo-ibuffer--suppress-header-line)

(defun hyalo-ibuffer--refresh ()
  "Refresh the sidebar ibuffer if it exists."
  (let ((buf (get-buffer "*Hyalo-Ibuffer*")))
    (when (and buf (buffer-live-p buf))
      (with-current-buffer buf
        (when (eq major-mode 'ibuffer-mode)
          (ibuffer-update nil t))))))

(defun hyalo-ibuffer--start-auto-refresh ()
  "Start auto-refresh timer for sidebar ibuffer."
  (when (and hyalo-ibuffer-auto-refresh-interval
             (not hyalo-ibuffer--auto-refresh-timer))
    (setq hyalo-ibuffer--auto-refresh-timer
          (run-with-timer
           hyalo-ibuffer-auto-refresh-interval
           hyalo-ibuffer-auto-refresh-interval
           #'hyalo-ibuffer--refresh))))

(defun hyalo-ibuffer--stop-auto-refresh ()
  "Stop auto-refresh timer for sidebar ibuffer."
  (when hyalo-ibuffer--auto-refresh-timer
    (cancel-timer hyalo-ibuffer--auto-refresh-timer)
    (setq hyalo-ibuffer--auto-refresh-timer nil)))

;;; Mode hook

(defun hyalo-ibuffer--setup-if-sidebar ()
  "Set up ibuffer for sidebar if this is a sidebar buffer."
  (when (frame-parameter (selected-frame) 'hyalo-embedded)
    ;; Store parent frame reference globally for visit function to use
    (setq hyalo-ibuffer--parent-frame
          (frame-parameter (selected-frame) 'parent-frame))
    (hyalo-ibuffer-sidebar-setup)
    ;; Start auto-refresh
    (hyalo-ibuffer--start-auto-refresh)))

(add-hook 'ibuffer-mode-hook #'hyalo-ibuffer--setup-if-sidebar)

;; Stop auto-refresh when ibuffer is killed
(add-hook 'kill-buffer-hook
          (lambda ()
            (when (string= (buffer-name) "*Hyalo-Ibuffer*")
              (hyalo-ibuffer--stop-auto-refresh))))

(provide 'hyalo-ibuffer)
;;; hyalo-ibuffer.el ends here
