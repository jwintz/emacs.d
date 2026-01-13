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
(require 'ibuf-ext)  ; Required for ibuffer-never-show-predicates
(require 'hyalo-icons)
(require 'cl-lib)
(require 'hyalo)

(defgroup hyalo-ibuffer nil
  "Hyalo sidebar ibuffer configuration."
  :group 'hyalo-sidebar
  :prefix "hyalo-ibuffer-")

(defcustom hyalo-ibuffer-excluded-buffer-names
  '("*Messages*" "*Completions*" "*Help*" "*Backtrace*"
    "*Compile-Log*" "*scratch*" "*Warnings*" "*Ibuffer*"
    " *Minibuf-" " *Echo Area" " *code-conversion-work*"
    " *Treemacs-" "*tramp/"
    "copilot-stderr"
    "*copilot events*" "*copilot-language-server-log*"
;   "~/.config/emacs/.local/copilot-stderr"
    "*elog*" "*Minimap*")
  "List of buffer name patterns to exclude from sidebar ibuffer.
Patterns starting with space match hidden buffers.
Patterns are matched as substrings of buffer names."
  :type '(repeat string)
  :group 'hyalo-ibuffer)

(defcustom hyalo-ibuffer-excluded-modes
  '(special-mode help-mode completion-list-mode
    Custom-mode messages-buffer-mode
    treemacs-mode ibuffer-mode dired-mode
    minibuffer-inactive-mode echo-area-mode
    dired-sidebar-mode pi-coding-agent-chat-mode pi-coding-agent-input-mode)
  "List of major modes to exclude from sidebar ibuffer."
  :type '(repeat symbol)
  :group 'hyalo-ibuffer)

(defcustom hyalo-ibuffer-show-modified-only nil
  "When non-nil, only show modified file buffers."
  :type 'boolean
  :group 'hyalo-ibuffer)

;;; Icon support

(defvar hyalo-ibuffer--icon-cache (make-hash-table :test 'equal)
  "Cache for buffer icons.")

(defun hyalo-ibuffer--get-icon (mode)
  "Get icon for major MODE.
Icon inherits its color from context (no face applied)."
  (or (gethash mode hyalo-ibuffer--icon-cache)
      (let ((computed (hyalo-ibuffer--compute-icon mode)))
        (puthash mode computed hyalo-ibuffer--icon-cache)
        computed)))

(defun hyalo-ibuffer--compute-icon (mode)
  "Compute icon string for major MODE using hyalo-icons wrapper.
Returns icon with face stripped - color inherited from context."
  (if (fboundp 'hyalo-icons-for-mode)
      (hyalo-icons-for-mode mode)
    ""))

;;; Filter predicates

(defun hyalo-ibuffer--buffer-excluded-p (buf)
  "Return non-nil if BUF should be excluded from sidebar ibuffer."
  (let ((name (buffer-name buf)))
    (or
     ;; Hidden buffers (start with space) - check first for efficiency
     (string-prefix-p " " name)
     ;; Check excluded buffer names (substring match)
     (cl-some (lambda (pattern)
                (and (stringp pattern)
                     (string-match-p (regexp-quote pattern) name)))
              hyalo-ibuffer-excluded-buffer-names)
     ;; Check excluded modes
     (with-current-buffer buf
       (cl-some (lambda (mode)
                  (derived-mode-p mode))
                hyalo-ibuffer-excluded-modes)))))

(defun hyalo-ibuffer--file-buffer-p (buf)
  "Return non-nil if BUF is visiting a file."
  (buffer-file-name buf))

;;; Custom format

(defvar hyalo-ibuffer-sidebar-format
  '((icon 2 2 :left :elide)
    " "
    (hyalo-name 30 30 :left :elide)
    "      "
    modified)

  "Ibuffer format for sidebar display.
Shows icon, name, and modified indicator only.")

(define-ibuffer-column icon
  (:name "" :inline t)  ; Empty name = no header
  (or (hyalo-ibuffer--get-icon major-mode) ""))

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

;;; Hook-based refresh (must be defined before setup function)

(defun hyalo-ibuffer--cleanup-display (&rest _)
  "Clean up ibuffer display: remove headers, filter groups, empty lines."
  (when (string= (buffer-name) "*Hyalo-Ibuffer*")
    (setq header-line-format nil)
    (let ((inhibit-read-only t))
      ;; Remove filter group headers like "[ Default ]"
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^\\[ .* \\]$" nil t)
          (delete-region (line-beginning-position)
                         (min (1+ (line-end-position)) (point-max)))))
      ;; Remove leading empty lines
      (save-excursion
        (goto-char (point-min))
        (while (and (< (point) (point-max))
                    (looking-at "^[ \t]*$"))
          (delete-region (point) (min (1+ (line-end-position)) (point-max)))))
      ;; Remove trailing empty lines
      (save-excursion
        (goto-char (point-max))
        (while (and (> (point) (point-min))
                    (looking-back "^[ \t]*\n" nil))
          (delete-char -1))))))

(advice-add 'ibuffer-update :after #'hyalo-ibuffer--cleanup-display)

(defvar hyalo-ibuffer--refreshing nil
  "Non-nil when refresh is in progress, to prevent recursion.")

(defun hyalo-ibuffer--refresh ()
  "Refresh the sidebar ibuffer if it exists."
  (unless hyalo-ibuffer--refreshing
    (let ((buf (get-buffer "*Hyalo-Ibuffer*")))
      (when (and buf
                 (buffer-live-p buf)
                 ;; Don't refresh if we're in the ibuffer itself
                 (not (eq (current-buffer) buf)))
        (let ((hyalo-ibuffer--refreshing t))
          (with-current-buffer buf
            (when (eq major-mode 'ibuffer-mode)
              (let ((inhibit-message t))
                (ibuffer-update nil t)))))))))

(defvar hyalo-ibuffer--refresh-hook-installed nil
  "Non-nil when buffer-list-update-hook is installed.")

(defun hyalo-ibuffer--install-refresh-hook ()
  "Install hook to refresh ibuffer on buffer list changes."
  (unless hyalo-ibuffer--refresh-hook-installed
    (add-hook 'buffer-list-update-hook #'hyalo-ibuffer--refresh)
    (setq hyalo-ibuffer--refresh-hook-installed t)))

(defun hyalo-ibuffer--remove-refresh-hook ()
  "Remove the buffer list update hook."
  (when hyalo-ibuffer--refresh-hook-installed
    (remove-hook 'buffer-list-update-hook #'hyalo-ibuffer--refresh)
    (setq hyalo-ibuffer--refresh-hook-installed nil)))

;;; Visit buffer in parent frame

(defun hyalo-ibuffer--visit-buffer-in-parent (&optional noselect)
  "Visit the buffer on this line in the parent frame.
If NOSELECT is non-nil, don't select the buffer."
  (interactive "P")
  (condition-case nil
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
                (switch-to-buffer buf))))))
    (error nil)))  ; Silently ignore "No buffer on this line"

;;; Setup function

(defun hyalo-ibuffer-sidebar-setup ()
  "Configure ibuffer for sidebar display.
Minimal display: just buffer list, no headers, no empty lines."
  (interactive)
  (hyalo-log 'ibuffer "Setup: Starting configuration...")

  ;; CRITICAL: Disable all filter groups BEFORE setting format
  ;; These must be set before ibuffer-update to prevent "[ Default ]"
  (setq ibuffer-saved-filter-groups nil)
  (setq ibuffer-filter-groups nil)
  (setq ibuffer-current-filter-groups nil)
  (setq ibuffer-show-empty-filter-groups nil)

  ;; Set format - use the defined sidebar format variable
  (setq-local ibuffer-formats (list hyalo-ibuffer-sidebar-format))

  ;; Use never-show predicate instead of filtering qualifiers
  ;; This avoids creating filter groups entirely
  ;; NOTE: Must use setq (not setq-local) as this is read during buffer list generation
  (setq-local ibuffer-filtering-qualifiers nil)
  (setq ibuffer-never-show-predicates
        (list #'hyalo-ibuffer--buffer-excluded-p))

  ;; Sort by creation order (first opened first)
  ;; Options: 'recency, 'alphabetic, 'major-mode, 'filename/process, 'size
  (setq-local ibuffer-default-sorting-mode 'alphabetic)

  ;; Completely disable header-line to remove column headers
  (setq-local ibuffer-use-header-line nil)
  (setq-local header-line-format nil)

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

  ;; Clean up any leading/trailing empty lines
  (hyalo-ibuffer--cleanup-display)

  ;; Move to first buffer line
  (goto-char (point-min))
  (ibuffer-forward-line 0 t)

  ;; Install refresh hook
  (hyalo-ibuffer--install-refresh-hook))

;;; Mode hook

(defun hyalo-ibuffer--setup-if-sidebar ()
  "Set up ibuffer for sidebar if this is a sidebar buffer."
  (when (frame-parameter (selected-frame) 'hyalo-embedded)
    ;; Store parent frame reference globally for visit function to use
    (setq hyalo-ibuffer--parent-frame
          (frame-parameter (selected-frame) 'parent-frame))
    (hyalo-ibuffer-sidebar-setup)))

(add-hook 'ibuffer-mode-hook #'hyalo-ibuffer--setup-if-sidebar)

;; Remove hook when ibuffer is killed
(add-hook 'kill-buffer-hook
          (lambda ()
            (when (string= (buffer-name) "*Hyalo-Ibuffer*")
              (hyalo-ibuffer--remove-refresh-hook))))

(provide 'hyalo-ibuffer)
;;; hyalo-ibuffer.el ends here
