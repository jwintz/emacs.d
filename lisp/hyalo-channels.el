;;; hyalo-channels.el --- Channel-based Swift communication -*- lexical-binding: t -*- no-byte-compile: t -*-

;; Copyright (C) 2025
;; Author: Julien Wintz <julien.wintz@inria.fr>
;; Keywords: frames, sidebar, channels

;;; Commentary:

;; Centralized channel management for Emacs ↔ Swift communication.
;; Replaces embedded child-frames with data-driven SwiftUI views.
;;
;; Data flows:
;; - Buffer list: Emacs → Swift (via hyalo-update-buffer-list)
;; - File tree: Emacs → Swift (via hyalo-update-file-tree)
;; - Buffer switch: Swift → Emacs (via channel callbacks)
;; - File open: Swift → Emacs (via channel callbacks)

;;; Code:

(require 'hyalo)
(require 'project)
(require 'json)

(defgroup hyalo-channels nil
  "Hyalo channel-based communication."
  :group 'hyalo
  :prefix "hyalo-channels-")

(defcustom hyalo-channels-buffer-list-delay 0.2
  "Delay in seconds before sending buffer list updates.
Debounces rapid buffer changes."
  :type 'number
  :group 'hyalo-channels)

(defcustom hyalo-channels-file-tree-depth 3
  "Maximum depth for file tree traversal.
Higher values show more nested directories but may be slower."
  :type 'integer
  :group 'hyalo-channels)

;;; State Variables

(defvar hyalo-channels--buffer-timer nil
  "Timer for debounced buffer list updates.")

(defvar hyalo-channels--last-buffer-list nil
  "Cache of last sent buffer list for change detection.")

(defvar hyalo-channels--last-project-root nil
  "Cache of last project root.")

(defvar hyalo-channels--channel-active nil
  "Non-nil when sidebar channel is active.")

;;; Buffer List Provider

(defun hyalo-channels--buffer-info (buf)
  "Generate buffer info alist for BUF."
  (when (buffer-live-p buf)
    (let* ((name (buffer-name buf))
           (path (buffer-file-name buf))
           (modified (buffer-modified-p buf))
           (mode (buffer-local-value 'major-mode buf))
           ;; Determine icon based on mode
           (icon (cond
                  ((derived-mode-p 'prog-mode) "doc.text")
                  ((derived-mode-p 'text-mode) "doc.plaintext")
                  ((derived-mode-p 'dired-mode) "folder")
                  (t "doc"))))
      `((id . ,name)
        (name . ,(if path (file-name-nondirectory path) name))
        (path . ,path)
        (modified . ,(if modified t :json-false))
        (icon . ,icon)))))

(defun hyalo-channels--collect-buffers ()
  "Collect visible/useful buffers for sidebar display."
  (let ((buffers nil))
    (dolist (buf (buffer-list))
      (let ((name (buffer-name buf)))
        ;; Include file-backed buffers and some special buffers
        (when (and name
                   (not (string-prefix-p " " name))  ; Hidden buffers
                   (or (buffer-file-name buf)
                       ;; Include some useful non-file buffers
                       (string-prefix-p "*scratch*" name)
                       (string-prefix-p "*Messages*" name)))
          (when-let ((info (hyalo-channels--buffer-info buf)))
            (push info buffers)))))
    (nreverse buffers)))

(defun hyalo-channels--buffer-list-to-json ()
  "Generate JSON string for current buffer list."
  (let ((buffers (hyalo-channels--collect-buffers)))
    (json-encode buffers)))

(defun hyalo-channels--update-buffer-list ()
  "Send buffer list update to Swift."
  (when (and hyalo-channels--channel-active
             (hyalo-available-p)
             (fboundp 'hyalo-update-buffer-list))
    (let ((json (hyalo-channels--buffer-list-to-json)))
      (unless (equal json hyalo-channels--last-buffer-list)
        (setq hyalo-channels--last-buffer-list json)
        (hyalo-update-buffer-list json)))))

(defun hyalo-channels--schedule-buffer-update ()
  "Schedule a debounced buffer list update."
  (when hyalo-channels--buffer-timer
    (cancel-timer hyalo-channels--buffer-timer))
  (setq hyalo-channels--buffer-timer
        (run-with-timer hyalo-channels-buffer-list-delay nil
                        #'hyalo-channels--update-buffer-list)))

;;; File Tree Provider

(defun hyalo-channels--file-tree-node (path depth)
  "Generate file tree node for PATH up to DEPTH levels."
  (let* ((name (file-name-nondirectory (directory-file-name path)))
         (is-dir (file-directory-p path))
         (icon (if is-dir "folder" "doc")))
    (if (and is-dir (> depth 0))
        ;; Directory with children
        (let ((children nil))
          (condition-case nil
              (dolist (entry (directory-files path nil "^[^.]" t))
                (let ((full-path (expand-file-name entry path)))
                  (unless (or (string-prefix-p "." entry)
                              (member entry '("node_modules" ".git" "__pycache__" ".build")))
                    (push (hyalo-channels--file-tree-node full-path (1- depth)) children))))
            (file-error nil))  ; Ignore permission errors
          `((id . ,path)
            (name . ,name)
            (isDirectory . t)
            (icon . "folder")
            (children . ,(nreverse children))))
      ;; File or max depth reached
      `((id . ,path)
        (name . ,name)
        (isDirectory . ,(if is-dir t :json-false))
        (icon . ,icon)))))

(defun hyalo-channels--get-project-root ()
  "Get project root: git root, project.el root, or default-directory."
  (or (when-let ((proj (project-current)))
        (if (fboundp 'project-root)
            (project-root proj)
          (car (project-roots proj))))
      (locate-dominating-file default-directory ".git")
      default-directory))

(defun hyalo-channels--file-tree-to-json ()
  "Generate JSON string for file tree at project root."
  (let* ((root (expand-file-name (hyalo-channels--get-project-root)))
         (tree (hyalo-channels--file-tree-node root hyalo-channels-file-tree-depth)))
    (json-encode tree)))

(defun hyalo-channels--update-file-tree ()
  "Send file tree update to Swift."
  (when (and hyalo-channels--channel-active
             (hyalo-available-p)
             (fboundp 'hyalo-update-file-tree))
    (let* ((root (hyalo-channels--get-project-root))
           (json (hyalo-channels--file-tree-to-json)))
      (setq hyalo-channels--last-project-root root)
      (hyalo-update-file-tree json))))

;;; Selected Buffer Tracking

(defun hyalo-channels--update-selected-buffer ()
  "Send current buffer selection and active file path to Swift."
  (when (and hyalo-channels--channel-active
             (hyalo-available-p)
             (fboundp 'hyalo-set-selected-buffer))
    (let ((name (buffer-name)))
      (hyalo-set-selected-buffer name))
    (when (fboundp 'hyalo-set-active-file-path)
      (let ((path (buffer-file-name)))
        (hyalo-set-active-file-path (if path (expand-file-name path) ""))))))

;;; Callback Handlers (Swift → Emacs)

(defun hyalo-channels--handle-switch-buffer (buffer-name)
  "Handle buffer switch request from Swift.
BUFFER-NAME is the name of the buffer to switch to."
  (when-let ((buf (get-buffer buffer-name)))
    (switch-to-buffer buf)))

(defun hyalo-channels--handle-find-file (file-path)
  "Handle file open request from Swift.
FILE-PATH is the absolute path to open."
  (when (and file-path (file-exists-p file-path))
    (if (file-directory-p file-path)
        (dired file-path)
      (find-file file-path))))

;;; Project Switch Integration

(defun hyalo-channels--on-project-switch (&rest _)
  "Update file tree when project changes."
  (run-with-timer 0.5 nil #'hyalo-channels--update-file-tree))

;;; Setup and Teardown

(defun hyalo-channels--maybe-update-project-root ()
  "Check if project root changed and update file tree if so."
  (when hyalo-channels--channel-active
    (let ((new-root (hyalo-channels--get-project-root)))
      (unless (equal new-root hyalo-channels--last-project-root)
        (hyalo-channels--update-file-tree)))))

(defun hyalo-channels-setup ()
  "Initialize all Emacs→Swift data channels."
  (when (hyalo-available-p)
    ;; Setup sidebar channel for callbacks
    (when (fboundp 'hyalo-setup-sidebar-channel)
      (condition-case err
          (progn
            (hyalo-setup-sidebar-channel)
            (setq hyalo-channels--channel-active t)
            (hyalo-log 'channels "Sidebar channel active"))
        (error
         (hyalo-log 'channels "Failed to setup channel: %s" err)
         (setq hyalo-channels--channel-active nil))))

    ;; Hook into buffer changes (debounced)
    (add-hook 'buffer-list-update-hook #'hyalo-channels--schedule-buffer-update)

    ;; Hook into window/buffer selection
    (add-hook 'window-buffer-change-functions
              (lambda (&rest _)
                (hyalo-channels--update-selected-buffer)
                (hyalo-channels--maybe-update-project-root)))

    ;; Hook into file opening (detects new project)
    (add-hook 'find-file-hook #'hyalo-channels--maybe-update-project-root)

    ;; Hook into project switch
    (add-hook 'project-switch-hook #'hyalo-channels--on-project-switch)
    (advice-add 'project-switch-project :after #'hyalo-channels--on-project-switch)

    ;; Initial updates
    (run-with-timer 0.5 nil #'hyalo-channels--update-buffer-list)
    (run-with-timer 0.5 nil #'hyalo-channels--update-file-tree)
    (run-with-timer 0.5 nil #'hyalo-channels--update-selected-buffer)

    (hyalo-log 'channels "Channels initialized")))

(defun hyalo-channels-teardown ()
  "Teardown all channels."
  (remove-hook 'buffer-list-update-hook #'hyalo-channels--schedule-buffer-update)
  (remove-hook 'find-file-hook #'hyalo-channels--maybe-update-project-root)
  (remove-hook 'project-switch-hook #'hyalo-channels--on-project-switch)
  (advice-remove 'project-switch-project #'hyalo-channels--on-project-switch)
  (when hyalo-channels--buffer-timer
    (cancel-timer hyalo-channels--buffer-timer)
    (setq hyalo-channels--buffer-timer nil))
  (setq hyalo-channels--channel-active nil)
  (hyalo-log 'channels "Channels teardown complete"))

;;;###autoload
(define-minor-mode hyalo-channels-mode
  "Global minor mode for Hyalo channel-based sidebar data."
  :global t
  :group 'hyalo-channels
  (if hyalo-channels-mode
      (hyalo-channels-setup)
    (hyalo-channels-teardown)))

(provide 'hyalo-channels)
;;; hyalo-channels.el ends here
