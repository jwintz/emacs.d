;;; init-tools.el --- Development tools: project, magit, diff-hl, eglot -*- lexical-binding: t; -*-

;;; Code:

(use-package project
  :ensure nil
  :defer t
  :general
  (leader-def
    "p f" '(project-find-file :wk "find file")
    "p b" '(project-switch-to-buffer :wk "switch buffer")
    "p d" '(project-dired :wk "dired")
    "p k" '(project-kill-buffers :wk "kill buffers")
    "p p" '(project-switch-project :wk "switch project")
    "p c" '(project-compile :wk "compile")
    "p v" '(magit-project-status :wk "magit")
    "p s" '(consult-ripgrep :wk "search"))
  :custom
  (project-switch-commands
   '((project-find-file "Find file")
     (consult-ripgrep "Search" ?s)
     (project-dired "Dired")
     (magit-project-status "Magit" ?v)
     (project-eshell "Eshell"))))

(use-package magit
  :ensure t
  :defer t
  :general
  (leader-def
    "v s" '(magit-status :wk "status")
    "v l" '(magit-log :wk "log")
    "v b" '(magit-blame :wk "blame")
    "v d" '(magit-diff :wk "diff")
    "v g" '(magit-generate-commit-message :wk "generate message"))
  (:keymaps 'git-commit-mode-map
   "C-c C-g" '(magit-generate-commit-message :wk "generate message"))
  :custom
  (magit-display-buffer-function
   (lambda (buffer) (display-buffer buffer '(display-buffer-same-window))))
  :config
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t)

  (defconst magit--commit-system-message
    "You are a commit message generator. Follow these rules strictly:

1. Use conventional commits format: <type>(<scope>): <description>
2. Types: feat, fix, docs, style, refactor, perf, test, chore
3. Keep the first line under 50 characters
4. Use imperative mood (\"add\" not \"added\", \"fix\" not \"fixed\")
5. Do not end with a period
6. Be specific but concise
7. If breaking change, add \"BREAKING CHANGE:\" footer

Respond with ONLY the commit message, no explanation, no markdown, no quotes."
    "System message for Gemini CLI commit message generation.")

  (defcustom magit-commit-provider "google-gemini-cli"
    "AI provider for commit message generation (google, openai, anthropic, google-gemini-cli)."
    :type 'string
    :group 'magit)

  (defcustom magit-commit-model "gemini-3-flash-preview"
    "Model ID for commit message generation."
    :type 'string
    :group 'magit)

  (defun magit-generate-commit-message ()
    "Generate commit message using pi CLI based on staged changes.
Inserts the message at point in the commit buffer."
    (interactive)
    (let ((diff (with-temp-buffer
                  (call-process "git" nil t nil "diff" "--cached" "--no-color")
                  (buffer-string))))
      (if (string-empty-p diff)
          (message "No staged changes to generate commit message from")
        (message "Generating commit message with %s/%s..." magit-commit-provider magit-commit-model)
        (let* ((truncated-diff (if (> (length diff) 50000)
                                   (concat (substring diff 0 50000)
                                           "\n\n[Diff truncated due to length]")
                                 diff))
               (prompt (concat "\n\nGenerate a commit message for these changes:\n\n"
                               "```diff\n"
                               truncated-diff
                               "\n```"))
               (commit-message
                (with-output-to-string
                  (with-current-buffer standard-output
                    ;; Redirect stderr to /dev/null to filter CLI startup noise
                    (call-process "pi" nil (list t nil) nil
                                  "--provider" magit-commit-provider
                                  "--model" magit-commit-model
                                  "--system-prompt" magit--commit-system-message
                                  "--no-tools"
                                  "--no-session"
                                  "-p" prompt)))))
          (insert (string-trim commit-message))
          (message "Commit message inserted"))))))

(use-package diff-hl
  :ensure t
  :custom
  (diff-hl-side 'right)
  (diff-hl-margin-symbols-alist '((insert . "┃") (delete . "┃") (change . "┃")))
  :config
  ;; Ensure margin module is loaded
  (require 'diff-hl-margin)

  (defun hyalo-diff-hl--update-faces (&rest _)
    "Update diff-hl margin colors from theme."
    (let ((insert-fg (or (face-foreground 'success nil t)
                         (face-foreground 'diff-added nil t)
                         "#50a14f"))
          (delete-fg (or (face-foreground 'error nil t)
                         (face-foreground 'diff-removed nil t)
                         "#e45649"))
          (change-fg (or (face-foreground 'warning nil t)
                         (face-foreground 'diff-changed nil t)
                         "#c18401")))
      (set-face-attribute 'diff-hl-insert nil :foreground insert-fg :background 'unspecified)
      (set-face-attribute 'diff-hl-delete nil :foreground delete-fg :background 'unspecified)
      (set-face-attribute 'diff-hl-change nil :foreground change-fg :background 'unspecified)))

  (add-hook 'enable-theme-functions #'hyalo-diff-hl--update-faces)
  (hyalo-diff-hl--update-faces)

  ;; Force enable in correct order
  (global-diff-hl-mode 1)
  (diff-hl-margin-mode 1)
  (diff-hl-flydiff-mode 1)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)

  ;; Safety: Ensure margins are actually allocated
  (defun hyalo-diff-hl-ensure-margins ()
    "Ensure window margins are allocated for diff-hl."
    (when (and diff-hl-mode diff-hl-margin-mode (not (minibufferp)))
      (unless (= right-margin-width 1)
        (setq right-margin-width 1)
        (set-window-buffer (selected-window) (current-buffer)))))

  (add-hook 'window-configuration-change-hook #'hyalo-diff-hl-ensure-margins)
  (add-hook 'diff-hl-mode-hook #'hyalo-diff-hl-ensure-margins))

(use-package diffview
  :ensure t
  :general
  (leader-def
    "v v" '(hyalo-diffview-file :wk "diffview file"))
  :config
  (require 'diff-mode)

  ;; Remove backgrounds from diff faces (weight-based highlights only)
  (defun hyalo-diffview--update-faces (&rest _)
    "Remove backgrounds from diff faces while preserving theme colors."
    (dolist (face '(diff-added diff-removed diff-changed
                    diff-indicator-added diff-indicator-removed diff-indicator-changed
                    diff-refine-added diff-refine-removed diff-refine-changed))
      (when (facep face)
        (set-face-attribute face nil :background 'unspecified))))

  (add-hook 'enable-theme-functions #'hyalo-diffview--update-faces)
  (hyalo-diffview--update-faces)

  ;; Force font-lock in diffview buffers
  (add-hook 'diffview-mode-hook
            (lambda ()
              (setq-local font-lock-defaults '(diff-font-lock-keywords t nil nil nil))
              (font-lock-mode 1)
              (font-lock-ensure)))

  ;; Custom command: get git diff for current file and show in diffview
  (defun hyalo-diffview-file ()
    "Show git diff for current file in side-by-side view."
    (interactive)
    (let* ((file (buffer-file-name))
           (default-directory (if file (file-name-directory file) default-directory)))
      (unless file
        (user-error "Buffer is not visiting a file"))
      (let ((diff-output (shell-command-to-string
                          (format "git diff -- %s" (shell-quote-argument file)))))
        (if (string-empty-p diff-output)
            (message "No changes in %s" (file-name-nondirectory file))
          ;; Use a named buffer so diffview-current works correctly
          (let ((diff-buf (get-buffer-create "*git-diff-output*")))
            (with-current-buffer diff-buf
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert diff-output)
                (goto-char (point-min))
                (diff-mode)))
            (switch-to-buffer diff-buf)
            (diffview-current))))))

  ;; Scroll synchronization for side-by-side windows using post-command-hook
  (defvar hyalo-diffview--last-window-starts nil
    "Alist of (buffer-name . window-start) for tracking scroll changes.")

  (defun hyalo-diffview--mouse-scroll-command-p ()
    "Return t if current command is a mouse scroll (not click)."
    (and this-command
         (memq this-command '(mwheel-scroll mac-mwheel-scroll ultra-scroll
                               pixel-scroll-precision scroll-bar-toolkit-scroll
                               hyalo-minimap-click hyalo-minimap-drag-scroll))))

  (defun hyalo-diffview--sync-scroll-post-command ()
    "Synchronize scrolling between side-by-side windows after mouse scroll."
    ;; Only sync for mouse scroll commands - let scroll-all-mode handle keyboard
    (when (hyalo-diffview--mouse-scroll-command-p)
      (when-let* ((win (selected-window))
                  (buf (window-buffer win))
                  (name (buffer-name buf)))
        (when (string-match-p "^\\*side-by-side" name)
          (let* ((current-start (window-start win))
                 (last-start (alist-get name hyalo-diffview--last-window-starts nil nil #'equal)))
            ;; Only sync if window-start changed
            (unless (eq current-start last-start)
              (setf (alist-get name hyalo-diffview--last-window-starts nil nil #'equal) current-start)
              (let* ((other-buf (if (string= name "*side-by-side-1*")
                                    (get-buffer "*side-by-side-2*")
                                  (get-buffer "*side-by-side-1*")))
                     (line (line-number-at-pos current-start)))
                (when other-buf
                  (dolist (other-win (get-buffer-window-list other-buf nil t))
                    (let ((pos (with-current-buffer other-buf
                                 (save-excursion
                                   (goto-char (point-min))
                                   (forward-line (1- line))
                                   (point)))))
                      (unless (eq (window-start other-win) pos)
                        (set-window-start other-win pos)
                        (setf (alist-get (buffer-name other-buf) hyalo-diffview--last-window-starts nil nil #'equal) pos))))))))))))

  (add-hook 'diffview-mode-hook
            (lambda ()
              (add-hook 'post-command-hook #'hyalo-diffview--sync-scroll-post-command nil t))))

(use-package swift-mode
  :ensure t
  :defer t
  :mode "\\.swift\\'")

(use-package eglot
  :disabled t
  :ensure nil
  :defer t
  :hook (swift-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '(swift-mode . ("xcrun" "sourcekit-lsp")))
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0))

(provide 'init-tools)

;;; init-tools.el ends here
