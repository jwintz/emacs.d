;;; hyalo-agent-extras.el --- Agent shell integration for Hyalo -*- lexical-binding: t -*- no-byte-compile: t -*-

;; Copyright (C) 2025
;; Author: Julien Wintz <julien.wintz@inria.fr>
;; Keywords: convenience, tools, extensions
;; Package-Requires: ((emacs "30.1") (agent-shell "0.0") (nerd-icons "0.0"))

;;; Commentary:

;; Customizes agent-shell UI elements.
;; - Disables the native agent-shell header/overlay in favor of Hyalo's Swift Inspector Header.
;; - Replaces permission and thought process icons with nerd-icons.
;; - Disables the ASCII welcome message banner.
;; - Adds padding to thought process blocks.
;; - Syncs agent state (busy/idle, model name) to the Swift inspector header.

;;; Code:

(require 'cl-lib)
(require 'agent-shell)
(require 'nerd-icons)
(require 'hyalo)

(defgroup hyalo-agent-extras nil
  "Hyalo agent-shell integration."
  :group 'hyalo
  :prefix "hyalo-agent-extras-")

;;; Inspector Header Sync

(defun hyalo-agent-extras--get-agent-icon (buffer-name)
  "Get icon name for BUFFER-NAME."
  (pcase buffer-name
    ("Claude" "sparkles")
    ("Gemini" "sparkles") 
    ("Cursor" "cursorarrow.rays")
    (_ "sparkles")))

(defun hyalo-agent-extras--update-inspector-header (&optional busy-override)
  "Update the inspector header via Swift.
If BUSY-OVERRIDE is non-nil, force that busy state.
Otherwise, infer from agent-shell state."
  (when (and (fboundp 'hyalo-set-inspector-header)
             (boundp 'agent-shell--state)
             agent-shell--state)
    (let* ((buffer-name (or (map-nested-elt agent-shell--state '(:agent-config :buffer-name)) "Agent"))
           (model-name (or (map-elt (seq-find (lambda (model)
                                                (string= (map-elt model :model-id)
                                                         (map-nested-elt agent-shell--state '(:session :model-id))))
                                              (map-nested-elt agent-shell--state '(:session :models)))
                                    :name)
                           (map-nested-elt agent-shell--state '(:session :model-id))))
           (status (map-nested-elt agent-shell--state '(:heartbeat :status)))
           (busy (if (not (null busy-override))
                     busy-override
                   (memq status '(busy started))))
           (icon (hyalo-agent-extras--get-agent-icon buffer-name))
           (title (if model-name
                      (format "%s (%s)" buffer-name model-name)
                    buffer-name)))
       (hyalo-log "Agent Extras: Updating header. Title: %s, Busy: %s (Status: %s, Override: %s)" 
                title busy status busy-override)
       (hyalo-set-inspector-header title icon busy))))

;;; Advice for Syncing

(defun hyalo-agent-extras--on-heartbeat-start (&rest _args)
  "Called when agent starts thinking."
  (hyalo-log "Agent Extras: Heartbeat START")
  (hyalo-agent-extras--update-inspector-header t))

(defun hyalo-agent-extras--on-heartbeat-stop (&rest _args)
  "Called when agent stops thinking."
  (hyalo-log "Agent Extras: Heartbeat STOP")
  (hyalo-agent-extras--update-inspector-header nil))

(defun hyalo-agent-extras--on-header-update (&rest _args)
  "Called when agent header would update (model/mode change).
Only sync if NOT busy (busy frames are handled by heartbeat start/stop to avoid spam)."
  (when (boundp 'agent-shell--state)
    (let ((status (map-nested-elt agent-shell--state '(:heartbeat :status))))
      (unless (memq status '(busy started))
        (hyalo-log "Agent Extras: Header update (idle)")
        (hyalo-agent-extras--update-inspector-header)))))

;;; Fragment Update Override (Thought Process Padding)

(defun hyalo-agent-extras--update-fragment-padding (orig-fun &rest args)
  "Advice to add padding to agent thought process fragments.
Injects display properties for robust vertical separation."
  (let* ((block-id (plist-get args :block-id))
         (body (plist-get args :body))
         (append (plist-get args :append)))
    (if (and block-id
             (string-match-p "agent_thought_chunk" block-id)
             body)
        (let* ((top-spacer (propertize "\n" 'display '(space :height 1.5)))
               (bottom-spacer (propertize "\n" 'display '(space :height 1.5)))
               (new-body (if append
                             body
                           (concat top-spacer body bottom-spacer))))
           (apply orig-fun (plist-put args :body new-body)))
      (apply orig-fun args))))

;;; Customization Logic

(defvar hyalo-agent-extras--original-icons nil)
(defvar hyalo-agent-extras--original-welcome-message nil)
(defvar hyalo-agent-extras--original-header-style nil)

(defun hyalo-agent-extras--apply-customizations ()
  "Apply customizations to agent-shell settings."
  (unless hyalo-agent-extras--original-icons
    (setq hyalo-agent-extras--original-icons
          (cons agent-shell-permission-icon agent-shell-thought-process-icon))
    (setq hyalo-agent-extras--original-welcome-message agent-shell-show-welcome-message)
    (setq hyalo-agent-extras--original-header-style agent-shell-header-style))

  ;; Icons (with fallbacks)
  (setq agent-shell-permission-icon
        (or (nerd-icons-codicon "nf-cod-warning" :face 'font-lock-warning-face) "⚠"))
  (setq agent-shell-thought-process-icon
        (or (nerd-icons-codicon "nf-cod-lightbulb" :face 'font-lock-doc-face) "💡"))

  ;; Welcome message
  (setq agent-shell-show-welcome-message nil)

  ;; Disable native header (the "overlay" effect)
  (setq agent-shell-header-style nil)
  
  ;; Install sync hooks
  (advice-add 'agent-shell-heartbeat-start :after #'hyalo-agent-extras--on-heartbeat-start)
  (advice-add 'agent-shell-heartbeat-stop :after #'hyalo-agent-extras--on-heartbeat-stop)
  (advice-add 'agent-shell--update-header-and-mode-line :after #'hyalo-agent-extras--on-header-update))

(defun hyalo-agent-extras--restore-customizations ()
  "Restore original agent-shell settings."
  (when hyalo-agent-extras--original-icons
    (setq agent-shell-permission-icon (car hyalo-agent-extras--original-icons))
    (setq agent-shell-thought-process-icon (cdr hyalo-agent-extras--original-icons))
    (setq hyalo-agent-extras--original-icons nil)
    (setq agent-shell-show-welcome-message hyalo-agent-extras--original-welcome-message)
    (setq agent-shell-header-style hyalo-agent-extras--original-header-style))
  
  ;; Remove sync hooks
  (advice-remove 'agent-shell-heartbeat-start #'hyalo-agent-extras--on-heartbeat-start)
  (advice-remove 'agent-shell-heartbeat-stop #'hyalo-agent-extras--on-heartbeat-stop)
  (advice-remove 'agent-shell--update-header-and-mode-line #'hyalo-agent-extras--on-header-update))

;;; Mode Definition

;;;###autoload
(define-minor-mode hyalo-agent-extras-mode
  "Minor mode to customize agent-shell UI with nerd-icons and minimal style."
  :global t
  :group 'hyalo-agent-extras
  (if hyalo-agent-extras-mode
      (progn
        ;; Add padding to thought process
        (advice-add 'agent-shell--update-fragment :around #'hyalo-agent-extras--update-fragment-padding)
        ;; Apply variables and hooks
        (hyalo-agent-extras--apply-customizations))
    ;; Restore
    (advice-remove 'agent-shell--update-fragment #'hyalo-agent-extras--update-fragment-padding)
    (hyalo-agent-extras--restore-customizations)))

(provide 'hyalo-agent-extras)
;;; hyalo-agent-extras.el ends here
