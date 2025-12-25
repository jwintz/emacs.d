;;; hyalo-module-traffic-lights.el --- Traffic light control for hyalo-module -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; Author: Julien Wintz <julien.wintz@inria.fr>
;; Version: 1.0.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: frames, faces, macos

;;; Commentary:

;; Traffic light button control for hyalo-module.
;; Handles:
;; - Auto-hide behavior (show on hover)
;; - Manual show/hide commands
;;
;; Usage:
;;   (require 'hyalo-module-traffic-lights)
;;   (hyalo-module-traffic-lights-mode 1)

;;; Code:

(require 'hyalo-module)

(defgroup hyalo-module-traffic-lights nil
  "Traffic light settings for hyalo-module."
  :group 'hyalo-module
  :prefix "hyalo-module-traffic-lights-")

(defcustom hyalo-module-traffic-lights-auto-hide t
  "When non-nil, auto-hide traffic lights and show on hover."
  :type 'boolean
  :group 'hyalo-module-traffic-lights)

;;; Public API

(defun hyalo-module-traffic-lights-show ()
  "Show the traffic light buttons."
  (interactive)
  (when (and (hyalo-module-available-p) (fboundp 'hyalo-show-traffic-lights))
    (hyalo-show-traffic-lights)))

(defun hyalo-module-traffic-lights-hide ()
  "Hide the traffic light buttons."
  (interactive)
  (when (and (hyalo-module-available-p) (fboundp 'hyalo-hide-traffic-lights))
    (hyalo-hide-traffic-lights)))

(defun hyalo-module-traffic-lights-set-auto-hide (enabled)
  "Set traffic lights auto-hide behavior.
When ENABLED is non-nil, traffic lights hide automatically and show on hover."
  (interactive "P")
  (setq hyalo-module-traffic-lights-auto-hide enabled)
  (when (and (hyalo-module-available-p) (fboundp 'hyalo-set-traffic-lights-auto-hide))
    (hyalo-set-traffic-lights-auto-hide enabled))
  (hyalo-module-log "Traffic lights auto-hide %s"
           (if enabled "enabled" "disabled")))

(defun hyalo-module-traffic-lights-toggle-auto-hide ()
  "Toggle traffic lights auto-hide behavior."
  (interactive)
  (hyalo-module-traffic-lights-set-auto-hide (not hyalo-module-traffic-lights-auto-hide)))

;;; Mode Definition

(defun hyalo-module-traffic-lights--enable ()
  "Enable traffic lights management."
  (hyalo-module-ensure)
  (when (fboundp 'hyalo-set-traffic-lights-auto-hide)
    (hyalo-set-traffic-lights-auto-hide hyalo-module-traffic-lights-auto-hide))
  (hyalo-module-log "Traffic lights: Enabled (auto-hide: %s)"
           hyalo-module-traffic-lights-auto-hide))

(defun hyalo-module-traffic-lights--disable ()
  "Disable traffic lights management."
  ;; Show traffic lights when disabling
  (when (and (hyalo-module-available-p) (fboundp 'hyalo-set-traffic-lights-auto-hide))
    (hyalo-set-traffic-lights-auto-hide nil))
  (hyalo-module-log "Traffic lights: Disabled"))

;;;###autoload
(define-minor-mode hyalo-module-traffic-lights-mode
  "Minor mode for hyalo-module traffic light control.
When enabled, manages the macOS window traffic light buttons."
  :global t
  :lighter " ηTrafficLights"
  :group 'hyalo-module-traffic-lights
  (if hyalo-module-traffic-lights-mode
      (hyalo-module-traffic-lights--enable)
    (hyalo-module-traffic-lights--disable)))

(provide 'hyalo-module-traffic-lights)
;;; hyalo-module-traffic-lights.el ends here
