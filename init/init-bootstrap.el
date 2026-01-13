;;; init-bootstrap.el --- Bootstrap configuration -*- lexical-binding: t; -*-

;;; Code:

;;; ===========================================================================
;;; Performance & Output
;;; ===========================================================================

(setq debug-on-error t)

;; Optimization: Temporarily reduce garbage collection and file handlers during startup
(defvar hyalo--default-gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)
(defvar hyalo--default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold hyalo--default-gc-cons-threshold)
            (setq file-name-handler-alist hyalo--default-file-name-handler-alist)))

;; Prevent early UI allocation
(setq-default tool-bar-mode nil)
(setq-default scroll-bar-mode nil)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)

;; Suppress warnings during startup
(setq byte-compile-warnings nil)
(setq warning-suppress-types '((comp) (bytecomp)))
(setq warning-minimum-level :error)

(defvar hyalo-init-start-time (float-time))
(defun hyalo-log-step (msg)
  (let ((now (float-time)))
    (if (and (featurep 'elog) (boundp 'emacs-logger))
        (elog-info emacs-logger "[init] %s (%.3fs)" msg (- now hyalo-init-start-time))
      (message "[init] %s (%.3fs)" msg (- now hyalo-init-start-time)))))

;;; ===========================================================================
;;; Directories
;;; ===========================================================================

(setq user-emacs-directory (expand-file-name ".local/" emacs-config-dir))

(dolist (subdir '("" "auto-save" "auto-save-list" "transient" "eshell" "etc"))
  (make-directory (expand-file-name subdir user-emacs-directory) t))

;; Generated files
(setq auto-save-list-file-prefix (locate-user-emacs-file "auto-save-list/.saves-"))
(setq auto-save-file-name-transforms `((".*" ,(locate-user-emacs-file "auto-save/") t)))
(setq custom-file (locate-user-emacs-file "etc/custom.el"))
(setq package-user-dir (locate-user-emacs-file "elpa/"))

(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))

;;; ===========================================================================
;;; Package System
;;; ===========================================================================

(defvar demap--tools-demap-defined-start-p t)

;; (setq package-quickstart t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package elog
  :ensure t
  :vc (:url "https://github.com/Kinneyzhang/elog" :rev :newest)
  :demand t
  :config
  ;; Initialize main Emacs logger
  (defvar emacs-logger
    (elog-logger
     :name "emacs"
     :level 'info
     :buffer "*elog*"
     :handlers '(buffer))))

(hyalo-log-step "Packages initialized")

;;;; Load Paths

(add-to-list 'load-path (expand-file-name "init" emacs-config-dir))
(add-to-list 'load-path (expand-file-name "lisp" emacs-config-dir))

;;; ===========================================================================
;;; Environment & Shell
;;; ===========================================================================

;; Initialize environment (PATH, exec-path)
(load (expand-file-name "conf/eshlogin" emacs-config-dir) 'noerror 'nomessage)

(provide 'init-bootstrap)
;;; init-bootstrap.el ends here
