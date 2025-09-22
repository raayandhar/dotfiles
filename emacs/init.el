;; -*- mode: Emacs-Lisp; -*-
;;; Package summary
;;; Commentary
;; Increase GC threshold for faster startup
(setq gc-cons-threshold (* 50 1000 1000))

;; Inhibit startup screen
(setq inhibit-startup-screen t)

;; Load custom file for Customize settings, if it exists
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Load the literate configuration from config.org
(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))

;; Lower GC threshold after startup
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 2 1000 1000))))
