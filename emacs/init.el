;; -*- mode: Emacs-Lisp; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; init.el
;; Minimal Emacs startup file that loads config.org.
;; No explicit (package-initialize) is needed in Emacs 27+.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Increase GC threshold for faster startup
(setq gc-cons-threshold (* 50 1000 1000))

;; Inhibit startup screen
(setq inhibit-startup-screen t)

;; Initialize package system (we set archives in config.org)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Load custom file for Customize settings, if it exists
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Bootstrap use-package if necessary
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Load the literate configuration from config.org
(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))

;; Lower GC threshold after startup
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 2 1000 1000))))
