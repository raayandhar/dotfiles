;; -*- mode: Emacs-Lisp; -*-
;; Loads after early-init.el

;; Temporarily increase the garbage collection (GC) threshold during startup.
(setq gc-cons-threshold (* 50 1000 1000))
;; Skip the default Emacs startup screen (the one with tutorial links)
(setq inhibit-startup-screen t)
;; Emacs' "Customize" interface saves settings to this file.
;; By default it writes to init.el, which is messy. We isolate it here.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Bootstrap straight.el (needed before org-babel-load-file)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Load org via straight BEFORE org-babel-load-file (prevents version mismatch)
(straight-use-package 'org)

;; Load literate configuration
(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
;; Restore garbage collection threshold
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 2 1000 1000))))

;;; init.el ends here
