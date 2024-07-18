;; -*- mode: Emacs-Lisp; mode: rainbow; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This program is free software; you can redistribute it and/or         ;;
;; modify it under the terms of the GNU General Public License as        ;;
;; published by the Free Software Foundation; either version 3, or (at   ;;
;; your option) any later version.                                       ;;
;;                                                                       ;;
;; This program is distributed in the hope that it will be useful, but   ;;
;; WITHOUT ANY WARRANTY; without even the implied warranty of            ;;
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU      ;;
;; General Public License for more details.                              ;;
;;                                                                       ;;
;; You should have received a copy of the GNU General Public License     ;;
;; along with this program. If not, see <http://www.gnu.org/licenses/>.  ;;
;;                                                                       ;;
;; Written by and Copyright (C) Francois Fleuret                         ;;
;; Contact <francois@fleuret.org> for comments & bug reports
;;
;; Modified by Raayan Dhar for personal use. Shared on Github.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Inhibit the startup screen
(setq inhibit-startup-screen t)

;; Disable the toolbar
(tool-bar-mode -1)

;; Disable the menu bar
(menu-bar-mode -1)

;; Disable visible scrollbar
(scroll-bar-mode -1)

;; Simplify the 'yes' or 'no' to 'y' or 'n'
(defalias 'yes-or-no-p 'y-or-n-p)

;; Enable display-line-numbers-mode globally
(global-display-line-numbers-mode t)

;; Line number appearance
(setq display-line-numbers-type 'relative
      display-line-numbers-width 3)

;; Initialize package sources
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Install and configure AUCTeX with latexmk
(unless (package-installed-p 'auctex)
  (package-install 'auctex))
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(eval-after-load 'tex
  '(add-to-list 'TeX-command-list
                '("LatexMk" "latexmk -pdf -shell-escape -synctex=1 %t"
                  TeX-run-TeX nil t
                  :help "Run latexmk on file")))
(setq TeX-command-default "LatexMk")
(setq LaTeX-command "latex -shell-escape")

;; I don't want huge titles!
;; https://tex.stackexchange.com/questions/176748/how-can-i-have-titles-with-normal-font-size-with-auctexemacs
(setq font-latex-fontify-sectioning 1.0)

;; Install and configure PDF Tools
(unless (package-installed-p 'pdf-tools)
  (package-install 'pdf-tools))
(pdf-tools-install)
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t)
(setq TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))

;; SyncTeX setup
(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)

;; Keybindings for jumping between TeX and PDF
(add-hook 'TeX-mode-hook
          (lambda ()
            (define-key TeX-mode-map (kbd "RET") 'TeX-view)))

;; Auto-revert for PDF refresh
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
  
;; Disable display-line-numbers-mode in pdf-view-mode
(add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1)))

;; Emacs python development
(unless (package-installed-p 'elpy)
  (package-refresh-contents)
  (package-install 'elpy))

(elpy-enable)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(elpy pdf-tools auctex)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Following this wonderful guide: https://emacs-lsp.github.io/lsp-mode/tutorials/CPP-guide/
;; We have support C++ stuff, need to do some review to take full advantage!
;; Probably worth re-visiting and changing some config options
(setq package-selected-packages '(lsp-mode yasnippet lsp-treemacs helm-lsp
    projectile hydra flycheck company avy which-key helm-xref dap-mode))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

;; sample `helm' configuration use https://github.com/emacs-helm/helm/ for details
(helm-mode)
(require 'helm-xref)
(define-key global-map [remap find-file] #'helm-find-files)
(define-key global-map [remap execute-extended-command] #'helm-M-x)
(define-key global-map [remap switch-to-buffer] #'helm-mini)

(which-key-mode)
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)  ;; clangd is fast

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools)
  (yas-global-mode))

;; The rest of this file is largely taken from Prof. Francois Fleuret's repository

;; This is where I put most of my emacs-related files
(setq ff/emacs-dir "~/private/emacs")
(unless (file-exists-p ff/emacs-dir)
  (mkdir ff/emacs-dir t))

;; Give the focus to the emacs window if we are under a windowing system

(when window-system
  ;; (x-focus-frame nil)
  (set-mouse-pixel-position (selected-frame) 4 4))

;; Where I keep my own scripts

(add-to-list 'load-path "~/src/gpl/elisp")
(add-to-list 'load-path "~/src/elisp")
(add-to-list 'load-path "~/local/elisp")

;; No fringes
(when (functionp 'fringe-mode) (fringe-mode 10))

;; The space bar acting as "yes" has been several times really problematic.
(define-key query-replace-map (kbd "SPC") nil)

;; Show the matching parenthesis and do it immediately, we are in a hurry
(setq show-paren-delay 0)
(show-paren-mode t)

;; use colorization for all modes
(global-font-lock-mode t)

(setq font-lock-maximum-decoration 3
      ;;'((latex-mode . 2) (t . 2))
      )

;; Save the minibuffer history
(setq savehist-file (concat ff/emacs-dir "/savehist"))
(when (functionp 'savehist-mode) (savehist-mode 1))

;; And allow minibuffer recursion
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

;; I do not like tooltips
(when (functionp 'tooltip-mode) (tooltip-mode nil))

;; Activate the dynamic completion in the mini-buffer
(icomplete-mode 1)

(defun ff/compile-when-needed (name)
  "Compiles the given file only if needed. Adds .el if required, and
uses `load-path' to find it."
  (if (not (string-match "\.el$" name))
      (ff/compile-when-needed (concat name ".el"))
    (mapc (lambda (dir)
            (let* ((src (concat dir "/" name)))
              ;; (message "Compiling " scr) ;;***********
              (when (file-newer-than-file-p src (concat src "c"))
                (if
                    ;; (let ((byte-compile-verbose nil))
                      ;; (condition-case nil
                          ;; (byte-compile-file src)
                        ;; (error nil)))

                    (byte-compile-file src)

                    (message (format "Compiled %s" src ))
                  (message (format "Failed compilation of %s" src))))))
          load-path)))

;; This is useful when using the same .emacs in many places

(defun ff/load-or-alert (name &optional compile-when-needed)
  "Tries to load the specified file and insert a warning message in a
load-warning buffer in case of failure."

  (when compile-when-needed (ff/compile-when-needed name))

  (if (load name t nil) t
    (let ((buf (get-buffer-create "*loading warnings*")))
      (display-buffer buf)
      (set-buffer buf)
      (insert (propertize "Warning:" 'face 'font-lock-warning-face) " could not load '" name "'\n")
      (fit-window-to-buffer (get-buffer-window buf))
      (set-buffer-modified-p nil))
    nil))
