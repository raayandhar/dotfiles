;; Basic setup
(setq inhibit-startup-screen t)  ; Disable the startup screen
(tool-bar-mode -1)               ; Disable the toolbar
(menu-bar-mode -1)               ; Disable the menu bar
(scroll-bar-mode -1)             ; Disable visible scrollbar
(setq make-backup-files nil)     ; Prevent creation of backup files
(setq auto-save-default nil)     ; Disable auto-saving

;; Simplify the 'yes' or 'no' to 'y' or 'n'
(defalias 'yes-or-no-p 'y-or-n-p)
;; Enable display-line-numbers-mode globally
(global-display-line-numbers-mode t)

;; Disable display-line-numbers-mode in pdf-view-mode
(add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1)))

;; Customize the appearance of line numbers (optional)
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


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(dracula))
 '(custom-safe-themes
   '("603a831e0f2e466480cdc633ba37a0b1ae3c3e9a4e90183833bc4def3421a961" default))
 '(package-selected-packages
   '(helm-projectile helm projectile smartparens modern-cpp-font-lock disaster rtags cmake-ide flycheck company-irony elpy pdf-tools dracula-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(unless (package-installed-p 'elpy)
  (package-refresh-contents)
  (package-install 'elpy))

(elpy-enable)


;; Install packages for C++ development
(defvar my-cpp-packages
  '(company
    company-irony
    flycheck
    irony
    cmake-ide
    rtags
    disaster
    modern-cpp-font-lock
    smartparens
    projectile))

(dolist (package my-cpp-packages)
  (unless (package-installed-p package)
    (package-install package)))

;; ;; Enable packages
;; (require 'company)
;; (require 'company-irony)
;; (require 'flycheck)
;; (require 'irony)
;; (require 'cmake-ide)
;; (require 'rtags)
;; (require 'disaster)
;; (require 'modern-cpp-font-lock)
;; (require 'smartparens)
;; (require 'projectile)

;; ;; Set up Irony
;; (add-hook 'c++-mode-hook 'irony-mode)
;; (add-hook 'c-mode-hook 'irony-mode)
;; (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; ;; Set up Company mode
;; (add-hook 'c++-mode-hook 'company-mode)

;; ;; Set up Flycheck
;; (add-hook 'c++-mode-hook 'flycheck-mode)

;; ;; Set up Projectile and Helm
;; (projectile-mode)

;; ;; Set up Modern C++ font lock
;; (modern-c++-font-lock-global-mode t)

;; ;; Set up Smartparens
;; (smartparens-global-mode t)

;; ;; Set the default compile command for C++ to g++
;; (setq-default compile-command "g++ -Wall -std=c++17 ")

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

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


