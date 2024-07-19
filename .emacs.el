;; -*- mode: Emacs-Lisp; -*-

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
(setq tool-bar-mode -1)

;; Disable the menu bar
(setq menu-bar-mode -1)

;; Disable visible scrollbar
(setq scroll-bar-mode -1)

;; Simplify the 'yes' or 'no' to 'y' or 'n'
(defalias 'yes-or-no-p 'y-or-n-p)

;; Enable display-line-numbers-mode globally
(global-display-line-numbers-mode t)

;; Line number appearance
(setq display-line-numbers-type 'relative display-line-numbers-width 3)

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
 ;;  ;; custom-set-variables was added by Custom.
 ;;  ;; If you edit it by hand, you could mess it up, so be careful.
 ;;  ;; Your init file should contain only one such instance.
 ;;  ;; If there is more than one, they won't work right.
 '(package-selected-packages '(elpy pdf-tools auctex)))
(custom-set-faces
 ;;  ;; custom-set-faces was added by Custom.
 ;;  ;; If you edit it by hand, you could mess it up, so be careful.
 ;;  ;; Your init file should contain only one such instance.
 ;;  ;; If there is more than one, they won't work right.
 )

;; ;; Following this wonderful guide: https://emacs-lsp.github.io/lsp-mode/tutorials/CPP-guide/
;; ;; We have support C++ stuff, need to do some review to take full advantage!
;; ;; Probably worth re-visiting and changing some config options
(setq package-selected-packages '(lsp-mode yasnippet lsp-treemacs helm-lsp
                                           projectile hydra flycheck company avy which-key helm-xref dap-mode))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

;; ;; sample `helm' configuration use https://github.com/emacs-helm/helm/ for details
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

;; ;; The rest of this file is largely taken from Prof. Francois Fleuret's repository

;; ;; This is where I put most of my emacs-related files
(setq ff/emacs-dir "~/private/emacs")
(unless (file-exists-p ff/emacs-dir)
  (mkdir ff/emacs-dir t))

;; ;; Give the focus to the emacs window if we are under a windowing system

(when window-system
  ;; (x-focus-frame nil)
  (set-mouse-pixel-position (selected-frame) 4 4))

;; ;; Where I keep my own scripts

(add-to-list 'load-path "~/src/gpl/elisp")
(add-to-list 'load-path "~/src/elisp")
(add-to-list 'load-path "~/local/elisp")

;; ;; No fringes
(when (functionp 'fringe-mode) (fringe-mode 10))

;; ;; The space bar acting as "yes" has been several times really problematic.
(define-key query-replace-map (kbd "SPC") nil)

;; ;; Show the matching parenthesis and do it immediately, we are in a hurry
(setq show-paren-delay 0)
(setq show-paren-mode t)

;; ;; use colorization for all modes
(global-font-lock-mode t)

(setq font-lock-maximum-decoration 3
      ;;'((latex-mode . 2) (t . 2))
      )

;; ;; Save the minibuffer history
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

;; Hold up to 1000 in *Messages* buffer
(setq message-log-max 1000)

;; Only logical line jumping
(setq line-move-visual nil)

;; Do not keep track of autosaved files
(setq auto-save-list-file-prefix nil)

;; Show empty lines at end of buffer
;; THE CULPRIT OF "Wrong number of arguments: setq, 3"
;; (setq default-indicate-empty lines t)

;; Show me the region until I do something on it
(setq transient-mark-mode t)

;; Do not color stuff which is clickable when hovering over it
(setq mouse-highlight nil)

;; SSH !!!
(setq tramp-default-method "ssh")

;; Files deserves local vars
(setq enable-local-eval t)

(setq-default

 ;; Show white spaces at the end of lines
 show-trailing-whitespace t

 ;; Do not show the cursor in non-active window
 cursor-in-non-selected-windows nil

 use-dialog-box nil
 use-file-dialog nil

 ;; when on a TAB, the cursor has the TAB length
 x-stretch-cursor t

 ;; This is the default coding system when toggle-input-method is
 ;; invoked (C-\)
 default-input-method "latin-1-prefix"

 ;; do not put tabs when indenting
 indent-tabs-mode nil
 ;; Stop indenting automatically, that's annoying
 electric-indent-chars nil

 ;; And yes, we have a fast display / connection / whatever
 baud-rate 524288
 ;; baud-rate 10

 ;; To keep the cursor always visible when it moves (thanks
 ;; snogglethrop!)
 redisplay-dont-pause t

 ;; I want to see the keys I type instantaneously
 echo-keystrokes 0.1
 )

;; Show the column number
(column-number-mode 1)

;; What modes for what file extentions
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(require 'org-table)

(add-to-list 'auto-mode-alist '("\\.txt\\'" . (lambda()
                                                (text-mode)
                                                (orgtbl-mode)
                                                ;; (auto-fill-mode)
                                                (flyspell-mode))))

(add-hook 'c++-mode-hook 'flyspell-prog-mode)
;; (add-hook 'lua-mode-hook 'flyspell-prog-mode)
(add-hook 'log-edit-mode-hook 'flyspell-mode)

(add-hook 'markdown-mode-hook 'flyspell-mode)
(add-hook 'markdown-mode-hook 'auto-fill-mode)

;; I am a power-user

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (boundp 'x-display-name)

  (setq-default

   ;; If the display is :0.0, we make the assumption that we are
   ;; running the emacs locally, and we do not show the
   ;; hostname. Otherwise, show @host.

   frame-title-format (concat "emacs" ;;invocation-name
                              (unless (string= x-display-name ":0.0")
                                (concat "@" system-name))
                              " (%b)")

   ;; Use the same for the icone

   icon-title-format frame-title-format
   ))

;; "tool" bar? Are you kidding?
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; My funky setting of face colors. Basically, we switch to a sober
;; look and darken a bit the colors which need to (because of the
;; darker background)

(defun ff/configure-faces (fl)
  "Set face attributes and create faces when necessary"
  (mapc (lambda (f)
          (unless (boundp (car f)) (make-empty-face (car f)))
          (eval `(set-face-attribute (car f) nil ,@(cdr f))))
        fl))

;; Not the same in xterm (which is gray in my case) and in
;; X-window

(when window-system

  (ff/configure-faces
   '(
     ;; (escape-glyph :foreground "#c0c0c0" :weight 'bold)
     (escape-glyph :foreground "green3" :weight 'bold)
     (default :background "gray82" :foreground "black")
     (cperl-array-face :background "gray82" :foreground "blue" :weight 'bold)
     (cperl-hash-face :background "gray82" :foreground "purple" :weight 'bold)
     (message-cited-text :foreground "firebrick4")
     (diff-mode :background "gray82" :weight 'bold)
     (diff-added :background "gray82" :foreground "SeaGreen3" :weight 'bold)
     (diff-removed :background "gray82" :foreground "firebrick3" :weight 'bold)
     (diff-changed :background "gray82" :foreground "goldenrod" :weight 'bold)
     (diff-file-header :background "white" :foreground "black"
                       :weight 'bold)
     (diff-header :background "white" :foreground "black")
     (diff-hunk-header :background "white" :foreground "black")
     (font-lock-builtin-face :foreground "light slate blue")
     (font-lock-string-face :foreground "slate blue")
     (font-lock-variable-name-face :foreground "dark slate blue")
     (font-lock-function-name-face :foreground "blue violet" :weight 'bold)
     ;; (font-lock-function-name-face :foreground "blue")
     ;; (font-lock-comment-delimiter-face :foreground "dark violet")
     ;; (font-lock-comment-face :foreground "dark violet")
     (flyspell-incorrect :background "#ff0000" :foreground "black")
     (flyspell-duplicate :background "#ff9000" :foreground "black")
     (hl-line :background "white")
     (sh-heredoc :foreground "black" :background "#fff0f0")
     (sh-heredoc-face :foreground "black" :background "#fff0f0")
     (header-line :background "gray65")
     (highlight :background "white")
     (message-cited-text-face :foreground "firebrick")
     (isearch :background "yellow" :foreground "black")
     (isearch-lazy-highlight-face' :background "yellow3" :foreground "black")
     (region :background "#b8b8e0" :foreground "black")
     ;; (region :background "plum" :foreground "black")
     (show-paren-match-face :background "gold" :foreground "black")
     (show-paren-mismatch-face :background "red" :foreground "black")
     (trailing-whitespace :background "gray65")
     (cursor :inverse-video t)
     (enotes/list-title-face :foreground "blue" :weight 'bold)
     (mode-line :background "#b0b0ff" :foreground "black" :box nil
                :inverse-video nil)
     (header-line :background "cornflowerblue" :foreground "black" :box nil
                  :inverse-video nil)
     (mode-line-inactive :background "gray80" :foreground "black" :box nil
                         :inverse-video nil)
     ;; (fringe :background "black" :foreground "gray23")
     (fringe :background "gray80")
     (ff/date-info-face :foreground "white")
     (ff/battery-info-face :foreground "blue")
     (ff/battery-info-alarm-face :foreground "red")
     ;; (ff/mail-alarm-face :foreground "white" :background "red2")
     ;; (alarm-vc-face :foreground "black" :background "yellow" :weight 'normal)
     (gui-button-face :background "green" :foreground "black")
     ))
  )
;; When we are root, put the modeline in red

(when (string= (user-real-login-name) "root")
  (ff/configure-faces
   '((mode-line :background "red3" :foreground "black" :box nil
                :inverse-video nil))
   ))

;; Why should I have to do this?
(add-hook 'sh-mode-hook
          (lambda ()
            (set-face-attribute 'sh-heredoc nil
                                :foreground "#604000"
                                :background "white"
                                :italic t)
            (set-face-attribute 'sh-heredoc-face nil
                                :foreground "#604000"
                                :background "white"
                                :italic t)
            ))

(defun ff/scroll-down ()
  "Scroll the buffer down one line and keep the cursor at the same location."
  (interactive)
  (condition-case nil
      (scroll-down 1)
    (error nil)))

(defun ff/scroll-up ()
  "Scroll the buffer up one line and keep the cursor at the same location."
  (interactive)
  (condition-case nil
      (scroll-up 1)
    (error nil)))

(defun ff/scroll-left ()
  "Scroll the buffer left one column and keep the cursor at the same location."
  (interactive)
  (condition-case nil
      (scroll-left 2)
    (error nil)))

(defun ff/scroll-right ()
  "Scroll the buffer right one column and keep the cursor at the same location."
  (interactive)
  (condition-case nil
      (scroll-right 2)
    (error nil)))

(define-key global-map [(meta up)] 'ff/scroll-down)
(define-key global-map [(meta down)] 'ff/scroll-up)

(define-key global-map [(meta shift up)]
            (lambda () (interactive) (condition-case nil (scroll-down 10) (error nil))))

(define-key global-map [(meta shift down)]
            (lambda () (interactive) (condition-case nil (scroll-up 10) (error nil))))

(define-key global-map [(meta p)] 'ff/scroll-down)
(define-key global-map [(meta n)] 'ff/scroll-up)
(define-key global-map [(meta right)] 'ff/scroll-left)
(define-key global-map [(meta left)] 'ff/scroll-right)

(defun ff/delete-trailing-whitespaces-and-indent ()
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil))

;; Prof. Fleuret the GOAT? so useful !!
(define-key global-map [(control c) (control q)] 'ff/delete-trailing-whitespaces-and-indent)

(define-key global-map [(control x) (control o)] 'other-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I comment stuff often, let's be efficient. shift + down comments
;; the current line and goes down, and shift + up uncomments the line
;; and goes up (they are not the dual of each other, but moving and
;; then uncommenting would be very counter-intuitive).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ff/comment-and-go-down (arg)
  "Comments and goes down ARG lines."
  (interactive "p")
  (condition-case nil
      (comment-region (point-at-bol) (point-at-eol)) (error nil))
  (next-line 1)
  (if (> arg 1) (ff/comment-and-go-down (1- arg))))

(defun ff/uncomment-and-go-up (arg)
  "Uncomments and goes up ARG lines."
  (interactive "p")
  (condition-case nil
      (uncomment-region (point-at-bol) (point-at-eol)) (error nil))
  (next-line -1)
  (if (> arg 1) (ff/uncomment-and-go-up (1- arg))))

(define-key global-map [(shift down)] 'ff/comment-and-go-down)
(define-key global-map [(shift up)] 'ff/uncomment-and-go-up)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ff/show-compilation-buffer-split-window ()
  "Split the window vertically and show the compilation buffer in the newly created right one"
  (interactive)

  (let ((b (get-buffer "*compilation*")))
    (if b (show-buffer (split-window-right) b)
      (error "Cannot find a compilation buffer"))
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Counting various entities in text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ff/word-occurences ()
  "Display in a new buffer the list of words sorted by number of
occurrences "
  (interactive)

  (let ((buf (get-buffer-create "*word counting*"))
        (map (make-sparse-keymap))
        (nb (make-hash-table))
        (st (make-hash-table))
        (result nil))

    ;; Collects all words in a hash table

    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\([\\-a-zA-Z\\\\]+\\)" nil t)
        (let* ((s (downcase (match-string-no-properties 1)))
               (k (sxhash s)))
          (puthash k s st)
          (puthash k (1+ (gethash k nb 0)) nb))))

    ;; Creates the result buffer

    (define-key map "q" 'kill-this-buffer)
    (display-buffer buf)
    (set-buffer buf)
    (setq show-trailing-whitespace nil)
    (erase-buffer)

    ;; Builds a list from the hash table

    (maphash
     (lambda (key value)
       (setq result (cons (cons value (gethash key st)) result)))
     nb)

    ;; Sort and display it

    (mapc (lambda (x)
            (if (and (> (car x) 3)
                     ;; No leading backslash and at least four characters
                     (string-match "^[^\\]\\{4,\\}" (cdr x))
                     )
                (insert (number-to-string (car x)) " " (cdr x) "\n")))
          (sort result (lambda (a b) (> (car a) (car b)))))

    ;; Adjust the window size and stuff

    (fit-window-to-buffer (get-buffer-window buf))
    (use-local-map map)
    (set-buffer-modified-p nil))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Moving through buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ff/next-buffer ()
  "Switches to the next buffer in cyclic order."
  (interactive)
  (let ((buffer (current-buffer)))
    (switch-to-buffer (other-buffer buffer))
    (bury-buffer buffer)))

(defun ff/prev-buffer ()
  "Switches to the previous buffer in cyclic order."
  (interactive)
  (let ((list (nreverse (buffer-list)))
        found)
    (while (and (not found) list)
      (let ((buffer (car list)))
        (if (and (not (get-buffer-window buffer))
                 (not (string-match "\\` " (buffer-name buffer))))
            (setq found buffer)))
      (setq list (cdr list)))
    (switch-to-buffer found)))

;; (define-key global-map [?\C-x right] 'ff/next-buffer)
;; (define-key global-map [?\C-x left] 'ff/prev-buffer)
;; (define-key global-map [?\M-\]] 'ff/next-buffer)
;; (define-key global-map [?\M-\[] 'ff/prev-buffer)

(define-key global-map [(meta right)] 'ff/next-buffer)
(define-key global-map [(meta left)] 'ff/prev-buffer)

;; Get the cool terminal emulator in emacs working properly

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cool stuff to navigate in emacs-lisp sources
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "find-func")

(defun ff/goto-function-definition (&optional goback)
  "Go directly to the definition of the function at point. With
goback argument, go back where we were."
  (interactive "P")
  (if goback
      (if (not (and (boundp 'goto-function-history) goto-function-history))
          (error "We were nowhere, buddy")
        (message "Come back")
        (switch-to-buffer (car (car goto-function-history)))
        (goto-char (cdr (car goto-function-history)))
        (setq goto-function-history (cdr goto-function-history)))

    (let ((function (function-called-at-point)))
      (when function
        (let ((location (find-function-search-for-symbol
                         function nil
                         (symbol-file function))))
          (setq goto-function-history
                (cons (cons (current-buffer) (point))
                      (and (boundp 'goto-function-history)
                           goto-function-history)))
          (pop-to-buffer (car location))
          (goto-char (cdr location)))))))

(define-key global-map [(meta g)] 'ff/goto-function-definition)
(define-key global-map [(meta G)] (lambda () (interactive)
                                    (ff/goto-function-definition t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spelling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq ;; For french, aspell is far better than ispell
 ispell-program-name "aspell"
 ;; To avoid ispell errors in figure filenames, labels, references.
 ;;       ispell-tex-skip-alists
 ;;       (list
 ;;        (append (car ispell-tex-skip-alists)
 ;;                '(("\\\\citep"           ispell-tex-arg-end) ;; JMLR
 ;;                  ("\\\\cite"            ispell-tex-arg-end)
 ;;                  ("\\\\nocite"          ispell-tex-arg-end)
 ;;                  ("\\\\includegraphics" ispell-tex-arg-end)
 ;;                  ("\\\\author"          ispell-tex-arg-end)
 ;;                  ("\\\\ref"             ispell-tex-arg-end)
 ;;                  ("\\\\label"           ispell-tex-arg-end)
 ;;                  ))
 ;;        (cadr ispell-tex-skip-alists))

 ;; So that reftex follows the text when moving in the summary
 reftex-toc-follow-mode nil
 ;; So that reftex visits files to follow
 reftex-revisit-to-follow t
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prevents many errors from beeping and makes the others play a nifty
;; sound
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ff/ring-bell ()
  (unless (memq this-command
                '(isearch-abort
                  abort-recursive-edit
                  exit-minibuffer
                  keyboard-quit
                  backward-delete-char-untabify
                  delete-backward-char
                  minibuffer-complete-and-exit
                  previous-line next-line
                  backward-char forward-char
                  scroll-up scroll-down
                  enlarge-window-horizontally shrink-window-horizontally
                  enlarge-window shrink-window
                  minibuffer-complete
                  ))
    ;; (message "command [%s]" (prin1-to-string this-command))
    ;; (ff/play-sound-async "~/local/sounds/short_la.wav")
    ))

;; Trying to rid of superscript/subscript
(setq tex-fontify-script nil)
(setq font-latex-fontify-script nil)
 '(tex-font-script-display (quote (-0.0 0.0)))
 '(tex-suscript-height-ratio 1.0)
;; Looks like one of them worked

