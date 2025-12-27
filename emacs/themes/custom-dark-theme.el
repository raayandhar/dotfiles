(deftheme custom-dark
  "A custom dark theme approximating the style I like.")

(let ((class '((class color) (min-colors 89)))
      (fg1 "#dddddd")
      (bg1 "#1c1c1c")
      (fg2 "#aaaaaa")
      (bg2 "#2c2c2c")
      (comment "#96b4dc")      ;; Pastel blue (matches bash prompt venv)
      (keyword "#d0c4e0")      ;; Mellow lavender
      (builtin "#c8bcd8")      ;; Soft lavender
      (string "#b8b0d0")       ;; Medium mellow lavender
      (func "#e8e0f0")         ;; Very light lavender
      (var "#d8d0e8")          ;; Light lavender
      (type "#c8c0d8")         ;; Gentle lavender
      (const "#b8b0d0")        ;; Medium lavender
      (warning "#ddaaaa"))
  (custom-theme-set-faces
   'custom-dark
   `(default ((,class (:background ,bg1 :foreground ,fg1))))
   `(font-lock-comment-face ((,class (:foreground ,comment :slant italic))))
   `(font-lock-doc-face ((,class (:foreground ,comment :slant italic))))  ; Docstrings (""" """)
   `(font-lock-keyword-face ((,class (:foreground ,keyword :weight bold))))
   `(font-lock-builtin-face ((,class (:foreground ,builtin))))
   `(font-lock-string-face ((,class (:foreground ,string))))
   `(font-lock-function-name-face ((,class (:foreground ,func :weight normal))))
   `(font-lock-variable-name-face ((,class (:foreground ,var))))
   `(font-lock-type-face ((,class (:foreground ,type))))
   `(font-lock-constant-face ((,class (:foreground ,const))))
   `(font-lock-warning-face ((,class (:foreground ,warning :weight bold))))
   `(region ((,class (:background ,bg2))))
   `(cursor ((,class (:background ,fg1))))
   `(fringe ((,class (:background ,bg1))))
   `(highlight ((,class (:background ,bg2))))
   `(hl-line ((,class (:background ,bg2))))
   `(linum ((,class (:background ,bg1 :foreground ,fg2))))
   `(mode-line ((,class (:background "#333333" :foreground "#ffffff" :box nil))))
   `(mode-line-inactive ((,class (:background "#2a2a2a" :foreground "#cccccc" :box nil))))
   `(header-line ((,class (:background "#2a2a2a" :foreground "#ffffff" :box nil))))
   `(helm-selection ((,class (:background ,bg2 :foreground ,fg1))))
   `(helm-match ((,class (:foreground ,keyword :underline t))))
   ;; diff-hl colors (git change indicators in fringe)
   `(diff-hl-insert ((,class (:foreground "#50a050" :background "#50a050"))))  ; Green for added
   `(diff-hl-change ((,class (:foreground "#5080b0" :background "#5080b0"))))  ; Blue for modified
   `(diff-hl-delete ((,class (:foreground "#d05050" :background "#d05050"))))  ; Red for deleted
   ;; Tree-sitter and Python docstrings
   `(tree-sitter-hl-face:doc ((,class (:foreground ,comment :slant italic))))
   `(tree-sitter-hl-face:string.documentation ((,class (:foreground ,comment :slant italic))))
   `(python-docstring-face ((,class (:foreground ,comment :slant italic))))))
(custom-theme-set-variables 'custom-dark)
(provide-theme 'custom-dark)
