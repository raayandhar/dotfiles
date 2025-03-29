(deftheme custom-dark
  "A custom dark theme approximating the style I like.")

(let ((class '((class color) (min-colors 89)))
      (fg1 "#dddddd")
      (bg1 "#1c1c1c")
      (fg2 "#aaaaaa")
      (bg2 "#2c2c2c")
      (comment "#888888")
      (keyword "#d0a8ff")
      (builtin "#9bd0ff")
      (string "#b5b5b5")
      (func "#ffffff")
      (var "#ffffff")
      (type "#fff09b")
      (const "#f78c6c")
      (warning "#ff5370"))
  (custom-theme-set-faces
   'custom-dark
   `(default ((,class (:background ,bg1 :foreground ,fg1))))
   `(font-lock-comment-face ((,class (:foreground ,comment :slant italic))))
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
   `(helm-match ((,class (:foreground ,keyword :underline t))))))
(custom-theme-set-variables 'custom-dark)
(provide-theme 'custom-dark)
