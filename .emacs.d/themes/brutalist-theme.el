;;; brutalist-theme.el --- Custom theme
;;;
;;; Dale Roberts <dale.o.roberts@gmail.com>

(deftheme brutalist
  "Brutally simple theme using base 16 colors")

(custom-theme-set-faces
 'brutalist

 '(default ((((class color) (min-colors 89)) (:foreground "white"))))

 '(font-lock-warning-face ((((class color) (min-colors 89)) (:background "brightred"))))
 '(evil-ex-info ((((class color) (min-colors 89)) (:background "brightred"))))
 '(comint-highlight-prompt ((((class color) (min-colors 89)) (:foreground "brightred"))))

 '(font-lock-doc-face ((((class color) (min-colors 89)) (:italic t :foreground "cyan"))))
 '(font-lock-comment-face ((((class color) (min-colors 89)) (:foreground "cyan"))))
 '(font-lock-string-face ((((class color) (min-colors 89)) (:foreground "brightmagenta"))))
 '(font-lock-type-face ((((class color) (min-colors 89)) (:fireground "white"))))
 '(font-lock-keyword-face ((((class color) (min-colors 89)) (:foreground "yellow"))))
 '(font-lock-variable-name-face ((((class color) (min-colors 89)) (:foreground "white"))))
 '(font-lock-function-name-face ((((class color) (min-colors 89)) (:foreground "white"))))
 '(font-lock-preprocessor-face ((((class color) (min-colors 89)) (:foreground "white"))))
 '(font-lock-builtin-face ((((class color) (min-colors 89)) (:foreground "white"))))
 '(font-lock-constant-face ((((class color) (min-colors 89)) (:foreground "cyan"))))

 '(show-paren-match ((((class color) (min-colors 89)) (:inverse-video t))))

 '(minibuffer-prompt ((((class color) (min-colors 89)) (:foreground "white"))))

 '(ac-selection-face ((((class color) (min-colors 89)) (:inverse-video t))))
 '(ac-candidate-face ((((class color) (min-colors 89)) (:inverse-video t))))

 '(region ((((class color) (min-colors 89)) (:inverse-video t))))
;'(isearch ((((class color) (min-colors 89)) (:inverse-video t))))
;'(isearch-lazy-highlight-face ((((class color) (min-colors 89)) (:inverse-video t))))

 '(lazy-highlight-face ((((class color) (min-colors 89)) (:inverse-video t))))

 '(completions-common-part ((((class color) (min-colors 89)) (:italic t :foreground "cyan"))))
 '(completions-first-difference ((((class color) (min-colors 89)) (:weight bold :italic t :foreground "brightcyan"))))

 `(font-latex-warning-face      ((((class color) (min-colors 89)) (:foreground "brightwhite"))))
 `(font-latex-sectioning-1-face ((((class color) (min-colors 89)) (:foreground "white"))))
 `(font-latex-sectioning-2-face ((((class color) (min-colors 89)) (:foreground "white"))))
 `(font-latex-sectioning-3-face ((((class color) (min-colors 89)) (:foreground "white"))))
 `(font-latex-sectioning-4-face ((((class color) (min-colors 89)) (:foreground "white"))))
 `(font-latex-sectioning-5-face ((((class color) (min-colors 89)) (:foreground "white"))))
 `(font-latex-math-face         ((((class color) (min-colors 89)) (:foreground "brightblack"))))
 `(font-latex-script-char-face  ((((class color) (min-colors 89)) (:foreground "brightblack"))))
 `(font-latex-string-face       ((((class color) (min-colors 89)) (:foreground "white"))))
 )

(provide-theme 'brutalist) 
