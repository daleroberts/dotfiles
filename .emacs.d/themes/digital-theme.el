;;; digital-theme.el
;;; Dale Roberts <dale.o.roberts@gmail.com>

(deftheme digital
  "Face colors inspired by DEC logo.")

(let ((class '((class color) (min-colors 89)))
      (lightgray "#dddddd")
      (burgudny "#4F1114")
      (skin "#E3A58D")
      (black "#000000")
      (white "#ffffff")
      (orange "#B04015")
      (skyblue "#228DC9")
      (brightgreen "#51B015")
      (armygreen "#9dac3b")
      (bluegreen "#04b888")
      (yellow "#f7db00")
      (blue "#6698c8")
      (red "#df4c0f")
      (green "#afd50d")
      (vermillion "#d55e00")
      (purple "#fe43ff")
      (redpurple "#cc79a7")
      (bluegray "#0F557D")) 
  (custom-theme-set-faces
   'digital
   ;;
   `(Info-title-1-face ((,class (:family "helv" :weight bold :height 1.728))))
   `(Info-title-2-face ((,class (:family "helv" :weight bold :height 1.44))))
   `(Info-title-3-face ((,class (:family "helv" :weight bold :height 1.2))))
   `(Info-title-4-face ((,class (:family "helv" :weight bold))))
   ;;
   `(compilation-column-number ((,class (:foreground ,vermillion))))
   `(compilation-error ((,class (:foreground ,red))))
   `(compilation-info ((,class (:weight normal :foreground ,white))))
   `(compilation-line-number ((,class (:foreground ,white))))
   ;;
   `(cursor ((,class (:background ,white))))
   `(default ((,class (:background ,burgudny :foreground ,lightgray))))
   ;;
   `(dired-marked ((,class (:background "dodgerblue3" :foreground ,white))))
   ;;
   `(flycheck-fringe-info ((,class (:background nil :foreground ,red))))
   `(flycheck-fringe-warning ((,class (:foreground ,orange))))
   `(flycheck-fringe-error ((,class (:background nil :foreground ,red))))
   `(flycheck-info ((,class (:background nil))))
   `(flycheck-warning ((,class (:foreground ,orange))))
   `(flycheck-error ((,class (:foreground ,red))))
   ;; 
   `(font-lock-exception-face ((,class (:foreground ,white))))
   `(font-lock-builtin-face ((,class (:foreground ,white))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,bluegreen :slant italic))))
   `(font-lock-comment-face ((,class (:foreground ,bluegreen :slant italic))))
   `(font-lock-constant-face ((,class (:foreground ,white))))
   `(font-lock-doc-face ((,class (:foreground ,white))))
   `(font-lock-doc-string-face ((,class (:foreground ,white))))
   `(font-lock-function-name-face ((,class (:foreground ,purple :slant italic))))
   `(font-lock-keyword-face ((,class (:foreground ,brightgreen))))
   `(font-lock-preprocessor-face ((,class (:foreground ,white))))
   `(font-lock-reference-face ((,class (:foreground ,white))))
   `(font-lock-regexp-grouping-backslash ((,class (:weight bold))))
   `(font-lock-regexp-grouping-construct ((,class (:weight bold))))
   `(font-lock-string-face ((,class (:foreground ,white))))
   `(font-lock-type-face ((,class (:foreground ,white :slant italic))))
   `(font-lock-variable-name-face ((,class (:foreground ,white))))
   `(font-lock-warning-face ((,class (:background ,vermillion :foreground ,white))))
   ;;
   `(fringe ((,class (:background ,burgudny))))
   `(highlight ((,class (:background ,skin))))
   ;;
   `(ido-first-match ((,class (:weight normal :foreground "Dark#E69f003"))))
   `(ido-only-match ((,class (:foreground "SeaGreen4"))))
   `(ido-subdir ((,class (:foreground nil :inherit font-lock-keyword-face))))
   ;;
   `(info-header-node ((,class (:foreground "DeepSkyBlue1"))))
   `(info-header-xref ((,class (:foreground "SeaGreen2"))))
   `(info-menu-header ((,class (:family "helv" :weight bold))))
   `(info-node ((,class (:foreground "DeepSkyBlue1"))))
   `(info-xref ((,class (:foreground "SeaGreen4"))))
   ;;
   `(isearch ((,class (:background ,brightgreen :foreground ,black))))
   `(lazy-highlight ((,class (:background ,bluegray :foreground ,black))))
   ;;
   `(match ((,class (:background "#fcf7ed" :foreground "black"))))
   `(minibuffer-prompt ((,class (:foreground ,white))))
   `(mode-line ((,class (:background "gray70" :foreground "black" :box nil))))
   `(mode-line-buffer-id ((,class (:weight bold :background nil :foreground "black" :box nil))))
   `(mode-line-inactive ((,class (:background "gray70" :foreground "black" :box nil))))
   `(outline-1 ((,class (:foreground "Blue3"))))
   `(outline-2 ((,class (:foreground "DodgerBlue"))))
   `(outline-3 ((,class (:foreground "SteelBlue"))))
   `(outline-4 ((,class (:foreground "RoyalBlue"))))
   `(outline-5 ((,class (:foreground "DeepSkyBlue"))))
   ;;
   `(primary-selection ((,class (:background "blue3"))))
   `(region ((,class (:background ,skin :foreground ,white))))
   ;;
   `(show-paren-match-face ((,class (:background ,green :foreground ,black))))
   `(show-paren-mismatch-face ((,class (:background ,red :foreground ,white))))
   `(warning ((,class (:foreground ,red))))
   ;; LaTeX
   `(font-latex-warning-face ((,class (:foreground ,vermillion))))
   `(font-latex-sectioning-1-face ((,class (:foreground ,white))))
   `(font-latex-sectioning-2-face ((,class (:foreground ,white))))
   `(font-latex-sectioning-3-face ((,class (:foreground ,white))))
   `(font-latex-sectioning-4-face ((,class (:foreground ,white :background ,vermillion))))
   `(font-latex-sectioning-5-face ((,class (:foreground ,burgudny :background ,brightgreen))))
   `(font-latex-math-face ((,class (:foreground ,redpurple))))
   `(font-latex-script-char-face ((,class (:foreground ,redpurple))))
   `(font-latex-string-face ((,class (:foreground ,white))))
   ))

(provide-theme 'digital)
