;;; dr-theme.el --- Custom theme for faces
;;; Commentary:
;;; Code:

(deftheme protonopia
  "Face colors inspired by slack protonopia.")

(let ((class '((class color) (min-colors 89)))
      (lightgray "#dddddd")
      (darkpurple "#4e2f4c")
      (lightpurple "#8c5888")
      (black "#000000")
      (white "#ffffff")
      (orange "#e69f00")
      (skyblue "#56b4e9")
      (cyan "#01ffb8")
      (bluegreen "#04b888")
      (yellow "#f7db00")
      (blue "#6698c8")
      (red "#df4c0f")
      (armygreen "#9dac3b")
      (green "#afd50d")
      (vermillion "#d55e00")
      (redpurple "#cc79a7")
      (bluegray "#848ea9")) 
  (custom-theme-set-faces
   'protonopia
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
   `(default ((,class (:background ,darkpurple :foreground ,lightgray))))
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
   `(font-lock-function-name-face ((,class (:foreground ,white))))
   `(font-lock-keyword-face ((,class (:foreground ,cyan))))
   `(font-lock-preprocessor-face ((,class (:foreground ,white))))
   `(font-lock-reference-face ((,class (:foreground ,white))))
   `(font-lock-regexp-grouping-backslash ((,class (:weight bold))))
   `(font-lock-regexp-grouping-construct ((,class (:weight bold))))
   `(font-lock-string-face ((,class (:foreground ,white))))
   `(font-lock-type-face ((,class (:foreground ,white :slant italic))))
   `(font-lock-variable-name-face ((,class (:foreground ,white))))
   `(font-lock-warning-face ((,class (:foreground ,red))))
   ;;
   `(fringe ((,class (:background ,darkpurple))))
   `(highlight ((,class (:background ,lightpurple))))
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
   `(isearch ((,class (:background ,cyan :foreground ,black))))
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
   `(region ((,class (:background ,lightpurple :foreground ,white))))
   ;;
   `(show-paren-match-face ((,class (:background ,green :foreground ,black))))
   `(show-paren-mismatch-face ((,class (:background ,red :foreground ,white))))
   `(warning ((,class (:foreground ,red))))
   ;; LaTeX
   `(font-latex-sectioning-1-face ((,class (:foreground ,white))))
   `(font-latex-sectioning-2-face ((,class (:foreground ,white))))
   `(font-latex-sectioning-3-face ((,class (:foreground ,white))))
   `(font-latex-sectioning-4-face ((,class (:foreground ,white))))
   `(font-latex-sectioning-5-face ((,class (:foreground ,white))))
   `(font-latex-math-face ((,class (:foreground ,redpurple))))
   `(font-latex-string-face ((,class (:foreground ,white))))
   ))

(provide-theme 'protonopia)

;;; dr-theme.el ends here
