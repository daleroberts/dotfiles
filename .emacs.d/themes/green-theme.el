;;; green-theme.el
;;; Dale Roberts <dale.o.roberts@gmail.com>

(deftheme green
  "Green monochrome color theme.")

(let ((class '((class color) (min-colors 89)))
      (black "#000000")
      (white "#FFFFFF")
      (green1 "#0C1900")
      (green2 "#193300")
      (green3 "#264C00")
      (green4 "#336600")
      (green5 "#407F00")
      (green6 "#4C9900")
      (green7 "#59B200")
      (green8 "#66CC00")
      (green9 "#73E500")
      (green10 "#80FF00")
      (green11 "#8CFF19")
      (green12 "#99FF33")
      (green13 "#A6FF4C")
      (green14 "#B2FF66")
      (green15 "#BFFF7F")
      (green17 "#CCFF99")
      (green18 "#D8FFB2")
      (green19 "#E5FFCC")
      )
  (custom-theme-set-faces
   'green
   ;;
   `(default ((,class (:background ,black :foreground ,green9))))

   `(cursor ((,class (:background ,green14))))

   `(escape-glyph ((,class (:foreground ,green4))))
   `(eshell-prompt ((,class (:foreground ,green18 :weight bold))))

   `(font-lock-builtin-face ((,class (:foreground ,green13 :weight semi-bold))))
   `(font-lock-comment-face ((,class (:foreground ,green5 :weight normal))))
   `(font-lock-constant-face ((,class (:foreground ,green15 :weight bold))))
   `(font-lock-doc-face ((,class (:inherit (font-lock-string-face)))))
   `(font-lock-function-name-face ((,class (:foreground ,green10 :weight normal :slant italic))))
   `(font-lock-keyword-face ((,class (:foreground ,green13 :weight semi-bold))))
   `(font-lock-negation-char-face ((,class nil)))
   `(font-lock-string-face ((,class (:foreground ,green7 :weight light))))
   `(font-lock-type-face ((,class (:foreground ,green19 :weight normal :slant italic))))
   `(font-lock-variable-name-face ((,class (:foreground ,green13 :weight normal))))
   `(font-lock-warning-face ((,class (:weight bold :foreground "chartreuse"))))

   `(font-lock-regexp-grouping-backslash ((,class (:inherit (bold)))))
   `(font-lock-regexp-grouping-construct ((,class (:inherit (bold)))))

   `(fringe ((,class (:background "black" :foreground ,green10))))
   `(highlight ((,class (:background ,green3 :foreground "lawn green" :weight normal))))
   `(hl-line ((,class (:inherit highlight :background "dark green" :foreground ,green10))))
   `(lazy-highlight ((,class (:background ,green14 :foreground ,black))))
   `(linum ((,class (:weight normal :box nil :foreground ,green6 :background ,green4 :inherit default))))
   `(match ((,class (:background ,green14 :foreground "black"))))

   `(minibuffer-prompt ((,class (:foreground ,green10 :weight normal))))

   `(mode-line ((,class (:inherit mode-line :background ,green3 :foreground ,green14 :box nil :weight normal))))
   `(mode-line-inactive ((,class (:inherit mode-line :background ,green3 :foreground ,green8 :box nil :weight light))))
   `(mode-line-buffer-id ((,class (:box nil))))
   `(mode-line-emphasis ((,class (:weight bold))))

   `(mode-line-highlight ((,class (:box nil))))
   `(next-error ((,class (:inherit (region)))))
   `(query-replace ((,class (:inherit (isearch)))))
   `(region ((,class (:background ,green5))))
   `(secondary-selection ((,class (:background ,green5))))
   `(shadow ((,class (:foreground ,green5))))
   `(sh-quoted-exec ((,class (:foreground ,green13))))
   `(trailing-whitespace ((,class (:background "#BFFF7F" :foreground "black"))))

   `(font-latex-warning-face ((,class (:foreground ,green10))))
   `(font-latex-sectioning-1-face ((,class (:foreground ,white))))
   `(font-latex-sectioning-2-face ((,class (:foreground ,white))))
   `(font-latex-sectioning-3-face ((,class (:foreground ,white))))
   `(font-latex-sectioning-4-face ((,class (:foreground ,white :background ,green4))))
   `(font-latex-sectioning-5-face ((,class (:foreground ,black :background ,green4))))
   `(font-latex-math-face ((,class (:foreground ,green15))))
   `(font-latex-script-char-face ((,class (:foreground ,green15))))
   `(font-latex-string-face ((,class (:foreground ,white))))
   `(font-latex-sedate-face ((,class (:inherit (font-lock-keyword-face)))))

   `(link ((t (:foreground ,green7 :underline t))))
   `(link-visited ((t (:inherit link))))
   `(button ((t (:inherit (link)))))

   ))

(provide-theme 'green)

;;; green-theme.el ends here
