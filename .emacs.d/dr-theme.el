;;; dr-theme.el --- Custom theme for faces
;;; Commentary:
;;; Code:

(deftheme dr
  "Face colors inspired by github.")

(defvar font-lock-exception-face (make-face 'font-lock-exception-face))

(setf python-font-lock-keywords
      ;; Keywords
      `(,(rx symbol-start
             (or
              "and" "del" "from" "not" "while" "as" "elif" "global" "or" "with"
              "assert" "else" "if" "pass" "yield" "break" "except" "import" "class"
              "in" "raise" "continue" "finally" "is" "return" "def" "for" "lambda"
              "try"
              ;; Python 2:
              "print" "exec"
              ;; Python 3:
              ;; False, None, and True are listed as keywords on the Python 3
              ;; documentation, but since they also qualify as constants they are
              ;; fontified like that in order to keep font-lock consistent between
              ;; Python versions.
              "nonlocal")
             symbol-end)
        ;; functions
        (,(rx symbol-start "def" (1+ space) (group (1+ (or word ?_))))
         (1 font-lock-function-name-face))
        ;; classes
        (,(rx symbol-start "class" (1+ space) (group (1+ (or word ?_))))
         (1 font-lock-type-face))
        ;; Constants
        (,(rx symbol-start
              (or "Ellipsis" "False" "None" "NotImplemented" "True" "__debug__" "self")
              symbol-end) . font-lock-constant-face)
        ;; Decorators.
        (,(rx line-start (* (any " \t")) (group "@" (1+ (or word ?_))
                                                (0+ "." (1+ (or word ?_)))))
         (1 font-lock-type-face))
        ;; Builtin Exceptions
        (,(rx symbol-start
              (or
               "ArithmeticError" "AssertionError" "AttributeError" "BaseException"
               "DeprecationWarning" "EOFError" "EnvironmentError" "Exception"
               "FloatingPointError" "FutureWarning" "GeneratorExit" "IOError"
               "ImportError" "ImportWarning" "IndexError" "KeyError"
               "KeyboardInterrupt" "LookupError" "MemoryError" "NameError"
               "NotImplementedError" "OSError" "OverflowError"
               "PendingDeprecationWarning" "ReferenceError" "RuntimeError"
               "RuntimeWarning" "StopIteration" "SyntaxError" "SyntaxWarning"
               "SystemError" "SystemExit" "TypeError" "UnboundLocalError"
               "UnicodeDecodeError" "UnicodeEncodeError" "UnicodeError"
               "UnicodeTranslateError" "UnicodeWarning" "UserWarning" "VMSError"
               "ValueError" "Warning" "WindowsError" "ZeroDivisionError"
               ;; Python 2:
               "StandardError"
               ;; Python 3:
               "BufferError" "BytesWarning" "IndentationError" "ResourceWarning"
               "TabError")
              symbol-end) . font-lock-exception-face)
        ;; Builtins
        (,(rx symbol-start
              (or
               "abs" "all" "any" "bin" "bool" "callable" "chr" "classmethod"
               "compile" "complex" "delattr" "dict" "dir" "divmod" "enumerate"
               "eval" "filter" "float" "format" "frozenset" "getattr" "globals"
               "hasattr" "hash" "help" "hex" "id" "input" "int" "isinstance"
               "issubclass" "iter" "len" "list" "locals" "map" "max" "memoryview"
               "min" "next" "object" "oct" "open" "ord" "pow" "print" "property"
               "range" "repr" "reversed" "round" "set" "setattr" "slice" "sorted"
               "staticmethod" "str" "sum" "super" "tuple" "type" "vars" "zip"
               "__import__"
               ;; Python 2:
               "basestring" "cmp" "execfile" "file" "long" "raw_input" "reduce"
               "reload" "unichr" "unicode" "xrange" "apply" "buffer" "coerce"
               "intern"
               ;; Python 3:
               "ascii" "bytearray" "bytes" "exec"
               ;; Extra:
               "__all__" "__doc__" "__name__" "__package__")
              symbol-end) . font-lock-builtin-face)
        ;; assignments
        ;; support for a = b = c = 5
        (,(lambda (limit)
            (let ((re (python-rx (group (+ (any word ?. ?_)))
                                 (? ?\[ (+ (not (any  ?\]))) ?\]) (* space)
                                 assignment-operator)))
              (when (re-search-forward re limit t)
                (while (and (python-syntax-context 'paren)
                            (re-search-forward re limit t)))
                (if (and (not (python-syntax-context 'paren))
                         (not (equal (char-after (point-marker)) ?=)))
                    t
                  (set-match-data nil)))))
         (1 font-lock-variable-name-face nil nil))
        ;; support for a, b, c = (1, 2, 3)
        (,(lambda (limit)
            (let ((re (python-rx (group (+ (any word ?. ?_))) (* space)
                                 (* ?, (* space) (+ (any word ?. ?_)) (* space))
                                 ?, (* space) (+ (any word ?. ?_)) (* space)
                                 assignment-operator)))
              (when (and (re-search-forward re limit t)
                         (goto-char (nth 3 (match-data))))
                (while (and (python-syntax-context 'paren)
                            (re-search-forward re limit t))
                  (goto-char (nth 3 (match-data))))
                (if (not (python-syntax-context 'paren))
                    t
                  (set-match-data nil)))))
         (1 font-lock-variable-name-face nil nil))))

(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   'dr
   `(Info-title-1-face ((,class (:family "helv" :weight bold :height 1.728))))
   `(Info-title-2-face ((,class (:family "helv" :weight bold :height 1.44))))
   `(Info-title-3-face ((,class (:family "helv" :weight bold :height 1.2))))
   `(Info-title-4-face ((,class (:family "helv" :weight bold))))
   `(compilation-column-number ((,class (:foreground "DarkGreen"))))
   `(compilation-error ((,class (:foreground "Red1"))))
   `(compilation-info ((,class (:weight normal :foreground "DeepSkyBlue4"))))
   `(compilation-line-number ((,class (:foreground "DarkGreen"))))
   `(cursor ((,class (:background "#000000"))))
   `(default ((,class (:background "white" :foreground "black"))))
   `(dired-marked ((,class (:background "dodgerblue3" :foreground "white"))))
   `(flycheck-fringe-info ((,class (:background nil :foreground "#dd1144"))))
   `(flycheck-fringe-warning ((,class (:background nil :foreground "#999988"))))
   `(flycheck-fringe-error ((,class (:background nil :foreground "#dd1144"))))
   `(flycheck-info ((,class (:background nil))))
   `(flycheck-warning ((,class (:background "#eeeeee"))))
   `(flycheck-error ((,class (:background "#fdd"))))
   `(font-lock-exception-face ((,class (:foreground "#990000" :weight bold))))
   `(font-lock-builtin-face ((,class (:foreground "#0086b3"))))
   `(font-lock-comment-delimiter-face ((,class (:foreground "#999988"))))
   `(font-lock-comment-face ((,class (:foreground "#999988" :slant italic))))
   `(font-lock-constant-face ((,class (:foreground "#990073"))))
   `(font-lock-doc-face ((,class (:foreground "peru"))))
   `(font-lock-doc-string-face ((,class (:foreground "peru"))))
   `(font-lock-function-name-face ((,class (:foreground "#990000" :weight bold))))
   `(font-lock-keyword-face ((,class (:foreground "#333333" :weight bold))))
   `(font-lock-preprocessor-face ((,class (:foreground "#000000"))))
   `(font-lock-reference-face ((,class (:foreground "#000000"))))
   `(font-lock-regexp-grouping-backslash ((,class (:weight bold))))
   `(font-lock-regexp-grouping-construct ((,class (:weight bold))))
   `(font-lock-string-face ((,class (:foreground "#DD1144"))))
   `(font-lock-type-face ((,class (:foreground "#445588" :weight bold))))
   `(font-lock-variable-name-face ((,class (:foreground "#000000"))))
   `(font-lock-warning-face ((,class (:foreground "red"))))
   `(fringe ((,class (:background "#ffffff"))))
   `(highlight ((,class (:background "gainsboro"))))
   `(ido-first-match ((,class (:weight normal :foreground "DarkOrange3"))))
   `(ido-only-match ((,class (:foreground "SeaGreen4"))))
   `(ido-subdir ((,class (:foreground nil :inherit font-lock-keyword-face))))
   `(info-header-node ((,class (:foreground "DeepSkyBlue1"))))
   `(info-header-xref ((,class (:foreground "SeaGreen2"))))
   `(info-menu-header ((,class (:family "helv" :weight bold))))
   `(info-node ((,class (:foreground "DeepSkyBlue1"))))
   `(info-xref ((,class (:foreground "SeaGreen4"))))
   `(isearch ((,class (:background "#fcf7ed" :foreground "black" :box '(:line-width 1 :color "teal")))))
   `(lazy-highlight ((,class (:background "#fcf7ed"))))
   `(match ((,class (:background "#fcf7ed" :foreground "black"))))
   `(minibuffer-prompt ((,class (:foreground "black"))))
   `(mode-line ((,class (:background "gray75" :foreground "black" ))))
   `(mode-line-buffer-id ((,class (:weight bold :background nil :foreground "black"))))
   `(mode-line-inactive ((,class (:background "gray75" :foreground "black" ))))
   `(outline-1 ((,class (:foreground "Blue3"))))
   `(outline-2 ((,class (:foreground "DodgerBlue"))))
   `(outline-3 ((,class (:foreground "SteelBlue"))))
   `(outline-4 ((,class (:foreground "RoyalBlue"))))
   `(outline-5 ((,class (:foreground "DeepSkyBlue"))))
   `(primary-selection ((,class (:background "blue3"))))
   `(region ((,class (:background "gray"))))
   `(show-paren-match-face ((,class (:background "#777" :foreground "white"))))
   `(show-paren-mismatch-face ((,class (:background "red1" :foreground "black"))))
   `(warning ((,class (:foreground "Yellow4"))))
   `(font-latex-sectioning-2-face ((,class (:foreground "black"))))
   `(font-latex-sectioning-3-face ((,class (:foreground "black"))))
   `(font-latex-sectioning-4-face ((,class (:foreground "black"))))
   `(font-latex-sectioning-5-face ((,class (:foreground "black"))))
   `(font-latex-math-face ((,class (:foreground "teal"))))
   `(font-latex-string-face ((,class (:foreground "#77507b"))))
  ))

(set-face-foreground 'git-gutter:modified "#ddffdd")
(set-face-background 'git-gutter:modified "#ddffdd")
(set-face-background 'git-gutter:added "#ddffdd")
(set-face-foreground 'git-gutter:added "#ddffdd")
(set-face-foreground 'git-gutter:deleted "#fdd")
(set-face-background 'git-gutter:deleted "#fdd")

(setq git-gutter:added-sign " ")
(setq git-gutter:deleted-sign " ")
(setq git-gutter:modified-sign " ")

(provide-theme 'dr)

;;; dr-theme.el ends here
