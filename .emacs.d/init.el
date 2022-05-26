;;; init --- Custom settings

(setq inhibit-default-init t)

(when (window-system)
  (tool-bar-mode 0)
  (set-scroll-bar-mode nil)
  (setq ns-pop-up-frames nil)
  (set-frame-font "IBM Plex Mono 14")
  (set-frame-position (selected-frame) 100 35)
  (set-frame-size (selected-frame) 100 56))

(setf gc-cons-threshold 100000000)

(setf inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      visible-bell nil
      ring-bell-function 'ignore
      column-number-mode nil
      size-indication-mode nil
      make-backup-files nil
      require-final-newline t
      user-full-name "Dale Roberts"
      user-mail-address "dale.o.roberts@gmail.com")

(when (not (window-system))
  (menu-bar-mode -1))

;;; install packages if needed

(add-to-list 'load-path "~/.emacs.d/lisp/")

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(setq package-list
      '(visual-regexp
	visual-regexp-steroids
	auto-complete
        py-autopep8
	clang-format
	cython-mode
	cuda-mode
	ess
	epc
	epl
	evil
	evil-leader
	evil-tabs
	fill-column-indicator
	flycheck
	git-gutter
	jedi
	key-chord
	evil-surround
	yasnippet
        smart-mode-line
	auctex
	pdf-tools
        exec-path-from-shell))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;; evil

(require 'evil)
(evil-mode 1)

(global-evil-leader-mode)
(evil-leader/set-leader ",")

(evil-set-initial-state 'help-mode 'emacs)
(evil-set-initial-state 'shell-mode 'emacs)
(evil-set-initial-state 'inferior-python-mode 'emacs)

(setq evil-esc-delay 0)
(setq evil-default-cursor 'bar)
(setq evil-normal-state-cursor 'box)
(setq evil-visual-state-cursor 'hollow)
(setq evil-replace-state-cursor 'box)
(setq evil-want-C-w-in-emacs-state t)

(define-key evil-normal-state-map (kbd "<s-return>") 'toggle-frame-sizes)

(define-key evil-normal-state-map (kbd "<SPC>") 'isearch-forward)
(define-key evil-normal-state-map (kbd "n") 'isearch-repeat-forward)
(define-key evil-normal-state-map (kbd "Q") 'fill-paragraph)
(define-key evil-normal-state-map (kbd "U") 'unfill-paragraph)
(define-key evil-normal-state-map (kbd ";") 'evil-ex)
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-normal-state-map ",b" 'ibuffer)
(define-key evil-normal-state-map ",k" 'kill-other-buffers)
(define-key evil-normal-state-map ",," 'evil-buffer)
(define-key evil-normal-state-map "\C-s\C-s" 'evil-buffer)
(define-key evil-normal-state-map ",p" 'run-python)
(define-key evil-normal-state-map ",s" 'shell)
(define-key evil-normal-state-map (kbd "<RET>") 'evil-write)
(define-key evil-normal-state-map (kbd "<s-right>") 'move-end-of-line)
(define-key evil-normal-state-map (kbd "C-e") 'move-end-of-line)
(define-key evil-normal-state-map (kbd "<s-left>") 'evil-beginning-of-line)

(define-key evil-normal-state-map (kbd "C-w <left>") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-w <right>") 'evil-window-right)
(define-key evil-normal-state-map (kbd "C-w <up>") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-w <down>") 'evil-window-down)

(define-key evil-normal-state-map (kbd "<s-left>") 'evil-window-left)
(define-key evil-normal-state-map (kbd "<s-right>") 'evil-window-right)
(define-key evil-normal-state-map (kbd "<s-up>") 'evil-window-up)
(define-key evil-normal-state-map (kbd "<s-down>") 'evil-window-down)

(define-key evil-emacs-state-map (kbd "C-w <left>") 'evil-window-left)
(define-key evil-emacs-state-map (kbd "C-w <right>") 'evil-window-right)
(define-key evil-emacs-state-map (kbd "C-w <up>") 'evil-window-up)
(define-key evil-emacs-state-map (kbd "C-w <down>") 'evil-window-down)
(define-key evil-emacs-state-map (kbd "C-w C-w") 'evil-window-next)
(define-key evil-emacs-state-map (kbd "<s-left>") 'evil-window-left)
(define-key evil-emacs-state-map (kbd "<s-right>") 'evil-window-right)
(define-key evil-emacs-state-map (kbd "<s-up>") 'evil-window-up)
(define-key evil-emacs-state-map (kbd "<s-down>") 'evil-window-down)

(define-key evil-emacs-state-map (kbd "C-w v") 'evil-window-vsplit)
(define-key evil-emacs-state-map (kbd "C-w s") 'evil-window-split)
(define-key evil-emacs-state-map (kbd "C-w c") 'evil-window-delete)

(define-key evil-emacs-state-map (kbd "<escape>") 'evil-emacs-state)

(define-key evil-visual-state-map (kbd ";") 'evil-ex)
; (define-key evil-visual-state-map (kbd "f") 'indent-region)

(define-key evil-insert-state-map "\C-s\C-s" 'evil-buffer)
(define-key evil-insert-state-map (kbd "<s-return>") 'toggle-frame-sizes)
(define-key evil-insert-state-map (kbd "<s-right>") 'move-end-of-line)
(define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
(define-key evil-insert-state-map (kbd "<s-left>") 'evil-beginning-of-line)

(define-key evil-motion-state-map (kbd ";") 'evil-ex)

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(define-key isearch-mode-map [escape] 'isearch-exit)

(define-key minibuffer-local-map (kbd "C-k") 'kill-line)

;;(global-set-key (kbd "RET") 'newline-and-indent)

(evil-ex-define-cmd "E[dit]" 'evil-edit)
(evil-ex-define-cmd "W[rite]" 'evil-write)
(evil-ex-define-cmd "Bn" 'evil-next-buffer)
(evil-ex-define-cmd "Bd" 'evil-delete-buffer)
(evil-ex-define-cmd "cn" 'flycheck-next-error)
(evil-ex-define-cmd "Cn" 'flycheck-next-error)
(evil-ex-define-cmd "cp" 'flycheck-prev-error)
(evil-ex-define-cmd "Cp" 'flycheck-prev-error)

(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-normal-state-map "ee" 'eval-buffer)
(key-chord-define evil-normal-state-map ";;" 'eval-expression)
(key-chord-define evil-insert-state-map ",," 'evil-buffer)
(key-chord-define evil-emacs-state-map ",," 'evil-buffer)
(key-chord-mode 1)

;; keyboard

(setq mac-command-modifier      'super
      ns-command-modifier       'super
      mac-option-modifier       'meta
      ns-option-modifier        'meta
      mac-right-option-modifier 'none
      ns-right-option-modifier  'none)

(global-set-key (kbd "s-s") 'evil-write)
(global-set-key (kbd "s-q") 'save-buffers-kill-terminal)
(global-set-key (kbd "s-c") 'clipboard-kill-ring-save)
(global-set-key (kbd "s-v") 'clipboard-yank)
(global-set-key (kbd "s-;") 'eval-last-sexp)
(global-set-key (kbd "s-;") 'eval-last-sexp)

;;; processes

(setq confirm-kill-processes nil)

;;; better regex

(require 'visual-regexp-steroids)

(setq vr/command-python (replace-regexp-in-string "^python "
						  "python3 "
						  vr--command-python-default))

(define-key evil-normal-state-map (kbd "<SPC>") 'vr/isearch-forward)
(define-key evil-normal-state-map (kbd "R") 'vr/replace)
(define-key evil-normal-state-map (kbd "S") 'vr/query-replace)

;;; 80 column

(when (window-system)
  (require 'fill-column-indicator)
  (setq fci-rule-use-dashes nil)
  (setq fci-always-use-textual-rule nil)
  (setq fci-rule-width 1)
  (setq fci-rule-color "gray"))

;;; flyspell

(setq ispell-program-name "aspell")
(setq ispell-dictionary "british-ise")

;;; flycheck

(defvar my-flycheck-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-p" 'flycheck-previous-error)
    (define-key map "\M-n" 'flycheck-next-error)
    map)
  "Keymap for my flycheck minor mode.")

(defun my-flymake-err-at (pos)
  (let ((overlays (overlays-at pos)))
    (remove nil
            (mapcar (lambda (overlay)
                      (and (overlay-get overlay 'flymake-overlay)
                           (overlay-get overlay 'help-echo)))
                    overlays))))

(defun my-flymake-err-echo ()
  (message "%s" (mapconcat 'identity (my-flymake-err-at (point)) "\n")))

(defadvice flymake-goto-next-error (after display-message activate compile)
  (my-flymake-err-echo))

(defadvice flymake-goto-prev-error (after display-message activate compile)
  (my-flymake-err-echo))

(define-minor-mode my-flycheck-minor-mode
  "Simple minor mode which adds some key bindings for moving to
   the next and previous errors."
  nil
  nil
  :keymap my-flycheck-minor-mode-map)

;;; Use the short version for yes/no

(fset 'yes-or-no-p 'y-or-n-p)

;;; ibuffer

(setq ibuffer-formats
      '((mark modified read-only " "
              (name 50 50 :left :elide) " "
              filename-and-process)
        (mark " " (name 16 -1) " " filename)))

(setq ibuffer-default-sorting-mode 'alphabetic)

;;; buffers

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;;; text edit functions

(require 'random-phrase)
(define-key evil-insert-state-map (kbd "C-r") 'random-word-pair-insert)

;;; unfill

(defun unfill-paragraph (&optional region)
      "Takes a multi-line paragraph and makes it into a single line of text."
      (interactive (progn (barf-if-buffer-read-only) '(t)))
      (let ((fill-column (point-max)))
        (fill-paragraph nil region)))

;;; python

(setq py-python-command "python3")
(setq python-shell-interpreter "ipython3")
(setq python-shell-interpreter-args "--pprint -c \"import numpy as np; import pandas as pd\" -i")

(defun current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

(defun my-python-send-region (&optional beg end)
  (interactive)
  (let ((beg (cond (beg beg)
		   ((region-active-p)
		    (region-beginning))
		   (t (line-beginning-position))))
	(end (cond (end end)
		   ((region-active-p)
		    (copy-marker (region-end)))
		   (t (line-end-position)))))
    (if (not (string-match-p "^\s*$" (buffer-substring beg end)))
	(python-shell-send-region beg end))
    (copy-region-as-kill beg end)
    (move-beginning-of-line 1)
    (next-line)))

(defun my-python-mode-hook ()
  (require 'python)

  (setq flycheck-python-pylint-executable "pylint")
  ;(setq jedi:environment-root "jedi")
  ;(setq elpy-rpc-python-command "python3")

  ;(jedi:setup)
  ;(setq jedi:complete-on-dot t)

  ;(define-key evil-normal-state-map (kbd "s") 'jedi:goto-definition)
  ;(define-key evil-normal-state-map (kbd "S") 'jedi:show-doc)
  ;(define-key evil-normal-state-map (kbd "<s-return>") 'my-python-send-region)

  (evil-leader/set-key-for-mode 'python-mode "f" 'py-autopep8)
  (evil-leader/set-key-for-mode 'python-mode "e" 'python-shell-send-region)
  
  (setq python-fill-docstring-style 'django)
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (toggle-truncate-lines 1)
  (setq fill-column 72)
  (auto-fill-mode t)
  
  (yas-minor-mode-on)
  (flycheck-mode 1)
  
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (add-hook 'before-save-hook 'py-autopep8-before-save))

(add-hook 'python-mode-hook 'fci-mode)
(add-hook 'python-mode-hook 'electric-pair-mode)
(add-hook 'python-mode-hook 'my-python-mode-hook)
(add-hook 'python-mode-hook 'my-flycheck-minor-mode)


;;; C++

(c-add-style "my-c++-style" 
	     '("stroustrup"
	       (indent-tabs-mode . nil) 
	       (c-basic-offset . 2)
	       (c-offsets-alist . ((inline-open . 0)
				   (brace-list-open . 0)
				   (statement-case-open . +)))))

(defun my-c++-mode-hook ()
  (require 'clang-format)
  (c-set-style "my-c++-style")
  (auto-fill-mode)         
  (c-toggle-auto-hungry-state 1)
  (define-key evil-normal-state-map ",f" 'clang-format-buffer))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)

;;; quit minibuffer

(defun minibuffer-keyboard-quit ()
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

;;; buffers

(defadvice next-buffer (after avoid-messages-buffer-in-next-buffer)
  (when (or (string-match "^\\*scratch" (buffer-name))
	    (string-match "^\\*Messages" (buffer-name))
	    (string-match "^\\*Backtrace" (buffer-name)))
    (next-buffer)))

(defadvice previous-buffer (after avoid-messages-buffer-in-next-buffer)
  (when (or (string-match "^\\*scratch" (buffer-name))
	    (string-match "^\\*Messages" (buffer-name))
	    (string-match "^\\*Backtrace" (buffer-name)))
    (previous-buffer)))

(ad-activate 'next-buffer)
(ad-activate 'previous-buffer)

;;; show paren

(setq show-paren-delay 0.1)
(show-paren-mode 1)
(setq blink-matching-paren nil)

;;; UTF-8 everywhere

(setq current-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;;; frame dimensions

(defun toggle-frame-width ()
  (interactive)
  (set-frame-size (selected-frame) (if (= (frame-width) 210) 100 210) 100)
  )

(defun toggle-frame-sizes ()
  (interactive)
  (let* ((dim-cycle (list '(100 56) '(200 56) '(100 76) '(200 76)))
	 (dim-tail (member (list (frame-width) (frame-height)) dim-cycle)))
    (if dim-tail
	(apply #'set-frame-size (selected-frame)
	       (or (car (cdr dim-tail))
		   (car dim-cycle)))
      (set-frame-size (selected-frame) (if (= (frame-width) 200) 100 200) 100))
    (message "frame-size: %s x %s" (frame-width) (frame-height))))

;;; scale fonts

(defun font-name-replace-size (font-name new-size)
  (let ((parts (split-string font-name "-")))
    (setcar (nthcdr 7 parts) (format "%d" new-size))
    (mapconcat 'identity parts "-")))

(defun increment-default-font-height (delta)
  (let* ((new-height (+ (face-attribute 'default :height) delta))
         (new-point-height (/ new-height 10)))
    (dolist (f (frame-list))
      (with-selected-frame f
        (set-frame-font (font-name-replace-size (face-font 'default)
						new-point-height) t)))
    (set-face-attribute 'default nil :height new-height)
    (message "default font size is now %d" new-point-height)))

(defun increase-default-font-height ()
  (interactive)
  (increment-default-font-height 10))

(defun decrease-default-font-height ()
  (interactive)
  (increment-default-font-height -10))

(global-set-key (kbd "s-=") 'increase-default-font-height)
(global-set-key (kbd "s--") 'decrease-default-font-height)

;;; isearch

(add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)
(defun my-goto-match-beginning ()
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end)))

(defadvice isearch-exit (after my-goto-match-beginning activate)
  "Go to beginning of match."
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end)))

(defadvice isearch-repeat (after isearch-no-fail activate)
  (unless isearch-success
    (ad-disable-advice 'isearch-repeat 'after 'isearch-no-fail)
    (ad-activate 'isearch-repeat)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-repeat 'after 'isearch-no-fail)
    (ad-activate 'isearch-repeat)))

;;; word count

(autoload 'word-count-mode "word-count"
           "Minor mode to count words." t nil)

;;; surround

(require 'evil-surround)
(global-evil-surround-mode 1)

;;; latex

(defun my-latex-chgenv ()
  (interactive)
  (let* ((env-cycle '("equation" "align" "multline"))
	 (env-tail (member (LaTeX-current-environment) env-cycle)))
    (when env-tail
      (LaTeX-modify-environment
       (or (car (cdr env-tail))
	   (car env-cycle))))))

(defun my-latex-toggle-star ()
  (interactive)
  (if (string-match "\*$" (LaTeX-current-environment))
      (LaTeX-modify-environment (substring (LaTeX-current-environment) 0 -1))
    (LaTeX-modify-environment (concat (LaTeX-current-environment) "*"))))

(defun TeX-texify ()
  (interactive)
  (save-buffer)
  (TeX-command-menu "LaTeX"))

(defun my-latex-mode-hook ()
  ;(undo-tree-mode 0)
  (auto-save-mode 0)
  (flyspell-mode 1)
  (flycheck-mode 0)
  (word-count-mode 0)
  (setq flycheck-chktexrc "~/.chktexrc")
  ;;(setq TeX-command-default "makepdf")
  (visual-line-mode 1)
  (auto-revert-mode 1)
  (yas-minor-mode-on)
  (yas-reload-all)
  (local-set-key (kbd "s-s") 'TeX-texify)
  (local-set-key (kbd "s-p") 'TeX-view)
  (local-set-key (kbd "s-c") 'my-latex-chgenv)
  (local-set-key (kbd "s-x") 'my-latex-toggle-star)
  (setq ispell-parser 'tex)
  (setq fill-column 99999)
  (fset 'font-latex-fontify-script nil)
  (fset 'tex-font-lock-subscript 'ignore))

(add-hook 'LaTeX-mode-hook 'my-latex-mode-hook)
(add-hook 'LaTeX-mode-hook 'my-flycheck-minor-mode)

(setq font-latex-fontify-sectioning 'color)
(setq font-latex-script-display (quote (nil)))
(setq font-latex-deactivated-keyword-classes
      '("italic-command" "bold-command" "italic-declaration" "bold-declaration"))

(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
      TeX-source-correlate-start-server t)

(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)

;;(eval-after-load "tex"
;;  '(add-to-list 'TeX-command-list
;;              '("makepdf" "makepdf2 %n %b" TeX-run-TeX nil t
;;                :help "Run makepdf on file")))

;; shell mode

(defadvice shell (before advice-utf-shell activate)
  (set-default-coding-systems 'utf-8))

(ad-activate 'shell)

(defun my-shell-mode-hook ()
  (setq ansi-color-names-vector
	["black" "tomato" "PaleGreen2" "gold1"
	 "DeepSkyBlue1" "MediumOrchid1" "cyan" "white"])
  (setq ansi-color-map (ansi-color-make-color-map))
  
  (local-set-key (kbd "C-u") 'eshell-kill-input)
  (local-set-key (kbd "C-k") 'kill-line)
  (local-set-key (kbd "C-a") 'move-beginning-of-line)
  (local-set-key (kbd "C-e") 'move-end-of-line)
  (local-set-key (kbd "<s-left>") 'evil-window-left)
  (local-set-key (kbd "<s-right>") 'evil-window-right)
  (local-set-key (kbd "<s-up>") 'evil-window-up)
  (local-set-key (kbd "<s-down>") 'evil-window-down)
  (local-set-key (kbd "C-w c") 'evil-window-delete)

  (define-key shell-mode-map "\C-a" 'move-beginning-of-line)
  (define-key shell-mode-map "\C-z" 'comint-stop-subjob)
  (define-key shell-mode-map "\C-c" 'comint-interrupt-subjob)
  (define-key shell-mode-map "\C-w" 'backward-kill-word)
  (define-key shell-mode-map "\C-l" 'comint-delete-output)
  (define-key shell-mode-map "\C-d" 'comint-delchar-or-maybe-eof)
  (define-key shell-mode-map "\C-p" 'comint-previous-input)
  (define-key shell-mode-map "\C-n" 'comint-next-input)
  (define-key shell-mode-map [up] 'comint-previous-input)
  (define-key shell-mode-map [down] 'comint-next-input))


(add-hook 'shell-mode-hook 'my-shell-mode-hook)

;;; inferior python

(defun my-inferior-python-mode-hook ()
  (setq ansi-color-names-vector
	["black" "tomato" "PaleGreen2" "gold1"
	 "DeepSkyBlue1" "MediumOrchid1" "cyan" "white"])
  (setq ansi-color-map (ansi-color-make-color-map))
  
  (local-set-key (kbd "C-u") 'eshell-kill-input)
  (local-set-key (kbd "C-k") 'kill-line)
  (local-set-key (kbd "C-a") 'move-beginning-of-line)
  (local-set-key (kbd "C-e") 'move-end-of-line)
  (local-set-key (kbd "<s-left>") 'evil-window-left)
  (local-set-key (kbd "<s-right>") 'evil-window-right)
  (local-set-key (kbd "<s-up>") 'evil-window-up)
  (local-set-key (kbd "<s-down>") 'evil-window-down)
  (local-set-key (kbd "C-w c") 'evil-window-delete)

  (local-set-key "\C-a" 'move-beginning-of-line)
  (local-set-key "\C-z" 'comint-stop-subjob)
  (local-set-key "\C-c" 'comint-interrupt-subjob)
  (local-set-key "\C-w" 'backward-kill-word)
  (local-set-key "\C-l" 'comint-delete-output)
  (local-set-key "\C-d" 'comint-delchar-or-maybe-eof)
  (local-set-key "\C-p" 'comint-previous-input)
  (local-set-key "\C-n" 'comint-next-input)
  (local-set-key [up] 'comint-previous-input)
  (local-set-key [down] 'comint-next-input))

(add-hook 'inferior-python-mode-hook 'my-inferior-python-mode-hook)

;;; gitgutter

(global-git-gutter-mode 1)

;;; color theme

(add-to-list 'custom-theme-load-path "~/.emacs.d")
;(if window-system (load-theme 'dr t))
(if window-system (load-theme 'protonopia t))

;;; yasnippet

(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(require 'yasnippet)

;;; paths

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "PYTHONPATH")
  (exec-path-from-shell-copy-env "PS1")
  )

;;; other window scroll

(defun my-scroll-other-window ()
  (interactive)
  (let* ((wind (other-window-for-scrolling))
         (mode (with-selected-window wind major-mode)))
    (if (eq mode 'pdf-view-mode)
        (with-selected-window wind
      (pdf-view-next-page))
      (scroll-other-window 5))))

(defun my-scroll-other-window-down ()
  (interactive)
  (let* ((wind (other-window-for-scrolling))
         (mode (with-selected-window wind major-mode)))
    (if (eq mode 'pdf-view-mode)
    (with-selected-window wind
      (progn
        (pdf-view-previous-page)
        (other-window 1)))
      (scroll-other-window-down 5))))

(global-set-key (kbd "s-<next>") 'my-scroll-other-window)
(global-set-key (kbd "s-<prior>") 'my-scroll-other-window-down)

;;; pdf-tools

(setq-default pdf-view-display-size 'fit-page)

(setq pdf-view-use-scaling t
      pdf-view-use-imagemagick nil
      pdf-view-display-size 'fit-width
      pdf-view-use-unicode-ligther nil
      pdf-view-midnight-colors '("#dddddd" . "#4e2f4c"))

(pdf-tools-install)

(evil-set-initial-state 'pdf-view-mode 'emacs)

(defun my-pdf-view-mode-hook ()
  ;(turn-off-evil-mode)
  (set (make-local-variable 'evil-emacs-state-cursor) (list nil))
  (setq pdf-view-display-size 'fit-width)
  (local-set-key "j" 'pdf-view-next-page-command)
  (local-set-key "k" 'pdf-view-previous-page-command)
  (local-set-key "g" 'beginning-of-buffer)
  (local-set-key "G" 'end-of-buffer))

(add-hook 'pdf-view-mode-hook 'my-pdf-view-mode-hook)
;(add-hook 'pdf-view-midnight-minor-mode 'my-pdf-view-mode-hook)
(add-hook 'pdf-tools-enabled-hook 'pdf-view-midnight-minor-mode)

;; help-mode

(defun my-help-mode-hook ()
  (local-set-key "q" 'quit-window))

(add-hook 'help-mode-hook 'my-help-mode-hook)

;;; LaTeX

;(require 'preview-dvisvgm)

;;; smart-mode-line

;(when (window-system)
;  (setq sml/theme 'respectful)
;  (setq sml/name-width 20)
;  (setq sml/mode-width 5)
;  (setq sml/shorten-directory 't)
;  (setq sml/shorten-mode 't)
;  (sml/setup))

;;; server
(load "server")
(unless (server-running-p) (server-start))

;;; colors

;(when window-system
;  (set-face-foreground 'git-gutter:modified "#000000")
;  (set-face-background 'git-gutter:modified "#47D6D6")
;  (set-face-background 'git-gutter:added "#59C44B")
;  (set-face-foreground 'git-gutter:added "#000000")
;  (set-face-foreground 'git-gutter:deleted "#000000")
;  (set-face-background 'git-gutter:deleted "#D64747")
;  (setq git-gutter:added-sign " ")
;  (setq git-gutter:deleted-sign " ")
					;  (setq git-gutter:modified-sign " "))



(setf gc-cons-threshold 20000000)

;;; show recently edited files on startup

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)

(ignore-errors (recentf-open-files))

;;; Other things set in GUI

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-source-correlate-mode t)
 '(TeX-view-program-selection
   '((output-dvi "open")
     (output-pdf "PDF Tools")
     (output-html "open")))
 '(package-selected-packages
   '(frame-cmds auctex pdf-tools yasnippet visual-regexp-steroids smart-mode-line py-autopep8 latex-preview-pane key-chord jedi git-gutter flycheck fill-column-indicator exec-path-from-shell evil-tabs evil-surround evil-leader ess cython-mode cuda-mode clang-format)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
