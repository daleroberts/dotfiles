;;; init --- Custom settings

(setf inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      visible-bell t
      column-number-mode nil
      size-indication-mode nil
      make-backup-files nil
      require-final-newline t
      user-full-name "Dale Roberts"
      user-mail-address "dale.o.roberts@gmail.com")

(when (window-system)
  (tool-bar-mode 0)
  (set-scroll-bar-mode nil)
  (set-frame-font "Consolas 12")
  (set-frame-size (selected-frame) 90 55))

(when (not (window-system))
  (menu-bar-mode -1))

;(setq-default indent-tabs-mode nil)

(global-set-key (kbd "RET") 'newline-and-indent)

;; install packages if needed

(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(setq package-list
      '(auctex
	auto-complete
        py-autopep8
	epc
	epl
	evil
	evil-leader
	fill-column-indicator
	flycheck
	git-gutter
	jedi
	key-chord
	surround
	yasnippet
        smart-mode-line
        exec-path-from-shell))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; 80 column

(require 'fill-column-indicator)
(setq fci-rule-use-dashes nil)
(setq fci-always-use-textual-rule nil)
(setq fci-rule-width 1)
(setq fci-rule-color "gray")

;; flyspell

(setq ispell-program-name "aspell")
(setq ispell-dictionary "british-ise")

;; Use the short version for yes/no

(fset 'yes-or-no-p 'y-or-n-p)

;; ibuffer

(setq ibuffer-formats
      '((mark modified read-only " "
              (name 50 50 :left :elide) " "
              filename-and-process)
        (mark " " (name 16 -1) " " filename)))

;; Undo tree

(global-undo-tree-mode -1)

;; python

(defun my-python-mode-hook ()
  (setq fill-column 72)
  (setq jedi:complete-on-dot t)
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (toggle-truncate-lines 1)
  (auto-fill-mode t)
  (flycheck-mode 1)
  (jedi:setup)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (add-hook 'before-save-hook 'py-autopep8-before-save))

(add-hook 'python-mode-hook 'fci-mode)
(add-hook 'python-mode-hook 'autopair-mode)
(add-hook 'python-mode-hook 'my-python-mode-hook)

(require 'python)
(setq python-shell-interpreter "ipython")
(setq python-shell-interpreter-args "--pylab")

;; quit minibuffer

(defun minibuffer-keyboard-quit ()
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

;; buffers

(defadvice next-buffer (after avoid-messages-buffer-in-next-buffer)
  (when (string-match "^\\*" (buffer-name))
    (next-buffer)))

(defadvice previous-buffer (after avoid-messages-buffer-in-next-buffer)
  (when (string-match "^\\*" (buffer-name))
    (previous-buffer)))

(ad-activate 'next-buffer)
(ad-activate 'previous-buffer)

;; show paren

(setq show-paren-delay 0.1)
(show-paren-mode 1)
(setq blink-matching-paren nil)

;; smooth-scroll

(setq scroll-margin 3
      scroll-conservatively 0
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01)
(setq-default scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01)

;; Make default encoding UTF-8 everywhere

(setq current-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; evil

(require 'evil)
(evil-mode 1)

(global-evil-leader-mode)
(evil-leader/set-leader ",")

(evil-set-initial-state 'shell-mode 'insert)
(evil-set-initial-state 'help-mode 'normal)

(setq evil-esc-delay 0)
(setq evil-default-cursor 'bar)
(setq evil-normal-state-cursor 'box)
(setq evil-visual-state-cursor 'hollow)
(setq evil-replace-state-cursor 'box)

(define-key evil-normal-state-map (kbd "<s-return>") 'toggle-frame-fullscreen)
(define-key evil-normal-state-map (kbd "<SPC>") 'evil-search-forward)
(define-key evil-normal-state-map (kbd "n") 'isearch-repeat-forward)
(define-key evil-normal-state-map (kbd "Q") 'fill-paragraph)
(define-key evil-normal-state-map (kbd ";") 'evil-ex)
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-normal-state-map ",b" 'ibuffer)
(define-key evil-normal-state-map ",g" 'jedi:goto-definition)
(define-key evil-normal-state-map (kbd "K") 'jedi:show-doc)
(define-key evil-normal-state-map ",," 'evil-buffer)
(define-key evil-normal-state-map ",s" 'shell)
(define-key evil-normal-state-map (kbd "C-w <left>") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-w <right>") 'evil-window-right)
(define-key evil-normal-state-map (kbd "C-w <up>") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-w <down>") 'evil-window-down)
(define-key evil-normal-state-map [escape] 'keyboard-quit)

(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map (kbd ";") 'evil-ex)
(define-key evil-visual-state-map (kbd "f") 'indent-region)

(define-key evil-motion-state-map (kbd ";") 'evil-ex)

(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(evil-ex-define-cmd "E[dit]" 'evil-edit)
(evil-ex-define-cmd "W[rite]" 'evil-write)

(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-normal-state-map "ee" 'eval-buffer)
(key-chord-define evil-normal-state-map ";;" 'eval-expression)
(key-chord-mode 1)

;; surround

(require 'surround)
(global-surround-mode 1)

;; latex

(defun my-latex-chgenv ()
  (interactive)
  (let* ((env-cycle '("equation" "align" "multline"))
	 (env-tail (member (LaTeX-current-environment) env-cycle)))
    (when env-tail
      (LaTeX-modify-environment
       (or (car (cdr env-tail))
	   (car env-cycle))))))

(defun my-latex-mode-hook ()
  (undo-tree-mode 0)
  (flyspell-mode 1)
  (flycheck-mode 1)
  (visual-line-mode 1)
  (auto-revert-mode 1)
  (yas-minor-mode-on)
  (setq ispell-parser 'tex))

(add-hook 'LaTeX-mode-hook 'my-latex-mode-hook)

(fset 'font-latex-fontify-script nil)
(fset 'tex-font-lock-subscript 'ignore)

(setq font-latex-fontify-sectioning 'color)
(setq font-latex-script-display (quote (nil)))
(setq font-latex-deactivated-keyword-classes
      '("italic-command" "bold-command" "italic-declaration" "bold-declaration"))

(add-hook 'LaTeX-mode-hook
          '(lambda ()
             (push
              '("makepdf" "makepdf2 %n %b" TeX-run-TeX nil t
                :help "Run makepdf on file")
              TeX-command-list)))

(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "makepdf")))

(defun TeX-texify ()
  (interactive)
  (save-buffer)
  (TeX-command-menu "makepdf"))

(evil-leader/set-key-for-mode 'latex-mode "r" 'TeX-texify)
(evil-leader/set-key-for-mode 'latex-mode "c" 'my-latex-chgenv)

;; gitgutter

(global-git-gutter-mode 1)

;; color theme

(add-to-list 'custom-theme-load-path "~/.emacs.d")
(if window-system (load-theme 'dr t))

;; yasnippet

(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))

;; paths

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "PYTHONPATH")
  (exec-path-from-shell-copy-env "PS1")
  )

;; smart-mode-line

(setq sml/theme 'respectful)
(setq sml/name-width 20)
(setq sml/mode-width 5)
(setq sml/shorten-directory 't)
(setq sml/shorten-mode 't)
(sml/setup)

;; server
;(load "server")
;(unless (server-running-p) (server-start))

;; colors

(when window-system
  (set-face-foreground 'git-gutter:modified "#ddffdd")
  (set-face-background 'git-gutter:modified "#ddffdd")
  (set-face-background 'git-gutter:added "#ddffdd")
  (set-face-foreground 'git-gutter:added "#ddffdd")
  (set-face-foreground 'git-gutter:deleted "#fdd")
  (set-face-background 'git-gutter:deleted "#fdd")
  (setq git-gutter:added-sign " ")
  (setq git-gutter:deleted-sign " ")
  (setq git-gutter:modified-sign " "))
