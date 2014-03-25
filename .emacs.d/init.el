;;; init --- Custom settings

(setf user-full-name "Dale Roberts")
(setf user-mail-address "dale.o.roberts@gmail.com")

(tool-bar-mode 0)
(setf inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(set-scroll-bar-mode nil)
(setf column-number-mode t)
(setf size-indication-mode t)
(setf visible-bell t)
(set-frame-font "Menlo 11")
(setf make-backup-files nil)
(setq-default indent-tabs-mode nil)
(setf require-final-newline t)

(global-set-key (kbd "RET") 'newline-and-indent)

;(setq display-time-string-forms '((format-time-string "%H:%M" now)))
;(display-time-mode 1)

;; install packages if needed

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
	git-commit-mode
	git-gutter
	jedi
	key-chord
	surround
	yasnippet
        smart-mode-line
        exec-path-from-shell))

(setq package-archives '(
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ;("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ;("marmalade" . "http://marmalade-repo.org/packages/")
                         ))
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

(global-undo-tree-mode 0)

;; python

(defun my-python-mode-hook ()
  (toggle-truncate-lines 1)
  (setq fill-column 72)
  (setq jedi:complete-on-dot t)
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (add-hook 'before-save-hook 'py-autopep8-before-save) )

(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'fci-mode)
(add-hook 'python-mode-hook 'autopair-mode)
(add-hook 'python-mode-hook 'my-python-mode-hook)

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

;; evil

(require 'evil)
(evil-mode 1)

(global-evil-leader-mode)
(evil-leader/set-leader ",")

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
(define-key evil-visual-state-map (kbd ";") 'evil-ex)
(define-key evil-visual-state-map (kbd "f") 'indent-region)
(define-key evil-motion-state-map (kbd ";") 'evil-ex)

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-normal-state-map "ee" 'eval-buffer)
(key-chord-mode 1)

;; surround

(require 'surround)
(global-surround-mode 1)

;; term

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; flycheck

(add-hook 'after-init-hook #'global-flycheck-mode)

;; latex

(defun my-latex-mode-hook ()
  (undo-tree-mode 0)
  (flyspell-mode 1)
  (visual-line-mode 1)
  (setq ispell-parser 'tex))

(add-hook 'LaTeX-mode-hook 'my-latex-mode-hook)

(fset 'font-latex-fontify-script nil)
(fset 'tex-font-lock-subscript 'ignore)

(setq font-latex-fontify-sectioning 'color)
(setq font-latex-script-display (quote (nil)))
(setq font-latex-deactivated-keyword-classes
      '("italic-command" "bold-command" "italic-declaration" "bold-declaration"))
(custom-set-faces
 '(flycheck-error ((t nil)))
 '(flycheck-info ((t nil)))
 '(flycheck-warning ((t nil)))
 '(font-latex-subscript-face ((t nil)) t)
 '(font-latex-superscript-face ((t nil)) t))

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

;; gitgutter

(global-git-gutter-mode 1)

;; color theme
(add-to-list 'custom-theme-load-path "~/.emacs.d")
(if window-system (load-theme 'dr t))

;; window size
(if (window-system) (set-frame-size (selected-frame) 90 55))

;; yasnippet
(require 'yasnippet)
;(yas-load-directory "~/.emacs.d/snippets")
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)

;; paths
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

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
