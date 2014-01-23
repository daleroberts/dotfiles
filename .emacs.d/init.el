(setf user-full-name "Dale Roberts")
(setf user-mail-address "dale.o.roberts@gmail.com")

(setq display-time-string-forms '((format-time-string "%H:%M" now)))
(display-time-mode 1)

(setenv "PATH" (concat "/usr/texbin:" (getenv "PATH")))

;; package archives

(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; 80 column

(require 'fill-column-indicator)
(setq fci-rule-use-dashes nil)
(setq fci-always-use-textual-rule nil)
(setq fci-rule-width 1)
(setq fci-rule-color "gray")

;; Use the short version for yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; IBuffer

(setq ibuffer-formats
      '((mark modified read-only " "
              (name 50 50 :left :elide) " "
              filename-and-process)
        (mark " " (name 16 -1) " " filename)))

;; Undo tree

(require 'undo-tree)
(global-undo-tree-mode 1)

;; python

(defun my-prog-mode-hook
  (setq fill-column 72)
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(add-hook 'python-mode-hook 'my-prog-mode-hook)
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'fci-mode)
(setq jedi:complete-on-dot t)

;; quit minibuffer

(defun minibuffer-keyboard-quit ()
  "Abort recursive edit. In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

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
(define-key evil-normal-state-map (kbd "<SPC>") 'isearch-forward)
(define-key evil-normal-state-map (kbd "Q") 'fill-paragraph)
(define-key evil-normal-state-map (kbd ";") 'evil-ex)
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-normal-state-map ",b" 'ibuffer)
(define-key evil-normal-state-map ",g" 'jedi:goto-definition)
(define-key evil-normal-state-map (kbd "K") 'jedi:show-doc)
(define-key evil-visual-state-map (kbd ";") 'evil-ex)
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

;; other

(setf make-backup-files nil)
(setq-default indent-tabs-mode nil)
(setf require-final-newline t)
(global-set-key (kbd "RET") 'newline-and-indent)

;; flycheck

(add-hook 'after-init-hook #'global-flycheck-mode)

;; interface

(set-frame-font "Source Code Pro 13")
(set-scroll-bar-mode nil)
(tool-bar-mode 0)
(setf inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(setf column-number-mode t)
(setf size-indication-mode t)
(setf visible-bell t)

;; latex

(add-hook 'latex-mode-hook 'flyspell-mode)
(add-hook 'latex-mode-hook 'variable-pitch-mode)
(add-hook 'latex-mode-hook 'auto-fill-mode)
(fset 'font-latex-fontify-script nil)
(fset 'tex-font-lock-subscript 'ignore)

(setq font-latex-fontify-sectioning 'color)
(setq font-latex-script-display (quote (nil)))
(setq font-latex-deactivated-keyword-classes
      '("italic-command" "bold-command" "italic-declaration" "bold-declaration"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-info ((t nil)))
 '(font-latex-subscript-face ((t nil)) t)
 '(font-latex-superscript-face ((t nil)) t))

(add-hook 'LaTeX-mode-hook (lambda ()
  (push
    '("makepdf" "makepdf2 %n %o %b" TeX-run-TeX nil t
      :help "Run makepdf on file")
    TeX-command-list)))

(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "makepdf")))

(defun TeX-texify ()
  (interactive)
  (save-buffer)
  (TeX-command-menu "makepdf"))

(evil-leader/set-key-for-mode 'latex-mode "r" 'TeX-texify)

;;(load "server")
;;(unless (server-running-p) (server-start))

;; gitgutter

(global-git-gutter-mode +1)

;; color theme
(add-to-list 'custom-theme-load-path "~/.emacs.d")
(load-theme 'dr t)
