;; personal information

(setf user-full-name "Dale Roberts")
(setf user-mail-address "dale.o.roberts@gmail.com")

(setf make-backup-files nil)
(setq-default indent-tabs-mode nil)
(setf require-final-newline t)

;; package archives

(package-initialize)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

;; evil

(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

(setq evil-esc-delay 0)
(setq evil-default-cursor 'bar)
(setq evil-normal-state-cursor 'box)
(setq evil-visual-state-cursor 'hollow)
(setq evil-replace-state-cursor 'box)

(global-set-key (kbd "RET") 'newline-and-indent)

;; flycheck

(add-hook 'after-init-hook #'global-flycheck-mode)

;; interface

(set-scroll-bar-mode nil)
(tool-bar-mode 0)
(menu-bar-mode 0)
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
