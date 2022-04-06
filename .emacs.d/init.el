;;; init.el --- GNU Emacs init file  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;; https://melpa.org/

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;; https://github.com/jwiegley/use-package

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;; https://github.com/magit/magit

(use-package magit)

;;; https://github.com/company-mode/company-mode

(use-package company
  :init
  (global-company-mode))

;;; https://github.com/bbatsov/projectile

(use-package projectile
  :init
  (projectile-mode t))

;;; https://github.com/flycheck/flycheck

(use-package flycheck
  :init
  (global-flycheck-mode))

;;; https://github.com/joaotavora/yasnippet

(use-package yasnippet
  :hook
  (prog-mode . yas-minor-mode))

;;; https://github.com/justbur/emacs-which-key

(use-package which-key
  :config
  (which-key-mode))

;;; https://github.com/magnars/expand-region.el
(setq alphabet-start "abc def")

(use-package expand-region
  :bind
  ("C-c e r" . er/expand-region))

;;; https://github.com/Fanael/rainbow-delimiters

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

;;; https://github.com/emacs-lsp/lsp-mode

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l"
	lsp-headerline-breadcrumb-enable nil)
  :hook
  ((html-mode . lsp)
   (css-mode . lsp)
   (js-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;;; https://github.com/Alexander-Miller/treemacs

(use-package treemacs)

;;; https://github.com/joshwnj/json-mode

(use-package json-mode)

;;; https://github.com/AndreaCrotti/yasnippet-snippets

(use-package yasnippet-snippets)

;;; https://github.com/seagle0128/doom-modeline

(use-package doom-modeline
  :config
  (doom-modeline-mode 1))

;;; https://github.com/doomemacs/themes

(use-package doom-themes
  :config
  (doom-themes-treemacs-config))

;;; https://github.com/nex3/sass-mode

(use-package sass-mode)

;;; https://github.com/smihica/emmet-mode

(use-package emmet-mode
  :hook
  ((sgml-mode . emmet-mode)
   (css-mode . emmet-mode)
   (js-mode . emmet-mode))
  :config
  (setq emmet-indent-after-insert nil))

;;; https://github.com/js-emacs/js2-refactor.el

(use-package js2-refactor
  :hook
  (js-mode . js2-refactor-mode))

;;; xterm-color

(use-package xterm-color
  :config
  (setq compilation-environment '("TERM=xterm-256color"))
  (defun my/advice-compilation-filter (f proc string)
    (funcall f proc (xterm-color-filter string)))
  (advice-add 'compilation-filter :around #'my/advice-compilation-filter))

;;; https://github.com/creichert/ido-vertical-mode.el

(use-package ido-vertical-mode
  :config
  (ido-vertical-mode))

;;; https://github.com/mooz/js-doc

(use-package js-doc
  :bind
  (("C-c d f" . js-doc-insert-function-doc)
   ("C-c d o" . js-doc-insert-file-doc)
   ("C-c d t" . js-doc-insert-tag)))

;;; https://github.com/prettier/prettier-emacs

(use-package prettier-js
  :hook
  (js-mode . prettier-js-mode))

;;; npm-mode

(use-package npm-mode
  :hook
  (prog-mode . npm-mode))

;;; https://www.nordtheme.com/

(use-package nord-theme)

;;; https://github.com/johnmastro/react-snippets.el

(use-package react-snippets)

;;; General

(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq auto-save-default nil)
(delete-selection-mode t)

(menu-bar-mode -1)
(line-number-mode -1)

(setq display-time-format "⌚%X")
(setq display-time-interval 1)
(display-time)

(load-theme 'doom-vibrant t)

;;; ido-mode

(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

;;; prog-mode

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;;; html-mode

(defun my-prettier-fix ()
  "Prettier self-closing tags fix."
  (beginning-of-buffer)
  (while (re-search-forward " />" nil t)
    (replace-match ">")))

(add-hook 'html-mode-hook (lambda () (add-hook 'after-save-hook 'my-prettier-fix nil t)))

;;; js-mode

(setq js-indent-level 2)

(provide 'init)

;;; init.el ends here
