;;; init.el --- GNU Emacs init file  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;; https://melpa.org/

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (and (message "Updating packages...")
       (package-refresh-contents)
       (message "Installing use-package...")
       (package-install 'use-package)))

;;; https://github.com/magit/magit

(use-package magit
  :ensure t)

;;; https://github.com/company-mode/company-mode

(use-package company
  :ensure t
  :init (global-company-mode))

;;; https://github.com/bbatsov/projectile

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1))

;;; https://github.com/flycheck/flycheck

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;;; https://github.com/joaotavora/yasnippet

(use-package yasnippet
  :ensure t)

;;; https://github.com/justbur/emacs-which-key

(use-package which-key
  :ensure t
  :config (which-key-mode))

;;; https://github.com/Fanael/rainbow-delimiters

(use-package rainbow-delimiters
  :ensure t)

;;; https://github.com/emacs-lsp/lsp-mode

(use-package lsp-mode
  :ensure t
  :init (setq lsp-keymap-prefix "C-c l"
	      lsp-headerline-breadcrumb-enable nil)
  :hook ((html-mode . lsp)
	 (css-mode . lsp)
	 (js-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration)
	 (lsp-mode . npm-mode)
	 (lsp-mode . yas-minor-mode))
  :commands lsp)

;;; https://github.com/Alexander-Miller/treemacs

(use-package treemacs
  :ensure t)

;;; https://github.com/zk-phi/indent-guide

(use-package indent-guide
  :ensure t)

;;; https://github.com/joshwnj/json-mode

(use-package json-mode
  :ensure t)

;;; https://github.com/AndreaCrotti/yasnippet-snippets

(use-package yasnippet-snippets
  :ensure t)

;;; https://github.com/seagle0128/doom-modeline

(use-package doom-modeline
  :ensure t
  :config (doom-modeline-mode 1))

;;; https://github.com/doomemacs/themes

(use-package doom-themes
  :ensure t
  :config
  (doom-themes-treemacs-config))

;;; https://github.com/nex3/sass-mode

(use-package sass-mode
  :ensure t)

;;; https://github.com/smihica/emmet-mode

(use-package emmet-mode
  :ensure t
  :hook ((sgml-mode . emmet-mode)
	 (css-mode . emmet-mode)
	 (js-mode . emmet-mode))
  :config (setq emmet-indent-after-insert nil))

;;; xterm-color

(use-package xterm-color
  :ensure t
  :config
  (setq compilation-environment '("TERM=xterm-256color"))
  (defun my/advice-compilation-filter (f proc string)
    (funcall f proc (xterm-color-filter string)))
  (advice-add 'compilation-filter :around #'my/advice-compilation-filter))

;;; https://github.com/creichert/ido-vertical-mode.el

(use-package ido-vertical-mode
  :ensure t
  :config (ido-vertical-mode))

;;; https://github.com/mooz/js-doc

(use-package js-doc
  :ensure t)

;;; https://github.com/prettier/prettier-emacs

(use-package prettier-js
  :ensure t
  :hook (js-mode . prettier-js-mode))

;;; npm-mode

(use-package npm-mode
  :ensure t)

;;; https://www.nordtheme.com/

(use-package nord-theme
  :ensure t)

;;; https://github.com/johnmastro/react-snippets.el

(use-package react-snippets
  :ensure t)

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

(load-theme 'doom-one t)

;;; ido-mode

(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

;;; prog-mode

(defun my-prog-mode-hook ()
  "Hook for 'prog-mode."
  (display-line-numbers-mode)
  (rainbow-delimiters-mode)
  (indent-guide-mode))
    
(add-hook 'prog-mode-hook 'my-prog-mode-hook)

(provide 'init)

;;; init.el ends here
