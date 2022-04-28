;;; init.el --- GNU Emacs init file  -*- lexical-binding: t -*-

;; Author: Kirill Beskhitrov <beskhitrov@gmail.com>

;;; Commentary:

;;; Code:

;; https://melpa.org/

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; https://github.com/jwiegley/use-package

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; https://github.com/magit/magit

(use-package magit)

;; https://github.com/company-mode/company-mode

(use-package company
  :init
  (global-company-mode))

;; https://github.com/bbatsov/projectile

(use-package projectile
  :init
  (projectile-mode t))

;; https://github.com/flycheck/flycheck

(use-package flycheck
  :init
  (setq flycheck-sql-sqlint-executable "/usr/local/lib/ruby/gems/3.1.0/bin/sqlint")
  (global-flycheck-mode))

;; https://github.com/joaotavora/yasnippet

(use-package yasnippet
  :hook
  (prog-mode . yas-minor-mode))

;; https://github.com/abo-abo/avy

(use-package avy)

;; https://github.com/justbur/emacs-which-key

(use-package which-key
  :config
  (which-key-mode))

;; https://github.com/magnars/expand-region.el

(use-package expand-region
  :bind
  ("C-c e r" . er/expand-region))

;; https://github.com/Fanael/rainbow-delimiters

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; https://github.com/emacs-lsp/lsp-mode

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l"
	lsp-headerline-breadcrumb-enable nil
	lsp-diagnostics-provider :none
	lsp-completion-provider :none)
  :hook
  ((mhtml-mode . lsp)
   (css-mode . lsp)
   (js-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration))
   :commands lsp)

;; https://github.com/purcell/exec-path-from-shell

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; https://github.com/Alexander-Miller/treemacs

(use-package treemacs)

;; https://github.com/magnars/multiple-cursors.el

(use-package multiple-cursors
  :init
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

;; https://github.com/joshwnj/json-mode

(use-package json-mode)

;; https://github.com/AndreaCrotti/yasnippet-snippets

(use-package yasnippet-snippets)

;; https://github.com/seagle0128/doom-modeline

(use-package doom-modeline
  :config
  (doom-modeline-mode 1))

;; https://github.com/doomemacs/themes

(use-package doom-themes
  :config
  (doom-themes-treemacs-config))

;; https://github.com/nex3/sass-mode

(use-package sass-mode)

;; https://github.com/smihica/emmet-mode

(use-package emmet-mode
  :hook
  ((sgml-mode . emmet-mode)
   (css-mode . emmet-mode)
   (js-mode . emmet-mode))
  :config
  (setq emmet-indent-after-insert nil))

;; https://github.com/emacs-lsp/dap-mode

(use-package dap-mode)

;; https://github.com/js-emacs/js2-refactor.el

(use-package js2-refactor
  :hook
  (js-mode . js2-refactor-mode))

;; https://github.com/atomontage/xterm-color

(use-package xterm-color
  :config
  (setq compilation-environment '("TERM=xterm-256color"))
  (defun my/advice-compilation-filter (f proc string)
    (funcall f proc (xterm-color-filter string)))
  (advice-add 'compilation-filter :around #'my/advice-compilation-filter))

;; https://github.com/creichert/ido-vertical-mode.el

(use-package ido-vertical-mode
  :config
  (ido-vertical-mode))

;; https://github.com/brotzeit/helm-xref

(use-package helm-xref)

;; https://github.com/mooz/js-doc

(use-package js-doc
  :config
  (setq
   js-doc-mail-address "beskhitrov@gmail.com"
   js-doc-author (format "Kirill Beskhitrov <%s>" js-doc-mail-address)
   js-doc-license "MIT")
  :bind
  (("C-c d f" . js-doc-insert-function-doc)
   ("C-c d o" . js-doc-insert-file-doc)
   ("C-c d t" . js-doc-insert-tag)))

;; https://github.com/prettier/prettier-emacs

(use-package prettier-js
  :hook
					;  (html-mode . prettier-js-mode)
  (css-mode . prettier-js-mode)
  (js-mode . prettier-js-mode))

;; https://github.com/emacs-lsp/helm-lsp

(use-package helm-lsp)

;; https://github.com/abicky/nodejs-repl.el

(use-package nodejs-repl
  :hook
  (js-mode . (lambda ()
               (define-key js-mode-map (kbd "C-x C-e") 'nodejs-repl-send-last-expression)
               (define-key js-mode-map (kbd "C-c C-j") 'nodejs-repl-send-line)
               (define-key js-mode-map (kbd "C-c C-r") 'nodejs-repl-send-region)
	       (define-key js-mode-map (kbd "C-c C-b") 'nodejs-repl-send-buffer)
               (define-key js-mode-map (kbd "C-c C-l") 'nodejs-repl-load-file)
               (define-key js-mode-map (kbd "C-c C-z") 'nodejs-repl-switch-to-repl))))

;; https://github.com/mojochao/npm-mode

(use-package npm-mode
  :hook
  (prog-mode . npm-mode))

;; https://github.com/purcell/sqlformat

(use-package sqlformat
  :commands (sqlformat sqlformat-buffer sqlformat-region)
  :hook (sql-mode . sqlformat-on-save-mode)
  :init
  (setq sqlformat-command 'pgformatter
        sqlformat-args '("-L" "-s2" "-W1")))

;; https://github.com/johnmastro/react-snippets.el

(use-package react-snippets)

;; General

(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq auto-save-default nil)
(delete-selection-mode t)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)
(setq ido-create-new-buffer 'always)
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))

(if window-system
    (progn
      (add-to-list 'default-frame-alist
      		   '(font . "BlexMono Nerd Font-16"))
      (scroll-bar-mode -1)
      (tool-bar-mode -1)
      (toggle-frame-fullscreen)
      (setq inhibit-startup-message t
	    inhibit-startup-echo-area-message t))
  (progn
    (menu-bar-mode -1)))

(load-theme 'doom-one t)
(column-number-mode)

(setq display-time-format (concat (all-the-icons-faicon "code") " %X "))
(setq display-time-interval 1)
(display-time)

(defun my-term ()
  "Start Z shell."
  (interactive)
  (ansi-term "/usr/local/bin/zsh"))

(global-set-key (kbd "C-c t") 'my-term)

;; ido-mode

(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

;; prog-mode

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; mhtml-mode

(defun my-prettier-fix ()
  "Prettier HTML self-closing tags fix."
  (interactive)
  (prettier-js)
  (goto-char (point-min))
  (while (re-search-forward " />" nil t)
    (replace-match ">"))
  (save-buffer))

(add-hook 'mhtml-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c C-p") 'my-prettier-fix)))

;; (add-to-list 'auto-mode-alist '("\\.handlebars\\'" . mhtml-mode))

;; js-mode

(setq js-indent-level 2)

;; sql-mode

(require 'sql)
(defalias 'sql-get-login 'ignore)

(add-hook 'sql-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c C-p") 'sql-postgres)))


(provide 'init)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("1704976a1797342a1b4ea7a75bdbb3be1569f4619134341bd5a4c1cfb16abad4" "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" "76ed126dd3c3b653601ec8447f28d8e71a59be07d010cd96c55794c3008df4d7" "e8df30cd7fb42e56a4efc585540a2e63b0c6eeb9f4dc053373e05d774332fc13" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
