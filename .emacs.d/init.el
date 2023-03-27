;; init.el --- GNU Emacs init file  -*- lexical-binding: t -*-

;; Author: Kirill Beskhitrov <beskhitrov@gmail.com>

;;; Commentary:

;;; Code:

;; https://melpa.org/

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; https://melpa.org/#/use-package

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

;; https://melpa.org/#/org-bullets

(use-package org-bullets
  :init
  (setq org-bullets-bullet-list '("•"))
  :hook
  (org-mode . org-bullets-mode))

;; https://github.com/joshwnj/json-mode

(use-package json-mode)

;; https://github.com/emacs-lsp/lsp-ui

(use-package lsp-ui)

;; https://github.com/AndreaCrotti/yasnippet-snippets

(use-package yasnippet-snippets)

;; https://github.com/seagle0128/doom-modeline

(use-package doom-modeline
  :config
  (doom-modeline-mode 1)
  (setq doom-modeline-buffer-encoding nil))

;; https://github.com/doomemacs/themes

(use-package doom-themes
  :config
  (doom-themes-treemacs-config))

;; https://melpa.org/#/org-pomodoro

(use-package org-pomodoro
  :config
  (setq
   org-pomodoro-length 60
   org-pomodoro-short-break-length 5)
  :bind
  ("C-c p" . org-pomodoro))

;; https://melpa.org/#/org-download

(use-package org-download)

;; https://melpa.org/#/org-superstar

(use-package org-superstar
  :config
  (setq
   org-superstar-remove-leading-stars t
   org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

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

;; https://github.com/emacs-typescript/typescript.el

(use-package typescript-mode)

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

;; https://melpa.org/#/org-journal

(use-package org-journal
  :config
  (setq org-journal-file-type 'yearly)
  :bind
  ("C-c j n" . org-journal-new-date-entry))

;; https://melpa.org/#/npm-mode

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

(set-locale-environment "ru_RU.UTF-8")

(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq auto-save-default nil)
(delete-selection-mode t)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)

(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))

;; Window system

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

(load-theme 'doom-solarized-dark t)
(column-number-mode)

;; date & time

(setq display-time-format " %X ")
(setq display-time-interval 1)
(setq display-time-default-load-average nil)
(display-time)

(setq calendar-week-start-day 1)
(global-set-key (kbd "C-c c") 'calendar)

;; ansi-term

(defun my-term ()
  "Start Z shell."
  (interactive)
  (ansi-term "/usr/local/bin/zsh"))

(global-set-key (kbd "C-c t") 'my-term)

;; text-mode

(add-hook 'text-mode-hook
	  (lambda ()
	    (turn-on-auto-fill)))

;; ido-mode

(ido-mode t)
(setq ido-create-new-buffer 'always)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

;; org-mode

(require 'ox-md)

(setq org-hide-emphasis-markers t)	; Отображать разметку без окружающих спецсимволов.
(setq org-html-doctype "html5")
(setq org-startup-with-inline-images t)
(setq org-image-actual-width nil)
(setq org-ellipsis " +")

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   (js . t)))

(add-hook 'org-mode-hook
	  (lambda ()
	    (org-indent-mode)
	    (org-superstar-mode)))

(add-to-list 'org-src-lang-modes (cons "jsx" 'js-jsx))

(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.8))

;; Increase the size of various headings
(set-face-attribute 'org-document-title nil :font "BlexMono Nerd Font" :weight 'bold :height 1.3)
  
(dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font "BlexMono Nerd Font" :weight 'medium :height (cdr face)))

;; Ensure that anything that should be fixed-pitch in Org files appears that way
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
(set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
;; (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

;; Get rid of the background on column views
(set-face-attribute 'org-column nil :background nil)
(set-face-attribute 'org-column-title nil :background nil)

;; prog-mode

(add-hook 'prog-mode-hook
	  (lambda ()
	    (display-line-numbers-mode)
	    (goto-address-mode)))

;; html-mode

(defun my-prettier-fix (&optional arg)
  "HTML self-closing tags fix and DOCTYPE fix if ARG."
  (interactive "P")
  (prettier-js)
  (goto-char (point-min))
  (while (re-search-forward " />" nil t)
    (replace-match ">"))
  (if arg
      (progn
	(goto-char (point-min))
	(insert "<!DOCTYPE html>\n")))
  (save-buffer))

(add-hook 'html-mode-hook
	  (lambda ()
	    (hs-minor-mode)
	    (local-set-key (kbd "C-x C-s") 'my-prettier-fix)))

(add-to-list 'auto-mode-alist '("\\.hbs\\'" . mhtml-mode))

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
