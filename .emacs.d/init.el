;;; package --- Summary

;;; Commentary:

;;; Code:

;;; https://melpa.org/

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;(package-refresh-contents)
;(package-install 'use-package)

;;; General

(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq auto-save-default nil)
(delete-selection-mode t)

;;; doom-theme

;(use-package doom-themes
;  :ensure t
;  :config (load-theme 'doom-vibrant t))

;;; https://www.nordtheme.com/

(use-package nord-theme
  :ensure t
  :config (load-theme 'nord t))

;;; Интерфейс

(menu-bar-mode -1)
(line-number-mode -1)

(setq display-time-format "⌚%X")
(setq display-time-interval 1)
(display-time)

;;; https://github.com/seagle0128/doom-modeline

(use-package doom-modeline
  :ensure t
  :config (doom-modeline-mode 1))

;;; ido-mode

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode t)

;;; npm-mode
(use-package npm-mode
  :ensure t)

;;; https://www.flycheck.org/

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;;; https://github.com/company-mode/company-mode

(use-package company
  :ensure t
  :init (global-company-mode))

;;; https://github.com/joaotavora/yasnippet

(use-package yasnippet
  :ensure t)

;;; https://github.com/AndreaCrotti/yasnippet-snippets

(use-package yasnippet-snippets
  :ensure t)

;;; https://github.com/johnmastro/react-snippets.el

(use-package react-snippets
  :ensure t)

;;; prog-mode

(defun my-prog-mode-hook ()
  "Hook for 'prog-mode."
  (display-line-numbers-mode))
    
(add-hook 'prog-mode-hook 'my-prog-mode-hook)

;;; https://github.com/smihica/emmet-mode

(use-package emmet-mode
  :ensure t
  :hook  ((sgml-mode . emmet-mode)
	  (css-mode . emmet-mode)
	  (js-mode . emmet-mode))
  :config (setq emmet-indent-after-insert nil))

;;; https://github.com/emacs-lsp/lsp-mode

(use-package lsp-mode
  :ensure t
  :init (setq
	 lsp-keymap-prefix "C-c l"
	 lsp-headerline-breadcrumb-enable nil)
  :hook ((html-mode . lsp)
	 (css-mode . lsp)
	 (js-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration)
	 (lsp-mode . npm-mode)
	 (lsp-mode . yas-minor-mode))
  :commands lsp)

;;; https://github.com/justbur/emacs-which-key

(use-package which-key
  :ensure t
  :config (which-key-mode))

;;; https://github.com/prettier/prettier-emacs

(use-package prettier-js
  :ensure t
  :hook (js-mode . prettier-js-mode))

;;; json-mode

(use-package json-mode
  :ensure t)

;;; ansi=color

(use-package ansi-color
  :ensure t
  :config
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  :hook (compilation-filter . my-colorize-compilation-buffer))

(provide 'init)

;;; init.el ends here
