;; init.el --- Default Emacs configuration.
;;; Commentary:
;;
;; Initialize Emacs configuration.
;;
;;; Code:

(load "~/.emacs.d/defaults.el")
(load "~/.emacs.d/utility_functions.el")

(require 'package)

(setq package-enable-at-startup nil)
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
			 ("gnu"       . "http://elpa.gnu.org/packages/")
			 ("melpa"     . "https://melpa.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package) ;; unless it is already installed
  (package-refresh-contents) ;; updage packages archive
  (package-install 'use-package)) ;; and install the most recent version of use-package

;; Enable use-package
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d9dab332207600e49400d798ed05f38372ec32132b3f7d2ba697e59088021555" default)))
 '(inferior-ess-r-program-name "/usr/local/bin/R")
 '(package-selected-packages
   (quote
    (helpful company-statistics flycheck eyebrowse elfeed syntactic-close company-web eval-in-repl org-bullets multiple-cursors suggest\.el suggest flatui-theme all-the-icons evil-lispy w3m shackle slime smartparens htmlize org-plus-contrib git-gutter powerline mode-icons worf better-shell dumb-jump ob-ipython lispyville counsel-projectile projectile flycheck-cask evil-surround exec-path-from-shell elpy evil-magit ace-popup-menu sublimity rainbow-identifiers aggressive-indent magit ranger buffer-move ivy-hydra rainbow-delimiters lispy cider ace-window company-jedi jedi yasnippet auto-complete smooth-scroll ess-eldoc f s dash ess which-key avy evil-escape evil counsel ivy general use-package))))

(use-package ace-window :ensure t)
(use-package avy :ensure t)
(use-package cl :ensure t)
(use-package counsel :ensure t)
(use-package dash :ensure t)
(use-package swiper :ensure t)
(use-package s :ensure t)
(use-package f :ensure t)
(use-package ht :ensure t)
(use-package ob-ipython :ensure t)
(use-package which-key :ensure t)
(use-package rainbow-identifiers :ensure t)
(use-package ivy-hydra :ensure t)
(use-package buffer-move :ensure t)
(use-package ranger :ensure t)
(use-package exec-path-from-shell :ensure t)
(use-package better-shell :ensure t)
(use-package bm :ensure t)
(use-package slime :ensure t)
(use-package multiple-cursors :ensure t)
(use-package exec-path-from-shell :ensure t)
(use-package all-the-icons :ensure t)
(use-package macrostep :ensure t)
(use-package suggest :ensure t)
(use-package helpful :ensure t)

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1))

(use-package lispy
  :ensure t
  :defer t
  :init
  (dolist (hook '(emacs-lisp-mode-hook
		  lisp-interaction-mode-hook
		  lisp-mode-hook
		  clojure-mode-hook))
    (add-hook hook (lambda () (lispy-mode 1)))))

(use-package magit
  :ensure t
  :defer t)

(use-package evil
  :ensure t
  :init
  (progn
    (use-package evil-escape
      :ensure t)

    (use-package evil-surround
      :ensure t)

    (use-package evil-lispy
      :ensure t)

    (use-package evil-magit
      :ensure t
      :config
      (add-hook 'magit-mode-hook 'evil-local-mode)))

  :config
  (evil-mode 1)
  (evil-escape-mode 1)
  (setq-default evil-escape-key-sequence "kj"))

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))

(use-package elpy
  :ensure t
  :defer 2
  :config
  (progn
    (when (require 'flycheck nil t)
      (remove-hook 'elpy-modules 'elpy-module-flymake)
      (remove-hook 'elpy-modules 'elpy-module-yasnippet)
      (remove-hook 'elpy-mode-hook 'elpy-module-highlight-indentation)
      (add-hook 'elpy-mode-hook 'flycheck-mode))
    (elpy-enable)
    (setq elpy-rpc-backend "jedi")))

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  (yas-global-mode 1))

(use-package projectile
  :ensure t
  :config
  (projectile-mode)
  (setq projectile-completion-system 'ivy))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-on))

(use-package dumb-jump
  :config (setq dumb-jump-selector 'ivy)
  :ensure t)

(use-package ace-link
  :ensure t
  :config (ace-link-setup-default))

(use-package cider
  :ensure t
  :config
  (add-hook 'cider-mode-hook #'eldoc-mode))

(use-package rainbow-delimiters
  :ensure t
  :config (rainbow-delimiters-mode 1))

(use-package hydra
  :ensure t
  :config
  (load "~/.emacs.d/hydras.el"))

(use-package aggressive-indent
  :ensure t
  :init
  (dolist (hook '(emacs-lisp-mode-hook
		  lisp-interaction-mode-hook
		  lisp-mode-hook
		  clojure-mode-hook))
    (add-hook hook (lambda () (aggressive-indent-mode 1)))))

(use-package ace-popup-menu
  :ensure t
  :config
  (ace-popup-menu-mode 1))

(use-package ess
  :ensure t
  :init (require 'ess-site))

(use-package worf
  :ensure t
  :diminish worf-mode
  :bind (:map org-mode-map ("C-c h" . worf-goto)))

(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-materia))

(use-package eyebrowse
  :ensure t
  :config
  (setq eyebrowse-keymap-prefix nil)
  (eyebrowse-mode 1))

(use-package git-gutter
  :ensure t
  :init (global-git-gutter-mode))

(use-package shackle
  :ensure t
  :init
  (setq shackle-rules '(("*alchemist test report*" :select nil :size 0.3 :align 'below)))
  :config
  (shackle-mode t))

(use-package eval-in-repl
  :ensure t
  :config
  (setq eir-ielm-eval-in-current-buffer t)
  (setq eir-repl-placement 'right)
  (require 'eval-in-repl-ielm)
  (require 'eval-in-repl-cider)
  (require 'eval-in-repl-python))

(use-package mode-icons
  :ensure t
  :config
  (mode-icons-mode t))

(use-package org
  :mode (("\\.org$" . org-mode))
  :ensure org-plus-contrib
  :config
  (progn
    ;; config stuff
    (org-indent-mode t)))

(use-package syntactic-close
  :ensure t
  :bind ("C-c x c" . syntactic-close))

(load "~/.emacs.d/init-flycheck.el")
(load "~/.emacs.d/init-powerline.el")
(load "~/.emacs.d/init-bm.el")
(load "~/.emacs.d/init-company.el")
(load "~/.emacs.d/init-general.el")
(load "~/.emacs.d/init-elfeed.el")
(load "~/.emacs.d/org-defaults.el")

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(when window-system (set-frame-size (selected-frame) 160 50)) ; set window size

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
