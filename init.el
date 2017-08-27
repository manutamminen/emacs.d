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
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

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
    (company-web eval-in-repl org-bullets multiple-cursors suggest\.el suggest flatui-theme all-the-icons evil-lispy w3m shackle slime smartparens htmlize org-plus-contrib git-gutter powerline mode-icons worf better-shell dumb-jump ob-ipython lispyville counsel-projectile projectile flycheck-cask evil-surround exec-path-from-shell elpy evil-magit ace-popup-menu sublimity rainbow-identifiers aggressive-indent magit ranger buffer-move ivy-hydra rainbow-delimiters lispy cider ace-window company-jedi jedi yasnippet auto-complete smooth-scroll ess-eldoc f s dash ess which-key avy evil-escape evil counsel ivy general use-package))))


;; (global-set-key (kbd "C-c o") 'ivy-occur)

(use-package general
  :ensure t
  :config
  (general-evil-setup 1))

(use-package cl
  :ensure t)

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1))

(use-package counsel
  :ensure t)

(use-package swiper
  :ensure t)

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

    ;; (use-package lispyville
    ;;   :ensure t)

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

(use-package avy
  :ensure t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))

(use-package dash
  :ensure t)

(use-package s
  :ensure t)

(use-package f
  :ensure t)

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

(use-package ob-ipython
  :ensure t)

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

(use-package ace-window
  :ensure t)

(use-package ace-link
  :ensure t
  :config (ace-link-setup-default))

(use-package cider
  :ensure t
  :config
  (add-hook 'cider-mode-hook #'eldoc-mode))

(use-package which-key
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :config (rainbow-delimiters-mode 1))

(use-package rainbow-identifiers
  :ensure t)

(use-package hydra
  :ensure t
  :config
  (load "~/.emacs.d/hydras.el"))

(use-package ivy-hydra
  :ensure t)

(use-package buffer-move
  :ensure t)

(use-package ranger
  :ensure t)

(use-package exec-path-from-shell
  :ensure t)

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

(use-package better-shell
  :ensure t)

(use-package worf
  :ensure t
  :diminish worf-mode
  :bind (:map org-mode-map ("C-c h" . worf-goto)))

(use-package bm
  :ensure t)

(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-materia))

(use-package eyebrowse
  :ensure t
  :config
  (setq eyebrowse-keymap-prefix nil)
  (eyebrowse-mode 1))

(use-package slime
  :ensure t)

(use-package multiple-cursors
  :ensure t)

(use-package git-gutter
  :ensure t
  :init (global-git-gutter-mode))

(use-package shackle
  :ensure t
  :init
  (setq shackle-rules '(("*alchemist test report*" :select nil :size 0.3 :align 'below)))
  :config
  (shackle-mode t))

(use-package exec-path-from-shell
  :ensure t)

(use-package w3m
  :ensure t
  :commands (w3m-browse-url w3m-session-crash-recovery-remove)
  :init
  (eval-when-compile
    (autoload 'w3m-search-escape-query-string "w3m-search")))

(use-package all-the-icons
  :ensure t)

(use-package macrostep
  :ensure t)

(use-package eval-in-repl
  :ensure t
  :config
;;; ielm support (for emacs lisp)
  (setq eir-ielm-eval-in-current-buffer t)
  (setq eir-repl-placement 'right)
  (require 'eval-in-repl-ielm)
  (require 'eval-in-repl-cider)
  (require 'eval-in-repl-python))

(use-package suggest
  :ensure t)

(use-package mode-icons
  :ensure t
  :config
  (mode-icons-mode t))

(load "~/.emacs.d/init-flycheck.el")
(load "~/.emacs.d/init-powerline.el")
(load "~/.emacs.d/init-bm.el")
(load "~/.emacs.d/init-company.el")
(load "~/.emacs.d/init-general.el")

(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-c o") 'ivy-occur)
(define-key evil-normal-state-map (kbd "ö") 'evil-end-of-line)
(define-key evil-normal-state-map (kbd ".") 'evil-avy-goto-line)
(define-key evil-normal-state-map (kbd ",") 'evil-avy-goto-char)
(define-key evil-insert-state-map (kbd "C-w") 'evil-window-map)
(define-key evil-insert-state-map (kbd "C-k") 'kill-line)
(define-key evil-insert-state-map (kbd "C") 'self-insert-command)
(define-key evil-insert-state-map (kbd "C-b") 'evil-scroll-page-up)
(define-key evil-insert-state-map (kbd "C-d") 'evil-scroll-down)
(evil-define-key '(insert normal) ess-mode-map (kbd "C-p") 'insert_then_R_operator_end_nl)
(evil-define-key '(insert normal) inferior-ess-mode-map (kbd "C-p") 'insert_then_R_operator_end_nl)
(evil-define-key '(insert normal) inferior-ess-mode-map (kbd "C-e") 'end-of-line)
(evil-define-key '(insert normal) inferior-ess-mode-map (kbd "C-<left>") 'left-word)
(evil-define-key '(insert normal) inferior-ess-mode-map (kbd "C-<right>") 'right-word)
(evil-define-key '(insert normal) inferior-ess-mode-map (kbd "<tab>") 'company-indent-or-complete-common)
(evil-define-key '(insert normal) ess-mode-map (kbd "C-f") 'insert_lambda_function)
(evil-define-key '(insert normal) ess-mode-map (kbd "C-e") 'end-of-line)
(evil-define-key '(insert normal) ess-mode-map (kbd "<tab>") 'company-indent-or-complete-common)
(evil-define-key '(insert normal) python-mode-map (kbd "<tab>") 'company-indent-or-complete-common)
(evil-define-key '(insert normal) python-mode-map (kbd "C-f") 'insert_lambda_function)
;; (evil-define-key '(insert normal) lisp-interaction-mode-map (kbd "C-c C-c") 'univ-eval)
(evil-define-key '(insert normal) lisp-interaction-mode-map (kbd "C-c C-l") 'eval-last-sexp)
(evil-define-key '(insert normal) lisp-interaction-mode-map (kbd "C-c C-f") 'eval-defun)
;; (evil-define-key '(insert normal) emacs-lisp-mode-map (kbd "C-c C-c") 'univ-eval)
(evil-define-key '(insert normal) emacs-lisp-mode-map (kbd "C-c C-l") 'eval-last-sexp)
(evil-define-key '(insert normal) emacs-lisp-mode-map (kbd "C-c C-f") 'eval-defun)
(evil-define-key '(insert normal) emacs-lisp-mode-map (kbd "C-c C-c") 'eval-buffer)
(evil-define-key '(insert normal) ess-help-mode-map (kbd "C-d") 'evil-scroll-down)
(evil-define-key '(insert normal) ess-help-mode-map (kbd "C-b") 'evil-scroll-up)
(evil-define-key '(insert normal) org-mode-map (kbd "C-e") 'end-of-line)
(evil-define-key '(insert normal) cider-stacktrace-mode-map (kbd "q") 'cider-popup-buffer-quit-function)
(evil-define-key '(insert normal) cider-repl-mode-map (kbd "<down>") 'cider-repl-next-input)
(evil-define-key '(insert normal) eshell-mode-map (kbd "C-v") 'evil-paste-after)
(evil-define-key '(insert normal) suggest-mode-map (kbd "C-c C-c") 'suggest-update)
(evil-define-key '(insert normal) python-mode-map (kbd "C-c C-c") 'eir-eval-in-python)
(evil-define-key '(insert normal) emacs-lisp-mode-map (kbd "C-c C-c") 'eir-eval-in-ielm)
(evil-define-key '(insert normal) lisp-interaction-mode-map (kbd "C-c C-c") 'eir-eval-in-ielm)
(evil-define-key '(insert normal) Info-mode-map (kbd "C-c C-c") 'eir-eval-in-ielm)
(evil-define-key '(insert normal) clojure-mode-map (kbd "C-c C-c") 'eir-eval-in-cider)
(evil-define-key '(insert normal) inferior-python-mode-map (kbd "C-e") 'end-of-line)
(evil-define-key '(insert normal) python-mode-map (kbd "C-e") 'end-of-line)
(define-key inferior-ess-mode-map (kbd "C-d") 'evil-scroll-down)
(define-key comint-mode-map (kbd "<up>") 'comint-previous-matching-input-from-input)
(define-key comint-mode-map (kbd "<down>") 'comint-next-matching-input-from-input)
(define-key org-mode-map (kbd "M-c") 'org-ctrl-c-ctrl-c)
(define-key org-mode-map (kbd "M-j") 'org-babel-next-src-block)
(define-key org-mode-map (kbd "M-k") 'org-babel-previous-src-block)
(define-key org-mode-map (kbd "M-l") 'org-edit-src-code)
(define-key org-src-mode-map (kbd "M-l") 'org-edit-src-exit)
(define-key emacs-lisp-mode-map (kbd "C-c m") 'macrostep-expand)
(define-key lispy-mode-map (kbd ")")
  (lambda () (interactive)
    (progn
      (hydra-lispy-magic/body)
      (lispy-right-nostring 1))))
(define-key org-mode-map (kbd "<")
  (lambda () (interactive)
    (if (looking-back "^")
	(hydra-org-template/body)
      (self-insert-command 1))))
(define-key org-mode-map ">"
  (lambda () (interactive)
    (if (looking-back "^")
	(hydra-org-mol-template/body)
      (self-insert-command 1))))
(define-key python-mode-map (kbd "<")
  (lambda () (interactive)
    (if (looking-back "^")
	(hydra-python-template/body)
      (self-insert-command 1))))
(define-key clojure-mode-map (kbd "M-r")
  (lambda () (interactive)
    "Empty the Clojure namespace"
    (cider-interactive-eval
     "(require 'clojure.tools.namespace.repl)
      (clojure.tools.namespace.repl/refresh)")))
(define-key clojure-mode-map (kbd "M-t")
  (lambda ()
    "Run tests in Clojure mode"
    (interactive "P")
    (save-buffer)
    (cider-load-current-buffer)
    (cider-interactive-eval "(speclj.core/run-specs)")
    (when arg 
      (cider-switch-to-relevant-repl-buffer nil))))

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
