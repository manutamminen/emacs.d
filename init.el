;; init.el --- Default Emacs configuration.
;;; Commentary:
;;
;; Initialize Emacs configuration.
;;
;;; Code:

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default 1)

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  (load-theme 'doom-one t))

(use-package mode-icons
  :config
  (mode-icons-mode 1))

(load "~/.emacs.d/defaults.el")

(when window-system (set-frame-size (selected-frame) 195 60)) ; set window size

(use-package s)
(use-package f)
(use-package ht)
(use-package cl)

(use-package ivy
  :config
  (ivy-mode 1))

(use-package ivy-prescient)

(use-package company-prescient
  :config
  (ivy-prescient-mode 1)
  (company-prescient-mode 1)
  (prescient-persist-mode 1))

(use-package magit)

(use-package evil
  :init
  (progn
    (use-package evil-surround)
    (use-package evil-escape)
    (use-package evil-lispy)
    (use-package evil-magit
      :config
      (add-hook 'magit-mode-hook 'evil-local-mode)))
  (loop for (mode . state) in '((inferior-emacs-lisp-mode . emacs)
                                (nrepl-mode . insert)
                                (pylookup-mode . emacs)
                                (epa-key-list-mode . insert)
                                (comint-mode . normal)
                                (shell-mode . insert)
                                (git-commit-mode . insert)
                                (git-rebase-mode . emacs)
                                (term-mode . emacs)
                                (mu4e-view-mode . insert)
                                (help-mode . insert)
                                (helpful-mode . insert)
                                (elfeed-search-mode . insert)
                                (elfeed-show-mode . insert)
                                (helm-grep-mode . emacs)
                                (grep-mode . emacs)
                                (magit-branch-manager-mode . emacs)
                                (dired-mode . emacs))
        do (evil-set-initial-state mode state))
  :config
  (evil-mode 1)
  (setq-default evil-escape-key-sequence "kj")
  (evil-escape-mode 1))

(use-package which-key
  :config
  (which-key-mode 1))

(use-package yasnippet
  :config
  (yas-reload-all)
  (yas-global-mode 1))

(use-package rainbow-delimiters
  :hook
  ((lisp-interaction-mode . rainbow-delimiters-mode)
   (emacs-lisp-mode . rainbow-delimiters-mode)
   (python-mode . rainbow-delimiters-mode)
   (ess-r-mode . rainbow-delimiters-mode)))

(use-package hydra
  :config
  (load "~/.emacs.d/hydras.el"))

(use-package ess
  :init
  (require 'ess-site) 
  :hook
  ((ess-mode . (lambda () (push '("%>%" . ?⇒) prettify-symbols-alist)))
   (ess-mode . (lambda () (push '("function" . ?λ) prettify-symbols-alist)))
   (inferior-ess-mode . (lambda () (push '("%>%" . ?⇒) prettify-symbols-alist)))
   (inferior-ess-mode . (lambda () (push '("function" . ?λ) prettify-symbols-alist)))))

(require 'subr-x)

(use-package git)

(use-package org
  :load-path "~/.emacs.d/org-mode/lisp"
  :bind (:map org-mode-map
         ("<" . (lambda () (interactive)
                  (if (looking-back "^")
                      (hydra-org-template/body)
                    (self-insert-command 1))))
         :map evil-insert-state-map
         ("C-e" . end-of-line)
         ("C-a" . beginning-of-line)))

(use-package polymode
  :straight
  (polymode :host github :repo "polymode/polymode"))

(use-package poly-R)
(use-package poly-markdown)
(use-package poly-org)

(use-package org-bullets
  :straight
  (org-bullets :host github :repo "sabof/org-bullets"))

(use-package org-journal
  :custom
  (org-journal-dir "~/Dropbox/Muistettavaa/Journal/")
  (org-journal-file-format "%Y%m%D")
  (org-journal-date-format "%e %b %Y (%A)"))

(use-package worf
  :diminish worf-mode
  :bind (:map org-mode-map
         ("C-c h" . worf-goto)))

(use-package super-save
  :config
  (super-save-mode 1))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package all-the-icons)

(use-package doom-modeline
  :hook
  (after-init . doom-modeline-mode))


(use-package ace-window :defer t)
(use-package avy :defer t)
(use-package dash :defer t)
(use-package swiper :defer t)
(use-package ob-ipython :defer t)
(use-package which-key :defer t)
(use-package rainbow-identifiers :defer t)
(use-package ivy-hydra :defer t)
(use-package buffer-move :defer t)
(use-package ranger :defer t)
(use-package better-shell :defer t)
(use-package bm :defer t)
(use-package multiple-cursors :defer t)
(use-package macrostep :defer t)
(use-package suggest :defer t)
(use-package helpful :defer t)
(use-package ag :defer t)
(use-package julia-mode :defer t)
(use-package prescient :defer t)
(use-package winum :defer t)

(use-package rg
  :straight
  (rg :host github :repo "dajva/rg.el")
  :config
  (rg-enable-default-bindings)
  :defer t)

(use-package counsel
  :config
  (setq counsel-rg-base-command
        "rg -i -M 120 --no-heading --line-number --color never %s .")
  :defer t)

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package idle-highlight-mode
  :hook prog-mode
  :defer t)

(use-package cider
  :defer t)

(use-package clojure-mode
  :bind (:map clojure-mode-map
         ("C-c C-c" . eir-eval-in-cider))
  :defer t)

(use-package lispy
  :hook
  ((emacs-lisp-mode
    lisp-interaction-mode
    lisp-mode
    clojure-mode) . lispy-mode)
  :defer t)

(use-package pyenv-mode
  :hook
  ((projectile-switch-project . (lambda () projectile-pyenv-mode-set))
   (python-mode . pyenv-mode))
  :config
  (defun projectile-pyenv-mode-set ()
    "Set pyenv version matching project name."
    (let ((project (projectile-project-name)))
      (if (member project (pyenv-mode-versions))
	  (pyenv-mode-set project)
	(pyenv-mode-unset))))
  :defer t)

(use-package pyenv-mode-auto
  :defer t)

(use-package python
  :mode
  ("\\.py'" . python-mode)
  ("\\.wsgi" . python-mode)
  :interpreter
  ("python" . python-mode)
  :init
  (setq-default indent-tabs-mode nil)
  :config
  (setq python-indent-offset 4)
  (add-hook 'python-mode-hook 'smartparens-mode)
  :bind (:map python-mode-map
         ("C-c C-c" . 'eir-eval-in-python)
         ("C-a" . beginning-of-line)
         ("C-e" . end-of-line)
         ("M-<left>" . left-word)
         ("M-<right>" . right-word)
         ("<" . (lambda () (interactive)
                  (if (looking-back "^")
                      (hydra-python-template/body)
                    (self-insert-command))))
         :map inferior-python-mode-map
         ("C-a" . beginning-of-line)
         ("C-e" . end-of-line))
  :defer t)

(use-package jedi
  :init
  (add-to-list 'company-backends 'company-jedi)
  :config
  (use-package company-jedi
    :init
    (add-hook 'python-mode-hook (lambda () (add-to-list 'company-backends 'company-jedi)))
    (setq company-jedi-python-bin "python"))
  :defer t)

(use-package elpy
  :init
  (with-eval-after-load 'python (elpy-enable))
  :config
  (elpy-enable)
  ;; Enable elpy in a Python mode
  (add-hook 'python-mode-hook 'elpy-mode)
  (setq elpy-rpc-backend "jedi")
  ;; Open the Python shell in a buffer after sending code to it
  (add-hook 'inferior-python-mode-hook 'python-shell-switch-to-shell)
  ;; Use IPython as the default shell, with a workaround to accommodate IPython 5
  ;; https://emacs.stackexchange.com/questions/24453/weird-shell-output-when-using-ipython-5  (setq python-shell-interpreter "ipython")
  (setq python-shell-interpreter-args "--simple-prompt -i")
  ;; Enable pyvenv, which manages Python virtual environments
  (pyvenv-mode 1)
  ;; Tell Python debugger (pdb) to use the current virtual environment
  ;; https://emacs.stackexchange.com/questions/17808/enable-python-pdb-on-emacs-with-virtualenv
  (setq gud-pdb-command-name "python -m pdb ")
  :defer t)

(use-package projectile
  :config
  (projectile-mode)
  (setq projectile-completion-system 'ivy)
  :defer t)

(use-package counsel-projectile
  :config
  (counsel-projectile-mode 1)
  :defer t)

(use-package dumb-jump
  :config
  (setq dumb-jump-selector 'ivy)
  :defer t)

(use-package ace-link
  :config
  (ace-link-setup-default)
  :defer t)

(use-package aggressive-indent
  :init
  (dolist (hook '(emacs-lisp-mode-hook
		  lisp-interaction-mode-hook
		  lisp-mode-hook
		  clojure-mode-hook))
    (add-hook hook (lambda () (aggressive-indent-mode 1))))
  :defer t)

(use-package ace-popup-menu
  :config
  (ace-popup-menu-mode 1)
  :defer t)

(use-package eyebrowse
  :config
  (setq eyebrowse-keymap-prefix nil)
  (eyebrowse-mode 1)
  :defer t)

(use-package git-gutter
  :init
  (global-git-gutter-mode 1)
  :defer t)

(use-package shackle
  :init
  (setq shackle-rules '(("*alchemist test report*" :select nil :size 0.3 :align 'below)))
  :config
  (shackle-mode 1)
  :defer t)

(use-package eval-in-repl
  :config
  (setq eir-ielm-eval-in-current-buffer t)
  (setq eir-repl-placement 'right)
  (require 'eval-in-repl-ielm)
  (require 'eval-in-repl-cider)
  (require 'eval-in-repl-python)
  :defer t)

(use-package sx
  :config
  (bind-keys :prefix "C-c s"
             :prefix-map my-sx-map
             :prefix-docstring "Global keymap for SX."
             ("q" . sx-tab-all-questions)
             ("i" . sx-inbox)
             ("o" . sx-open-link)
             ("u" . sx-tab-unanswered-my-tags)
             ("a" . sx-ask)
             ("s" . sx-search))
  :defer t)

(use-package syntactic-close
  :bind
  ("C-c x c" . syntactic-close)
  :defer t)

(use-package slime
  :init
  (setq inferior-lisp-program "/usr/local/bin//sbcl")
  :defer t)

(use-package dna-mode
  :load-path "~/gits/dna-mode")

(load "~/.emacs.d/utility-functions.el")
(load "~/.emacs.d/init-flycheck.el")
(load "~/.emacs.d/init-powerline.el")
(load "~/.emacs.d/init-bm.el")
(load "~/.emacs.d/init-company.el")
(load "~/.emacs.d/init-general.el")
(load "~/.emacs.d/init-elfeed.el")
(load "~/.emacs.d/init-smartparens.el")
(load "~/.emacs.d/org-defaults.el")
;; (load "~/.emacs.d/init-mu4e.el")
;; (load "~/.emacs.d/hugo.el")
;; (load "~/.emacs.d/init-js.el")
;; (load "~/.emacs.d/init-gcal.el")

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(provide 'init)
;;; init.el ends here
