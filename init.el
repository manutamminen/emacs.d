;; init.el --- Default Emacs configuration.
;;; Commentary:
;;
;; Initialize Emacs configuration.
;;
;;; Code:

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(load "~/.emacs.d/defaults.el")

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

(use-package ace-window)
(use-package avy)
(use-package dash)
(use-package swiper)
(use-package s)
(use-package f)
(use-package ht)
(use-package ob-ipython)
(use-package which-key)
(use-package rainbow-identifiers)
(use-package ivy-hydra)
(use-package buffer-move)
(use-package ranger)
(use-package better-shell)
(use-package bm)
(use-package multiple-cursors)
(use-package all-the-icons)
(use-package macrostep)
(use-package suggest)
(use-package helpful)
(use-package ag)
(use-package julia-mode)
(use-package prescient)
(use-package ivy-prescient)
(use-package cl)
(use-package winum)

(use-package rg
  :straight
  (rg :host github :repo "dajva/rg.el")
  :config
  (rg-enable-default-bindings))

(use-package counsel
  :config
  (setq counsel-rg-base-command
        "rg -i -M 120 --no-heading --line-number --color never %s ."))
;; (use-package
;;   '(blackout :host github :repo "raxod502/blackout"))

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package idle-highlight-mode
  :hook prog-mode)

(use-package ivy
  :config
  (ivy-mode 1))

(use-package company-prescient
  :config
  (ivy-prescient-mode 1)
  (company-prescient-mode 1)
  (prescient-persist-mode 1))

(use-package lispy
  :hook
  ((emacs-lisp-mode
    lisp-interaction-mode
    lisp-mode
    clojure-mode) . lispy-mode))

(use-package magit)

(use-package evil
  :init
  (progn
    (use-package evil-surround)
    (use-package evil-escape)
    (use-package evil-lispy)
    (use-package evil-magit
      :config
      (add-hook 'magit-mode-hook 'evil-local-mode))
    (loop for (mode . state) in '((inferior-emacs-lisp-mode . emacs)
                                  (nrepl-mode . insert)
                                  (pylookup-mode . emacs)
                                  (epa-key-list-mode . insert)
                                  (mu4e-view-mode . insert)
                                  (comint-mode . normal)
                                  (shell-mode . insert)
                                  (git-commit-mode . insert)
                                  (git-rebase-mode . emacs)
                                  (term-mode . emacs)
                                  (help-mode . insert)
                                  (helpful-mode . insert)
                                  (elfeed-search-mode . insert)
                                  (elfeed-show-mode . insert)
                                  (helm-grep-mode . emacs)
                                  (grep-mode . emacs)
                                  (magit-branch-manager-mode . emacs)
                                  (dired-mode . emacs))
          do (evil-set-initial-state mode state)))
  :config
  (evil-mode 1)
  (setq-default evil-escape-key-sequence "kj")
  (evil-escape-mode 1))

(use-package which-key
  :config
  (which-key-mode 1))

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
	(pyenv-mode-unset)))))

(use-package pyenv-mode-auto)

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
  (add-hook 'python-mode-hook 'smartparens-mode))

(use-package jedi
  :init
  (add-to-list 'company-backends 'company-jedi)
  :config
  (use-package company-jedi
    :init
    (add-hook 'python-mode-hook (lambda () (add-to-list 'company-backends 'company-jedi)))
    (setq company-jedi-python-bin "python")))

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
  (setq gud-pdb-command-name "python -m pdb "))

(use-package yasnippet
  :config
  (yas-reload-all)
  (yas-global-mode 1))

(use-package projectile
  :config
  (projectile-mode)
  (setq projectile-completion-system 'ivy))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode 1))

(use-package dumb-jump
  :config
  (setq dumb-jump-selector 'ivy))

(use-package ace-link
  :config
  (ace-link-setup-default))

(use-package cider)

(use-package rainbow-delimiters
  :hook
  ((lisp-interaction-mode . rainbow-delimiters-mode)
   (emacs-lisp-mode . rainbow-delimiters-mode)
   (python-mode . rainbow-delimiters-mode)
   (ess-r-mode . rainbow-delimiters-mode)))

(use-package hydra
  :config
  (load "~/.emacs.d/hydras.el"))

(use-package aggressive-indent
  :init
  (dolist (hook '(emacs-lisp-mode-hook
		  lisp-interaction-mode-hook
		  lisp-mode-hook
		  clojure-mode-hook))
    (add-hook hook (lambda () (aggressive-indent-mode 1)))))

(use-package ace-popup-menu
  :config
  (ace-popup-menu-mode 1))

(use-package ess
  :init
  (require 'ess-site) 
  :hook
  ((ess-mode . (lambda () (push '("%>%" . ?⇒) prettify-symbols-alist)))
   (ess-mode . (lambda () (push '("function" . ?λ) prettify-symbols-alist)))
   (inferior-ess-mode . (lambda () (push '("%>%" . ?⇒) prettify-symbols-alist)))
   (inferior-ess-mode . (lambda () (push '("function" . ?λ) prettify-symbols-alist)))))

(use-package worf
  :diminish worf-mode
  :bind
  (:map org-mode-map ("C-c h" . worf-goto)))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  (load-theme 'doom-one t))

(use-package eyebrowse
  :config
  (setq eyebrowse-keymap-prefix nil)
  (eyebrowse-mode 1))

(use-package git-gutter
  :init
  (global-git-gutter-mode 1))

(use-package shackle
  :init
  (setq shackle-rules '(("*alchemist test report*" :select nil :size 0.3 :align 'below)))
  :config
  (shackle-mode 1))

(use-package eval-in-repl
  :config
  (setq eir-ielm-eval-in-current-buffer t)
  (setq eir-repl-placement 'right)
  (require 'eval-in-repl-ielm)
  (require 'eval-in-repl-cider)
  (require 'eval-in-repl-python))

(use-package mode-icons
  :config
  (mode-icons-mode 1))

(require 'subr-x)

(use-package git)

(defun org-git-version ()
  "The Git version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (git-run "describe"
              "--match=release\*"
              "--abbrev=6"
              "HEAD"))))

(defun org-release ()
  "The release version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (string-remove-prefix
      "release_"
      (git-run "describe"
               "--match=release\*"
               "--abbrev=0"
               "HEAD")))))

(provide 'org-version)

(use-package org
  :hook
  ((org-mode . (lambda () (push '("%>%" . ?⇒) prettify-symbols-alist)))
   (org-mode . (lambda () (push '("function" . ?λ) prettify-symbols-alist)))
   (org-mode . (lambda () (font-lock-mode 0)))
   (org-mode . poly-org-mode)
   (org-mode . org-indent-mode)))

(use-package org-bullets
  :straight
  (org-bullets :host github :repo "sabof/org-bullets")
  :hook
  (org-mode . org-bullets-mode))

(use-package org-journal
  :custom
  (org-journal-dir "~/Dropbox/Muistettavaa/Journal/")
  (org-journal-file-format "%Y%m%D")
  (org-journal-date-format "%e %b %Y (%A)"))

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
             ("s" . sx-search)))

(use-package syntactic-close
  :bind
  ("C-c x c" . syntactic-close))

(use-package super-save
  :config
  (super-save-mode 1))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package slime
  :init
  (setq inferior-lisp-program "/usr/local/bin//sbcl"))

(use-package dna-mode
  :load-path "~/gits/dna-mode")

(use-package doom-modeline
  :hook
  (after-init . doom-modeline-mode))

(use-package polymode
  :straight
  (polymode :host github :repo "polymode/polymode")
  :mode
  ("\\.org'" . poly-org-mode)
  ("\\.md" . poly-markdown-mode)
  ("\\.Rmd" . poly-markdown+r-mode))

(use-package poly-R)
(use-package poly-markdown)
(use-package poly-org)

(load "~/.emacs.d/utility-functions.el")
(load "~/.emacs.d/init-flycheck.el")
(load "~/.emacs.d/init-powerline.el")
(load "~/.emacs.d/init-bm.el")
(load "~/.emacs.d/init-company.el")
(load "~/.emacs.d/init-mu4e.el")
(load "~/.emacs.d/init-general.el")
(load "~/.emacs.d/init-elfeed.el")
(load "~/.emacs.d/init-smartparens.el")
(load "~/.emacs.d/org-defaults.el")
;; (load "~/.emacs.d/hugo.el")
;; (load "~/.emacs.d/init-js.el")
;; (load "~/.emacs.d/init-gcal.el")

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(when window-system (set-frame-size (selected-frame) 160 50)) ; set window size

(provide 'init)
;;; init.el ends here
