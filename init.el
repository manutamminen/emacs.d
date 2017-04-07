;;; init.el --- Default Emacs configuration.
;;; Commentary:
;;
;; Initialize Emacs configuration.
;;
;;; Code:

(load "~/.emacs.d/defaults.el")
(load "~/.emacs.d/utility_functions.el")

(require 'package)

(setq package-enable-at-startup nil) ; tells emacs not to load any packages before starting up
;; the following lines tell emacs where on the internet to look up
;; for new packages.
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
			 ("gnu"       . "http://elpa.gnu.org/packages/")
			 ("melpa"     . "https://melpa.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize) ; guess what this one does ?

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package) ; unless it is already installed
  (package-refresh-contents) ; updage packages archive
  (package-install 'use-package)) ; and install the most recent version of use-package

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
 '(package-selected-packages
   (quote
    (ob-ipython lispyville ivy-rich counsel-projectile projectile flycheck-cask evil-surround evil-org exec-path-from-shell elpy evil-magit ace-popup-menu sublimity rainbow-identifiers aggressive-indent magit ranger buffer-move ivy-hydra rainbow-delimiters lispy cider ace-window company-jedi jedi yasnippet auto-complete smooth-scroll ess-eldoc f s dash ess which-key avy evil-escape evil counsel ivy general use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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
    (add-hook hook (lambda () (lispy-mode 1)))
    (add-hook 'lispy-mode-hook #'lispyville-mode)))

(use-package magit
  :ensure t
  :defer t)

(use-package evil
  :ensure t
  :init
  (progn
    (use-package evil-escape
      :ensure t)

    ;; (use-package evil-lispy
    ;;   :ensure t)

    (use-package lispyville
      :ensure t)

    (use-package evil-org
      :ensure t)

    (use-package evil-surround
      :ensure t
      :config
      (evil-org-mode 0))

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

(use-package flycheck
  :ensure t
  :config
  (progn
    (setq flycheck-display-errors-function nil)
    (add-hook 'after-init-hook #'global-flycheck-mode)
    ;; (add-hook 'python-mode-hook #'flycheck-python-setup)
    (add-hook 'python-mode-hook (lambda ()
				  (progn
				    (flycheck-mode 1)
				    #'flycheck-python-setup
				    ;; (semantic-mode 1)
				    (setq flycheck-checker 'python-pylint
					  flycheck-checker-error-threshold 900
					  ;; flycheck-highlighting-mode 'lines
					  flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list
					  flycheck-pylintrc "~/.pylintrc"))))))

;; (use-package flycheck-cask
;;   :ensure t
;;   :init (add-hook 'flycheck-mode-hook 'flycheck-cask-setup))

(use-package elpy
  :ensure t
  :defer 2
  :config
  (progn
    ;; Use Flycheck instead of Flymake
    (when (require 'flycheck nil t)
      (remove-hook 'elpy-modules 'elpy-module-flymake)
      (remove-hook 'elpy-modules 'elpy-module-yasnippet)
      (remove-hook 'elpy-mode-hook 'elpy-module-highlight-indentation)
      (add-hook 'elpy-mode-hook 'flycheck-mode))
    (elpy-enable)
    ;; jedi is great
    (setq elpy-rpc-backend "jedi")))

(use-package ob-ipython
  :ensure t)

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package yasnippet
  :ensure t
  :defer t
  :config
  (yas-reload-all)
  (yas-global-mode 1))

(use-package projectile
  :ensure t
  :config
  (projectile-mode 1))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-on))

(use-package ivy-rich
  :ensure t
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer)
  (setq ivy-virtual-abbreviate 'full
	ivy-rich-switch-buffer-align-virtual-buffer t))

(use-package ace-window
  :ensure t)

(use-package cider
  :ensure t
  :config
  (add-hook 'cider-mode-hook #'eldoc-mode))

(use-package which-key
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :config
  (rainbow-delimiters-mode 1))

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

;; (use-package ess-site
;;   :ensure t
;;   :init
;;   (add-to-list 'Info-default-directory-list "~/elisp/ess-16.10/doc/info/")
;;   :load-path "~/elisp/ess-16.10/lisp/"
;;   :defer 5
;;   :config)

(use-package general
  :ensure t
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"

   ;; simple command
   "/"   'counsel-ag
   "SPC" '(avy-goto-word-or-subword-1  :which-key "go to char")

   ;; Applications

   "a" '(:ignore t :which-key "Applications")
   "ar" 'ranger
   "ac" 'calc
   "ad" 'dired

   "b" '(:ignore t :which-key "Buffer tools")
   "bd" 'kill-this-buffer
   "bb" 'ivy-switch-buffer

   "c" '(:ignore t :which-key "Flycheck")
   "cb" 'flycheck-buffer
   "cl" 'flycheck-list-errors

   "e" '(:ignore t :which-key "Evaluation")
   "ee" 'univ-eval
   "el" 'univ-eval-line-last-sexp
   "eb" 'univ-eval-buffer

   "f" '(:ignore t :which-key "File operations")
   "ff" 'counsel-find-file
   "fs" 'save-buffer

   "m" '(:ignore t :which-key "Magit")
   "ms" 'magit-status
   "mp" 'magit-dispatch-popup

   "o" '(:ignore t :which-key "Org-mode tools")
   "ob" 'org-iswitchb
   "oc" 'org-capture
   "oa" 'org-agenda
   "os" 'org-schedule
   "om" 'org-columns
   "ol" 'org-store-link
   "oo" 'org-show-todo-tree
   "ot" 'org-todo
   "or" 'org-archive-subtree

   "q" '(:ignore t :which-key "Quit!")
   "qq" 'evil-quit

   "s" '(:ignore t :which-key "Search tools")
   "ss" 'swiper
   "sa" 'swiper-all

   "t" '(:ignore t :which-key "Toggles")
   "tl" 'lispy-mode
   "to" 'evil-org-mode
   "tr" 'rainbow-delimiters-mode
   "ti" 'rainbow-identifiers-mode
   "tw" 'toggle-truncate-lines
   "tf" 'font-lock-mode

   "w" '(:ignore t :which-key "Window tools")
   "ww" 'hydra-windows/body
   "wl" 'windmove-right
   "wh" 'windmove-left
   "wk" 'windmove-up
   "wj" 'windmove-down
   "wH" 'buf-move-left
   "wL" 'buf-move-right
   "wK" 'buf-move-up
   "wJ" 'buf-move-down
   "wi" (lambda () (interactive) (split-window-right) (windmove-right))
   "w-" (lambda () (interactive) (split-window-below) (windmove-down))
   "wm" 'delete-other-windows
   "wo" 'delete-other-windows
   "wm" 'delete-other-windows
   "wc" 'evil-window-delete
   "wv" 'evil-window-vsplit
   "ws" 'evil-window-split)

  (general-nmap
   :prefix "SPC"
   "." 'evil-avy-goto-line)

  (general-vmap
   :prefix "SPC"
   "." 'evil-avy-goto-line)

  (general-nmap
   :prefix "SPC"
   "," 'evil-avy-goto-char)

  (general-nvmap
   "'" (general-simulate-keys "C-c")
   "M-'" 'evil-goto-mark
   "M-b" 'ivy-switch-buffer)

  (general-define-key
   "C-c c" 'company-complete)

  (general-nmap
   :prefix "SPC"
   "TAB" 'mode-line-other-buffer)

  ;; (define-key inferior-ess-mode-map "<up>"
  ;;   'comint-previous-matching-input-from-input)

  ;; (define-key inferior-python-mode-map "<up>"
  ;;   'comint-previous-matching-input-from-input)

  ;; (define-key ess-mode-map "<tab>"
  ;;   'then_R_operator)
  (define-key evil-insert-state-map (kbd "C-k")
    'kill-line)

  (define-key ess-mode-map (kbd "C-%") 'then_R_operator)
  (define-key inferior-ess-mode-map (kbd "C-%") 'then_R_operator)

  (define-key lispy-mode-map ")"
    (lambda () (interactive)
      (progn
	(hydra-lispy-magic/body)
	(lispy-right-nostring 1))))

  (define-key evil-motion-state-map (kbd "ö")
    'evil-end-of-line)

  (define-key evil-normal-state-map (kbd ".")
    'evil-avy-goto-line)

  (define-key evil-normal-state-map (kbd ",")
    'evil-avy-goto-char)

  (define-key evil-insert-state-map (kbd "C-k")
    'kill-line)

  (define-key evil-insert-state-map (kbd "C-w")
    'evil-window-map)

  (define-key lispy-mode-map (kbd "C-d")
    'lispy-delete)

  (define-key comint-mode-map (kbd "<up>")
    'comint-previous-matching-input-from-input)

  (define-key comint-mode-map (kbd "<down>")
    'comint-next-matching-input-from-input)

  ;; (eval-after-load 'comint
  ;;   '(progn
  ;;      ;; originally on C-c M-r and C-c M-s
  ;;      (define-key comint-mode-map (kbd "M-p") #'comint-previous-matching-input-from-input)
  ;;      (define-key comint-mode-map (kbd "M-n") #'comint-next-matching-input-from-input)
  ;;      ;; originally on M-p and M-n
  ;;      (define-key comint-mode-map (kbd "C-c M-r") #'comint-previous-input)
  ;;      (define-key comint-mode-map (kbd "C-c M-s") #'comint-next-input)))

  (define-key org-mode-map "<"
    (lambda () (interactive)
      (if (looking-back "^")
	  (hydra-org-template/body)
	(self-insert-command 1))))

  (define-key org-mode-map ">"
    (lambda () (interactive)
      (if (looking-back "^")
	  (hydra-org-mol-template/body)
	(self-insert-command 1))))

  (define-key python-mode-map "<"
    (lambda () (interactive)
      (if (looking-back "^")
	  (hydra-python-template/body)
	(self-insert-command 1)))))

(load "~/.emacs.d/org-defaults.el")

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(when window-system (set-frame-size (selected-frame) 160 50)) ; set window size


(provide 'init)
;;; init.el ends here
