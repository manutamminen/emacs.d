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
 '(custom-safe-themes
   (quote
    ("d9dab332207600e49400d798ed05f38372ec32132b3f7d2ba697e59088021555" default)))
 '(package-selected-packages
   (quote
    (htmlize org-plus-contrib git-gutter powerline mode-icons worf better-shell dumb-jump ob-ipython lispyville counsel-projectile projectile flycheck-cask evil-surround exec-path-from-shell elpy evil-magit ace-popup-menu sublimity rainbow-identifiers aggressive-indent magit ranger buffer-move ivy-hydra rainbow-delimiters lispy cider ace-window company-jedi jedi yasnippet auto-complete smooth-scroll ess-eldoc f s dash ess which-key avy evil-escape evil counsel ivy general use-package))))

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

    ;; (use-package evil-org
    ;;   :ensure t)

    (use-package evil-surround
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
  (projectile-mode)
  (setq projectile-completion-system 'ivy))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-on))

;; (use-package ivy-rich
;;   :ensure t
;;   :config
;;   (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer)
;;   (setq ivy-virtual-abbreviate 'full
;; 	ivy-rich-switch-buffer-align-virtual-buffer t))

(use-package dumb-jump
  :config (setq dumb-jump-selector 'ivy)
  :ensure t)

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

(use-package better-shell
  :ensure t)

(use-package worf
  :ensure t
  :diminish worf-mode
  :bind (:map org-mode-map ("C-c h" . worf-goto)))

(use-package bm
  :ensure t
  :bind (("C-c =" . bm-toggle)
         ("C-c [" . bm-previous)
         ("C-c ]" . bm-next)))

(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-materia))

(use-package git-gutter
  :ensure t
  :init (global-git-gutter-mode))

;; Set font to Hack
;; (if (eq system-type 'darwin)
;;     (set-face-attribute 'default nil :font "Hack-14")
;;   (set-face-attribute 'default nil :font "DejaVu Sans Mono" :height 110))

;; Reference: https://github.com/hlissner/.emacs.d/blob/master/core/core-modeline.el
;; and https://buildfunthings.com/emacs/emacs-config.html
(use-package powerline
  :ensure t
  :config
  (defvar mode-line-height 30 "A little bit taller, a little bit baller.")

  (defvar mode-line-bar          (eval-when-compile (pl/percent-xpm mode-line-height 100 0 100 0 3 "#909fab" nil)))
  (defvar mode-line-eldoc-bar    (eval-when-compile (pl/percent-xpm mode-line-height 100 0 100 0 3 "#B3EF00" nil)))
  (defvar mode-line-inactive-bar (eval-when-compile (pl/percent-xpm mode-line-height 100 0 100 0 3 "#9091AB" nil)))

  ;; Custom faces
  (defface mode-line-is-modified nil
    "Face for mode-line modified symbol")

  (defface mode-line-2 nil
    "The alternate color for mode-line text.")

  (defface mode-line-highlight nil
    "Face for bright segments of the mode-line.")

  (defface mode-line-count-face nil
    "Face for anzu/evil-substitute/evil-search number-of-matches display.")

  ;; Git/VCS segment faces
  (defface mode-line-vcs-info '((t (:inherit warning)))
    "")
  (defface mode-line-vcs-warning '((t (:inherit warning)))
    "")

  ;; Flycheck segment faces
  (defface doom-flycheck-error '((t (:inherit error)))
    "Face for flycheck error feedback in the modeline.")
  (defface doom-flycheck-warning '((t (:inherit warning)))
    "Face for flycheck warning feedback in the modeline.")


  (defun doom-ml-flycheck-count (state)
    "Return flycheck information for the given error type STATE."
    (when (flycheck-has-current-errors-p state)
      (if (eq 'running flycheck-last-status-change)
	  "?"
	(cdr-safe (assq state (flycheck-count-errors flycheck-current-errors))))))

  (defun doom-fix-unicode (font &rest chars)
    "Display certain unicode characters in a specific font.
  e.g. (doom-fix-unicode \"DejaVu Sans\" ?⚠ ?★ ?λ)"
    (declare (indent 1))
    (mapc (lambda (x) (set-fontset-font
		  t (cons x x)
		  (cond ((fontp font)
			 font)
			((listp font)
			 (font-spec :family (car font) :size (nth 1 font)))
			((stringp font)
			 (font-spec :family font))
			(t (error "FONT is an invalid type: %s" font)))))
	  chars))

  ;; Make certain unicode glyphs bigger for the mode-line.
  ;; FIXME Replace with all-the-icons?
  (doom-fix-unicode '("DejaVu Sans Mono" 15) ?✱) ;; modified symbol
  (let ((font "DejaVu Sans Mono for Powerline")) ;;
    (doom-fix-unicode (list font 12) ?)  ;; git symbol
    (doom-fix-unicode (list font 16) ?∄)  ;; non-existent-file symbol
    (doom-fix-unicode (list font 15) ?)) ;; read-only symbol

  ;; So the mode-line can keep track of "the current window"
  (defvar mode-line-selected-window nil)
  (defun doom|set-selected-window (&rest _)
    (let ((window (frame-selected-window)))
      (when (and (windowp window)
		 (not (minibuffer-window-active-p window)))
	(setq mode-line-selected-window window))))
  (add-hook 'window-configuration-change-hook #'doom|set-selected-window)
  (add-hook 'focus-in-hook #'doom|set-selected-window)
  (advice-add 'select-window :after 'doom|set-selected-window)
  (advice-add 'select-frame  :after 'doom|set-selected-window)

  (defun doom/project-root (&optional strict-p)
    "Get the path to the root of your project."
    (let (projectile-require-project-root strict-p)
      (projectile-project-root)))

  (defun *buffer-path ()
    "Displays the buffer's full path relative to the project root (includes the
  project root). Excludes the file basename. See `*buffer-name' for that."
    (when buffer-file-name
      (propertize
       (f-dirname
	(let ((buffer-path (file-relative-name buffer-file-name (doom/project-root)))
	      (max-length (truncate (/ (window-body-width) 1.75))))
	  (concat (projectile-project-name) "/"
		  (if (> (length buffer-path) max-length)
		      (let ((path (reverse (split-string buffer-path "/" t)))
			    (output ""))
			(when (and path (equal "" (car path)))
			  (setq path (cdr path)))
			(while (and path (<= (length output) (- max-length 4)))
			  (setq output (concat (car path) "/" output))
			  (setq path (cdr path)))
			(when path
			  (setq output (concat "../" output)))
			(when (string-suffix-p "/" output)
			  (setq output (substring output 0 -1)))
			output)
		    buffer-path))))
       'face (if active 'mode-line-2))))

  (defun *buffer-name ()
    "The buffer's base name or id."
    ;; FIXME Don't show uniquify tags
    (s-trim-left (format-mode-line "%b")))

  (defun *buffer-pwd ()
    "Displays `default-directory', for special buffers like the scratch buffer."
    (propertize
     (concat "[" (abbreviate-file-name default-directory) "]")
     'face 'mode-line-2))

  (defun *buffer-state ()
    "Displays symbols representing the buffer's state (non-existent/modified/read-only)"
    (when buffer-file-name
      (propertize
       (concat (if (not (file-exists-p buffer-file-name))
		   "∄"
		 (if (buffer-modified-p) "✱"))
	       (if buffer-read-only ""))
       'face 'mode-line-is-modified)))

  (defun *buffer-encoding-abbrev ()
    "The line ending convention used in the buffer."
    (if (memq buffer-file-coding-system '(utf-8 utf-8-unix))
	""
      (symbol-name buffer-file-coding-system)))

  (defun *major-mode ()
    "The major mode, including process, environment and text-scale info."
    (concat (format-mode-line mode-name)
	    (if (stringp mode-line-process) mode-line-process)
	    (and (featurep 'face-remap)
		 (/= text-scale-mode-amount 0)
		 (format " (%+d)" text-scale-mode-amount))))

  (defun *vc ()
    "Displays the current branch, colored based on its state."
    (when vc-mode
      (let ((backend (concat " " (substring vc-mode (+ 2 (length (symbol-name (vc-backend buffer-file-name)))))))
	    (face (let ((state (vc-state buffer-file-name)))
		    (cond ((memq state '(edited added))
			   'mode-line-vcs-info)
			  ((memq state '(removed needs-merge needs-update conflict removed unregistered))
			   'mode-line-vcs-warning)))))
	(if active
	    (propertize backend 'face face)
	  backend))))

  (defvar-local doom--flycheck-err-cache nil "")
  (defvar-local doom--flycheck-cache nil "")
  (defun *flycheck ()
    "Persistent and cached flycheck indicators in the mode-line."
    (when (and (featurep 'flycheck)
	       flycheck-mode
	       (or flycheck-current-errors
		   (eq 'running flycheck-last-status-change)))
      (or (and (or (eq doom--flycheck-err-cache doom--flycheck-cache)
		   (memq flycheck-last-status-change '(running not-checked)))
	       doom--flycheck-cache)
	  (and (setq doom--flycheck-err-cache flycheck-current-errors)
	       (setq doom--flycheck-cache
		     (let ((fe (doom-ml-flycheck-count 'error))
			   (fw (doom-ml-flycheck-count 'warning)))
		       (concat
			(if fe (propertize (format " •%d " fe)
					   'face (if active
						     'doom-flycheck-error
						   'mode-line)))
			(if fw (propertize (format " •%d " fw)
					   'face (if active
						     'doom-flycheck-warning
						   'mode-line))))))))))

  (defun *buffer-position ()
    "A more vim-like buffer position."
    (let ((start (window-start))
	  (end (window-end))
	  (pend (point-max)))
      (if (and (= start 1)
	       (= end pend))
	  ":All"
	(cond ((= start 1) ":Top")
	      ((= end pend) ":Bot")
	      (t (format ":%d%%%%" (/ end 0.01 pend)))))))

  (defun my-mode-line (&optional id)
    `(:eval
      (let* ((active (eq (selected-window) mode-line-selected-window))
	     (lhs (list (propertize " " 'display (if active mode-line-bar mode-line-inactive-bar))
			(*flycheck)
			" "
			(*buffer-path)
			(*buffer-name)
			" "
			(*buffer-state)
			,(if (eq id 'scratch) '(*buffer-pwd))))
	     (rhs (list (*buffer-encoding-abbrev) "  "
			(*vc)
			;;                          " "
			;;                          (when persp-curr persp-modestring)
			" " (*major-mode) "  "
			(propertize
			 (concat "(%l,%c) " (*buffer-position))
			 'face (if active 'mode-line-2))))
	     (middle (propertize
		      " " 'display `((space :align-to (- (+ right right-fringe right-margin)
							 ,(1+ (string-width (format-mode-line rhs)))))))))
	(list lhs middle rhs))))

  (setq-default mode-line-format (my-mode-line)))

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
   "as" 'better-shell-shell
   "ar" 'better-shell-remote-open

   "b" '(:ignore t :which-key "Buffer tools")
   "bd" 'kill-this-buffer
   "bb" 'ivy-switch-buffer
   "bm" 'bm-toggle
   "bn" 'bm-next
   "bp" 'bm-previous
   "bc" 'bm-remove-all-current-buffer

   "c" '(:ignore t :which-key "Flycheck")
   "cb" 'flycheck-buffer
   "cl" 'flycheck-list-errors

   "d" '(:ignore t :which-key "Dumb-jump")
   "dd" 'dumb-hydra/body
   "dj" 'dumb-jump-go
   "do" 'dumb-jump-go-other-window

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
   "og" 'worf-goto
   "os" 'org-schedule
   "om" 'org-columns
   "ol" 'org-store-link
   "oo" 'org-show-todo-tree
   "ot" 'org-todo
   "or" 'org-archive-subtree

   "p" '(:ignore t :which-key "Projectile tools")
   "pp" 'counsel-projectile-switch-project
   "pf" 'counsel-projectile-find-file

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
   "ws" 'evil-window-split

   "z" '(:ignore t :which-key "Font scaling")
   "zz" 'hydra-font/body)

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

  (define-key evil-insert-state-map (kbd "C")
    'self-insert-command)

  (define-key evil-insert-state-map (kbd "C-d")
    'evil-scroll-down)

  (define-key inferior-ess-mode-map (kbd "C-d")
    'evil-scroll-down)
  
  (define-key evil-insert-state-map (kbd "C-b")
    'evil-scroll-page-up)
  
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
