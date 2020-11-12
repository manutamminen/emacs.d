;;; defaults.el --- Default Emacs configuration.
;;; Commentary:
;;
;; Initialize the Emacs configuration with good defaults.
;;
;;; Code:

(setq delete-old-versions -1 )		; delete excess backup versions silently
(setq version-control t )		; use version control
(setq vc-make-backup-files t )		; make backups file even when in version controlled dir
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")) ) ; which directory to put backups file
(setq vc-follow-symlinks t )				       ; don't ask for confirmation when opening symlinked file
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) ) ;transform backups file name
(setq inhibit-startup-screen t )	; inhibit useless and old-school startup screen
(setq ring-bell-function 'ignore )	; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8 )	; use utf-8 by default
(setq coding-system-for-write 'utf-8 )
(setq sentence-end-double-space nil)	; sentence SHOULD end with only a point.
(setq fill-column 80)		        ; toggle wrapping text at the 80th character
(setq initial-scratch-message "")
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)
(setq shell-file-name "/bin/bash")

;; encryption stuff
(setq auth-source-debug t)
(setf epa-pinentry-mode 'loopback)

(setq auth-sources
      '((:source "~/.emacs.d/.authinfo.gpg")))

;; eshell settings
(setq pcomplete-ignore-case t)

;; ivy settings
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

;; Fix mousewheen scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

(let ((display-table (or standard-display-table (make-display-table))))
  (set-display-table-slot display-table 'vertical-border (make-glyph-code ?┃))
  (setq standard-display-table display-table))

(scroll-bar-mode -1)

(setq org-agenda-files '("~/Dropbox/Muistettavaa/notes.org"
                         "~/Dropbox/Muistettavaa/todo.org"
                         "~/Dropbox/Muistettavaa/inbox.org"
                         "~/Dropbox/Muistettavaa/gtd.org"
                         "~/Dropbox/Muistettavaa/someday.org"
                         "~/Dropbox/Muistettavaa/tickler.org"))
(setq inferior-R-program-name "~/miniconda3/bin/R")
(setq inferior-ess-program "~/miniconda3/bin/R")
(setq inferior-ess-program-name "~/miniconda3/bin/R")
(setq python-shell-interpreter "~/miniconda3/bin/ipython"
      python-shell-interpreter-args "--simple-prompt -i")
(setq python-shell-enable-font-lock nil)
(add-to-list 'exec-path "~/bin")
(setq inferior-julia-program-name "/Applications/Julia-1.5.app/Contents/Resources/julia/bin/julia")

;; /Applications/Julia-1.5.app/Contents/Resources/julia/bin/julia

(fset 'yes-or-no-p 'y-or-n-p)

;; More around window configurations with C-c left and right.
(winner-mode 1)

;; Make TRAMP respect remote path variables
;; (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;; Prettify things for the specified modes
(global-prettify-symbols-mode 1)

(defface org-block-begin-line
  '((t (:underline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF")))
  "Face used for the line delimiting the begin of source blocks.")

(defface org-block-background
  '((t (:background "#52527a")))
  "Face used for the source block background.")

(defface org-block-end-line
  '((t (:overline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF")))
  "Face used for the line delimiting the end of source blocks.")

(tool-bar-mode 0)

(setq org-src-fontify-natively t)
(setq tab-always-indent 'complete)

(setq eshell-prompt-function
      (lambda ()
	(concat
	 (propertize "┌─[" 'face `(:foreground "grey"))
	 ;; (propertize (user-login-name) 'face `(:foreground "white"))
	 ;; (propertize "@" 'face `(:foreground "white"))
	 ;; (propertize (system-name) 'face `(:foreground "white"))
	 ;; (propertize "]──[" 'face `(:foreground "grey"))
	 ;; (propertize (format-time-string "%H:%M" (current-time)) 'face `(:foreground "white"))
	 ;; (propertize "]──[" 'face `(:foreground "grey"))
	 (propertize (concat (eshell/pwd)) 'face `(:foreground "white"))
	 (propertize "]\n" 'face `(:foreground "white"))
	 (propertize "└─>" 'face `(:foreground "grey"))
	 (propertize (if (= (user-uid) 0) " # " " $ ") 'face `(:foreground "grey")))))

(add-hook 'emacs-lisp-mode-hook 'linum-mode)
(add-hook 'lisp-interaction-mode-hook 'linum-mode)
(add-hook 'ess-r-mode-hook 'linum-mode)
(add-hook 'python-mode-hook 'linum-mode)

(setq ess-eval-visibly 'nowait)

(provide 'defaults)
;;; defaults.el ends here
