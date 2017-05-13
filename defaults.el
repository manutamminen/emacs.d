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
(setq default-fill-column 80)		; toggle wrapping text at the 80th character

(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

;; Fix mousewheen scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

(let ((display-table (or standard-display-table (make-display-table))))
  (set-display-table-slot display-table 'vertical-border (make-glyph-code ?┃))
  (setq standard-display-table display-table))

(scroll-bar-mode -1)

(fset 'yes-or-no-p 'y-or-n-p)

(global-prettify-symbols-mode 1)
(add-hook 'ess-mode-hook
	  (lambda ()
	    (push '("%>%" . ?⇒) prettify-symbols-alist)))
(add-hook 'inferior-ess-mode-hook
	  (lambda ()
	    (push '("%>%" . ?⇒) prettify-symbols-alist)))


(global-set-key (kbd "M-x") 'counsel-M-x)

(defface org-block-begin-line
  '((t (:underline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF")))
  "Face used for the line delimiting the begin of source blocks.")

(defface org-block-background
  '((t (:background "#f2f2f2")))
  "Face used for the source block background.")

(defface org-block-end-line
  '((t (:overline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF")))
  "Face used for the line delimiting the end of source blocks.")

(tool-bar-mode 0)

(setq org-src-fontify-natively t)
(setq tab-always-indent 'complete)

(add-to-list 'exec-path "/Users/tamminma/bin")
(add-to-list 'exec-path "/usr/local/Cellar/emacs/25.1/bin/")
(setq with-editor-emacsclient-executable "/usr/local/Cellar/emacs/25.1/bin/emacsclient")

;; (setq org-capture-templates
;;       '(("t" "todo" entry (file org-default-notes-file)
;; 	 "* TODO %?\n%u\n%a\n" :clock-in t :clock-resume t)
;; 	("m" "Meeting" entry (file org-default-notes-file)
;; 	 "* MEETING with %? :MEETING:\n%t" :clock-in t :clock-resume t)
;; 	("d" "Diary" entry (file+datetree "~/org/diary.org")
;; 	 "* %?\n%U\n" :clock-in t :clock-resume t)
;; 	("i" "Idea" entry (file org-default-notes-file)
;; 	 "* %? :IDEA: \n%t" :clock-in t :clock-resume t)
;; 	("n" "Next Task" entry (file+headline org-default-notes-file "Tasks")
;; 	 "** NEXT %? \nDEADLINE: %t") ))

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))

(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

(setq org-use-fast-todo-selection t)

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

(setq org-directory "~/Dropbox/Muistettavaa/")
(setq org-default-notes-file "~/Dropbox/Muistettavaa/notes.org")

(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/Dropbox/Muistettavaa/notes.org")
               "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
              ("r" "respond" entry (file "~/Dropbox/Muistettavaa/notes.org")
               "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
              ("n" "note" entry (file "~/Dropbox/Muistettavaa/notes.org")
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("j" "Journal" entry (file+datetree "~/Dropbox/Muistettavaa/diary.org")
               "* %?\n%U\n" :clock-in t :clock-resume t)
              ("w" "org-protocol" entry (file "~/Dropbox/Muistettavaa/notes.org")
               "* TODO Review %c\n%U\n" :immediate-finish t)
              ("m" "Meeting" entry (file "~/Dropbox/Muistettavaa/notes.org")
               "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
              ("p" "Phone call" entry (file "~/Dropbox/Muistettavaa/notes.org")
               "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
              ("h" "Habit" entry (file "~/Dropbox/Muistettavaa/notes.org")
               "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))
