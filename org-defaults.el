
(setq org-confirm-babel-evaluate (not org-confirm-babel-evaluate))
(setq python-shell-prompt-detect-failure-warning nil)

;;; display/update images in the buffer after I evaluate
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

;; Set the babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (R . t)
   (shell . t)
   (ipython . t)))

;; This makes org remote blocks work at least on Euler
(defun org-babel-temp-file (prefix &optional suffix)
  "Create a temporary file in the `org-babel-temporary-directory'.
Passes PREFIX and SUFFIX directly to `make-temp-file' with the
value of `temporary-file-directory' temporarily set to the value
of `org-babel-temporary-directory'."
  (if (file-remote-p default-directory)
      (let ((prefix
             ;; We cannot use `temporary-file-directory' as local part
             ;; on the remote host, because it might be another OS
             ;; there.  So we assume "/tmp", which ought to exist on
             ;; relevant architectures.
             (concat (file-remote-p default-directory)
                     ;; REPLACE temporary-file-directory with /tmp:
                     (expand-file-name prefix "/tmp/"))))
        (make-temp-file prefix nil suffix))
    (let ((temporary-file-directory
           (or (and (boundp 'org-babel-temporary-directory)
                    (file-exists-p org-babel-temporary-directory)
                    org-babel-temporary-directory)
               temporary-file-directory)))
      (make-temp-file prefix nil suffix))))

(setq org-agenda-files '("~/Dropbox/Muistettavaa/todo.org"
                         "~/Dropbox/Muistettavaa/inbox.org"
                         "~/Dropbox/Muistettavaa/gtd.org"
                         "~/Dropbox/Muistettavaa/notes.org"
                         "~/Dropbox/Muistettavaa/someday.org"
                         "~/Dropbox/Muistettavaa/tickler.org"))

(setq org-agenda-custom-commands
      '(("c" "Simple agenda view"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")))
          (agenda "")
          (alltodo "")))))

(setq org-capture-templates
      '(("t" "todo" entry (file+headline "~/Dropbox/Muistettavaa/todo.org" "To do")
         "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
        ("T" "Tickler" entry (file+headline "~/Dropbox/Muistettavaa/tickler.org" "Tickler")
         "* %i%? \n %U")
        ("n" "Note" entry (file+headline "~/Dropbox/Muistettavaa/notes.org" "Notes")
         "* %i%? \n %U")
        ("c" "Capture" entry (file+headline "~/Dropbox/Muistettavaa/todo.org" "To do")
         "* TODO %?\n%i\nEntered on %U\n  %a")))

(setq org-use-fast-todo-selection t)

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))

(setq org-refile-targets '(("~/Dropbox/Muistettavaa/gtd.org" :maxlevel . 4)
                           ("~/Dropbox/Muistettavaa/notes.org" :level . 1)
                           ("~/Dropbox/Muistettavaa/someday.org" :level . 2)
                           ("~/Dropbox/Muistettavaa/tickler.org" :maxlevel . 3)))

(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

(setq org-edit-src-content-indentation 0)

(provide 'org-default)
;;; org-defaults.el ends here
