
(setq org-confirm-babel-evaluate (not org-confirm-babel-evaluate))
(setq python-shell-prompt-detect-failure-warning nil)

;;; display/update images in the buffer after I evaluate
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

;; Set the babel languages
;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '(
;;    (R . t)
;;    (sh . t)
;;    (ipython . t)))

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

(setq org-agenda-files '("~/Dropbox/Muistettavaa/"))

(setq org-agenda-custom-commands
      '(("c" "Simple agenda view"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")))
          (agenda "")
          (alltodo "")))))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Dropbox/Muistettavaa/todo.org" "To do:")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")))

(provide 'org-default)
;;; org-defaults.el ends here
