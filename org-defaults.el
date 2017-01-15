(setq inferior-R-program-name "/Users/tamminma/miniconda3/bin/R")
(setq org-confirm-babel-evaluate (not org-confirm-babel-evaluate))
(setq exec-path (append exec-path '("/Users/tamminma/miniconda3/bin")))
(setq python-shell-interpreter "/Users/tamminma/miniconda3/bin/ipython")
(setq python-shell-prompt-detect-failure-warning nil)

;;; display/update images in the buffer after I evaluate
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

;; Set the babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (R . t)
   (sh . t)
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
