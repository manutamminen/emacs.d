
(defun uniq-lines (beg end)
  "Unique lines in region.
Called from a program, there are two arguments:
BEG and END (region to sort)."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (not (eobp))
        (kill-line 1)
        (yank)
        (let ((next-line (point)))
          (while
              (re-search-forward
               (format "^%s" (regexp-quote (car kill-ring))) nil t)
            (replace-match "" nil nil))
          (goto-char next-line))))))

(defun hot-expand (str)
  "Expand org template."
  (insert str)
  (org-try-structure-completion))

(defun y-expand (str)
  "Expand yas snippet."
  (insert str)
  (yas-expand))

(defun my-python-send-region (&optional beg end)
  (interactive)
  (let ((beg (cond (beg beg)
                   ((region-active-p)
                    (region-beginning))
                   (t (line-beginning-position))))
        (end (cond (end end)
                   ((region-active-p)
                    (copy-marker (region-end)))
                   (t (line-end-position)))))
    (python-shell-send-region beg end)))

(defun univ-eval ()
  (interactive)
  (if (string= evil-state "visual")
      (univ-eval-region)
    (univ-eval-defun)))

(defun univ-eval-defun ()
  (interactive)
  (cond ((eq major-mode 'lisp-interaction-mode) (eval-defun nil))
	((eq major-mode 'emacs-lisp-mode) (eval-defun nil))
	((eq major-mode 'python-mode) (python-shell-send-defun))
	((eq major-mode 'clojure-mode) (cider-eval-defun-at-point nil))
	((eq major-mode 'ess-mode) (ess-eval-function nil))
	(t (message "Not defined for this major mode"))))

(defun univ-eval-line-last-sexp ()
  (interactive)
  (cond ((eq major-mode 'lisp-interaction-mode) (eval-last-sexp nil))
	((eq major-mode 'emacs-lisp-mode) (eval-last-sexp nil))
	((eq major-mode 'python-mode) (my-python-send-region))
	((eq major-mode 'ess-mode) (ess-eval-line))
	(t (message "Not defined for this major mode"))))

(defun univ-eval-region ()
  (interactive)
  (cond ((eq major-mode 'lisp-interaction-mode) (eval-region (region-beginning) (region-end)))
	((eq major-mode 'emacs-lisp-mode) (eval-region (region-beginning) (region-end)))
	((eq major-mode 'python-mode) (python-shell-send-region (region-beginning) (region-end)))
	((eq major-mode 'ess-mode) (ess-eval-region (region-beginning) (region-end)))
	(t (message "Not defined for this major mode"))))

(defun univ-eval-buffer ()
  (interactive)
  (cond ((eq major-mode 'lisp-interaction-mode) (eval-buffer nil))
	((eq major-mode 'emacs-lisp-mode) (eval-buffer nil))
	((eq major-mode 'python-mode) (python-shell-send-buffer))
	((eq major-mode 'ess-mode) (ess-eval-buffer nil))
	(t (message "Not defined for this major mode"))))

