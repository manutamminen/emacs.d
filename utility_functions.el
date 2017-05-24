;;; utility_functions.el --- Default Emacs configuration.
;;; Commentary:
;;
;; Provide some utility functions for Emacs session.
;;
;;; Code:

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
  "Expand org template. Provide the snippet key as STR."
  (insert str)
  (org-try-structure-completion))

(defun y-expand (str)
  "Expand yas snippet. Provide the snippet key as STR."
  (insert str)
  (yas-expand))

(defun my-python-send-region (&optional beg end)
  "Evaluate the selected region in Python shell. BEG and END: selection start and end."
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
  "Evaluate the function or region of any specified language."
  (interactive)
  (if (string= evil-state "visual")
      (univ-eval-region)
    (univ-eval-defun)))

(defun univ-eval-defun ()
  "Evaluate the function definition in the defined languages."
  (interactive)
  (cond ((eq major-mode 'lisp-interaction-mode) (eval-defun nil))
	((eq major-mode 'emacs-lisp-mode) (eval-defun nil))
	((eq major-mode 'python-mode) (python-shell-send-defun))
	((eq major-mode 'clojure-mode) (cider-eval-defun-at-point nil))
	((eq major-mode 'ess-mode) (ess-eval-function nil))
	(t (message "Not defined for this major mode"))))

(defun univ-eval-line-last-sexp ()
  "Evaluate the current line or last sexp in the defined languages."
  (interactive)
  (cond ((eq major-mode 'lisp-interaction-mode) (eval-last-sexp nil))
	((eq major-mode 'emacs-lisp-mode) (eval-last-sexp nil))
	((eq major-mode 'python-mode) (my-python-send-region))
	((eq major-mode 'clojure-mode) (cider-eval-last-sexp nil))
	((eq major-mode 'ess-mode) (ess-eval-line))
	(t (message "Not defined for this major mode"))))

(defun univ-eval-region ()
  "Evaluate the selected region in the defined languages."
  (interactive)
  (cond ((eq major-mode 'lisp-interaction-mode) (eval-region (region-beginning) (region-end)))
	((eq major-mode 'emacs-lisp-mode) (eval-region (region-beginning) (region-end)))
	((eq major-mode 'python-mode) (python-shell-send-region (region-beginning) (region-end)))
	((eq major-mode 'clojure-mode) (cider-eval-region (region-beginning) (region-end)))
	((eq major-mode 'ess-mode) (ess-eval-region (region-beginning) (region-end)))
	(t (message "Not defined for this major mode"))))

(defun univ-eval-buffer ()
  "Evaluate the current buffer in the defined languages."
  (interactive)
  (cond ((eq major-mode 'lisp-interaction-mode) (eval-buffer nil))
	((eq major-mode 'emacs-lisp-mode) (eval-buffer nil))
	((eq major-mode 'python-mode) (python-shell-send-buffer))
	((eq major-mode 'clojure-mode) (cider-eval-buffer nil))
	((eq major-mode 'ess-mode) (ess-eval-buffer nil))
	(t (message "Not defined for this major mode"))))

(defun insert_then_R_operator ()
  "R - %>% operator or 'then' pipe operator."
  (interactive)
  (cond ((eq major-mode 'ess-mode) (progn
				     (evil-forward-word-end)
				     (evil-forward-char)
				     (insert " %>% ")))
	(t (message "Only valid in ESS mode"))))

(defun insert_then_R_operator_end_nl ()
  "R - %>% operator; place to line end and start new line."
  (interactive)
  (cond ((member major-mode '(ess-mode inferior-ess-mode))
	 (progn
	   (evil-end-of-line)
	   (evil-append 1)
	   (insert " %>% ")
	   (cond ((eq major-mode 'ess-mode) (lispy-newline-and-indent-plain)))))
	(t (message "Only valid in ESS mode"))))

(defun insert_then_R_operator_nl ()
  "R - %>% operator or 'then' pipe operator."
  (interactive)
  (just-one-space 1)
  (insert "%>%")
  (reindent-then-newline-and-indent))

(defun get_right_lambda (str)
  (progn
    (insert str)))

(defun insert_lambda_function ()
  "Insert lambda function definition into Python or R."
  (interactive)
  (evil-forward-word-end)
  (cond ((eq major-mode 'python-mode) (get_right_lambda "lambda "))
	((eq major-mode 'ess-mode) (get_right_lambda "function"))
	(t (message "Not defined for this major mode"))))

(defun wrap-selected-region (&optional beg end)
  "Evaluate the selected region in Python shell. BEG and END: selection start and end."
  (interactive)
  (let ((beg (cond (beg beg)
                   ((region-active-p)
                    (region-beginning))
                   (t (line-beginning-position))))
        (end (cond (end end)
                   ((region-active-p)
                    (copy-marker (region-end)))
                   (t (line-end-position)))))
    (save-excursion
      (set-window-point beg)
      (insert "JEP"))))

(provide 'utility_functions)
;;; utility_functions.el ends here
