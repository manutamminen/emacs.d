;;; utility-functions.el --- Default Emacs configuration.
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

(defun insert_right_lambda (str)
  (progn
    (insert str)))

(defun insert_lambda_function (start end)
  "Insert lambda function definition into Python or R."
  (interactive "r")
  (evil-forward-word-end)
  (cond ((eq major-mode 'python-mode) (insert_right_lambda "lambda "))
	((eq major-mode 'ess-mode) (insert_right_lambda "function"))
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

(defun xah-toggle-read-novel-mode ()
  "Setup current buffer to be suitable for reading long novel/article text.

• Line wrap at word boundaries.
• Set a right margin.
• line spacing is increased.
• variable width font is used.

Call again to toggle back.
URL `http://ergoemacs.org/emacs/emacs_novel_reading_mode.html'
Version 2017-02-27"
  (interactive)
  (if (null (get this-command 'state-on-p))
      (progn
        (set-window-margins nil 0 9)
        (variable-pitch-mode 1)
        (setq line-spacing 0.4)
        (setq word-wrap t)
        (put this-command 'state-on-p t)
	(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
	(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
	(setq-default evil-cross-lines t))
    (progn
      (set-window-margins nil 0 0)
      (variable-pitch-mode 0)
      (setq line-spacing nil)
      (setq word-wrap nil)
      (put this-command 'state-on-p nil)
      (define-key evil-normal-state-map (kbd "j") 'evil-next-line)
      (define-key evil-normal-state-map (kbd "k") 'evil-previous-line)
      (setq-default evil-cross-lines nil)))
  (redraw-frame (selected-frame)))

;; Auto-rename new eww buffers
(defun xah-rename-eww-hook ()
  "Rename eww browser's buffer so sites open in new page."
  (rename-buffer "eww" t))
(add-hook 'eww-mode-hook #'xah-rename-eww-hook)

;; From here: http://akhilsbehl.github.io/blog/2016/05/30/inspecting-objects-at-point-with-ess/
;;; Show a popup by executing arbitrary commands on object at point.
;;; Inspiration:
;;; blogisticreflections.wordpress.com/2009/10/01/r-object-tooltips-in-ess/

;; emacs.stackexchange.com/questions/696/get-content-of-a-buffer
(defun asb-read-into-string (buffer)
  (with-current-buffer buffer
    (buffer-string)))

(defun asb-ess-R-object-popup (r-func)
  "R-FUNC: The R function to use on the object.
Run R-FUN for object at point, and display results in a popup."
  (let ((objname (current-word))
        (tmpbuf (get-buffer-create "**ess-R-object-popup**")))
    (if objname
        (progn
          (ess-command (concat "class(" objname ")\n") tmpbuf)
          (let ((bs (asb-read-into-string tmpbuf)))
            (if (not(string-match "\(object .* not found\)\|unexpected" bs))
                (progn
                  (ess-command (concat r-func "(" objname ")\n") tmpbuf)
                  (let ((bs (asb-read-into-string tmpbuf)))
                    (popup-tip bs)))))))
    (kill-buffer tmpbuf)))


(defun ess-R-object-inspect (r-func)
  "R-FUNC: The R function to use on the object.
Run the R-FUN for the object at point and display the results in the R buffer"
  (let* ((objname (current-word))
         (r-process (get-process "R"))
         (r-command (concat r-func "(" objname ")\n")))
    (progn (ess-send-string r-process r-command)
           (set-window-point
            (get-buffer-window "*R*")
            (+ 1 (buffer-size (get-buffer "*R*")))))))

(defun inspect-R-object-head ()
  (interactive)
  (ess-R-object-inspect "head"))

(defun inspect-R-object-str ()
  (interactive)
  (ess-R-object-inspect "str"))

(defun asb-ess-R-object-popup-str ()
  (interactive)
  (asb-ess-R-object-popup "str"))

(defun asb-ess-R-object-popup-head ()
  (interactive)
  (asb-ess-R-object-popup "head"))

(defun asb-ess-R-object-popup-interactive (r-func)
  (interactive "sR function to execute: ")
  (asb-ess-R-object-popup r-func))

(provide 'utility-functions)
;;; utility-functions.el ends here
