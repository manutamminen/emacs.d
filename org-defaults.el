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
 '((R . t)
   (ipython . t)))
