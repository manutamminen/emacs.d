;;; package --- Summary
;;; Commentary:
;;; Useful hydra shortcuts
;;; Code:
(defhydra hydra-org-mol-template (:color blue :hint nil)
  "
Lab protocols
"
  ("a" (y-expand "ampure_") "AMPure cleanup")
  ("p" (y-expand "pcr_") "PCR")
  ("c" (y-expand "cycle_") "PCR cycle")
  ("e" (y-expand "epic_") "epicPCR")
  ("d" (y-expand "mda_") "MDA")
  ("q" (y-expand "qpcr_") "qPCR")
  ("<" self-insert-command "ins")
  ("o" nil "quit"))


(defhydra hydra-org-template (:color blue :hint nil)
  "
Code blocks
"
  ("r" (y-expand "r_") "R block")
  ("h" (y-expand "shell") "shell script")
  ("p" (y-expand "python_script") "Python shell script")
  ("m" (y-expand "pyi") "IPython block with image")
  ("i" (y-expand "py") "IPython block")
  ("l" (y-expand "lsf") "LSF batch script")
  ("s" (y-expand "lsf_parallel") "LSF array script")
  ("<" self-insert-command "ins")
  ("o" nil "quit"))

(defhydra hydra-python-template (:color blue :hint nil)
  "
Python completions
"
  ("c" (y-expand "cls") "Class")
  ("f" (y-expand "f") "Function")
  ("d" (y-expand "d") "Documentation string")
  ("<" self-insert-command "ins")
  ("o" nil "quit"))

(defhydra hydra-windows ()
  ("h" windmove-left nil)
  ("l" windmove-right nil)
  ("k" windmove-up nil)
  ("j" windmove-down nil)
  ("H" buf-move-left nil)
  ("L" buf-move-right nil)
  ("K" buf-move-up nil)
  ("J" buf-move-down nil)
  ("<up>" enlarge-window nil)
  ("<down>" shrink-window nil)
  ("<right>" shrink-window-horizontally nil)
  ("<left>" enlarge-window-horizontally nil)
  ("p" previous-buffer "Switch to previous buffer")
  ("n" next-buffer "Switch to next buffer")
  ("m" delete-other-windows "Maximize current window")
  ("b" ivy-switch-buffer "Buffer list")
  ("c" delete-window "Delete current window")
  ("s" save-buffer "Save current buffer")
  ("i" (lambda () (interactive) (split-window-right) (windmove-right)) "Vertical split")
  ("-" (lambda () (interactive) (split-window-below) (windmove-down)) "Horizontal split")
  ("q" nil "cancel"))

(defhydra hydra-lispy-magic ()
  "
Modified lispy bindings
"
  ("b" univ-eval-buffer "Evaluate buffer")
  ("e" univ-eval "Evaluate function")
  ("p" univ-eval-line-last-sexp "Evaluate last sexp"))

(provide 'hydras)
;;; hydras.el ends here
