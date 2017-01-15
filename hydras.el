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
  ("p" previous-buffer "prev-buf")
  ("n" next-buffer "next-buf")
  ("1" delete-other-windows "1")
  ("d" delete-window "del")
  ("s" save-buffer "save")
  ("u" (progn (winner-undo) (setq this-command 'winner-undo)) "undo")
  ("r" winner-redo "redo")
  ("i" (lambda () (interactive) (split-window-right) (windmove-right)))
  ("-" (lambda () (interactive) (split-window-below) (windmove-down)))
  ("q" nil "cancel"))
