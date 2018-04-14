;;; package --- Summary
;;; Commentary:
;;; Useful hydra shortcuts
;;; Code:
(defhydra hydra-org-mol-template (:color blue :hint nil)
  "Lab protocols"
  ("a" (y-expand "ampure_") "AMPure cleanup")
  ("p" (y-expand "pcr_") "PCR")
  ("c" (y-expand "cycle_") "PCR cycle")
  ("e" (y-expand "epic_") "epicPCR")
  ("d" (y-expand "mda_") "MDA")
  ("q" (y-expand "qpcr_") "qPCR")
  ("<" self-insert-command "ins")
  ("o" nil "quit"))


(defhydra hydra-org-template (:color blue :hint nil)
  "Code blocks"
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
  "Python completions"
  ("c" (y-expand "cls") "Class")
  ("f" (y-expand "f") "Function")
  ("d" (y-expand "d") "Documentation string")
  ("<" self-insert-command "ins")
  ("o" nil "quit"))

(defhydra hydra-windows ()
  "Window operations"
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
  ("q" quit-window "Close current window"))

(defhydra hydra-font ()
  "Font scaling"
  ("+" text-scale-increase)
  ("-" text-scale-decrease))

(defhydra hydra-lispy-magic ()
  "Modified lispy bindings"
  ("b" univ-eval-buffer "Evaluate buffer")
  ("e" univ-eval "Evaluate function")
  ("p" univ-eval-line-last-sexp "Evaluate last sexp"))

(defhydra dumb-hydra ()
  "Jump to definitions etc"
  ("n" dumb-jump-go "Next occurrence"))

(defhydra hydra-eyebrowse ()
  "Window config management"
  ("s" eyebrowse-create-window-config "Save window config")
  ("n" eyebrowse-next-window-config "Next window config")
  ("p" eyebrowse-previous-window-config "Previous window config")
  ("c" eyebrowse-close-window-config "Close window config")
  ("0" eyebrowse-switch-to-window-config-0 "Switch to config 0")
  ("1" eyebrowse-switch-to-window-config-1 "Switch to config 1")
  ("2" eyebrowse-switch-to-window-config-2 "Switch to config 2")
  ("3" eyebrowse-switch-to-window-config-3 "Switch to config 3")
  ("4" eyebrowse-switch-to-window-config-4 "Switch to config 4")
  ("5" eyebrowse-switch-to-window-config-5 "Switch to config 5")
  ("6" eyebrowse-switch-to-window-config-6 "Switch to config 6")
  ("7" eyebrowse-switch-to-window-config-7 "Switch to config 7")
  ("8" eyebrowse-switch-to-window-config-8 "Switch to config 8")
  ("9" eyebrowse-switch-to-window-config-9 "Switch to config 9"))

(defhydra hydra-org (:color blue :timeout 12 :columns 4)
  "Org commands"
  ("i" (lambda () (interactive) (org-clock-in '(4))) "Clock in")
  ("o" org-clock-out "Clock out")
  ("q" org-clock-cancel "Cancel a clock"))

(provide 'hydras)
;;; hydras.el ends here
