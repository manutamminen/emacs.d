;; (defhydra hydra-org-template (:color blue :hint nil)
;;   "
;; _c_enter  _q_uote    _L_aTeX:
;; _l_atex   _e_xample  _i_ndex:
;; _a_scii   _v_erse    _I_NCLUDE:
;; _s_rc     ^ ^        _H_TML:
;; _h_tml    ^ ^        _A_SCII:
;; "
;;   ("s" (hot-expand "<s"))
;;   ("e" (hot-expand "<e"))
;;   ("q" (hot-expand "<q"))
;;   ("v" (hot-expand "<v"))
;;   ("c" (hot-expand "<c"))
;;   ("l" (hot-expand "<l"))
;;   ("h" (hot-expand "<h"))
;;   ("a" (hot-expand "<a"))
;;   ("L" (hot-expand "<L"))
;;   ("i" (hot-expand "<i"))
;;   ("I" (hot-expand "<I"))
;;   ("H" (hot-expand "<H"))
;;   ("A" (hot-expand "<A"))
;;   ("P" (y-expand "pcr_"))
;;   ("<" self-insert-command "ins")
;;   ("o" nil "quit"))


(defhydra hydra-org-template (:color blue :hint nil)
  "
Molecular biology
-----------------
_a_mpure  _e_pic  m_d_a
_p_cr   _c_ycle   _q_pcr

Computation
-----------
_i_python    i_m_age ipython
_l_sf block  l_s_f parallel block
_r_ block 

"
  ("a" (y-expand "ampure_"))
  ("r" (y-expand "r_"))
  ("p" (y-expand "pcr_"))
  ("c" (y-expand "cycle_"))
  ("e" (y-expand "epic_"))
  ("m" (y-expand "pyi"))
  ("i" (y-expand "py"))
  ("d" (y-expand "mda_"))
  ("q" (y-expand "qpcr_"))
  ("l" (y-expand "lsf"))
  ("s" (y-expand "lsf_parallel"))
  ("<" self-insert-command "ins")
  ("o" nil "quit"))

(defhydra hydra-python-template (:color blue :hint nil)
  "
_c_lass 
_f_unction
_d_oc
"
  ("c" (y-expand "cls"))
  ("f" (y-expand "f"))
  ("d" (y-expand "d"))
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
