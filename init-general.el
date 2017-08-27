
(use-package general
  :ensure t
  :config
  (general-define-key
   :states '(normal visual emacs)
   :prefix "SPC"
   
   "a" '(:ignore t :which-key "Applications")
   "ar" 'ranger
   "ac" 'calc
   "ad" 'dired
   "as" 'eshell
   "au" 'suggest
   "ar" 'better-shell-remote-open

   "b" '(:ignore t :which-key "Buffer tools")
   "bd" 'kill-this-buffer
   "bb" 'ivy-switch-buffer
   "bm" 'bm-toggle
   "bn" 'bm-next
   "bp" 'bm-previous
   "bc" 'bm-remove-all-current-buffer

   "c" '(:ignore t :which-key "Flycheck")
   "cb" 'flycheck-buffer
   "cl" 'flycheck-list-errors

   "d" '(:ignore t :which-key "Dumb-jump")
   "dd" 'dumb-hydra/body
   "dj" 'dumb-jump-go
   "do" 'dumb-jump-go-other-window

   "e" '(:ignore t :which-key "Evaluation")
   "ee" 'univ-eval
   "el" 'univ-eval-line-last-sexp
   "eb" 'univ-eval-buffer
   "es" 'asb-ess-R-object-popup-str
   "eh" 'asb-ess-R-object-popup-head
   "ei" 'asb-ess-R-object-popup-interactive

   "f" '(:ignore t :which-key "File operations")
   "ff" 'counsel-find-file
   "fs" 'save-buffer
   "fr" 'counsel-recentf

   "j" '(:ignore t :which-key "Yasnippets etc")
   "jp" 'insert_then_R_operator
   "je" 'insert_then_R_operator_end_nl
   "jf" 'insert_lambda_function
   
   "m" '(:ignore t :which-key "Magit")
   "ms" 'magit-status
   "mp" 'magit-dispatch-popup

   "o" '(:ignore t :which-key "Org-mode tools")
   "ob" 'org-iswitchb
   "oc" 'org-capture
   "oa" 'org-agenda
   "og" 'worf-goto
   "os" 'org-schedule
   "om" 'org-columns
   "ol" 'org-store-link
   "oo" 'org-show-todo-tree
   "ot" 'org-todo
   "or" 'org-archive-subtree

   "p" '(:ignore t :which-key "Projectile tools")
   "pp" 'counsel-projectile-switch-project
   "pf" 'counsel-projectile-find-file

   "q" '(:ignore t :which-key "Quit!")
   "qq" 'evil-quit

   "s" '(:ignore t :which-key "Search tools")
   "sd" 'xref-find-definitions
   "ss" 'swiper
   "sa" 'swiper-all

   "t" '(:ignore t :which-key "Toggles")
   "tc" 'font-lock-mode
   "tf" 'toggle-frame-fullscreen
   "ti" 'rainbow-identifiers-mode
   "tl" 'lispy-mode
   "to" 'evil-org-mode
   "tp" 'prettify-symbols-mode
   "tr" 'rainbow-delimiters-mode
   "tw" 'toggle-truncate-lines
   "tn" 'xah-toggle-read-novel-mode
   
   "w" '(:ignore t :which-key "Window tools")
   "ww" 'hydra-windows/body
   "we" 'hydra-eyebrowse/body
   "wl" 'windmove-right
   "wh" 'windmove-left
   "wk" 'windmove-up
   "wj" 'windmove-down
   "wH" 'buf-move-left
   "wL" 'buf-move-right
   "wK" 'buf-move-up
   "wJ" 'buf-move-down
   "wi" (lambda () (interactive) (split-window-right) (windmove-right))
   "w-" (lambda () (interactive) (split-window-below) (windmove-down))
   "wm" 'delete-other-windows
   "wo" 'delete-other-windows
   "wm" 'delete-other-windows
   "wc" 'evil-window-delete
   "wv" 'evil-window-vsplit
   "ws" 'evil-window-split

   "z" '(:ignore t :which-key "Font scaling")
   "zz" 'hydra-font/body)

  (general-nmap
   :prefix "SPC"
   "TAB" 'mode-line-other-buffer))


(provide 'init-general)
;;; init-general.el ends here
