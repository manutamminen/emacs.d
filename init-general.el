;; init-general.el --- Default Emacs configuration.
;;; Commentary:
;;
;; Custom Emacs keybindings
;;
;;; Code:

(use-package general
  :config
  (general-evil-setup 1)
  (general-define-key
   :states '(normal visual emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   
   "a" '(:ignore t :which-key "Applications")
   "ar" 'ranger
   "ac" 'calc
   "ad" 'dired
   "af" 'elfeed
   ;; "am" 'mu4e
   "ap" 'package-list-packages
   "as" 'eshell
   "au" 'suggest
   "ar" 'better-shell-remote-open
   "ae" 'easy-hugo

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

   "d" '(:ignore t :which-key "DNA utilities")
   "dc" 'complement-seq
   "de" 'insert-expansion
   "dr" 'reverse-seq
   "dx" 'reverse-complement-seq
   "dC" 'complement-seq-next-line
   "dR" 'reverse-seq-next-line
   "dX" 'reverse-complement-seq-next-line

   "e" '(:ignore t :which-key "Evaluation")
   "ee" 'univ-eval
   "el" 'univ-eval-line-last-sexp
   "eb" 'univ-eval-buffer
   "es" 'inspect-R-object-str
   "eh" 'inspect-R-object-head
   "ei" 'indent-region
   "e=" 'align-to-equals

   "f" '(:ignore t :which-key "File operations")
   "ff" 'counsel-find-file
   "fs" 'save-buffer
   "fr" 'counsel-recentf
   "f." 'find-function-at-point

   "j" '(:ignore t :which-key "Yasnippets etc")
   "jp" 'insert_then_R_operator
   "je" 'insert_then_R_operator_end_nl
   "jf" 'insert_lambda_function
   
   "m" '(:ignore t :which-key "Magit")
   "ml" 'magit-log-buffer-file
   "ms" 'magit-status
   "mp" 'magit-dispatch-popup

   ;; "n" '(:ignore t :which-key "Mail")
   ;; "nb" 'mu4e-headers-search-bookmark
   ;; "nc" 'mu4e-compose-new
   ;; "nn" 'mu4e~headers-jump-to-maildir
   ;; "ns" 'mu4e-headers-search

   "o" '(:ignore t :which-key "Org-mode tools")
   "oa" 'org-agenda
   "ob" 'org-iswitchb
   "oc" 'org-capture
   "od" 'org-deadline
   "of" 'org-refile
   "og" 'worf-goto
   "oh" 'hydra-clocking/body
   "os" 'org-schedule
   "om" 'org-columns
   "on" 'org-narrow-to-element
   "ol" 'org-store-link
   "oo" 'org-show-todo-tree
   "ot" 'org-todo
   "or" 'org-archive-subtree
   "ow" 'widen

   "p" '(:ignore t :which-key "Projectile tools")
   "pf" 'counsel-projectile-find-file
   "pp" 'counsel-projectile-switch-project
   "ps" 'projectile-ag

   "q" '(:ignore t :which-key "Quit!")
   "qq" 'evil-quit

   "s" '(:ignore t :which-key "Search tools")
   "sd" 'xref-find-definitions
   "ss" 'swiper
   "sa" 'swiper-all
   "sr" 'counsel-rg

   "t" '(:ignore t :which-key "Toggles")
   "tc" 'font-lock-mode
   "tf" 'toggle-frame-fullscreen
   "ti" 'org-indent-mode
   "tl" 'lispy-mode
   "to" 'evil-org-mode
   "tp" 'prettify-symbols-mode
   "tr" 'rainbow-delimiters-mode
   "tn" 'xah-toggle-read-novel-mode
   "tv" 'visual-line-mode
   "tw" 'toggle-truncate-lines
   
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

   "x" '(:ignore t :which-key "teXt tools")
   "xb" 'comment-box
   "xu" 'uncomment-region
   "xs" 'surround-with-code

   "z" '(:ignore t :which-key "Font scaling")
   "zz" 'hydra-font/body)

  (general-nmap
    :prefix "SPC"
    "TAB" 'mode-line-other-buffer))

(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-c o") 'ivy-occur)
(global-set-key (kbd "C-h k") 'helpful-key)
(global-set-key (kbd "C-h f") 'helpful-function)
(global-set-key (kbd "C-h v") 'helpful-variable)
(define-key evil-normal-state-map (kbd "ö") 'evil-end-of-line)
(define-key evil-normal-state-map (kbd ".") 'evil-avy-goto-line)
(define-key evil-normal-state-map (kbd ",") 'evil-avy-goto-char)
(define-key evil-insert-state-map (kbd "C-w") 'evil-window-map)
(define-key evil-insert-state-map (kbd "C-k") 'kill-line)
(define-key evil-insert-state-map (kbd "C") 'self-insert-command)
(define-key evil-insert-state-map (kbd "C-b") 'evil-scroll-page-up)
(define-key evil-insert-state-map (kbd "C-d") 'evil-scroll-down)
(evil-define-key '(insert normal) ess-r-mode-map (kbd "C-p") 'insert_then_R_operator_end_nl)
(evil-define-key '(insert normal) inferior-ess-r-mode-map (kbd "C-p") 'insert_then_R_operator_end_nl)
(evil-define-key '(insert normal) inferior-ess-r-mode-map (kbd "C-e") 'end-of-line)
(evil-define-key '(insert normal) inferior-ess-r-mode-map (kbd "C-<left>") 'left-word)
(evil-define-key '(insert normal) inferior-ess-r-mode-map (kbd "C-<right>") 'right-word)
(evil-define-key '(insert normal) inferior-ess-r-mode-map (kbd "<tab>") 'company-indent-or-complete-common)
(evil-define-key '(insert normal) inferior-ess-r-mode-map (kbd "_") 'ess-insert-assign)
(evil-define-key '(insert normal) ess-r-mode-map (kbd "C-f") 'insert_lambda_function)
(evil-define-key '(insert normal) ess-r-mode-map (kbd "C-e") 'end-of-line)
(evil-define-key '(insert normal) ess-r-mode-map (kbd "<tab>") 'company-indent-or-complete-common)
(evil-define-key '(insert normal) ess-r-mode-map (kbd "_") 'ess-insert-assign)
(evil-define-key '(insert normal) lisp-interaction-mode-map (kbd "C-c C-l") 'eval-last-sexp)
(evil-define-key '(insert normal) lisp-interaction-mode-map (kbd "C-c C-f") 'eval-defun)
(evil-define-key '(insert normal) emacs-lisp-mode-map (kbd "C-c C-l") 'eval-last-sexp)
(evil-define-key '(insert normal) emacs-lisp-mode-map (kbd "C-c C-f") 'eval-defun)
(evil-define-key '(insert normal) ess-help-mode-map (kbd "C-d") 'evil-scroll-down)
(evil-define-key '(insert normal) ess-help-mode-map (kbd "C-b") 'evil-scroll-up)
;; (evil-define-key '(insert normal) org-mode-map (kbd "C-e") 'end-of-line)
(evil-define-key '(insert normal) cider-stacktrace-mode-map (kbd "q") 'cider-popup-buffer-quit-function)
(evil-define-key '(insert normal) cider-repl-mode-map (kbd "<down>") 'cider-repl-next-input)
;; (evil-define-key '(insert normal) eshell-mode-map (kbd "C-v") 'evil-paste-after)
(evil-define-key '(insert normal) suggest-mode-map (kbd "C-c C-c") 'suggest-update)
(evil-define-key '(insert normal) emacs-lisp-mode-map (kbd "C-c C-c") 'eir-eval-in-ielm)
(evil-define-key '(insert normal) lisp-interaction-mode-map (kbd "C-c C-c") 'eir-eval-in-ielm)
(evil-define-key '(insert normal) Info-mode-map (kbd "C-c C-c") 'eir-eval-in-ielm)
(evil-define-key '(insert normal) eshell-mode-map (kbd "C-a") 'beginning-of-line)
(evil-define-key '(insert normal) eshell-mode-map (kbd "C-e") 'end-of-line)
(evil-define-key '(insert normal) shell-mode-map (kbd "C-a") 'beginning-of-line)
(evil-define-key '(insert normal) shell-mode-map (kbd "C-e") 'end-of-line)
(evil-define-key '(insert normal) markdown-mode-map (kbd "C-a") 'beginning-of-line)
(evil-define-key '(insert normal) markdown-mode-map (kbd "C-e") 'end-of-line)
(evil-define-key '(insert normal motion) elfeed-search-mode-map (kbd ".") 'evil-avy-goto-line)
(evil-define-key '(insert normal motion) dired-mode-map (kbd ".") 'evil-avy-goto-line)
(evil-define-key '(insert normal motion) python-mode-map (kbd "C-c C-c") 'eir-eval-in-python)
(evil-define-key '(insert normal motion) elpy-mode-map (kbd "C-c C-c") 'eir-eval-in-python)
(define-key python-mode-map (kbd "C-c C-c") 'eir-eval-in-python)
(define-key elpy-mode-map (kbd "C-c C-c") 'eir-eval-in-python)
;; (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point)
(define-key inferior-ess-mode-map (kbd "C-d") 'evil-scroll-down)
(define-key comint-mode-map (kbd "<up>") 'comint-previous-matching-input-from-input)
(define-key comint-mode-map (kbd "<down>") 'comint-next-matching-input-from-input)
;; (define-key org-mode-map (kbd "M-c") 'org-ctrl-c-ctrl-c)
;; (define-key org-mode-map (kbd "M-j") 'org-babel-next-src-block)
;; (define-key org-mode-map (kbd "M-k") 'org-babel-previous-src-block)
;; (define-key org-mode-map (kbd "M-l") 'org-edit-src-code)
(define-key org-src-mode-map (kbd "M-l") 'org-edit-src-exit)
(define-key emacs-lisp-mode-map (kbd "C-c m") 'macrostep-expand)
(define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)
(define-key global-map (kbd "C-c o") 'iedit-mode)
(define-key ivy-minibuffer-map (kbd "M-j") 'bjm/ivy-yank-whole-word)
(define-key lispy-mode-map (kbd ")")
  (lambda () (interactive)
    (progn
      (hydra-lispy-magic/body)
      (lispy-right-nostring 1))))
;; (define-key org-mode-map (kbd "<")
;;   (lambda () (interactive)
;;     (if (looking-back "^")
;; 	(hydra-org-template/body)
;;       (self-insert-command 1))))
;; (define-key org-mode-map ">"
;;   (lambda () (interactive)
;;     (if (looking-back "^")
;; 	(hydra-org-mol-template/body)
;;       (self-insert-command 1))))

(provide 'init-general)
;;; init-general.el ends here
