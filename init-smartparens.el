
(use-package smartparens
  :config
  (defun indent-between-pair (&rest _ignored)
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))

  (sp-local-pair 'prog-mode "{" nil :post-handlers '((indent-between-pair "RET")))
  (sp-local-pair 'prog-mode "[" nil :post-handlers '((indent-between-pair "RET")))
  (sp-local-pair 'prog-mode "(" nil :post-handlers '((indent-between-pair "RET"))))

(use-package evil-smartparens
  :hook
  ((prog-mode . evil-smartparens-mode)
   (org-mode . evil-smartparens-mode))
  :config
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  ;; (smartparens-strict-mode t)
  )

;; (require 'smartparens)
;; (require 'evil-smartparens)
;; (require 'evil-smartparens-keybindings)

;; (require 'smartparens-config)

;; (smartparens-global-mode t)
;; (evil-smartparens-keybindings-mode t)
;; (show-smartparens-global-mode t)
;; (smartparens-strict-mode t)

;; (add-hook 'prog-mode-hook #'evil-smartparens-mode)
;; (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
;; (add-hook 'org-mode-hook #'evil-smartparens-mode)
;; (add-hook 'org-mode-hook 'turn-on-smartparens-strict-mode)

;; (add-hook
;;  'smartparens-enabled-hook #'evil-smartparens-mode)

;; (setq sp-show-pair-from-inside t)

;; (use-package smartparens
;;   :bind (:map lisp-interaction-mode-map
;;          ("C-M-a" . sp-beginning-of-sexp)
;;          ("C-M-e" . sp-end-of-sexp)

;;          ("C-<down>" . sp-down-sexp)
;;          ("C-<up>"   . sp-up-sexp)
;;          ("M-<down>" . sp-backward-down-sexp)
;;          ("M-<up>"   . sp-backward-up-sexp)

;;          ("C-M-f" . sp-forward-sexp)
;;          ("C-M-b" . sp-backward-sexp)

;;          ("C-M-n" . sp-next-sexp)
;;          ("C-M-p" . sp-previous-sexp)

;;          ("C-S-f" . sp-forward-symbol)
;;          ("C-S-b" . sp-backward-symbol)

;;          ("M-<right>" . sp-forward-slurp-sexp)
;;          ("C-M-<right>" . sp-forward-barf-sexp)
;;          ("M-<left>"  . sp-backward-slurp-sexp)
;;          ("C-M-<left>"  . sp-backward-barf-sexp)

;;          ("C-M-t" . sp-transpose-sexp)
;;          ("C-M-k" . sp-kill-sexp)
;;          ("C-k"   . sp-kill-hybrid-sexp)
;;          ("M-k"   . sp-backward-kill-sexp)
;;          ("C-M-w" . sp-copy-sexp)
;;          ("C-M-d" . delete-sexp)

;;          ("M-<backspace>" . backward-kill-word)
;;          ("C-<backspace>" . sp-backward-kill-word)
;;          ([remap sp-backward-kill-word] . backward-kill-word)

;;          ("M-[" . sp-backward-unwrap-sexp)
;;          ("M-]" . sp-unwrap-sexp)

;;          ("C-x C-t" . sp-transpose-hybrid-sexp)

;;          ("C-c ("  . wrap-with-parens)
;;          ("C-c ["  . wrap-with-brackets)
;;          ("C-c {"  . wrap-with-braces)
;;          ("C-c '"  . wrap-with-single-quotes)
;;          ("C-c \"" . wrap-with-double-quotes)
;;          ("C-c _"  . wrap-with-underscores)
;;          ("C-c `"  . wrap-with-back-quotes)


;;          :map emacs-lisp-mode-map
;;          ("C-M-a" . sp-beginning-of-sexp)
;;          ("C-M-e" . sp-end-of-sexp)

;;          ("C-<down>" . sp-down-sexp)
;;          ("C-<up>"   . sp-up-sexp)
;;          ("M-<down>" . sp-backward-down-sexp)
;;          ("M-<up>"   . sp-backward-up-sexp)

;;          ("C-M-f" . sp-forward-sexp)
;;          ("C-M-b" . sp-backward-sexp)

;;          ("C-M-n" . sp-next-sexp)
;;          ("C-M-p" . sp-previous-sexp)

;;          ("C-S-f" . sp-forward-symbol)
;;          ("C-S-b" . sp-backward-symbol)

;;          ("M-<right>" . sp-forward-slurp-sexp)
;;          ("C-M-<right>" . sp-forward-barf-sexp)
;;          ("M-<left>"  . sp-backward-slurp-sexp)
;;          ("C-M-<left>"  . sp-backward-barf-sexp)

;;          ("C-M-t" . sp-transpose-sexp)
;;          ("C-M-k" . sp-kill-sexp)
;;          ("C-k"   . sp-kill-hybrid-sexp)
;;          ("M-k"   . sp-backward-kill-sexp)
;;          ("C-M-w" . sp-copy-sexp)
;;          ("C-M-d" . delete-sexp)

;;          ("M-<backspace>" . backward-kill-word)
;;          ("C-<backspace>" . sp-backward-kill-word)
;;          ([remap sp-backward-kill-word] . backward-kill-word)

;;          ("M-[" . sp-backward-unwrap-sexp)
;;          ("M-]" . sp-unwrap-sexp)

;;          ("C-x C-t" . sp-transpose-hybrid-sexp)

;;          ("C-c ("  . wrap-with-parens)
;;          ("C-c ["  . wrap-with-brackets)
;;          ("C-c {"  . wrap-with-braces)
;;          ("C-c '"  . wrap-with-single-quotes)
;;          ("C-c \"" . wrap-with-double-quotes)
;;          ("C-c _"  . wrap-with-underscores)
;;          ("C-c `"  . wrap-with-back-quotes)

;;          :map elpy-mode-map
;;          ("M-<right>" . sp-forward-slurp-sexp)
;;          ("C-M-<right>" . sp-forward-barf-sexp)
;;          ("M-<left>"  . sp-backward-slurp-sexp)
;;          ("C-M-<left>"  . sp-backward-barf-sexp)

;;          :map lispy-mode-map
;;          ("M-<right>" . sp-forward-slurp-sexp)
;;          ("C-M-<right>" . sp-forward-barf-sexp)
;;          ("M-<left>"  . sp-backward-slurp-sexp)
;;          ("C-M-<left>"  . sp-backward-barf-sexp)))


(provide 'init-smartparens)
;;; init-smartparens.el ends here
