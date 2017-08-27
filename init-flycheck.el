(use-package flycheck
  :ensure t
  :config
  (progn
    (setq flycheck-display-errors-function nil)
    (add-hook 'after-init-hook #'global-flycheck-mode)
    (add-hook 'python-mode-hook (lambda ()
				  (progn
				    (flycheck-mode 1)
				    #'flycheck-python-setup
				    (setq flycheck-checker 'python-pylint
					  flycheck-checker-error-threshold 900
					  flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list
					  flycheck-pylintrc "~/.pylintrc"))))))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
