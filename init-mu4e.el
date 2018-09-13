
;; Big thanks to this thread
;; http://irreal.org/blog/?p=6122

(require 'smtpmail)

(use-package mu4e

  :load-path "/usr/local/share/emacs/site-lisp/mu4e"

  :init 
  (progn

    ;; use mu4e for e-mail in emacs
    (setq mail-user-agent 'mu4e-user-agent)

    ;; Set the main directories
    (setq mu4e-maildir       "~/Mail"
          mu4e-sent-folder   "/Sent"
          mu4e-drafts-folder "/Drafts"
          mu4e-trash-folder  "/Trash"
          mu4e-refile-folder "/Archive")

    ;; setup some handy shortcuts
    ;; you can quickly switch to your Gmail Inbox -- press ``jg''
    ;; then, when you want archive some messages, move them to
    ;; the 'All Mail' folder by pressing ``ma''.
    (setq mu4e-maildir-shortcuts
          '( ("/Gmail/INBOX" . ?g)
             ("/Utu/INBOX" . ?u)
             ("/Sent"  . ?s)
             ("/Trash" . ?t)))

    ;; Set the mail update functionality
    (setq mu4e-get-mail-command "offlineimap")

    ;; Other settings
    (setq mu4e-update-interval 300
          mu4e-compose-format-flowed t
          mu4e-view-show-addresses t)

    ;; Default folder for saving attachments
    (setq mu4e-attachment-dir  "~/Downloads")

    ;; Disable the quit confirmation prompt
    (setq mu4e-confirm-quit nil)

    ;; Try to show images
    (setq mu4e-view-show-images t
          mu4e-show-images t
          mu4e-view-image-max-width 800)

    ;; use org structures and tables in message mode
    (add-hook 'message-mode-hook 'turn-on-orgtbl)
    (add-hook 'message-mode-hook 'turn-on-orgstruct++))

  :config
  (progn

    ;; not using smtp-async yet
    ;; some of these variables will get overridden by the contexts
    (setq send-mail-function 'smtpmail-send-it
          message-send-mail-function 'smtpmail-send-it)
    
    ;; the list of all of my e-mail addresses
    (setq mu4e-user-mail-address-list '("mavata@gmail.com"
                                        "mavatam@utu.fi"))

    ;; Contexts: One for each mail personality.
    (setq mu4e-contexts
          `( ,(make-mu4e-context
               :name "a mavata@gmail.com"
               :enter-func (lambda () (mu4e-message "Enter mavata@gmail.com context"))
               :leave-func (lambda () (mu4e-message "Leave mavata@gmail.com context"))
               :match-func (lambda (msg)
                             (when msg 
                               (mu4e-message-contact-field-matches msg :to "mavata@gmail.com")))
               :vars '((user-mail-address      . "mavata@gmail.com")
                       (user-full-name         . "Manu Tamminen")
                       (smtpmail-smtp-server   . "smtp.gmail.com")
                       (smtpmail-smtp-service  . 587)
                       (smtpmail-stream-type   . starttls)))

             ,(make-mu4e-context
               :name "i mavatam@utu.fi"
               :enter-func (lambda () (mu4e-message "Enter mavatam@utu.fi context"))
               :leave-func (lambda () (mu4e-message "Leave mavatam@utu.fi context"))
               :match-func (lambda (msg)
                             (when msg 
                               (mu4e-message-contact-field-matches msg :to "mavatam@utu.fi")))
               :vars '((user-mail-address       . "mavatam@utu.fi")
                       (user-full-name          . "Manu Tamminen")
                       (smtpmail-smtp-server    . "mail.utu.fi")
                       (smtpmail-smtp-service   . 587)
                       (smtpmail-stream-type    . starttls)))))

    ;; something about ourselves
    (setq mu4e-compose-signature
          (concat
           "Manu Tamminen\n"
           "http://manutamminen.info\n"))))

(use-package evil-mu4e)

(require 'org-mu4e)

(setq org-mu4e-link-query-in-headers-mode nil)
