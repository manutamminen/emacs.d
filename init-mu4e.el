
;; Big thanks to this thread
;; http://irreal.org/blog/?p=6122

(require 'smtpmail)

(use-package mu4e

  :load-path "/usr/local/Cellar/mu/HEAD-35951da/share/emacs/site-lisp/mu/mu4e"

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

    ;; (setq mu4e-refile-folder
    ;;       (lambda (msg)
    ;;         (cond
    ;;          ;; messages to the mu mailing list go to the /mu folder
    ;;          ((mu4e-message-contact-field-matches msg :to
    ;;                                               "mu-discuss@googlegroups.com")
    ;;           "/mu")
    ;;          ;; messages sent directly to me go to /archive
    ;;          ;; also `mu4e-user-mail-address-p' can be used
    ;;          ((mu4e-message-contact-field-matches msg :to "me@example.com")
    ;;           "/private")
    ;;          ;; messages with football or soccer in the subject go to /football
    ;;          ((string-match "football\\|soccer"
    ;;                         (mu4e-message-field msg :subject))
    ;;           "/football")
    ;;          ;; messages sent by me go to the sent folder
    ;;          ((find-if
    ;;            (lambda (addr)
    ;;              (mu4e-message-contact-field-matches msg :from addr))
    ;;            mu4e-user-mail-address-list)
    ;;           mu4e-sent-folder)
    ;;          ;; everything else goes to /archive
    ;;          ;; important to have a catch-all at the end!
    ;;          (t  "/archive"))))

    ;; allow for updating mail using 'U' in the main view:
    (setq mu4e-get-mail-command "offlineimap")

    ;; Other settings
    (setq mu4e-update-interval 300
          mu4e-compose-format-flowed t
          mu4e-view-show-addresses t)

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
               ;; Match based on the contact-fields of the message (that we are replying to)
               :match-func (lambda (msg)
                             (when msg 
                               (mu4e-message-contact-field-matches msg 
                                                                   :to "mavata@gmail.com")))
               :vars '( ( user-mail-address      . "mavata@gmail.com"  )
                        ( user-full-name         . "Manu Tamminen" )
                        ( smtpmail-smtp-server   . "smtp.gmail.com" )
                        ( smtpmail-smtp-service  . 587)
                        ( smtpmail-stream-type   . starttls)))

             ,(make-mu4e-context
               :name "i mavatam@utu.fi"
               :enter-func (lambda () (mu4e-message "Enter mavatam@utu.fi context"))
               :leave-func (lambda () (mu4e-message "Leave mavatam@utu.fi context"))
               ;; we match based on the contact-fields of the message
               :match-func (lambda (msg)
                             (when msg 
                               (mu4e-message-contact-field-matches msg 
                                                                   :to "mavatam@utu.fi")))
               :vars '( ( user-mail-address       . "mavatam@utu.fi" )
                        ( user-full-name          . "Manu Tamminen" )
                        ( smtpmail-smtp-server    . "mail.utu.fi" )
                        ( smtpmail-stream-type    . starttls)
                        ( smtpmail-smtp-service   . 587)))))

    ;; something about ourselves
    (setq mu4e-compose-signature
          (concat
           "Manu Tamminen\n"
           "http://manutamminen.info\n"))))

(use-package evil-mu4e :ensure t)

(require 'org-mu4e)

(setq org-mu4e-link-query-in-headers-mode nil)
