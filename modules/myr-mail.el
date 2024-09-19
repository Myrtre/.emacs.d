;;; myr-mail.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'myr-straight)

;; -- Mailing -----
(use-package mu4e
  :straight nil ;; Installed via distro package
  :defer t
  :bind (("C-c m m" . mu4e)
         ("C-c m c" . 'mu4e-compose-new)
         ("C-c m i" . 'myr/go-to-inbox)
         ("C-c m s" . 'mu4e-update-mail-and-index))
  :hook ((mu4e-compose-mode . turn-off-auto-fill)
         (mu4e-compose-mode . turn-off-auto-fill))
  :config
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-maildir "~/mail")
  ;; Use Ivy for mu4e completions maildir folders, etc)
  (setq mu4e-completing-read-function #'completing-read)
  (setq mu4e-change-filenames-when-moving t)


  ;; Set up contexts for email accounts
  (setq mu4e-contexts
        `(,(make-mu4e-context
            :name "proton"
            :vars '((user-full-name . "Davy Gút")
                    (user-mail-address . "myrtre@proton.me")
                    (mu4e-sent-folder . "/proton/sent")
                    (mu4e-trash-folder . "/proton/trash")
                    (mu4e-drafts-folder . "/proton/drafts")
                    (mu4e-refile-folder . "/proton/archive")
                    (mu4e-sent-messages-behavior . sent)))))
  (setq mu4e-context-policy 'pick-first)

  (setf (alist-get 'trash mu4e-marks)
        '(:char ("d" . "▼")
                :prompt "dtrash"
                :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
                :action (lambda (docid msg target)
                          (mu4e--server-move docid
                                             (mu4e--mark-check-target target) "-N"))))

  ;; Display options
  (setq mu4e-view-show-images t)
  (setq mu4e-view-show-addresses 't)

  ;; Composing mail
  (setq mu4e-compose-dont-reply-to-self t)
  
  (setq mu4e-maildir-shortcuts
        '(("/proton/inbox" . ?i)
          ("/proton/archive" . ?a)
          ("/proton/lists/*" . ?l)
          ("/proton/sent" . ?s)
          ("/proton/trash" . ?t)))

  (setq message-send-mail-function 'smtmail-send-it
        auth-sources '("~/.authinfo")
        smtpmail-smtp-server "127.0.0.1"
        smtpmail-smtp-service 1025
        smtpmail-stream-type 'ssl)
  
  (add-to-list 'mu4e-bookmarks
               '(:name "All Inboxes"
                       :query "maildir:/Proton/INBOX"
                       :key ?i))

  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t)

  (setq myr/mu4e-inbox-query
        "(maildir:/Proton/INBOX) AND flag:unread")
  
  (defun rune/go-to-inbox ()
    (interactive)
    (mu4e-headers-search myr/mu4e-inbox-query))
  
  ;; Start mu4e in the background so that it syncs mail periodically
  (mu4e t))

(use-package mu4e-alert
  :after mu4e
  :config
  ;; Show unread emails from all inboxes
  (setq mu4e-alert-interesting-mail-query
        (concat myr/mu4e-inbox-query
                " date:30M..now"))

  ;; Show notifications for mails already notified
  (setq mu4e-alert-notify-repeated-mails nil)

  (mu4e-alert-enable-notifications))


;; -- HTML Emails -----

(use-package org-msg
  :straight nil ;; Installed already with guix
  :after mu4e
  :config
  (setq mail-user-agen 'mu4e-user-agent)
  (require 'org-msg)
  (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
        org-msg-startup "hidestarts indent inlineimages"
        org-msg-default-alternatives '((new           . (text html))
                                       (reply-to-html . (text html))
                                       (reply-to-text . (text)))
        org-msg-convert-citation t)
  (org-msg-mode))


(provide 'myr-mail)
;;; myr-mail.el ends here
