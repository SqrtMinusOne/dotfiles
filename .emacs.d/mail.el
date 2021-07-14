(let ((default-directory  "/home/pavel/.guix-extra-profiles/mail/mail/share/emacs/site-lisp"))
  (normal-top-level-add-subdirs-to-load-path))

(my-leader-def "am" 'notmuch)

(use-package notmuch
  ;; :ensure nil
  :commands (notmuch)
  :config
  (setq mail-specify-envelope-from t)
  (setq message-sendmail-envelope-from 'header)
  (setq mail-envelope-from 'header)
  (setq notmuch-always-prompt-for-sender t)
  (setq sendmail-program (executable-find "msmtp"))
  (setq send-mail-function #'sendmail-send-it)
  (setq mml-secure-openpgp-sign-with-sender t)
  (add-hook 'notmuch-hello-mode-hook
            (lambda () (display-line-numbers-mode 0)))
  (setq notmuch-saved-searches
        '((:name "inbox (main)" :query "tag:inbox AND tag:main")
          (:name "unread (main)" :query "tag:unread AND tag:main")
          (:name "sent (main)" :query "tag:sent AND tag:main")
          (:name "all mail (main)" :query "tag:main")
          (:name "inbox (progin)" :query "tag:inbox AND tag:progin")
          (:name "unread (progin)" :query "tag:unread AND tag:progin")
          (:name "sent (progin)" :query "tag:sent AND tag:progin")
          (:name "all main (progin)" :query "tag:progin")
          (:name "drafts" :query "tag:draft")))
  (custom-set-faces
   `(notmuch-wash-cited-text ((t (:foreground ,(doom-color 'yellow)))))))

(with-eval-after-load 'notmuch
  (add-hook 'message-setup-hook 'mml-secure-sign-pgpmime))

(setq mml-secure-key-preferences
      '((OpenPGP
         (sign
          ("thexcloud@gmail.com" "914472A1FD6775C166F96EBEED739ADF81C78160"))
         (encrypt))
        (CMS
         (sign)
         (encrypt))))
