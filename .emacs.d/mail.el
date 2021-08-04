(setq user-mail-address "thexcloud@gmail.com")
(setq user-full-name "Pavel Korytov")

(let ((default-directory  "/home/pavel/.guix-extra-profiles/mail/mail/share/emacs/site-lisp"))
  (normal-top-level-add-subdirs-to-load-path))

(use-package notmuch
  ;; :ensure nil
  :commands (notmuch notmuch-search)
  :config
  (setq mail-specify-envelope-from t)
  (setq message-sendmail-envelope-from 'header)
  (setq mail-envelope-from 'header)
  (setq notmuch-always-prompt-for-sender t)
  (setq sendmail-program (executable-find "msmtp"))
  (setq send-mail-function #'sendmail-send-it)
  (setq mml-secure-openpgp-sign-with-sender t)
  (setq notmuch-mua-user-agent-function 'notmuch-mua-user-agent-full)
  (add-hook 'notmuch-hello-mode-hook
            (lambda () (display-line-numbers-mode 0))))

(my-leader-def
  :infix "am"
  "" '(:which-key "notmuch")
  "m" 'notmuch)

(setq notmuch-saved-searches
      '((:name "drafts" :query "tag:draft")
        (:name "main (inbox)" :query "tag:main AND tag:inbox")
        (:name "main (unread)" :query "tag:main AND tag:unread")
        (:name "main (sent)" :query "tag:main AND tag:sent")
        (:name "main (all mail)" :query "tag:main")
        (:name "progin (inbox)" :query "tag:progin AND tag:inbox")
        (:name "progin (unread)" :query "tag:progin AND tag:unread")
        (:name "progin (sent)" :query "tag:progin AND tag:sent")
        (:name "progin (all mail)" :query "tag:progin")
        (:name "pvkorytov (inbox)" :query "tag:pvkorytov AND tag:inbox")
        (:name "pvkorytov (unread)" :query "tag:pvkorytov AND tag:unread")
        (:name "pvkorytov (sent)" :query "tag:pvkorytov AND tag:sent")
        (:name "pvkorytov (all mail)" :query "tag:pvkorytov")))

(my-leader-def
  :infix "am"
  "t" '(:which-key "thexcloud@gmail.com")
  "ti" '((lambda () (interactive) (notmuch-search "tag:main AND tag:inbox")) :which-key "inbox")
  "tu" '((lambda () (interactive) (notmuch-search "tag:main AND tag:unread")) :which-key "unread")
  "ts" '((lambda () (interactive) (notmuch-search "tag:main AND tag:sent")) :which-key "sent")
  "ta" '((lambda () (interactive) (notmuch-search "tag:main")) :which-key "all mail")
  "p" '(:which-key "progin6304@gmail.com")
  "pi" '((lambda () (interactive) (notmuch-search "tag:progin AND tag:inbox")) :which-key "inbox")
  "pu" '((lambda () (interactive) (notmuch-search "tag:progin AND tag:unread")) :which-key "unread")
  "ps" '((lambda () (interactive) (notmuch-search "tag:progin AND tag:sent")) :which-key "sent")
  "pa" '((lambda () (interactive) (notmuch-search "tag:progin")) :which-key "all mail")
  "e" '(:which-key "pvkorytov@etu.ru")
  "ei" '((lambda () (interactive) (notmuch-search "tag:pvkorytov AND tag:inbox")) :which-key "inbox")
  "eu" '((lambda () (interactive) (notmuch-search "tag:pvkorytov AND tag:unread")) :which-key "unread")
  "es" '((lambda () (interactive) (notmuch-search "tag:pvkorytov AND tag:sent")) :which-key "sent")
  "ea" '((lambda () (interactive) (notmuch-search "tag:pvkorytov")) :which-key "all mail"))

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
