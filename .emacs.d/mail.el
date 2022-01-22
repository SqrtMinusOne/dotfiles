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
  (setq message-send-mail-function #'message-send-mail-with-sendmail)
  (setq sendmail-program (executable-find "msmtp"))
  (setq send-mail-function #'sendmail-send-it)
  (setq mml-secure-openpgp-sign-with-sender t)
  (setq notmuch-mua-user-agent-function 'notmuch-mua-user-agent-full)
  ;; Use org-contacts for completion
  (setq notmuch-address-command 'as-is)
  (add-hook 'notmuch-hello-mode-hook
            (lambda () (display-line-numbers-mode 0))))

(my-leader-def
  :infix "am"
  "" '(:which-key "notmuch")
  "m" (my/command-in-persp "notmuch" "mail" 0 (notmuch)))

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
  "ti" (my/command-in-persp "tag:main AND tag:inbox" "mail" 0 (notmuch-search "inbox"))
  "tu" (my/command-in-persp "tag:main AND tag:unread" "mail" 0 (notmuch-search "unread"))
  "ts" (my/command-in-persp "tag:main AND tag:sent" "mail" 0 (notmuch-search "sent"))
  "ta" (my/command-in-persp "tag:main" "mail" 0 (notmuch-search "all mail"))
  "p" '(:which-key "progin6304@gmail.com")
  "pi" (my/command-in-persp "tag:progin AND tag:inbox" "mail" 0 (notmuch-search "inbox"))
  "pu" (my/command-in-persp "tag:progin AND tag:unread" "mail" 0 (notmuch-search "unread"))
  "ps" (my/command-in-persp "tag:progin AND tag:sent" "mail" 0 (notmuch-search "sent"))
  "pa" (my/command-in-persp "tag:progin" "mail" 0 (notmuch-search "all mail"))
  "e" '(:which-key "pvkorytov@etu.ru")
  "ei" (my/command-in-persp "tag:pvkorytov AND tag:inbox" "mail" 0 (notmuch-search "inbox"))
  "eu" (my/command-in-persp "tag:pvkorytov AND tag:unread" "mail" 0 (notmuch-search "unread"))
  "es" (my/command-in-persp "tag:pvkorytov AND tag:sent" "mail" 0 (notmuch-search "sent"))
  "ea" (my/command-in-persp "tag:pvkorytov" "mail" 0 (notmuch-search "all mail")))

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
