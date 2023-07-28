(setq user-mail-address "thexcloud@gmail.com")
(setq user-full-name "Pavel Korytov")

(let ((default-directory  "/home/pavel/.guix-extra-profiles/mail/mail/share/emacs/site-lisp"))
  (normal-top-level-add-subdirs-to-load-path))

(defun my/notmuch-toggle-trash ()
  (interactive)
  (evil-collection-notmuch-toggle-tag "trash" "search" #'ignore))

(defun my/notmuch-toggle-inbox ()
  (interactive)
  (evil-collection-notmuch-toggle-tag "inbox" "search" #'ignore))

(defun my/notmuch-toggle-unread ()
  (interactive)
  (evil-collection-notmuch-toggle-tag "unread" "search" #'ignore))

(use-package notmuch
  ;; :ensure nil
  :commands (notmuch notmuch-search)
  :init
  (my/use-doom-colors
   (notmuch-wash-cited-text :foreground (doom-color 'yellow)))
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
  (general-define-key
   :keymaps 'notmuch-search-mode-map
   :states '(normal)
   "d" #'my/notmuch-toggle-trash
   "i" #'my/notmuch-toggle-inbox
   "u" #'my/notmuch-toggle-unread)
  ;; Use org-contacts for completion
  (require 'org-contacts)
  (setq notmuch-address-command 'as-is)
  (add-hook 'notmuch-hello-mode-hook
            (lambda () (display-line-numbers-mode 0))))

(my-leader-def
  "am" (my/command-in-persp "notmuch" "mail" 0 (notmuch)))

(my/persp-add-rule
  notmuch-hello-mode 0 "mail"
  notmuch-search-mode 0 "mail"
  notmuch-tree-mode 0 "mail"
  notmuch-message-mode 0 "mail")

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

(defun my/message-insert-signature-need-on-top ()
  t)

(defun my/message-maybe-fix-signature (&rest _)
  (when (my/message-insert-signature-need-on-top)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward message-signature-separator nil t)
        (move-beginning-of-line 0)
        (kill-region (point) (point-max)))
      (message-goto-body)
      (when (re-search-forward (rx "sign=pgpmime") nil t)
        (forward-line))
      (insert (current-kill 0))
      (insert "\n\n")
      (set-buffer-modified-p nil))))

(with-eval-after-load 'notmuch-mua
  (advice-add #'notmuch-mua-reply :after #'my/message-maybe-fix-signature))

(defun my/message-ensure-subject ()
  (unless (or (message-field-value "Subject")
              (y-or-n-p "No subject. Send? "))
    (user-error "Aborting.")))

(add-hook 'notmuch-mua-send-hook #'my/message-ensure-subject)
