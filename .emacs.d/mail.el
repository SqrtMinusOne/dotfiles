(use-package notmuch
  :if (not my/is-termux)
  ;; :ensure nil
  :commands (notmuch notmuch-search)
  :init
  (my/use-colors
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
