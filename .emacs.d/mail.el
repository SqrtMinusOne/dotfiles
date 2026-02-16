(setq user-mail-address "thexcloud@gmail.com")
(setq user-full-name "Pavel Korytov")

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
  :commands (notmuch notmuch-search)
  :init
  (my/use-colors
   (notmuch-wash-cited-text :foreground (doom-color 'yellow)))
  :config
  (setq notmuch-fcc-dirs
        '(("pvkorytov@etu.ru" . "pvkorytov_etu_mbox/Sent")
          (".*" . "sent")))
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
  notmuch-message-mode 0 "mail"
  notmuch-show-mode 0 "mail")

(setq notmuch-saved-searches
      '((:name "drafts" :query "tag:draft" :key "d")
        (:name "main (inbox)" :query "tag:main AND tag:inbox" :key "mi")
        (:name "main (unread)" :query "tag:main AND tag:unread" :key "mu")
        (:name "main (sent)" :query "tag:main AND tag:sent" :key "ms")
        (:name "main (all mail)" :query "tag:main" :key "ma")
        (:name "progin (inbox)" :query "tag:progin AND tag:inbox" :key "pi")
        (:name "progin (unread)" :query "tag:progin AND tag:unread" :key "pu")
        (:name "progin (sent)" :query "tag:progin AND tag:sent" :key "ps")
        (:name "progin (all mail)" :query "tag:progin" :key "pa")
        (:name "pvkorytov (inbox)" :query "tag:pvkorytov AND tag:inbox" :key "vi")
        (:name "pvkorytov (unread)" :query "tag:pvkorytov AND tag:unread" :key "vu")
        (:name "pvkorytov (sent)" :query "tag:pvkorytov AND tag:sent" :key "vs")
        (:name "pvkorytov (all mail)" :query "tag:pvkorytov" :key "va")))
(setq notmuch-show-empty-saved-searches nil)

(general-define-key
 :states '(normal visual)
 :keymaps '(notmuch-hello-mode-map)
 "f" #'notmuch-jump-search)

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

(defvar my/ru-formal-pronous
  '("вы" "вас" "вам" "вами" "ваш" "ваша" "ваше" "ваши" "вашего"
    "вашей" "вашему" "вашим" "вашем" "вашеми"))

(defvar my/ru-formal-pronous-regex
  (regexp-opt
   (mapcar (lambda (p) (format " %s " p)) my/ru-formal-pronous) 'words))

(defun my/message-ensure-capitalized-formal-pronouns ()
  (interactive)
  (save-excursion
    (message-goto-body)
    (cl-block nil
      (let ((case-fold-search nil)
            confirmed)
        (while (re-search-forward my/ru-formal-pronous-regex nil t)
          (let* ((match (match-string 0))
                 (capitalized (capitalize match))
                 (beg (match-beginning 0))
                 (end (match-end 0)))
            (if (or confirmed
                    (y-or-n-p (format "Replace %s with %s? "
                                      match capitalized)))
                (progn
                  (delete-region beg end)
                  (insert capitalized)
                  (setq confirmed t))
              (cl-return))))))))

(add-hook 'notmuch-mua-send-hook #'my/message-ensure-capitalized-formal-pronouns)

(defun my/ensure-password ()
  (interactive)
  (my/password-store-get "Digital/Email/pvkorytov@etu.ru"))

(add-hook 'notmuch-mua-send-hook #'my/ensure-password)

(use-package ol-notmuch
  :straight t
  :after (org notmuch))

(defvar my/blacklist-file
  (expand-file-name "~/10-19 Code/11 Config/11.11 email-blacklist/blacklist.txt"))

(defun my/notmuch-blacklist-address (addr)
  "Add ADDR to `my/blacklist-file' if not already present.

Return t if added, nil if already present."
  (let ((contents (if (file-exists-p my/blacklist-file)
                      (with-temp-buffer
                        (insert-file-contents my/blacklist-file)
                        (downcase (buffer-string)))
                    "")))
    (unless (string-match-p (concat "^" (regexp-quote addr) "$") contents)
      (with-temp-buffer
        (insert addr "\n")
        (append-to-file (point-min) (point-max) my/blacklist-file))
      t)))

(defun my/notmuch-blacklist-sender-show ()
  "Mark current message as spam and blacklist the sender.

For use in `notmuch-show-mode'."
  (interactive)
  (let* ((from (notmuch-show-get-header :From))
         (addr (when from
                 (downcase
                  (cadr (mail-extract-address-components from))))))
    (unless addr
      (user-error "Could not extract sender address"))
    (notmuch-show-tag '("+spam" "-inbox" "-unread"))
    (if (my/notmuch-blacklist-address addr)
        (message "Marked as spam, added %s to blacklist" addr)
      (message "Marked as spam (%s already in blacklist)" addr))))

(defun my/notmuch-blacklist-sender-search (&optional beg end)
  "Mark threads as spam and blacklist their senders.

Acts on the region if active, otherwise the thread at point.  For use
in `notmuch-search-mode'."
  (interactive (notmuch-search-interactive-region))
  (let ((addrs '()))
    (save-excursion
      (goto-char beg)
      (while (and (< (point) end) (not (eobp)))
        (when-let* ((thread-id (notmuch-search-find-thread-id))
                    (from (car (process-lines
                                notmuch-command "address" "--output=sender"
                                thread-id)))
                    (addr (downcase
                           (cadr (mail-extract-address-components from)))))
          (push addr addrs))
        (forward-line 1)))
    (notmuch-search-tag '("+spam" "-inbox" "-unread") beg end)
    (setq addrs (delete-dups addrs))
    (let ((added 0))
      (dolist (addr addrs)
        (when (my/notmuch-blacklist-address addr)
          (cl-incf added)))
      (message "Marked as spam, added %d/%d addresses to blacklist"
               added (length addrs)))))
