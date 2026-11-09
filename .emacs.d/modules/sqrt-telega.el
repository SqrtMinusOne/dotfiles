;;; -*- lexical-binding: t -*-
(use-package telega
  :straight (telega
             :type git
             :host github
             :repo "zevlg/telega.el"
             :files (:defaults "contrib/*.el" "etc" "server" "contrib" "Makefile"))
  :commands (telega)
  :init
  (my-leader-def "a l" (my/command-in-persp "telega" "telega" 3 (telega)))
  (my/use-colors
   (telega-button-active :foreground (my/color-value 'base0)
                         :background (my/color-value 'cyan))
   (telega-webpage-chat-link :foreground (my/color-value 'base0)
                             :background (my/color-value 'fg))
   (telega-entity-type-spoiler :background (my/color-value 'base8)))
  :config
  (setq telega-server-libs-prefix "/usr")

  (setq telega-emoji-use-images nil)
  (setq telega-chat-fill-column 80)
  (remove-hook 'telega-chat-mode-hook #'telega-chat-auto-fill-mode)
  (setq telega-completing-read-function #'completing-read)
  (setq telega-sticker-size '(12 . 24))
  (add-to-list 'savehist-additional-variables 'telega-msg-add-reaction)
  (general-define-key
   :keymaps '(telega-root-mode-map telega-chat-mode-map)
   :states '(normal)
   "gp" telega-prefix-map)
  (general-define-key
   :keymaps '(telega-msg-button-map)
   "<SPC>" nil
   "g?" #'telega-describe-message)
  (general-define-key
   :keymaps '(telega-chat-mode-map)
   "C-<return>" #'newline)
  (general-define-key
   :keymaps '(telega-chat-mode-map)
   :states '(insert)
   "<return>" #'telega-chatbuf-newline-or-input-send)
  (general-define-key
   :keymaps '(telega-chat-mode-map)
   :states '(normal)
   "T" #'telega-chatbuf-filter-by-topic)
  (my/persp-add-rule
    telega-root-mode 3 "telega"
    telega-chat-mode 3 "telega"
    telega-image-mode 3 "telega"
    telega-webpage-mode 3 "telega"))

(defun my/telega-mode-line-unread-private ()
  "Format unread unmuted private chat count."
  (let* ((chats (telega-filter-chats (telega-chats-list)
                  '(and unread unmuted (type private secret bot))))
         (count (length chats)))
    (unless (zerop count)
      (concat
       " P:"
       (propertize (number-to-string count)
                   'face 'telega-unread-unmuted-modeline
                   'local-map
                   (make-mode-line-mouse-map
                    'mouse-1 (telega-mode-line-filter-gen
                              '(and unread unmuted (type private secret bot))))
                   'mouse-face 'mode-line-highlight
                   'help-echo "Unread unmuted private chats")))))

(defun my/telega-mode-line-unread-groups ()
  "Format unread unmuted group/channel chat count."
  (let* ((chats (telega-filter-chats (telega-chats-list)
                  '(and unread unmuted (type basicgroup supergroup))))
         (count (length chats)))
    (unless (zerop count)
      (concat
       " G:"
       (propertize (number-to-string count)
                   'face 'telega-unread-unmuted-modeline
                   'local-map
                   (make-mode-line-mouse-map
                    'mouse-1 (telega-mode-line-filter-gen
                              '(and unread unmuted (type basicgroup supergroup channel))))
                   'mouse-face 'mode-line-highlight
                   'help-echo "Unread unmuted group/channel chats")))))

(add-hook 'telega-load-hook #'telega-mode-line-mode)

(with-eval-after-load 'telega
  (advice-add 'telega-server--idle-timer-function
              :after #'telega-mode-line-update))

(setq telega-mode-line-string-format
      '("["
        (:eval
         (telega-mode-line-online-status))
        (:eval
         (when telega-use-tracking-for
           (telega-mode-line-tracking)))
        (:eval
         (my/telega-mode-line-unread-groups))
        (:eval
         (my/telega-mode-line-unread-private))
        ;; (:eval
        ;;  (telega-mode-line-unread-unmuted))
        (:eval
         (telega-mode-line-mentions 'messages))
        "]"))

(defun my/telega-tdlib-input-photo-p ()
  (let ((header "/usr/include/td/telegram/td_api.h"))
    (and (file-readable-p header)
         (with-temp-buffer
           (insert-file-contents header nil 0 4000000)
           (search-forward "object_ptr<inputPhoto> photo_" nil t)))))

(defun my/telega-tdlib-fix-input-message-photo (imc)
  (if (and (eq (telega--tl-type imc) 'inputMessagePhoto)
           (not (eq (telega--tl-type (plist-get imc :photo)) 'inputPhoto)))
      (nconc
       (list :@type "inputMessagePhoto"
             :photo (nconc
                     (list :@type "inputPhoto"
                           :photo (plist-get imc :photo))
                     (when-let ((thumbnail (plist-get imc :thumbnail)))
                       (list :thumbnail thumbnail))
                     (when-let ((video (plist-get imc :video)))
                       (list :video video))
                     (when-let ((sticker-ids (plist-get imc :added_sticker_file_ids)))
                       (list :added_sticker_file_ids sticker-ids))
                     (when-let ((width (plist-get imc :width)))
                       (list :width width))
                     (when-let ((height (plist-get imc :height)))
                       (list :height height))))
       (when-let ((caption (plist-get imc :caption)))
         (list :caption caption))
       (when-let ((caption-above (plist-get imc :show_caption_above_media)))
         (list :show_caption_above_media caption-above))
       (when-let ((self-destruct (plist-get imc :self_destruct_type)))
         (list :self_destruct_type self-destruct))
       (when-let ((spoiler-p (plist-get imc :has_spoiler)))
         (list :has_spoiler spoiler-p)))
    imc))

(defun my/telega-tdlib-fix-send-message-photo (fun chat imc &rest args)
  (apply fun chat (my/telega-tdlib-fix-input-message-photo imc) args))

(defun my/telega-tdlib-fix-send-album-photo (fun chat imcs &rest args)
  (apply fun chat (mapcar #'my/telega-tdlib-fix-input-message-photo imcs) args))

(with-eval-after-load 'telega-tdlib
  (when (my/telega-tdlib-input-photo-p)
    (advice-add 'telega--sendMessage
                :around #'my/telega-tdlib-fix-send-message-photo)
    (advice-add 'telega--sendMessageAlbum
                :around #'my/telega-tdlib-fix-send-album-photo)))

(defun my/telega-ins-user-status-when-known (fun user)
  (when (telega--tl-get user :status :@type)
    (funcall fun user)))

(with-eval-after-load 'telega-ins
  (advice-add 'telega-ins--user-status
              :around #'my/telega-ins-user-status-when-known))

(defun my/telega-ins-chat-status-when-known (fun chat &optional topic)
  (let ((start (point)))
    (condition-case err
        (funcall fun chat topic)
      (wrong-type-argument
       (if (equal err '(wrong-type-argument stringp nil))
           (progn
             (delete-region start (point))
             (when-let ((last-msg (plist-get (or topic chat) :last_message)))
               (if (telega-msg-match-p last-msg 'ignored)
                   (telega-ins--one-lined (telega-ins--message-ignored last-msg))
                 (telega-ins--chat-msg-one-line chat last-msg))))
         (signal (car err) (cdr err)))))))

(with-eval-after-load 'telega-ins
  (advice-add 'telega-ins--chat-status
              :around #'my/telega-ins-chat-status-when-known))

(defun my/telega-chat-setup ()
  (interactive)
  (require 'telega-company)
  (set (make-local-variable 'company-backends)
       (append (list 'telega-company-emoji
                     'telega-company-username
                     'telega-company-hashtag
                     'telega-company-markdown-precode)
               (when (telega-chat-bot-p telega-chatbuf--chat)
                 '(telega-company-botcmd))))
  (company-mode 1)
  (setopt visual-fill-column-width
          (+ telega-chat-fill-column
             (if (display-graphic-p) 5 6)))
  (setq-local split-width-threshold 1))
(add-hook 'telega-chat-mode-hook #'my/telega-chat-setup)

(with-eval-after-load 'telega-info
  (defun telega-proxy-last-used (&optional added-proxies)
    "Return the most recently used proxy."
    (car (cl-sort (or (copy-sequence added-proxies)
                      (telega--getProxies))
                  #'>
                  :key (lambda (proxy)
                         (or (plist-get proxy :last_used_date) 0))))))

(defun my/telega-online-status ()
  (derived-mode-p 'telega-root-mode 'telega-chat-mode
                  'telega-image-mode 'telega-webpage-mode))

(setq telega-online-status-function #'my/telega-online-status)

(defun my/telega-setup-proxies ()
  (telega--addProxy
      `(:server ,(my/password-store-get-field "Selfhosting/Machines/bern" "ip")
                :port 443
                :type (:@type "proxyTypeMtproto"
                              :secret ,(my/password-store-get-field "Selfhosting/Machines/bern" "secret")))
    :enable-p nil
    :comment "bern mtproto")
  (telega--addProxy
      '(:server "192.168.122.51"
                :port 1080
                :type (:@type "proxyTypeSocks5"))
    :enable-p t
    :comment "local socks5"))

(add-hook 'telega-before-auth-hook #'my/telega-setup-proxies)

(defun my/org-telega-text (msg)
  "Return the plain text or caption of MSG."
  (substring-no-properties
   (or (telega-msg-content-text msg) "[Media message]")))

(defun my/org-telega-sender (msg)
  "Return the sender's name for MSG."
  (substring-no-properties
   (telega-msg-sender-title (telega-msg-sender msg)
     :with-badges-p nil)))

(defun my/org-telega-body (messages)
  "Format sorted MESSAGES, grouping consecutive messages by sender."
  (let (previous-sender parts)
    (dolist (msg messages)
      (let ((sender (plist-get msg :sender_id))
            (text (replace-regexp-in-string
                   "^\\([ \t]*\\)\\(\\*+[ \t]\\|#\\+\\)" "\\1\u200B\\2"
                   (my/org-telega-text msg))))
        (push
         (if (and parts (equal sender previous-sender))
             (concat "\n" text)
           (concat
            (when parts "\n\n")
            (org-link-make-string (telega-tme-internal-link-to msg)
                                  (my/org-telega-sender msg))
            (format-time-string " (%H:%M)\n"
                                (seconds-to-time (plist-get msg :date)))
            text))
         parts)
        (setq previous-sender sender)))
    (apply #'concat (nreverse parts))))

(defun my/org-telega-store-link (&optional _interactive)
  "Store a link and capture marked messages, or the message at point."
  (let* ((messages (or telega-chatbuf--marked-messages
                       (when-let* ((msg (telega-msg-at (point))))
                         (list msg))))
         ;; Sort a copy so Telega's marked-message list is unchanged.
         (messages (cl-sort (copy-sequence messages) #'<
                            :key (lambda (msg) (plist-get msg :id))))
         (msg (car messages)))
    (if (null msg)
        (org-telega-store-link)
      (unless (telega-msg-internal-p msg)
        (let* ((text (my/org-telega-text msg))
               (summary (truncate-string-to-width
                         (mapconcat #'identity (split-string text) " ")
                         80 nil nil "…"))
               (from (my/org-telega-sender msg))
               (chat (substring-no-properties
                      (telega-chat-title (telega-msg-chat msg) nil t))))
          (org-link-store-props
           :type "telega"
           :link (telega-tme-internal-link-to msg)
           :description (format "%s: %s" from summary)
           :from from :subject summary :chat chat
           :date-timestamp-inactive
           (format-time-string (org-time-stamp-format t t)
                               (seconds-to-time (plist-get msg :date)))
           :body (my/org-telega-body messages)))))))

(with-eval-after-load 'telega
  (require 'ol-telega)
  (org-link-set-parameters "telega" :store #'my/org-telega-store-link))

(with-eval-after-load 'org-capture
  (setf (alist-get "t" org-capture-templates nil nil #'equal)
        `("Telegram" entry (file "inbox.org")
          ,(concat "* TODO %:from: %:subject %?\n"
                   "/Entered on/ %U\n"
                   "/Received on/ %:date-timestamp-inactive\n\n"
                   "#+begin_quote\n%:body\n#+end_quote\n"))))

(provide 'sqrt-telega)
