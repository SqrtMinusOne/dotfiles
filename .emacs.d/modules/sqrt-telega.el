;;; -*- lexical-binding: t -*-
(use-package telega
  :straight t
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
  (setq telega-completing-read-function #'completing-read)
  (setq telega-sticker-size '(12 . 24))
  (setq telega-proxies
        `((:server
           ,(my/password-store-get-field "Selfhosting/Machines/bern" "ip")
           :port 443 :enable t :type
           (:@type "proxyTypeMtproto" :secret
                   ,(my/password-store-get-field "Selfhosting/Machines/bern" "secret")))))
  (add-to-list 'savehist-additional-variables 'telega-msg-add-reaction)
  (remove-hook 'telega-chat-mode-hook #'telega-chat-auto-fill-mode)
  (general-define-key
   :keymaps '(telega-root-mode-map telega-chat-mode-map)
   :states '(normal)
   "gp" telega-prefix-map)
  (general-define-key
   :keymaps '(telega-msg-button-map)
   "<SPC>" nil)
  (general-define-key
   :keymaps '(telega-chat-mode-map)
   "C-<return>" #'newline)
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

(defun my/telega-chat-setup ()
  (interactive)
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

(defun my/telega-online-status ()
  (derived-mode-p 'telega-root-mode 'telega-chat-mode
                  'telega-image-mode 'telega-webpage-mode))

(setq telega-online-status-function #'my/telega-online-status)

(defun my/telega-switch-to-topic ()
  (interactive)
  (let* ((topics-data (gethash
                       (plist-get telega-chatbuf--chat :id)
                       telega--chat-topics))
         (topics-string
          (mapcar
           (lambda (topic)
             (let* ((name (plist-get (plist-get topic :info) :name))
                    (unread-count (plist-get topic :unread_count))
                    (name-string (with-temp-buffer
                                   (telega-ins--topic-title topic 'with-icon)
                                   (buffer-string))))
               (if (zerop unread-count)
                   name-string
                 (format "%-40s (%s)"
                         name-string
                         (propertize (format "%d" unread-count)
                                     'face 'telega-unread-unmuted-modeline)))))
           topics-data))
         (topics-collection (cl-loop for datum in topics-data
                                     for string in topics-string
                                     collect (cons string datum)))
         (topic (completing-read "Topic: " topics-collection nil t)))
    (telega-chat--goto-thread
     telega-chatbuf--chat
     (plist-get
      (plist-get
       (alist-get topic topics-collection nil nil #'equal)
       :info)
      :message_thread_id))))

(with-eval-after-load 'telega
  (general-define-key
   :states '(normal)
   :keymaps 'telega-chat-mode-map
   "T" #'my/telega-switch-to-topic))

(with-eval-after-load 'telega-tdlib
  (defun my/telega-proxy-enable-value (proxy-spec)
    (if (eq (plist-get proxy-spec :enable) :false)
        :false
      (if (plist-get proxy-spec :enable) t :false)))

  (defun my/telega-proxy-body (proxy-spec)
    (list :server (plist-get proxy-spec :server)
          :port (plist-get proxy-spec :port)
          :type (plist-get proxy-spec :type)))

  (defun my/telega-normalize-added-proxy (added-proxy)
    (let ((proxy (plist-get added-proxy :proxy)))
      (append
       (list :id (plist-get added-proxy :id)
             :last_used_date (plist-get added-proxy :last_used_date)
             :is_enabled (plist-get added-proxy :is_enabled))
       proxy)))

  (defun my/telega-same-proxy-p (proxy-spec normalized-proxy)
    (and (equal (plist-get proxy-spec :server)
                (plist-get normalized-proxy :server))
         (equal (plist-get proxy-spec :port)
                (plist-get normalized-proxy :port))
         (equal (plist-get (plist-get proxy-spec :type) :@type)
                (plist-get (plist-get normalized-proxy :type) :@type))
         (equal (plist-get (plist-get proxy-spec :type) :secret)
                (plist-get (plist-get normalized-proxy :type) :secret))
         (equal (plist-get (plist-get proxy-spec :type) :username)
                (plist-get (plist-get normalized-proxy :type) :username))
         (equal (plist-get (plist-get proxy-spec :type) :password)
                (plist-get (plist-get normalized-proxy :type) :password))
         (equal (plist-get (plist-get proxy-spec :type) :http_only)
                (plist-get (plist-get normalized-proxy :type) :http_only))))

  (defun my/telega-get-proxies-compat (&optional callback)
    (if callback
        (telega-server--call
         (list :@type "getProxies")
         (lambda (reply)
           (funcall callback
                    (mapcar #'my/telega-normalize-added-proxy
                            (append (plist-get reply :proxies) nil)))))
      (mapcar #'my/telega-normalize-added-proxy
              (append (plist-get (telega-server--call (list :@type "getProxies"))
                                 :proxies)
                      nil))))

  (defun my/telega-resolve-added-proxy (proxy-spec raw-reply)
    (let ((normalized (my/telega-normalize-added-proxy raw-reply)))
      (if (plist-get normalized :id)
          normalized
        (cl-find-if (lambda (proxy)
                      (my/telega-same-proxy-p proxy-spec proxy))
                    (my/telega-get-proxies-compat)))))

  (defun my/telega-add-proxy-compat (proxy-spec &optional callback)
    (telega-server--call
     (list :@type "addProxy"
           :proxy (my/telega-proxy-body proxy-spec)
           :enable (my/telega-proxy-enable-value proxy-spec))
     (lambda (reply)
       (when callback
         (funcall callback
                  (my/telega-resolve-added-proxy proxy-spec reply))))))

  (defun my/telega-ping-proxy-compat (proxy &optional callback)
    (telega-server--call
     (list :@type "pingProxy"
           :proxy (when proxy
                    (my/telega-proxy-body proxy)))
     callback))

  (defun my/telega-add-proxies-sequentially (proxies done)
    (if (null proxies)
        (funcall done)
      (telega--addProxy
       (car proxies)
       (lambda (_proxy)
         (my/telega-add-proxies-sequentially (cdr proxies) done)))))

  (defvar my/telega-tdlib-parameters-sent nil)

  (defun my/telega-set-tdlib-parameters-once ()
    (unless my/telega-tdlib-parameters-sent
      (setq my/telega-tdlib-parameters-sent t)
      (telega--setTdlibParameters)))

  (defun my/telega-on-update-authorization-state-compat (orig-fun event)
    (let* ((state (plist-get event :authorization_state))
           (stype (plist-get state :@type)))
      (if (not (eq (intern stype) 'authorizationStateWaitTdlibParameters))
          (funcall orig-fun event)
        (setq telega--auth-state (substring stype 18))
        (telega-status--set (concat "Auth " telega--auth-state))
        (when-let ((devices-chown-cmd
                    (telega-docker-exec-cmd
                     "chmod -R o+rw /dev/snd /dev/video0" nil
                     "-u 0" 'no-error)))
          (telega-debug "docker RUN: %s" devices-chown-cmd)
          (shell-command-to-string devices-chown-cmd))
        (setq my/telega-tdlib-parameters-sent nil)
        (if telega-proxies
            (progn
              (run-with-timer 1 nil #'my/telega-set-tdlib-parameters-once)
              (my/telega-add-proxies-sequentially
               telega-proxies
               #'my/telega-set-tdlib-parameters-once))
          (my/telega-set-tdlib-parameters-once)))))

  (advice-add 'telega--addProxy :override #'my/telega-add-proxy-compat)
  (advice-add 'telega--getProxies :override #'my/telega-get-proxies-compat)
  (advice-add 'telega--pingProxy :override #'my/telega-ping-proxy-compat)
  (advice-add 'telega--on-updateAuthorizationState :around
              #'my/telega-on-update-authorization-state-compat))

(with-eval-after-load 'telega-info
  (defun my/telega-ping-proxies-compat (proxies &optional callback)
    (let ((track-timeout nil))
      (dolist (proxy proxies)
        (let* ((proxy-id (plist-get proxy :id))
               (ping (assq proxy-id telega--proxy-pings))
               (currts (time-to-seconds)))
          (when (> currts (+ (or (cadr ping) 0) 60))
            (setq track-timeout t)
            (setf (alist-get proxy-id telega--proxy-pings) (cons currts nil))
            (telega--pingProxy
             proxy
             (lambda (seconds)
               (setf (alist-get proxy-id telega--proxy-pings)
                     (cons (time-to-seconds) (plist-get seconds :seconds)))
               (when callback
                 (funcall callback)))))))
      (when (and track-timeout callback)
        (run-with-timer 10 nil callback))))

  (advice-add 'telega--pingProxies :override #'my/telega-ping-proxies-compat))

(provide 'sqrt-telega)
