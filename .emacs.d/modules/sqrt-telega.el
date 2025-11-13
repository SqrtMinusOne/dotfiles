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

(add-hook 'telega-load-hook #'telega-mode-line-mode)
(setq telega-mode-line-string-format
      '("["
        (:eval
         (telega-mode-line-online-status))
        (:eval
         (when telega-use-tracking-for
           (telega-mode-line-tracking)))
        (:eval
         (telega-mode-line-unread-unmuted))
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

(provide 'sqrt-telega)
