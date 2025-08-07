;;; -*- lexical-binding: t -*-
(use-package plz
  :straight (:host github :repo "alphapapa/plz.el")
  :defer t)

(defun my/ement ()
  (interactive)
  (ement-connect
   :user-id "@sqrtminusone:matrix.org"
   :password (my/password-store-get "My_Online/Accounts/matrix")))

(use-package ement
  :straight (:host github :repo "alphapapa/ement.el")
  :commands (ement-connect)
  :init
  (my-leader-def "ax" #'my/ement)
  :config
  (setq ement-room-list-auto-update t)
  (setq ement-room-mark-rooms-read 'send)
  (my/persp-add-rule
    ement-room-mode 3 "ement"
    ement-describe-room-mode 3 "ement"
    ement-room-occur-mode 3 "ement"
    ement-room-list-mode 3 "ement")
  ;; Room UI
  (setq ement-room-message-format-spec "%S> %W%B%r%R[%t]")
  (setq ement-room-left-margin-width 0)
  (setq ement-room-right-margin-width 10)
  (setq ement-room-sender-in-left-margin nil)
  (setq ement-room-sender-headers nil)
  (setq ement-room-sender-in-headers nil)
  (setq ement-room-wrap-prefix "-> ")
  ;; Changing some default faces
  (set-face-attribute 'ement-room-reactions nil :height 'unspecified)
  (set-face-attribute 'ement-room-reactions-key nil :height 'unspecified)
  (set-face-attribute 'ement-room-timestamp nil :inherit 'font-lock-function-name-face)
  (set-face-attribute 'ement-room-membership nil :height 0.9
                      :inherit 'font-lock-warning-face)
  (set-face-attribute 'ement-room-wrap-prefix nil :inherit 'unspecified)
  (set-face-attribute 'ement-room-timestamp-header nil :height 'unspecified)
  (set-face-attribute 'ement-room-wrap-prefix nil :inherit 'unspecified)
  ;; Notify only on mentions
  (setq ement-notify-notification-predicates
        '(ement-notify--event-mentions-session-user-p
          ement-notify--event-mentions-room-p
          ement-notify--room-unread-p))
  ;; Fix the anti-synergy with major mode re-activation in `ement-room-list-revert'
  (advice-add #'ement-room-list-revert
              :around #'my/perspective-assign-ignore-advice))

(with-eval-after-load 'ement-room-list
  (general-define-key
   :states '(normal visual)
   :keymaps '(ement-room-list-mode-map)
   "<tab>" #'magit-section-toggle
   "C-j" #'magit-section-forward
   "C-k" #'magit-section-backward
   "q" #'quit-window
   "gr" #'revert-buffer
   "RET" #'ement-room-list-RET))

(with-eval-after-load 'ement-tabulated-room-list
  (general-define-key
   :states '(normal visual)
   :keymaps '(ement-tabulated-room-list-mode-map)
   "q" #'quit-window))

(defun my/ement-room-send-reaction (key position)
  (interactive (list
                (completing-read "Add reaction: " (append telega-emoji-reaction-list '("👋")))
                (point)))
  (ement-room-send-reaction key position))

(defun my/ement-room-compose-quit ()
  (interactive)
  (when (or (string-empty-p (buffer-string))
            (y-or-n-p "Quit compose? "))
    (quit-window t)))

(defun my/ement-room-compose-setup ()
  (ement-room-compose-org)
  (setq company-backends '(telega-company-emoji company-capf))
  (general-define-key
   :states '(normal visual)
   :keymaps 'local
   "Q" #'my/ement-room-compose-quit
   "C-c C-k" (lambda () (interactive) (quit-window t))
   "C-c C-c" #'ement-room-compose-send))

(add-hook 'ement-room-compose-hook #'my/ement-room-compose-setup)

(with-eval-after-load 'ement
  (general-define-key
   :states '(normal visual)
   :keymaps '(ement-room-mode-map)
   "q" #'quit-window
   "?" #'ement-room-transient
   "C-u" #'ement-room-scroll-down-command
   "C-d" #'ement-room-scroll-up-mark-read
   "r" #'ement-room-write-reply
   "a" #'ement-room-send-message
   "i" #'ement-room-send-message
   "e" #'ement-room-edit-message
   "M-<RET>" #'ement-room-compose-message
   "<RET>" #'ement-room-send-message
   "K" #'ement-room-goto-prev
   "J" #'ement-room-goto-next
   "gr" #'ement-room-sync
   "g?" #'ement-describe-room
   "R?" #'ement-describe-room
   "Rm" #'ement-list-members
   "Rn" #'ement-room-set-notification-state
   "Rt" #'ement-room-set-topic
   "!" #'my/ement-room-send-reaction
   "m?" #'ement-room-view-event
   "Zf" #'ement-room-send-file
   "ui" #'ement-invite-user)
  (general-define-key
   :states '(normal visual)
   :keymaps '(ement-describe-room-mode-map)
   "q" #'quit-window)
  (general-define-key
   :states '(motion)
   :keymaps '(ement-room-mode-map)
   "C-u" #'ement-room-scroll-down-command
   "C-d" #'ement-room-scroll-up-mark-read))

(defun my/ement-about-me-p (event)
  (let ((me (ement-user-id (ement-session-user ement-session))))
    (or
     (equal (ement-user-id (ement-event-sender event)) me)
     (when-let ((formatted-body
                 (alist-get
                  'formatted_body
                  (ement-event-content event))))
       (string-match-p me formatted-body)))))

(defun my/ement-scroll-to-previous-about-me ()
  (interactive)
  (let ((scrolled 0))
    (when (< (line-number-at-pos) 20)
      (forward-line 20))
    (if ement-room-retro-loading
        (run-with-timer 0.5 nil #'my/ement-scroll-to-previous-about-me)
      (while (let ((event (ewoc-data (ewoc-locate ement-ewoc))))
               (and
                (not ement-room-retro-loading)
                (or
                 (not (ement-event-p event))
                 (not (my/ement-about-me-p event)))))
        (condition-case _err
            (scroll-down 1)
          (beginning-of-buffer
           (call-interactively #'ement-room-retro)
           (run-with-timer 0.5 nil #'my/ement-scroll-to-previous-about-me)))
        (cl-incf scrolled)
        (message "Scrolled %s" scrolled)))))

(provide 'sqrt-ement)
