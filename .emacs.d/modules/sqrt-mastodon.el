;;; -*- lexical-binding: t -*-
(use-package mastodon
  :straight t
  :commands (my/mastodon)
  :init
  (my-leader-def "an" #'my/mastodon)
  :config
  (setq mastodon-instance-url "https://mastodon.bsd.cafe")
  (setq mastodon-active-user "sqrtminusone")
  (my/persp-add-rule mastodon-mode 0 "mastodon")
  ;; Hide spoilers by default
  ;; (setq-default mastodon-toot--content-warning nil)
  (setq mastodon-media--avatar-height 40)
  (setq mastodon-tl--timeline-posts-count "40")
  (setq mastodon-tl--show-avatars t)
  (setq mastodon-tl--horiz-bar
        (make-string shr-max-width
                     (if (char-displayable-p ?―) ?― ?-)))
  ;; The default emojis take two characters for me
  (mapcar (lambda (item)
            (setf (alist-get (car item) mastodon-tl--symbols)
                  (cdr item)))
          '((reply "" . "R")
            (boost "" . "B")
            (favourite "" . "F")
            (bookmark "" . "K")
            (media "" . "[media]")
            (verified "" . "V")
            (locked "" . "[locked]")
            (private "" . "[followers]")
            (direct "" . "[direct]")
            (edited "" . "[edited]"))))

(use-package mastodon-alt
  :straight (:host github :repo "rougier/mastodon-alt")
  :disabled t
  :after (mastodon)
  :config
  (mastodon-alt-tl-activate))

(use-package transient
  :straight t
  :defer t)

(unless (display-graphic-p)
  (defun image-transforms-p () nil)
  (setq image-types '(svg png gif tiff jpeg xpm xbm pbm)))

(defun my/mastodon-configure ()
  (display-line-numbers-mode -1))

(add-hook 'mastodon-mode-hook #'my/mastodon-configure)

(defun my/mastodon-reset ()
  (interactive)
  (cl-loop for process in (process-list)
           if (string-match-p "emacs.ch" (process-name process))
           do (delete-process process)))

(with-eval-after-load 'mastodon
  (general-define-key
   :states '(normal motion)
   :keymaps '(mastodon-mode-map)
   "J" #'mastodon-tl-goto-next-item
   "K" #'mastodon-tl-goto-prev-item
   "M-j" #'mastodon-tl-next-tab-item
   "M-k" #'mastodon-tl-prev-tab-item
   "<tab>" #'mastodon-tl-next-tab-item
   "<backtab>" #'mastodon-tl-previous-tab-item
   "o" #'my/mastodon-toot
   "r" 'mastodon-tl-update
   "c" #'mastodon-tl-toggle-spoiler-text-in-toot
   "q" #'kill-current-buffer))

(defvar my/mastodon-mode-string "")

(defvar my/mastodon-mode-line-unread-ids nil)

(defvar my/mastodon-mode-line-saved-ids nil)

(defvar my/mastodon-mode-line-timer nil)

(defvar my/mastodon-mode-line-file
  (concat no-littering-var-directory "mastodon/notif-ids"))

(defun my/mastodon-mode-line-load-meta ()
  (when (file-exists-p my/mastodon-mode-line-file)
    (ignore-errors
      (with-temp-buffer
        (insert-file-contents my/mastodon-mode-line-file)
        (setq my/mastodon-mode-line-saved-ids
              (read (current-buffer)))))))

(defun my/mastodon-mode-line-persist-meta ()
  (mkdir (file-name-directory my/mastodon-mode-line-file) t)
  (let ((coding-system-for-write 'utf-8))
    (ignore-errors
      (with-temp-file my/mastodon-mode-line-file
        (let ((standard-output (current-buffer))
              (print-level nil)
              (print-length nil)
              (print-circle nil))
          (princ ";;; Mastodon Saved Notifications\n\n")
          (prin1 my/mastodon-mode-line-saved-ids))))))

(defun my/mastodon-mode-line-update ()
  (if my/mastodon-mode-line-unread-ids
      (setq my/mastodon-mode-string
            (concat "["
                    (propertize (number-to-string
                                 (length my/mastodon-mode-line-unread-ids))
                                'face 'success)
                    "]"))
    (setq my/mastodon-mode-string "")))

(defun my/mastodon-mode-line-update-fetch ()
  (mastodon-http--get-json-async
   (mastodon-http--api "notifications") nil
   (lambda (data)
     (let ((fetched-ids
            (cl-loop for datum in data collect (alist-get 'id datum))))
       (setq my/mastodon-mode-line-unread-ids
             (seq-difference fetched-ids my/mastodon-mode-line-saved-ids))
       (setq my/mastodon-mode-line-saved-ids
             (seq-intersection my/mastodon-mode-line-saved-ids fetched-ids)))
     (my/mastodon-mode-line-update))))

(defun my/mastodon-notifications--timeline-before (toots)
  (let* ((all-ids (seq-uniq
                   (append
                    my/mastodon-mode-line-saved-ids
                    (cl-loop for datum in toots
                             collect (alist-get 'id datum))))))
    (setq my/mastodon-mode-line-unread-ids
          (seq-difference my/mastodon-mode-line-unread-ids all-ids))
    (setq my/mastodon-mode-line-saved-ids all-ids))
  (my/mastodon-mode-line-update))

(with-eval-after-load 'mastodon
  (define-minor-mode my/mastodon-mode-line
    "Display mastodon notification count in mode line."
    :require 'mastodon
    :global t
    :group 'mastodon
    :after-hook
    (progn
      (when (timerp my/mastodon-mode-line-timer)
        (cancel-timer my/mastodon-mode-line-timer))
      (if my/mastodon-mode-line
          (progn
            (add-to-list 'mode-line-misc-info '(:eval my/mastodon-mode-string) t)
            (my/mastodon-mode-line-load-meta)
            (setq my/mastodon-mode-line-timer
                  (run-with-timer 0 150 #'my/mastodon-mode-line-update-fetch))
            (advice-add #'mastodon-notifications--timeline :before
                        #'my/mastodon-notifications--timeline-before)
            (add-hook 'kill-emacs-hook #'my/mastodon-mode-line-persist-meta))
        (setq mode-line-misc-info (delete '(:eval my/mastodon-mode-string)
                                          mode-line-misc-info))
        (advice-remove #'mastodon-notifications--timeline
                       #'my/mastodon-notifications--timeline-before)
        (remove-hook 'kill-emacs-hook #'my/mastodon-mode-line-persist-meta)
        (my/mastodon-mode-line-persist-meta)))))

(defun my/mastodon-get-update-funciton (hide-replies hide-boosts)
  (lambda (toots)
    (let* ((is-profile (eq (mastodon-tl--get-buffer-type) 'profile-statuses))
           (hide-replies (and (not is-profile) hide-replies))
           (hide-boosts (and (not is-profile) hide-boosts))
           (toots (seq-filter
                   (lambda (toot)
                     (and
                      (or (not hide-replies)
                          (not (mastodon-tl--is-reply toot)))
                      (or (not hide-boosts)
                          (not (alist-get 'reblog toot)))))
                   toots))
           (start-pos (point)))
      (mapc #'mastodon-tl--toot toots)
      (when mastodon-tl--display-media-p
        (save-excursion
          (mastodon-media--inline-images start-pos (point)))))))

(defun my/mastodon-tl--get-home (hide-replies hide-boosts)
  (mastodon-tl--init
   "home"
   "timelines/home"
   (my/mastodon-get-update-funciton hide-replies hide-boosts)
   nil
   `(("limit" . ,mastodon-tl--timeline-posts-count))
   nil))

(with-eval-after-load 'mastodon
  (require 'transient)
  (transient-define-prefix my/mastodon-tl ()
    ["Home timeline params"
     ("-r" "--hide-replies" "--hide-replies" :init-value
      (lambda (obj) (oset obj value "--hide-replies")))
     ("-b" "--hide-boosts" "--hide-boosts" :init-value
      (lambda (obj) (oset obj value "--hide-boosts")))]
    ["Timelines"
     :class transient-row
     ("t" "Home" (lambda (args)
                   (interactive (list (transient-args transient-current-command)))
                   (my/mastodon-tl--get-home
                    (seq-contains-p args "--hide-replies")
                    (seq-contains-p args "--hide-boosts"))))
     ("l" "Local" mastodon-tl-get-local-timeline)
     ("f" "Federated" mastodon-tl-get-federated-timeline)
     ("g" "One tag" mastodon-tl-get-tag-timeline)
     ("a" "Followed tags" mastodon-tl-followed-tags-timeline)
     ("s" "Some followed tags" mastodon-tl-some-followed-tags-timeline)]
    ["Misc"
     :class transient-row
     ("q" "Quit" transient-quit-one)]))

(with-eval-after-load 'mastodon
  (require 'transient)
  (transient-define-prefix my/mastodon ()
    "Mastodon."
    ["Various views"
     :class transient-row
     ("m" "Mastodon" mastodon)
     ("t" "Timelines" my/mastodon-tl)
     ("n" "Notifications" mastodon-notifications-get)
     ("s" "Search query" mastodon-search-query)]
    ["Tags"
     :class transient-row
     ("aa" "Followed tags" mastodon-tl-list-followed-tags)
     ("af" "Follow tag" mastodon-tl-follow-tag)
     ("aF" "Unfollow tag" mastodon-tl-unfollow-tag)]
    ["Own profile"
     :class transient-row
     ("c" "Toot" mastodon-toot)
     ("o" "My profile" mastodon-profile-my-profile)
     ("u" "Update profile note" mastodon-profile-update-user-profile-note)
     ("f" "Favourites" mastodon-profile-view-favourites)
     ("b" "Bookmarks" mastodon-profile-view-bookmarks)]
    ["Minor views"
     :class transient-row
     ("F" "Follow requests" mastodon-views-view-follow-requests)
     ("S" "Scheduled toots" mastodon-views-view-scheduled-toots)
     ("I" "Filters" mastodon-views-view-filters)
     ("G" "Follow suggestions" mastodon-views-view-follow-suggestions)
     ("L" "Lists" mastodon-views-view-lists)]
    ["Misc"
     :class transient-row
     ("/" "Switch to buffer" mastodon-switch-to-buffer)
     ("Q" "Kill all buffers" mastodon-kill-all-buffers)
     ("q" "Quit" transient-quit-one)]))

(defmacro my/def-confirmer (func text)
  `(defun ,(intern (concat "my/" (symbol-name func) "-confirm")) ()
     (interactive)
     (when (y-or-n-p ,text)
       (call-interactively #',func))))

(defun my/mastodon-toot--browse ()
  "Copy URL of toot at point.
If the toot is a fave/boost notification, copy the URLof the
base toot."
  (interactive)
  (let* ((toot (or (mastodon-tl--property 'base-toot)
                   (mastodon-tl--property 'toot-json)))
         (url (if (mastodon-tl--field 'reblog toot)
                  (alist-get 'url (alist-get 'reblog toot))
                (alist-get 'url toot))))
    (browse-url url)))

(with-eval-after-load 'mastodon
  (my/def-confirmer mastodon-toot-toggle-boost "Toggle boost for this post? ")
  (my/def-confirmer mastodon-toot-toggle-favourite "Toggle favourite this post? ")
  (my/def-confirmer mastodon-toot-toggle-bookmark "Toggle bookmark this post? ")
  (my/def-confirmer mastodon-tl-follow-user "Follow this user? ")
  (my/def-confirmer mastodon-tl-unfollow-user "Unfollow this user? ")
  (my/def-confirmer mastodon-tl-block-user "Block this user? ")
  (my/def-confirmer mastodon-tl-unblock-user "Unblock this user? ")
  (my/def-confirmer mastodon-tl-mute-user "Mute this user? ")
  (my/def-confirmer mastodon-tl-unmute-user "Unmute this user? ")
  (my/def-confirmer mastodon-tl-unmute-user "Unmute this user? ")

  (transient-define-prefix my/mastodon-toot ()
    "Mastodon toot actions."
    ["View"
     :class transient-row
     ("o" "Thread" mastodon-tl-thread)
     ("w" "Browser" my/mastodon-toot--browse)
     ("le" "List edits" mastodon-toot-view-toot-edits)
     ("lf" "List favouriters" mastodon-toot-list-favouriters)
     ("lb" "List boosters" mastodon-toot-list-boosters)]
    ["Toot Actions"
     :class transient-row
     ("r" "Reply" mastodon-toot-reply)
     ("v" "Vote" mastodon-tl-poll-vote)
     ("b" "Boost" my/mastodon-toot-toggle-boost-confirm)
     ("f" "Favourite" my/mastodon-toot-toggle-favourite-confirm)
     ("k" "Bookmark" my/mastodon-toot-toggle-bookmark-confirm)]
    ["My Toot Actions"
     :class transient-row
     ("md" "Delete" mastodon-toot-delete-toot)
     ("mD" "Delete and redraft" mastodon-toot-delete-and-redraft-toot)
     ("mp" "Pin" mastodon-toot-pin-toot-toggle)
     ("me" "Edit" mastodon-toot-edit-toot-at-point)]
    ["Profile Actions"
     :class transient-row
     ("pp" "Profile" mastodon-profile-show-user)
     ("pf" "List followers" mastodon-profile-open-followers)
     ("pF" "List following" mastodon-profile-open-following)
     ("ps" "List statues (no reblogs)" mastodon-profile-open-statuses-no-reblogs)]
    ["User Actions"
     :class transient-row
     ("uf" "Follow user" my/mastodon-tl-follow-user-confirm)
     ("uF" "Unfollow user" my/mastodon-tl-unfollow-user-confirm)
     ("ub" "Block user" my/mastodon-tl-block-user-confirm)
     ("uB" "Unblock user" my/mastodon-tl-unblock-user-confirm)
     ("um" "Mute user" my/mastodon-tl-mute-user-confirm)
     ("uB" "Unmute user" my/mastodon-tl-unmute-user-confirm)]
    ["Misc"
     :class transient-row
     ("q" "Quit" transient-quit-one)]))

(provide 'sqrt-mastodon)
