;;; -*- lexical-binding: t -*-
(use-package deterred
  :if (file-exists-p "/home/pavel/10-19 Code/13 Other Projects/13.02 sqrt-data/13.02.R Repos/13.02.R.05 deterred/")
  :straight (:local-repo
             "/home/pavel/10-19 Code/13 Other Projects/13.02 sqrt-data/13.02.R Repos/13.02.R.05 deterred/"
             :files (:defaults "dashboards/*.el" "migrations" "python"))
  :commands (deterred)
  :init
  (my-leader-def "ag" #'deterred)
  :config
  (setq deterred-sources
        (list
         (deterred-activitywatch)
         (deterred-digikam :digikam-db "~/30-39 Life/35 Photos/35.00D DigiKam/digikam4.db")
         (deterred-hledger)
         ;; (deterred-habits :org-files '("~/30-39 Life/32 org-mode/misc/habit.org"))
         ;; (deterred-locations)
         (deterred-mastodon :server "https://mastodon.bsd.cafe/"
                            :account-id 113136872089175263)
         (deterred-messengers)
         (deterred-mpd)
         (deterred-org-journal-tags)
         (deterred-org-roam)
         (deterred-podcasts)
         (deterred-read-it-later :sources '(readeck))
         (deterred-reddit)
         (deterred-transport)
         (deterred-wakatime)))

  (setq deterred-backups-location "/home/pavel/10-19 Code/13 Other Projects/13.02 sqrt-data/13.02.B Backups/13.02.B.01 deterred/")
  (setq deterred-dashboard-python "/home/pavel/micromamba/envs/deterred/bin/python")
  (setq deterred-messengers-my-id "098236bb-5fc5-4a04-8b6d-477afa5105fa")
  (setq deterred-digikam-folder "~/30-39 Life/35 Photos/")
  (setq deterred-wakatime-api-key (my/password-store-get "My_Online/APIs/wakatime"))
  (setq deterred-wakatime-project-name-map
        '(("digital-nlp-docs-3" . "digital-nlp-docs-3 (Graduate)")
          ("digital-nlp-docs" . "digital-nlp-docs (Master's)")))
  (setq deterred-wakatime-process-project-name
        (lambda (name)
          (let ((result (string-replace
                         "\\" ""
                         (my/index--bare-project-name name))))
            (or (alist-get result deterred-wakatime-project-name-map
                           nil nil #'equal)
                result))))
  (setq deterred-read-it-later-readeck-token
        (my/password-store-get-field "Selfhosted/readeck" "deterred_api_token"))
  (setq deterred-read-it-later-readeck-url "https://readeck.sqrtminusone.xyz/")
  (setq deterred-read-it-later-wallabag-url "https://wallabag.sqrtminusone.xyz/")
  (setq deterred-read-it-later-wallabag-client-id
        (my/password-store-get-field "Selfhosted/wallabag" "client_id"))
  (setq deterred-read-it-later-wallabag-client-secret
        (my/password-store-get-field "Selfhosted/wallabag" "client_secret"))
  (setq deterred-read-it-later-wallabag-username
        (my/password-store-get-field "Selfhosted/wallabag" "username"))
  (setq deterred-read-it-later-wallabag-password
        (my/password-store-get "Selfhosted/wallabag"))
  (setq deterred-transport-podorozhnik-login
        (my/password-store-get-field "My_Online/Accounts/podorozhnik.spb.ru" "username"))
  (setq deterred-transport-podorozhnik-password
        (my/password-store-get "My_Online/Accounts/podorozhnik.spb.ru"))
  (add-hook 'deterred-dispatcher-startup-hook #'deterred-backup))

(provide 'sqrt-deterred)
