;;; -*- lexical-binding: t -*-
(use-package pomm
  :straight t
  ;; :straight (:local-repo "~/Code/Emacs/pomm" :files (:defaults "resources"))
  :commands (pomm pomm-third-time)
  :init
  (my-leader-def "ap" #'pomm)
  ;; (my-leader-def "ap" #'pomm-third-time)
  (setq alert-default-style 'libnotify)
  (setq pomm-audio-enabled t)
  (setq pomm-audio-player-executable (executable-find "mpv"))
  :config
  (pomm-mode-line-mode)
  (add-hook 'pomm-on-status-changed-hook #'pomm--sync-org-clock)
  (add-hook 'pomm-third-time-on-status-changed-hook
            #'pomm-third-time--sync-org-clock))

(use-package hledger-mode
  :straight t
  :mode (rx ".journal" eos)
  :config
  (setq hledger-jfile (concat org-directory "/ledger/ledger.journal"))
  (add-hook 'hledger-mode-hook
            (lambda ()
              (make-local-variable 'company-backends)
              (add-to-list 'company-backends 'hledger-company))))

(use-package flycheck-hledger
  :straight t
  :after (hledger-mode))

(setq calendar-date-style 'iso) ;; YYYY/mm/dd
(setq calendar-week-start-day 1)
(setq calendar-time-display-form '(24-hours ":" minutes))

(setq calendar-latitude 59.9375)
(setq calendar-longitude 30.308611)

(provide 'sqrt-productivity-apps)
