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

(use-package life-calendar
  :straight (:host github :repo "vshender/emacs-life-calendar")
  :commands (life-calendar)
  :config
  (setq life-calendar-birthday "1998-08-14")
  (setq life-calendar-future-char "·")
  (setq life-calendar-past-char "×")
  (setq life-calendar-chapter-char "E")
  (setq life-calendar-current-char "ø")

  (setq life-calendar-chapters
        '(("2005-01-01" . "School")
          ("2016-09-01" . "Baccalaureate")
          ("2020-09-01" . "Master's")
          ("2022-10-01" . "Grad School")
          ("2025-12-22" . "Failed Thesis Defence"))))

(provide 'sqrt-productivity-apps)
