;;; -*- lexical-binding: t -*-
(use-package elia
  :if (file-exists-p "/home/pavel/10-19 Code/12 My Emacs Packages/12.26 elia")
  :straight (:local-repo "/home/pavel/10-19 Code/12 My Emacs Packages/12.26 elia"
                         :files (:defaults "migrations"))
  :commands (elia)
  :init
  (my-leader-def "aE" #'elia)
  :config
  (my/persp-add-rule
    elia-mode 0 "elia"
    elia-home-mode 0 "elia"
    elia-event-mode 0 "elia"
    elia-query-mode 0 "elia"
    elia-market-mode 0 "elia"
    elia-article-mode 0 "elia"
    elia-dashboard-mode 0 "elia"
    elia-detail-mode 0 "elia"
    elia-indicator-mode 0 "elia"
    elia-history-mode 0 "elia"
    elia-builder-mode 0 "elia"
    elia-categories-mode 0 "elia"
    elia-stored-queries-mode 0 "elia")
  (add-hook 'elia-display-buffer-hook #'my/perspective-assign)
  (setq elia-proxy "socks5h://192.168.122.51:1080"))

(provide 'sqrt-elia)
