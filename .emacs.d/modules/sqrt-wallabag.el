;;; -*- lexical-binding: t -*-
(use-package wallabag
  :straight (:host github :repo "chenyanming/wallabag.el" :files (:defaults "default.css" "emojis.alist"))
  :init
  (my-leader-def "aE" #'wallabag)
  :commands (wallabag wallabag-add-entry)
  :config
  (setq wallabag-host "https://wallabag.sqrtminusone.xyz")
  (setq wallabag-username "sqrtminusone")
  (setq wallabag-password (my/password-store-get "Selfhosted/wallabag"))
  (setq wallabag-clientid (password-store-get-field "Selfhosted/wallabag" "client_id"))
  (setq wallabag-secret (password-store-get-field "Selfhosted/wallabag" "client_secret")))

(provide 'sqrt-wallabag)
