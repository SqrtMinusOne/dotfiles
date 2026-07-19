;;; -*- lexical-binding: t -*-
(use-package el-rss
  :if (file-exists-p "/home/pavel/10-19 Code/12 My Emacs Packages/12.23 el-rss/")
  :straight (:local-repo "/home/pavel/10-19 Code/12 My Emacs Packages/12.23 el-rss/"
                         :files ("*.el" ("migrations" "migrations/*.sql")))
  :commands (el-rss)
  :config
  (setq el-rss-tt-rss-url "https://sqrtminusone.xyz/tt-rss/")
  (setq el-rss-tt-rss-username "sqrtminusone")
  (setq el-rss-tt-rss-password-function
        (lambda ()
          (my/password-store-get
           "Selfhosting/Accounts/sqrtminusone.xyz-tt-rss")))
  (setq el-rss-readeck-url "https://readeck.sqrtminusone.xyz/")
  (setq el-rss-readeck-token-function
        (lambda ()
          (my/password-store-get-field
           "Selfhosting/Accounts/readeck.sqrtminusone.xyz"
           "elrss_token")))
  (setq el-rss-tree-width 50)
  (setq el-rss-tt-rss-sync-plugin 'required)
  (setq el-rss-tt-rss-proxy-media t)
  (setq el-rss-tt-rss-download-media-during-sync nil
        el-rss-tt-rss-fetch-media-on-open t)
  (my/persp-add-rule
    el-rss-list-mode 0 "el-rss"
    el-rss-tree-mode 0 "el-rss"
    el-rss-show-mode 0 "el-rss"))

(provide 'sqrt-el-rss)
