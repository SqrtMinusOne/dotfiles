;;; -*- lexical-binding: t -*-
(use-package rocket
  :if (file-exists-p "/home/pavel/10-19 Code/12 My Emacs Packages/12.22 rocket.el/")
  :straight (:local-repo "/home/pavel/10-19 Code/12 My Emacs Packages/12.22 rocket.el/")
  :commands (rocket)
  :init
  (my-leader-def "av" #'rocket)
  :config
  (setq rocket-servers
        '((etu :name "ETU" :url "https://chat.etudevs.ru"
               :auth browser)
          (moevm :name "MOEVM" :url "https://chat.moevm.pro"
                 :auth password :username "pvkorytov")))
  (add-hook 'rocket-chat-mode-hook #'rocket-company-setup)
  (setq rocket-chat-fill-column 80)
  (remove-hook 'rocket-chat-mode-hook #'rocket-chat-auto-fill-mode)
  (my/persp-add-rule
    rocket-root-mode 3 "rocket"
    rocket-chat-mode 3 "rocket"
    rocket-image-mode 3 "rocket"))

(provide 'sqrt-rocket)
