;;; -*- lexical-binding: t -*-
(use-package max
  :if (file-exists-p "/home/pavel/10-19 Code/12 My Emacs Packages/12.24 max.el/")
  :straight (:local-repo "/home/pavel/10-19 Code/12 My Emacs Packages/12.24 max.el/")
  :commands (max-messenger)
  :init
  (my-leader-def "aM" #'max-messenger)
  :config
  (setq max-account `(:name "MAX" :auth sms
                            :phone ,(my/password-store-get-field
                                     "Accounts/max" "username")))
  (setq max-debug t)
  (setq max-chat-fill-column 80)
  (remove-hook 'max-chat-mode-hook #'max-chat-auto-fill-mode)
  (add-hook 'max-chat-mode-hook #'max-company-setup)
  (my/persp-add-rule
    max-root-mode 3 "max"
    max-chat-mode 3 "max"
    max-image-mode 3 "max")
  (general-define-key
   :states '(insert)
   :keymaps '(max-chat-mode-map)
   "C-<return>" #'newline))

(provide 'sqrt-max)
