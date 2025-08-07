;;; -*- lexical-binding: t -*-
(use-package wakatime-mode
  :straight (:host github :repo "SqrtMinusOne/wakatime-mode")
  :config
  (setq wakatime-ignore-exit-codes '(0 1 102 112))
  (advice-add 'wakatime-init :after
              (lambda ()
                (setq wakatime-cli-path (or
                                         (executable-find "wakatime-cli")
                                         (expand-file-name "~/bin/wakatime-cli")))))
  (when (file-exists-p "~/.wakatime.cfg")
    (setq wakatime-api-key
          (string-trim
           (shell-command-to-string "awk '/api-key/{print $NF}' ~/.wakatime.cfg"))))
  ;; (setq wakatime-cli-path (executable-find "wakatime"))
  (global-wakatime-mode))

(provide 'sqrt-wakatime)
