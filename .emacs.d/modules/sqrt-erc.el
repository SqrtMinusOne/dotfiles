;;; -*- lexical-binding: t -*-
(use-package erc
  :commands (erc erc-tls)
  :straight (:type built-in)
  :config
  (setq erc-log-channels-directory "~/.erc/logs")
  (setq erc-save-buffer-on-part t)
  (add-to-list 'erc-modules 'autojoin)
  (add-to-list 'erc-modules 'notifications)
  (add-to-list 'erc-modules 'log)
  (erc-update-modules)
  (setq erc-autojoin-channels-alist
        `((,(rx "libera.chat")
           "#systemcrafters" "#systemcrafters-emacs")))
  (setq erc-kill-buffer-on-part t)
  (setq erc-track-shorten-start 8))

(setq erc-track-exclude-types '("NICK" "JOIN" "LEAVE" "QUIT" "PART"
                                "301"   ; away notice
                                "305"   ; return from awayness
                                "306"   ; set awayness
                                "324"   ; modes
                                "329"   ; channel creation date
                                "332"   ; topic notice
                                "333"   ; who set the topic
                                "353"   ; Names notice
                                ))

(use-package erc-hl-nicks
  :hook (erc-mode . erc-hl-nicks-mode)
  :after (erc)
  :straight t)

(use-package znc
  :straight t
  :commands (znc-erc)
  :init
  ;; (my-leader-def "ai" #'znc-erc)
  (my/persp-add-rule
    erc-mode 3 "ERC")
  :config
  (setq znc-servers
        `(("sqrtminusone.xyz" 6697 t
           ((libera "sqrtminusone"
                    ,(my/password-store-get "Selfhosted/ZNC")))))))

(defun my/erc-detach-all ()
  (interactive)
  (cl-loop for buf being the buffers
           if (eq (buffer-local-value 'major-mode buf) 'erc-mode)
           do (with-current-buffer buf
                (when (erc-server-process-alive)
                  (let ((tgt (erc-default-target)))
                    (erc-server-send (format "DETACH %s" tgt) nil tgt))))))

(provide 'sqrt-erc)
