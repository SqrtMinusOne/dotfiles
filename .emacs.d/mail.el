(setq my/notmuch-loaded nil)

(defun my/run-notmuch ()
  (interactive)
  (when (not my/notmuch-loaded)
    (let* ((notmuch-dir (shell-command-to-string "readlink -f $(which notmuch)"))
           (notmuch-version (substring (shell-command-to-string "notmuch --version") 8 -1))
           (notmuch-lisp-dir (concat
                              (substring notmuch-dir 0 -13)
                              "/share/emacs/site-lisp/notmuch-"
                              notmuch-version
                              "/")))
      (push notmuch-lisp-dir load-path))
    (setq my/notmuch-loaded t))
  (notmuch))

(my-leader-def "am" 'my/run-notmuch)

(use-package notmuch
  ;; :ensure nil
  :commands (notmuch)
  :config
  (setq mail-specify-envelope-from t)
  (setq message-sendmail-envelope-from 'header)
  (setq mail-envelope-from 'header)
  (setq notmuch-always-prompt-for-sender t)
  (setq sendmail-program (executable-find "msmtp"))
  (setq send-mail-function #'sendmail-send-it)
  (add-hook 'notmuch-hello-mode-hook
            (lambda () (display-line-numbers-mode 0)))
  (custom-set-faces
   `(notmuch-wash-cited-text ((t (:foreground ,(doom-color 'yellow)))))))
