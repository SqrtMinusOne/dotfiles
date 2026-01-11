;;; -*- lexical-binding: t -*-
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

(setq auth-source-debug nil)
(setq auth-sources '("~/.authinfo.gpg"))

(let ((private-file "/home/pavel/10-19 Code/11 Config/11.10 dotfiles-private/private.el"))
  (when (file-exists-p private-file)
    (load-file private-file)))

(use-package no-littering
  :straight t)

(defun my/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun my/quit-window-and-buffer ()
  (interactive)
  (quit-window t))

(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message "Hallo Leben")

(provide 'sqrt-misc-initial)
