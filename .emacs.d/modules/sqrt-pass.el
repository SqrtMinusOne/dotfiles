;;; -*- lexical-binding: t -*-
(use-package pass
  :straight t
  :commands (pass)
  :init
  (my-leader-def "ak" #'pass)
  :config
  (setq pass-show-keybindings nil))

(defun my/password-store-get (entry)
  (if-let ((res (password-store-get entry)))
      res
    (my/password-store-get entry)))

(defun my/password-store-get-field (entry field)
  (if-let (field (password-store-get-field entry field))
      field
    (my/password-store-get-field entry field)))

(provide 'sqrt-pass)
