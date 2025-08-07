;;; -*- lexical-binding: t -*-
(use-package guix
  :straight t
  :if (executable-find "guix")
  :commands (guix)
  :init
  (my-leader-def "ag" 'guix)
  (defun geiser-company--setup (&rest args)
    "A dummy function.")
  (defvar geiser-repl-company-p nil
    "A dummy variable."))

(provide 'sqrt-guix)
