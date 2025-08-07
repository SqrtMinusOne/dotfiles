;;; -*- lexical-binding: t -*-
(general-define-key
 ;; "C-c c" (my/command-in-persp "Emacs.org" "conf" 1 (find-file "~/Emacs.org"))
 "C-c c" `(,(lambda () (interactive) (find-file "~/Emacs.org")) :wk "Emacs.org"))

(my-leader-def
  :infix "c"
  "" '(:which-key "configuration")
  ;; "c" (my/command-in-persp "Emacs.org" "conf" 1 (find-file "~/Emacs.org"))
  "c" `(,(lambda () (interactive) (find-file "~/Emacs.org")) :wk "Emacs.org"))

(with-eval-after-load 'tramp
  (add-to-list 'tramp-methods
               `("yadm"
                 (tramp-login-program "yadm")
                 (tramp-login-args (("enter")))
                 (tramp-login-env (("SHELL") "/bin/sh"))
                 (tramp-remote-shell "/bin/sh")
                 (tramp-remote-shell-args ("-c")))))


(defun my/yadm-magit ()
  (interactive)
  (magit-status "/yadm::"))

(my-leader-def "cm" 'my/yadm-magit)

(defun my/open-yadm-file ()
  "Open a file managed by yadm"
  (interactive)
  (find-file
   (concat
    (file-name-as-directory (getenv "HOME"))
    (completing-read
     "yadm files: "
     (split-string
      (shell-command-to-string "yadm ls-files $HOME --full-name") "\n")))))

(general-define-key
 "C-c f" '(my/open-yadm-file :wk "yadm file"))

(my-leader-def
  "cf" '(my/open-yadm-file :wk "yadm file"))

(provide 'sqrt-dotfiles)
