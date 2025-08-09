;;; -*- lexical-binding: t -*-
(let ((mail-file (expand-file-name "mail.el" user-emacs-directory)))
  (if (file-exists-p mail-file)
      (load-file mail-file)
    (message "Can't load mail.el")))

(provide 'sqrt-mail)
