;;; -*- lexical-binding: t -*-
(when (and my/is-termux (not (equal (my/system-name) "snow")))
  (define-key key-translation-map (kbd "`") (kbd "<escape>"))
  (define-key key-translation-map (kbd "<escape>") (kbd "`")))

(when my/is-termux
  (setq split-width-threshold 90))

(provide 'sqrt-termux)
