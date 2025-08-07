;;; -*- lexical-binding: t -*-
(use-package lispy
  :commands (lispy-mode)
  :straight t)

(use-package lispyville
  :hook (lispy-mode . lispyville-mode)
  :straight t)

(sp-with-modes sp-lisp-modes
  (sp-local-pair "'" nil :actions nil))

(use-package flycheck-package
  :straight t
  :defer t
  :init
  (defun my/flycheck-package-setup ()
    (require 'flycheck-package)
    (flycheck-package-setup)
    (remove-hook 'emacs-lisp-mode-hook #'my/flycheck-package-setup))
  (add-hook 'emacs-lisp-mode-hook #'my/flycheck-package-setup))

(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
;; (add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)
(add-hook 'emacs-lisp-mode-hook #'lispy-mode)

(defun advice-unadvice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

(add-hook 'inferior-emacs-lisp-mode-hook #'smartparens-mode)
(my-leader-def "bi" #'ielm)

(use-package slime
  :straight t
  :commands (slime)
  :config
  (setq inferior-lisp-program "sbcl")
  (add-hook 'slime-repl-mode 'smartparens-mode))

(add-hook 'lisp-mode-hook #'aggressive-indent-mode)
;; (add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)
(add-hook 'lisp-mode-hook #'lispy-mode)

(use-package clojure-mode
  :straight t
  :mode "\\.clj[sc]?\\'"
  :config
  ;; (add-hook 'clojure-mode-hook #'smartparens-strict-mode)
  (add-hook 'clojure-mode-hook #'lispy-mode)
  (add-hook 'clojure-mode-hook #'aggressive-indent-mode))

(use-package cider
  :after clojure-mode
  :straight t)

(use-package hy-mode
  :straight t
  :mode "\\.hy\\'"
  :config
  (add-hook 'hy-mode-hook #'lispy-mode)
  (add-hook 'hy-mode-hook #'aggressive-indent-mode))

(use-package geiser
  :straight t
  :commands (geiser run-geiser)
  :config
  (setq geiser-default-implementation 'guile))

(use-package geiser-guile
  :straight t
  :after geiser)

(add-hook 'scheme-mode-hook #'aggressive-indent-mode)
(add-hook 'scheme-mode-hook #'lispy-mode)

(use-package clips-mode
  :straight t
  :mode "\\.cl\\'"
  :disabled t
  :config
  (add-hook 'clips-mode 'lispy-mode))

(provide 'sqrt-lisp)
