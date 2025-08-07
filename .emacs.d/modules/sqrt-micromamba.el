;;; -*- lexical-binding: t -*-
(use-package micromamba
  :straight t
  :if (executable-find "micromamba")
  :config
  (ignore-errors
    (micromamba-activate "general")))

(provide 'sqrt-micromamba)
