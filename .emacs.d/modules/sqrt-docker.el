;;; -*- lexical-binding: t -*-
(use-package docker
  :straight t
  :commands (docker)
  :init
  (my-leader-def "ao" 'docker))

(provide 'sqrt-docker)
