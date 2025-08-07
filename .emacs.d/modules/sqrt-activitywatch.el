;;; -*- lexical-binding: t -*-
(use-package request
  :straight t
  :defer t)

(use-package activity-watch-mode
  :straight t
  :config
  (global-activity-watch-mode))

(provide 'sqrt-activitywatch)
