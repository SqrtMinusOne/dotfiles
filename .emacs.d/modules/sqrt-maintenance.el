;;; -*- lexical-binding: t -*-
(use-package straight-overview
  :straight (:host github :repo "alberti42/straight-overview")
  :commands (straight-overview)
  :config
  (general-define-key
   :states '(normal)
   :keymaps '(straight-overview-mode-map)
   "m" #'straight-overview-mark
   "u" #'straight-overview-unmark
   "U" #'straight-overview-unmark-all
   "M" #'straight-overview-mark-outdated
   "x" #'straight-overview-execute
   "P" #'straight-overview-pin
   "F" #'straight-overview-unpin
   "R" #'straight-overview-restore
   "c" #'straight-overview-changelog
   "o" #'straight-overview-browse
   "RET" #'straight-overview-browse
   "a" #'straight-overview-toggle-show
   "gr" #'straight-overview-refresh
   "gR" #'straight-overview-fetch))

(provide 'sqrt-maintenance)
