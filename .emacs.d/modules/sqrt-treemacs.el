;;; -*- lexical-binding: t -*-
(use-package treemacs
  :straight t
  :defer t
  :config
  ;; (setq treemacs-follow-mode nil)
  ;; (setq treemacs-follow-after-init nil)
  (setq treemacs-space-between-root-nodes nil)
  ;; (treemacs-git-mode 'extended)
  ;; (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?)
  (general-define-key
   :keymaps 'treemacs-mode-map
   [mouse-1] #'treemacs-single-click-expand-action
   "M-l" #'treemacs-root-down
   "M-h" #'treemacs-root-up
   "q" #'treemacs-quit)
  (general-define-key
   :keymaps 'treemacs-mode-map
   :states '(normal emacs)
   "q" 'treemacs-quit))

(use-package treemacs-evil
  :after (treemacs evil)
  :straight t)

(provide 'sqrt-treemacs)
