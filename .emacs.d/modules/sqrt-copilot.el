;;; -*- lexical-binding: t -*-
(defun my/copilot-tab ()
  (interactive)
  (or (copilot-accept-completion)
      (when (my/should-run-emmet-p) (my/emmet-or-tab))
      (when (and (eq evil-state 'normal)
                 (or hs-minor-mode treesit-fold-mode outline-minor-mode))
        (evil-toggle-fold)
        t)
      (indent-for-tab-command)))

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el")
  :commands (copilot-mode)
  :disabled t
  :init
  (add-hook 'emacs-startup-hook
            (lambda ()
              (add-hook 'prog-mode-hook #'copilot-mode)))
  :config
  (push '(copilot) warning-suppress-types)
  (setq copilot-node-executable "/home/pavel/.guix-extra-profiles/dev/dev/bin/node")
  (general-define-key
   :keymaps 'company-active-map
   "<backtab>" #'my/copilot-tab)
  (general-define-key
   :keymaps 'copilot-mode-map
   "<tab>" #'my/copilot-tab
   "M-j" #'copilot-accept-completion-by-line
   "M-l" #'copilot-accept-completion-by-word))

(provide 'sqrt-copilot)
