;;; -*- lexical-binding: t -*-
(use-package windsurf-copilot
  :commands (windsurf-copilot-mode)
  :straight (:local-repo
             "/home/pavel/10-19 Code/12 My Emacs Packages/12.18 windsurf-copilot")
  :init
  (add-hook 'typescript-mode-hook #'windsurf-copilot-mode)
  (add-hook 'web-mode-hook #'windsurf-copilot-mode)
  (add-hook 'python-mode-hook #'windsurf-copilot-mode)
  :config
  (add-hook 'windsurf-copilot-mode-hook #'windsurf-copilot-modeline-mode)
  (general-define-key
   :keymaps '(windsurf-copilot-completion-map)
   "M-l" #'windsurf-copilot-accept-completion-by-word
   "M-j" #'windsurf-copilot-accept-completion-by-line))

(provide 'sqrt-copilot)
