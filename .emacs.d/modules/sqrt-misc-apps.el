;;; -*- lexical-binding: t -*-
(my-leader-def "ah" 'proced)
(setq proced-auto-update-interval 1)
(add-hook 'proced-mode-hook (lambda ()
                              (visual-line-mode -1)
                              (setq-local truncate-lines t)
                              (proced-toggle-auto-update 1)))

(use-package casual
  :straight (:host github :repo "kickingvegas/Casual")
  :after calc
  :config
  (general-define-key
   :states '(normal)
   :keymaps 'calc-mode-map
   "M-o" #'casual-main-menu))

(use-package snow
  :straight (:repo "alphapapa/snow.el" :host github)
  :commands (snow))

(use-package redacted
  :commands (redacted-mode)
  :straight (:host github :repo "bkaestner/redacted.el"))

(use-package zone
  :ensure nil
  :commands (zone)
  :config
  (setq original-zone-programs (copy-sequence zone-programs)))

(defun my/zone-with-select ()
  (interactive)
  (let ((zone-programs
         (vector
          (intern
           (completing-read
            "Zone programs"
            (cl-mapcar 'symbol-name original-zone-programs))))))
    (zone)))

(use-package imgur
  :straight (:host github :repo "larsmagne/imgur.el")
  :defer t)

(use-package meme
  :straight (:host github :repo "larsmagne/meme" :files (:defaults "images"))
  :commands (meme))

(use-package ed-mode
  :straight (:host github :repo "ryanprior/ed-mode")
  :commands (ed))

(provide 'sqrt-misc-apps)
