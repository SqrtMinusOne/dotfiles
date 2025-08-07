;;; -*- lexical-binding: t -*-
(defun my/elcord-mask-buffer-name (name)
  (cond
   ((string-match-p (rx bos (? "CAPTURE-") (= 14 num) "-" (* not-newline) ".org" eos) name)
    "<ORG-ROAM>")
   ((string-match-p (rx "german" (* not-newline) ".org" eos) name)
    "<LEARNING>")
   ((string-match-p (rx bos (+ num) "-" (+ num) "-" (+ num) ".org" eos) name)
    "<ORG-JOURNAL>")
   ((string-match-p (rx bos "EXWM") name)
    "<EXWM>")
   ((string-match-p (rx bos "*Org-Habit") name)
    "<ORG>")
   ((with-current-buffer (get-buffer name)
      (derived-mode-p 'telega-root-mode 'telega-chat-mode))
    "<TELEGA>")
   (t name)))

(defun my/elcord-buffer-details-format-functions ()
  (format "Editing %s" (my/elcord-mask-buffer-name (buffer-name))))

(defun my/elcord-update-presence-mask-advice (r)
  (list (my/elcord-mask-buffer-name (nth 0 r)) (nth 1 r)))

(defun my/elcord-symlink ()
  (shell-command-to-string "bash -c 'ln -sf {app/com.discordapp.Discord,$XDG_RUNTIME_DIR}/discord-ipc-0 &'"))

(use-package elcord
  :straight t
  :if (and (or
            (string= (system-name) "violet")
            (string= (system-name) "eminence")
            (string= (system-name) "iris"))
           (not my/remote-server)
           (not my/nested-emacs))
  :config
  (setq elcord-buffer-details-format-function #'my/elcord-buffer-details-format-functions)
  (advice-add 'elcord--try-update-presence :filter-args #'my/elcord-update-presence-mask-advice)
  (add-to-list 'elcord-mode-text-alist '(telega-chat-mode . "Telega Chat"))
  (add-to-list 'elcord-mode-text-alist '(telega-root-mode . "Telega Root"))
  ;; (elcord-mode)
  (my/elcord-symlink))

(provide 'sqrt-discord)
