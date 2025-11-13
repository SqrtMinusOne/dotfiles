;;; -*- lexical-binding: t -*-
(use-package dired
  :ensure nil
  :custom ((dired-listing-switches "-alh --group-directories-first"))
  :commands (dired)
  :config
  (setq dired-dwim-target t)
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-kill-when-opening-new-dired-buffer t)
  (add-hook 'dired-mode-hook
            (lambda ()
              (setq truncate-lines t)
              (visual-line-mode nil)))

  (when my/is-termux
    (add-hook 'dired-mode-hook #'dired-hide-details-mode))
  (general-define-key
   :states '(normal)
   :keymaps 'dired-mode-map
   "h" #'dired-up-directory
   "l" #'dired-find-file
   "=" #'dired-narrow
   "-" #'my/dired-create-empty-file-subtree
   "~" #'eshell
   "M-r" #'wdired-change-to-wdired-mode
   "<left>" #'dired-up-directory
   "<right>" #'dired-find-file
   "M-<return>" #'my/dired-open-xdg))

(defun my/dired-home ()
  "Open dired at $HOME"
  (interactive)
  (dired (expand-file-name "~")))

(my-leader-def
  "ad" #'dired
  "aD" #'my/dired-bookmark-open)

(use-package diredfl
  :straight t
  :after (dired)
  :config
  (diredfl-global-mode 1))

(use-package dired-subtree
  :after (dired)
  :straight t)

(defun my/dired-create-empty-file-subtree ()
  (interactive)
  (let ((default-directory (dired-current-directory)))
    (dired-create-empty-file
     (read-file-name "Create empty file: "))))

(defun my/dired-sidebar-toggle ()
  (interactive)
  (if (not current-prefix-arg)
      (dired-sidebar-toggle-sidebar)
    (let ((dired-sidebar-follow-file-at-point-on-toggle-open
           current-prefix-arg)
          (current-prefix-arg nil))
      (dired-sidebar-toggle-sidebar))))

(use-package dired-sidebar
  :straight t
  :after (dired)
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (setq dired-sidebar-follow-file-at-point-on-toggle-open nil)
  (general-define-key
   :keymaps '(normal override global)
   "C-n" `(my/dired-sidebar-toggle
           :wk "dired-sidebar"))
  :config
  (setq dired-sidebar-width 45)
  (defun my/dired-sidebar-setup ()
    (toggle-truncate-lines 1)
    (display-line-numbers-mode -1)
    (setq-local dired-subtree-use-backgrounds nil)
    (setq-local window-size-fixed nil))
  (general-define-key
   :keymaps 'dired-sidebar-mode-map
   :states '(normal emacs)
   "l" #'dired-sidebar-find-file
   "h" #'dired-sidebar-up-directory
   "=" #'dired-narrow)
  (add-hook 'dired-sidebar-mode-hook #'my/dired-sidebar-setup)
  (advice-add #'dired-create-empty-file :after 'dired-sidebar-refresh-buffer))

(use-package dired-recent
  :straight t
  :after dired
  :config
  (dired-recent-mode)
  (general-define-key
   :keymaps 'dired-recent-mode-map
   "C-x C-d" nil))

(use-package all-the-icons-dired
  :straight t
  :disabled t
  :after (dired)
  :if (display-graphic-p)
  :hook (dired-mode . (lambda ()
                        (unless (string-match-p "/gnu/store" default-directory)
                          (all-the-icons-dired-mode)))))

(use-package nerd-icons-dired
  :straight t
  :after (dired)
  :hook (dired-mode . (lambda ()
                        (unless (or (file-remote-p default-directory)
                                    (string-match-p "/gnu/store" default-directory))
                          (nerd-icons-dired-mode))))
  :config
  ;; (advice-add #'dired-create-empty-file :around #'nerd-icons-dired--refresh-advice)
  )

(use-package dired-open
  :straight t
  :after (dired)
  :commands (dired-open-xdg))

(use-package dired-du
  :straight t
  :commands (dired-du-mode)
  :config
  (setq dired-du-size-format t))

(use-package dired-narrow
  :straight t
  :commands (dired-narrow)
  :config
  (general-define-key
   :keymaps 'dired-narrow-map
   [escape] 'keyboard-quit))

(use-package dired-git-info
  :straight t
  :after dired
  :config
  (general-define-key
   :keymap 'dired-mode-map
   :states '(normal emacs)
   ")" 'dired-git-info-mode))

(use-package avy-dired
  :straight (:host github :repo "SqrtMinusOne/avy-dired")
  :after (dired)
  :init
  (my-leader-def "aa" #'avy-dired-goto-line))

(defun my/dired-rsync--refresh ()
  (cl-loop for window being the windows
           do (with-current-buffer (window-buffer window)
                (when (derived-mode-p 'dired-mode)
                  (revert-buffer)))))

(use-package dired-rsync
  :straight t
  :after (dired)
  :config
  (add-to-list 'global-mode-string '(:eval dired-rsync-modeline-status))
  (add-hook 'dired-rsync-success-hook #'my/dired-rsync--refresh)
  (general-define-key
   :states '(normal)
   :keymaps '(dired-mode-map)
   "C" #'dired-rsync
   "gC" #'dired-rsync-transient
   "gd" #'dired-do-copy))

(use-package dired-rsync-transient
  :straight t
  :after (dired))

(defun my/dired-open-this-subdir ()
  (interactive)
  (dired (dired-current-directory)))

(defun my/dired-kill-all-subdirs ()
  (interactive)
  (let ((dir dired-directory))
    (kill-buffer (current-buffer))
    (dired dir)))

(with-eval-after-load 'dired
  (general-define-key
   :states '(normal)
   :keymaps 'dired-mode-map
   "s" nil
   "ss" 'dired-maybe-insert-subdir
   "sl" 'dired-maybe-insert-subdir
   "sq" 'dired-kill-subdir
   "sk" 'dired-prev-subdir
   "sj" 'dired-next-subdir
   "sS" 'my/dired-open-this-subdir
   "sQ" 'my/dired-kill-all-subdirs
   (kbd "TAB") 'dired-hide-subdir))

(defun my/dired-goto-project-root ()
  (interactive)
  (dired--find-possibly-alternative-file (projectile-project-root)))

(with-eval-after-load 'dired
  (general-define-key
   :states '(normal)
   :keymaps 'dired-mode-map
   "H" #'my/dired-goto-project-root))

(defun my/dired-open-xdg ()
  "Try to run `xdg-open' to open the file under point."
  (interactive)
  (when (executable-find "xdg-open")
    (let ((file (ignore-errors (dired-get-file-for-visit))))
      (start-process "dired-open" nil
                     "xdg-open" (file-truename file)))))

(defun my/dired-do-async-shell-command (command &optional arg file-list)
  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg nil nil t)))
     (list
      ;; Want to give feedback whether this file or marked files are used:
      (dired-read-shell-command "& on %s: " current-prefix-arg files)
      current-prefix-arg
      files)))
  (start-process-shell-command
   "*Dired Command*" nil
   (dired-shell-stuff-it command file-list arg)))

(with-eval-after-load 'dired
  (general-define-key
   :states '(normal insert)
   :keymaps '(dired-mode-map)
   "&" #'my/dired-do-async-shell-command))

(defun my/dired-bookmark-open ()
  (interactive)
  (let ((bookmarks
         (mapcar
          (lambda (el) (cons (format "%-30s %s" (car el) (cdr el)) (cdr el)))
          my/dired-bookmarks)))
    (dired
     (cdr
      (assoc
       (completing-read "Dired: " bookmarks nil nil "^")
       bookmarks)))))

(defun my/get-good-buffer (buffer-major-mode prompt)
  (or
   (cl-loop
    for buf being the buffers
    if (eq (buffer-local-value 'major-mode buf) buffer-major-mode)
    collect buf into all-buffers
    if (and (eq (buffer-local-value 'major-mode buf) buffer-major-mode)
            (get-buffer-window buf t))
    collect buf into visible-buffers
    finally return (if (= (length visible-buffers) 1)
                       (car visible-buffers)
                     (if (= (length all-buffers) 1)
                         (car all-buffers)
                       (when-let ((buffers-by-name (mapcar (lambda (b)
                                                             (cons (buffer-name b) b))
                                                           all-buffers)))
                         (cdr
                          (assoc
                           (completing-read prompt buffers-by-name nil t)
                           buffers-by-name))))))
   (user-error "No buffer found!")))

(defun my/dired-attach-to-telega (files telega-buffer)
  (interactive
   (list (dired-get-marked-files nil nil #'dired-nondirectory-p)
         (my/get-good-buffer 'telega-chat-mode "Telega buffer: ")))
  (unless files
    (user-error "No (non-directory) files selected"))
  (with-current-buffer telega-buffer
    (dolist (file files)
      (telega-chatbuf-attach-file file))))

(defun my/telega-save-to-dired (msg arg)
  (interactive
   (list (telega-msg-for-interactive)
         (prefix-numeric-value current-prefix-arg)))
  (if (eq arg 4)
      (progn
        (setq telega-msg-save-dir
              (with-current-buffer (my/get-good-buffer 'dired-mode "Dired buffer: ")
                (dired-current-directory)))
        (telega-msg-save msg))
    (setq default-directory (expand-file-name "~"))
    (setq telega-msg-save-dir nil)
    (telega-msg-save msg)))

(defun my/dired-attach-to-notmuch (files notmuch-buffer)
  (interactive
   (list (dired-get-marked-files nil nil #'dired-nondirectory-p)
         (my/get-good-buffer 'notmuch-message-mode "Notmuch message buffer: ")))
  (unless files
    (user-error "No (non-directory) files selected"))
  (with-current-buffer notmuch-buffer
    (goto-char (point-max))
    (dolist (file files)
      (let ((type
             (or (mm-default-file-type file)
                 "application/octet-stream")))
        (mml-attach-file
         file
         type
         (mml-minibuffer-read-description)
         (mml-minibuffer-read-disposition type nil file))))))

(defun my/notmuch-save-to-dired (arg)
  (interactive
   (list (prefix-numeric-value current-prefix-arg)))
  (if (eq arg 4)
      (let ((default-directory
             (with-current-buffer (my/get-good-buffer 'dired-mode "Dired buffer: ")
               (dired-current-directory))))
        (notmuch-show-save-part))
    (notmuch-show-save-part)))

(defun my/dired-attach-to-ement (files ement-buffer)
  (interactive
   (list (dired-get-marked-files nil nil #'dired-nondirectory-p)
         (my/get-good-buffer 'ement-room-mode "Ement room buffer: ")))
  (unless files
    (user-error "No (non-directory) files selected"))
  (with-current-buffer ement-buffer
    (ement-with-room-and-session
      (dolist (file files)
        (ement-room-send-file
         file
         (read-from-minibuffer (format "Message body for %s: " file))
         ement-room
         ement-session)))))

(defun my/dired-attach-to-mastodon (files mastodon-buffer)
  (interactive
   (list (dired-get-marked-files nil nil #'dired-nondirectory-p)
         (or (cl-loop for buf being the buffers
                      if (eq (buffer-local-value 'mastodon-toot-mode buf) t)
                      return buf)
             (user-error "No buffer found!"))))
  (unless files
    (user-error "No (non-directory) files selected"))
  (with-current-buffer mastodon-buffer
    (dolist (file files)
      (mastodon-toot-attach-media
       file
       (read-from-minibuffer (format "Description for %s: " file))))))

(defun my/dired-attach-to-gptel (files)
  (interactive
   (list (dired-get-marked-files nil nil #'dired-nondirectory-p)))
  (unless files
    (user-error "No (non-directory) files selected"))
  (dolist (file files)
    (gptel-context-add-file file)))

(with-eval-after-load 'dired
  (general-define-key
   :states '(normal)
   :keymaps 'dired-mode-map
   "a" nil
   "at" #'my/dired-attach-to-telega
   "am" #'my/dired-attach-to-notmuch
   "ae" #'my/dired-attach-to-ement
   "ai" #'my/dired-attach-to-gptel
   "an" #'my/dired-attach-to-mastodon))

(with-eval-after-load 'telega
  (general-define-key
   :keymaps 'telega-msg-button-map
   "S" #'my/telega-save-to-dired))

(with-eval-after-load 'notmuch
  (general-define-key
   :keymaps 'notmuch-show-mode-map
   :states 'normal
   ". s" #'my/notmuch-save-to-dired))

(provide 'sqrt-dired)
