;;; -*- lexical-binding: t -*-
(setq remote-file-name-inhibit-cache nil)
(setq tramp-use-connection-share nil)
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

(with-eval-after-load 'tramp
  (connection-local-set-profile-variables
   'my/tramp-ssh
   '((tramp-direct-async-process . t)))
  (connection-local-set-profiles
   '(:application tramp :protocol "ssh")
   'my/tramp-ssh))

(when my/remote-server
  (setq explicit-shell-file-name "/bin/bash"))

(setq tramp-fuse-unmount-on-cleanup t)

(defun my/auth-source-netrc-saver-local (fun &rest args)
  (let ((default-directory temporary-file-directory))
    (apply fun args)))

(with-eval-after-load 'auth-source
  (advice-add #'auth-source-netrc-saver :around
              #'my/auth-source-netrc-saver-local))

(setq tramp-verbose 0)

(defun my/tramp-p (&optional buffer)
  (file-remote-p
   (buffer-local-value 'default-directory (or buffer (current-buffer)))))

(defun my/tramp-void-if-tramp (fun &rest args)
  (unless (my/tramp-p)
    (apply fun args)))

(defun my/tramp-void-if-file-is-tramp (fun &optional dir)
  (unless (file-remote-p (or dir default-directory))
    (funcall fun dir)))

(defun my/editorconfig--advice-find-file-noselect-around (f f1 filename &rest args)
  (if (file-remote-p filename)
      (apply f1 filename args)
    (apply f f1 filename args)))

(with-eval-after-load 'editorconfig
  (advice-add #'editorconfig-apply :around #'my/tramp-void-if-tramp)
  (advice-add #'editorconfig--disabled-for-filename
              :around #'my/tramp-void-if-file-is-tramp)
  (advice-add #'editorconfig--advice-find-file-noselect :around
              #'my/editorconfig--advice-find-file-noselect-around))

(with-eval-after-load 'all-the-icons-dired
  (advice-add #'all-the-icons-dired-mode :around #'my/tramp-void-if-tramp))

(with-eval-after-load 'projectile
  (advice-add #'projectile-project-root :around #'my/tramp-void-if-file-is-tramp))

(with-eval-after-load 'lsp-mode
  (advice-add #'lsp :around #'my/tramp-void-if-tramp)
  (advice-add #'lsp-deferred :around #'my/tramp-void-if-tramp))

(with-eval-after-load 'git-gutter
  (advice-add #'git-gutter--turn-on :around #'my/tramp-void-if-tramp))

(with-eval-after-load 'dired-git-info
  (advice-add #'dired-git-info-mode :around #'my/tramp-void-if-tramp))

(with-eval-after-load 'pipenv
  (advice-add #'pipenv-mode :around #'my/tramp-void-if-tramp))

(with-eval-after-load 'wakatime-mode
  (advice-add #'wakatime-call :around #'my/tramp-void-if-tramp))

(with-eval-after-load 'activity-watch-mode
  (advice-add #'activity-watch--save :around #'my/tramp-void-if-tramp))

(defun my/shell-maybe-configure-for-tramp ()
  (when (my/tramp-p)
    (setq-local company-idle-delay nil)))

(add-hook 'find-file-hook #'my/shell-maybe-configure-for-tramp)
(add-hook 'eshell-mode-hook #'my/shell-maybe-configure-for-tramp)
(add-hook 'shell-mode-hook #'my/shell-maybe-configure-for-tramp)

(defun my/tramp-cleanup-here ()
  "Kill buffers for the current TRAMP connection and clean it."
  (interactive)
  (unless (tramp-tramp-file-p default-directory)
    (user-error "Not in a TRAMP buffer"))
  (let* ((vec (tramp-dissect-file-name default-directory 'noexpand))
         (buffers
          (seq-filter
           (lambda (buffer)
             (with-current-buffer buffer
               (and
                (tramp-file-name-equal-p
                 vec (tramp-dissect-file-name default-directory 'noexpand))
                (run-hook-with-args-until-success
                 'tramp-cleanup-some-buffers-hook))))
           (tramp-list-remote-buffers)))
         (modified
          (seq-filter
           (lambda (buffer)
             (with-current-buffer buffer
               (and (buffer-modified-p)
                    (or buffer-file-name
                        (derived-mode-p 'dired-mode)))))
           buffers)))
    (when modified
      (user-error "Modified buffers: %s"
                  (mapconcat #'buffer-name modified ", ")))
    (tramp-cleanup-connection vec)
    (mapc (lambda (buffer)
            (when (buffer-live-p buffer)
              (kill-buffer buffer)))
          buffers)))

(defun my/tramp-cleanup-here-maybe ()
  (interactive)
  (if (tramp-tramp-file-p default-directory)
      (when (y-or-n-p "Cleanup this connection? ")
        (my/tramp-cleanup-here))
    (when (y-or-n-p "Close this Dired buffer?")
      (quit-window t))))

(with-eval-after-load 'dired
  (general-define-key
   :keymaps '(dired-mode-map)
   :states '(normal)
   "Q" #'my/tramp-cleanup-here-maybe))

(defun my/dired-ssh-copy (sources target-folder)
  (interactive
   (list (dired-get-marked-files)
         (with-current-buffer
             (my/get-good-buffer 'dired-mode "Dired buffer: ")
           (dired-current-directory))))

  (unless sources
    (user-error "No source files selected"))

  (let* ((source-remote-prefix (file-remote-p (car sources)))
         (sources-remote-p (not (null source-remote-prefix)))
         (target-remote-p
          (not (null (file-remote-p target-folder)))))

    (when (eq sources-remote-p target-remote-p)
      (user-error "Sources and target must be on opposite sides of SSH"))

    ;; All sources must be local, or use the same remote connection.
    (dolist (source sources)
      (unless (equal (file-remote-p source) source-remote-prefix)
        (user-error "All source files must use the same local or remote connection")))

    ;; Reject remote methods other than ordinary /ssh: paths.
    (dolist (remote-file
             (if sources-remote-p sources (list target-folder)))
      (unless (string-prefix-p "/ssh:" remote-file)
        (user-error "%s doesn't look like an SSH location" remote-file)))

    ;; Check this before converting the TRAMP names.
    (let* ((recursive-p (seq-some #'file-directory-p sources))
           (local-directory
            (if sources-remote-p
                (file-name-as-directory
                 (expand-file-name target-folder))
              (file-name-directory
               (expand-file-name (car sources)))))
           (scp-sources
            (mapcar
             (lambda (source) (string-remove-prefix "/ssh:" source))
             sources))
           (scp-target (string-remove-prefix "/ssh:" target-folder))
           (buf (get-buffer-create "*dired-copy*")))

      (when-let ((process (get-buffer-process buf)))
        (when (process-live-p process)
          (user-error "A Dired copy process is already running")))

      (with-current-buffer buf
        (comint-mode)

        ;; Ensure `start-file-process' starts scp locally.
        (setq default-directory local-directory)

        (let ((inhibit-read-only t))
          (erase-buffer))

        (setq-local comint-inhibit-carriage-motion nil
                    ansi-color-for-comint-mode t)
        (add-hook 'comint-output-filter-functions
                  #'ansi-color-process-output nil t))

      (let ((process-connection-type t))
        (comint-exec
         buf "dired-copy" "scp" nil
         (append
          (when recursive-p '("-r"))
          '("--")
          scp-sources
          (list scp-target))))

      (display-buffer buf))))

(with-eval-after-load 'dired
  (general-define-key
   :states '(normal)
   :keymaps '(dired-mode-map)
   "gD" #'my/dired-ssh-copy))

(defun my/open-as-sudo-dwim ()
  (interactive)
  (let ((file
         (if (derived-mode-p 'dired-mode)
             (or (car (dired-get-marked-files nil nil nil t))
                 default-directory)
           buffer-file-name)))
    (unless file
      (user-error "No file"))
    (find-file
     (if-let ((remote (file-remote-p file)))
         (concat
          (substring remote 0 -1)
          "|sudo:root@"
          (file-remote-p file 'host)
          ":"
          (file-remote-p file 'localname))
       (concat "/sudo::" file)))))

(provide 'sqrt-tramp)
