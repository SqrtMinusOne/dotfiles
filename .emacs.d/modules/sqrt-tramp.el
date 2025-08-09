;;; -*- lexical-binding: t -*-
(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

(with-eval-after-load 'tramp
  (setq tramp-remote-path
        (append tramp-remote-path
                '(tramp-own-remote-path))))

(when my/remote-server
  (setq explicit-shell-file-name "/bin/bash"))

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

(defun my/shell-maybe-configure-for-tramp ()
  (when (my/tramp-p)
    (setq company-idle-delay nil)))

(add-hook 'eshell-mode-hook #'my/shell-maybe-configure-for-tramp)
(add-hook 'shell-mode-hook #'my/shell-maybe-configure-for-tramp)

(provide 'sqrt-tramp)
