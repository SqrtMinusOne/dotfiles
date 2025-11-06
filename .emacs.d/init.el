;;; -*- lexical-binding: t -*-

(setq straight-use-version-specific-build-dir t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
      (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(eval-when-compile (require 'use-package))

(defun my/get-env ()
  (or (getenv "EMACS_ENV")
      (when (member (system-name) '("dev-digital" "viridian"))
        "remote")
      (when (string-match-p (rx (* nonl) "com.termux" (* nonl)) (getenv "HOME"))
        "termux")
      "normal"))

(setq my/env (my/get-env))

(setq my/nested-emacs (and (getenv "IS_EMACS") t))
(setenv "IS_EMACS" "true")

(defun my/system-name ()
  (or (getenv "ANDROID_NAME")
      (system-name)))

(setq my/is-termux (equal (my/get-env) "termux"))
(setq my/remote-server (equal (my/get-env) "remote"))

(setq my/modules-dir (file-name-as-directory
                      (expand-file-name "modules" user-emacs-directory)))

(setq my/modules-prefix "sqrt-")

(defun my/modules--refresh-and-list ()
  (let (modules-list
        headlines)
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (elem) (push elem headlines)))
    (mapc
     (lambda (elem)
       (when-let ((module-name (org-element-property :MODULE_NAME elem)))
         (save-excursion
           (goto-char (org-element-property :begin elem))
           (org-set-property
            "header-args:emacs-lisp"
            (concat ":tangle " my/modules-dir my/modules-prefix module-name ".el"
                    " :comments links"))
           (push module-name modules-list))))
     headlines)
    (seq-uniq modules-list)))

(defun my/modules--read-table ()
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (rx bol "#+NAME: modules-settings"))
    (forward-line)
    (let* ((table (org-element-at-point))
           (table-contents
            (mapcar
             (lambda (elem)
               (if (listp elem)
                   (mapcar #'substring-no-properties elem)
                 elem))
             (org-table-to-lisp)))
           (envs-list (cdar table-contents)))
      (mapcar
       (lambda (elem)
         (cons (car elem)
               (cl-loop for sign in (cdr elem)
                        for env in envs-list
                        collect (cons env (not (string-empty-p sign))))))
       (cddr table-contents)))))

(defun my/modules--refresh-table (modules-list)
  (save-excursion
    (goto-char (point-min))
    (search-forward "#+NAME: modules-settings")
    (let* ((table (org-element-at-point))
           (table-contents
            (my/modules--read-table))
           (envs-list (mapcar #'car (cdar table-contents))))
      (delete-region (org-element-property :contents-begin table)
                     (org-element-property :contents-end table))
      (insert
       "\n"
       (string-trim
        (with-temp-buffer
          (insert "| Module name |"
                  (mapconcat #'identity envs-list " | ")
                  "\n")
          (insert "|--|\n")
          (dolist (module modules-list)
            (insert "| " module " | "
                    (cl-loop
                     for env in envs-list
                     if (alist-get
                         env
                         (alist-get module table-contents nil nil #'equal)
                         nil nil #'equal)
                     concat " + |"
                     else
                     concat " |")
                     "\n"))
          (goto-char (point-min))
          (org-table-align)
          (buffer-substring-no-properties (point-min) (point-max))))))))

(defun my/modules--refresh-loading ()
  (let* ((modules-table (my/modules--read-table))
         (envs-list (mapcar #'car (cdar modules-table))))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward (rx bol "#+NAME: modules-loading"))
      (beginning-of-line 3)
      (delete-region
       (point)
       (save-excursion
         (re-search-forward (rx bol "#+end_src"))
         (end-of-line 0)
         (point)))
      (insert
       (pp-to-string
        `(pcase my/env
           ,@(mapcar
              (lambda (env)
                `(,env
                  ,@(seq-filter
                     #'identity
                     (mapcar
                      (lambda (elem)
                        (when (alist-get env (cdr elem) nil nil #'equal)
                          `(require ',(intern (concat my/modules-prefix (car elem))))))
                      modules-table))))
              envs-list)))))))

(defun my/modules--cleanup-folder (modules-list)
  (let ((source-files (seq-filter
                       (lambda (f)
                         (equal (file-name-extension f) "el"))
                       (directory-files my/modules-dir)))
        (target-files
         (mapcar (lambda (m)
                   (concat my/modules-prefix m ".el"))
                 modules-list)))
    (dolist (extra-file (seq-difference source-files target-files))
      (delete-file (concat my/modules-dir extra-file)))))

(defun my/modules-settings-refresh ()
  (interactive)
  (org-fold-show-all)
  (let ((modules-list (my/modules--refresh-and-list)))
    (my/modules--refresh-table modules-list)
    (my/modules--refresh-loading)
    (my/modules--cleanup-folder modules-list)
    (setq my/modules-list modules-list)))

(push my/modules-dir load-path)

(pcase my/env
  ("normal"
   (require 'sqrt-performance)
   (require 'sqrt-micromamba)
   (require 'sqrt-misc-initial)
   (require 'sqrt-keybindings)
   (require 'sqrt-i3)
   (require 'sqrt-general-config)
   (require 'sqrt-wakatime)
   (require 'sqrt-activitywatch)
   (require 'sqrt-general-ui)
   (require 'sqrt-doom-modeline)
   (require 'sqrt-perspective)
   (require 'sqrt-treemacs)
   (require 'sqrt-lsp)
   (require 'sqrt-flycheck)
   (require 'sqrt-tree-sitter)
   (require 'sqrt-reformatter)
   (require 'sqrt-web)
   (require 'sqrt-latex)
   (require 'sqrt-markup)
   (require 'sqrt-lisp)
   (require 'sqrt-misc-programming)
   (require 'sqrt-general-org)
   (require 'sqrt-org-literate)
   (require 'sqrt-org-productivity)
   (require 'sqrt-org-export)
   (require 'sqrt-dired)
   (require 'sqrt-tramp)
   (require 'sqrt-terms)
   (require 'sqrt-dotfiles)
   (require 'sqrt-elfeed)
   (require 'sqrt-mail)
   (require 'sqrt-gnus)
   (require 'sqrt-emms)
   (require 'sqrt-misc-internet)
   (require 'sqrt-erc)
   (require 'sqrt-mastodon)
   (require 'sqrt-ement)
   (require 'sqrt-telega)
   (require 'sqrt-docs)
   (require 'sqrt-ai)
   (require 'sqrt-index)
   (require 'sqrt-pass)
   (require 'sqrt-docker)
   (require 'sqrt-misc-apps)
   (require 'sqrt-guix)
   (require 'sqrt-productivity-apps)
   (require 'sqrt-chess)
   (require 'sqrt-gource))
  ("termux"
   (require 'sqrt-performance)
   (require 'sqrt-misc-initial)
   (require 'sqrt-keybindings)
   (require 'sqrt-termux)
   (require 'sqrt-general-config)
   (require 'sqrt-wakatime)
   (require 'sqrt-general-ui)
   (require 'sqrt-doom-modeline)
   (require 'sqrt-perspective)
   (require 'sqrt-treemacs)
   (require 'sqrt-flycheck)
   (require 'sqrt-tree-sitter)
   (require 'sqrt-latex)
   (require 'sqrt-markup)
   (require 'sqrt-lisp)
   (require 'sqrt-general-org)
   (require 'sqrt-org-productivity)
   (require 'sqrt-dired)
   (require 'sqrt-terms)
   (require 'sqrt-dotfiles)
   (require 'sqrt-misc-internet)
   (require 'sqrt-telega)
   (require 'sqrt-docs)
   (require 'sqrt-index)
   (require 'sqrt-pass)
   (require 'sqrt-misc-apps)
   (require 'sqrt-emacs-pinentry)
   (require 'sqrt-productivity-apps))
  ("remote"
   (require 'sqrt-performance)
   (require 'sqrt-misc-initial)
   (require 'sqrt-keybindings)
   (require 'sqrt-general-config)
   (require 'sqrt-general-ui)
   (require 'sqrt-doom-modeline)
   (require 'sqrt-perspective)
   (require 'sqrt-flycheck)
   (require 'sqrt-tree-sitter)
   (require 'sqrt-markup)
   (require 'sqrt-lisp)
   (require 'sqrt-dired)
   (require 'sqrt-terms)
   (require 'sqrt-dotfiles)
   (require 'sqrt-misc-internet)))

(defun my/modules--post-tangle ()
  (when (string-match-p (rx bos (literal my/modules-dir)
                            (* nonl) ".el")
                        (buffer-file-name))
    (goto-char (point-min))
    (insert ";;; -*- lexical-binding: t -*-\n")
    (goto-char (point-max))
    (insert "\n(provide '" (file-name-base
                            (buffer-file-name)) ")")
    (save-buffer)
    (message "Processed %s as emacs config module" (buffer-file-name))))

(add-hook 'org-babel-post-tangle-hook #'my/modules--post-tangle)

(defun my/emacs-tramp ()
  (interactive)
  (with-environment-variables (("EMACS_ENV" "remote"))
    (start-process "emacs-tramp" nil "emacs")))
