;;; -*- lexical-binding: t -*-
(use-package ein
  :commands (ein:run)
  :straight t)

(setq my/pipenv-python-alist '())

(defun my/get-pipenv-python ()
  (let ((default-directory (projectile-project-root)))
    (if (file-exists-p "Pipfile")
        (let ((asc (assoc default-directory my/pipenv-python-alist)))
          (if asc
              (cdr asc)
            (let ((python-executable
                   (string-trim (shell-command-to-string "PIPENV_IGNORE_VIRTUALENVS=1 pipenv run which python 2>/dev/null"))))
              (if (string-match-p ".*not found.*" python-executable)
                  (message "Pipfile found, but not pipenv executable!")
                (message (format "Found pipenv python: %s" python-executable))
                (add-to-list 'my/pipenv-python-alist (cons default-directory python-executable))
                python-executable))))
      "python")))

(use-package lsp-pyright
  :straight t
  :defer t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (setq-local lsp-pyright-python-executable-cmd (my/get-pipenv-python))
                         (lsp))))

(add-hook 'python-mode-hook #'smartparens-mode)
(add-hook 'python-mode-hook #'treesit-fold-mode)

(use-package pipenv
  :straight t
  :hook (python-mode . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended))

(use-package yapfify
  :straight (:repo "JorisE/yapfify" :host github)
  :disabled
  :commands (yapfify-region
             yapfify-buffer
             yapfify-region-or-buffer
             yapf-mode))

(use-package python-black
  :straight t
  :commands (python-black-buffer)
  :config
  (setq python-black-command "black"))

(use-package py-isort
  :straight t
  :commands (py-isort-buffer py-isort-region))

(my-leader-def
  :keymaps '(python-mode-map python-ts-mode-map)
  "rr" (lambda ()
         (interactive)
         (unless (and (fboundp #'org-src-edit-buffer-p) (org-src-edit-buffer-p))
           (py-isort-buffer))
         (python-black-buffer)))

(use-package numpydoc
  :straight t
  :commands (numpydoc-generate)
  :init
  (my-leader-def
    :keymaps 'python-ts-mode-map
    "rd" #'numpydoc-generate)
  :config
  (setq numpydoc-insertion-style 'prompt)
  (setq numpydoc-insert-return-without-typehint nil))

(defun my/set-pipenv-pytest ()
  (setq-local
   python-pytest-executable
   (concat (my/get-pipenv-python) " -m pytest")))

(use-package python-pytest
  :straight t
  :commands (python-pytest-dispatch)
  :init
  (my-leader-def
    :keymaps '(python-mode-map python-ts-mode-map)
    :infix "t"
    "t" 'python-pytest-dispatch)
  :config
  (cl-defun python-pytest--run-as-comint (&key command)
    "Run a pytest comint session for COMMAND."
    (let* ((buffer (python-pytest--get-buffer))
           (process (get-buffer-process buffer)))
      (with-current-buffer buffer
        (when (comint-check-proc buffer)
          (unless (or compilation-always-kill
                      (yes-or-no-p "Kill running pytest process?"))
            (user-error "Aborting; pytest still running")))
        (when process
          (delete-process process))
        (let ((inhibit-read-only t))
          (erase-buffer))
        (unless (eq major-mode 'python-pytest-mode)
          (python-pytest-mode))
        (compilation-forget-errors)
        (display-buffer buffer)
        (setq command (format "export COLUMNS=%s; %s"
                              (- (window-width (get-buffer-window buffer)) 5)
                              command))
        (insert (format "cwd: %s\ncmd: %s\n\n" default-directory command))
        (setq python-pytest--current-command command)
        (when python-pytest-pdb-track
          (add-hook
           'comint-output-filter-functions
           'python-pdbtrack-comint-output-filter-function
           nil t))
        (run-hooks 'python-pytest-setup-hook)
        (make-comint-in-buffer "pytest" buffer "bash" nil "-c" command)
        (run-hooks 'python-pytest-started-hook)
        (setq process (get-buffer-process buffer))
        (set-process-sentinel process #'python-pytest--process-sentinel))))
  (add-hook 'python-mode-hook #'my/set-pipenv-pytest)
  (when (derived-mode-p 'python-mode)
    (my/set-pipenv-pytest)))

(use-package code-cells
  :straight t
  :commands (code-cells-mode code-cells-convert-ipynb))

(setq my/tensorboard-buffer "TensorBoard-out")

(defun my/tensorboard ()
  (interactive)
  (start-process
   "tensorboard"
   my/tensorboard-buffer
   "tensorboard"
   "serve"
   "--logdir"
   (car (find-file-read-args "Directory: " t)))
  (display-buffer my/tensorboard-buffer))

(use-package json-mode
  :straight t
  :mode "\\.json\\'"
  :config
  (add-hook 'json-mode-hook #'smartparens-mode)
  (add-hook 'json-mode-hook #'treesit-fold-mode)
  (my/set-smartparens-indent 'json-mode))

(use-package csv-mode
  :straight t
  :disabled
  :mode "\\.csv\\'")

(use-package yaml-mode
  :straight t
  :mode "\\.yml\\'"
  :config
  (add-hook 'yaml-mode-hook 'smartparens-mode)
  ;; (add-hook 'yaml-mode-hook 'highlight-indent-guides-mode)
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(use-package dotenv-mode
  :straight t
  :mode "\\.env\\..*\\'")

(use-package gitignore-templates
  :straight t
  :commands (gitignore-templates-insert
             gitignore-templates-new-file))

(use-package dockerfile-mode
  :mode "Dockerfile\\'"
  :straight t
  :config
  (add-hook 'dockerfile-mode 'smartparens-mode))

(use-package jenkinsfile-mode
  :straight t
  :mode "Jenkinsfile\\'"
  :config
  (add-hook 'jenkinsfile-mode-hook #'smartparens-mode)
  (my/set-smartparens-indent 'jenkinsfile-mode))

(use-package crontab-mode
  :mode "/crontab\\(\\.X*[[:alnum:]]+\\)?\\'"
  :straight t)

(use-package nginx-mode
  :straight t
  :config
  (my/set-smartparens-indent 'nginx-mode))

(use-package hcl-mode
  :mode "\\.hcl\\'"
  :straight t)

(add-hook 'sh-mode-hook #'smartparens-mode)

(use-package fish-mode
  :straight t
  :mode "\\.fish\\'"
  :config
 (add-hook 'fish-mode-hook #'smartparens-mode))

(setq my/sqlformatter-dialect-choice
      '("db2" "mariadb" "mysql" "n1ql" "plsql" "postgresql" "redshift" "spark" "sql" "tsql"))

(setq my/sqlformatter-dialect "postgresql")

(defun my/sqlformatter-set-dialect ()
  "Set dialect for sql-formatter"
  (interactive)
  (setq my/sqlformatter-dialect
        (completing-read "Dialect: " my/sqlformatter-dialect-choice)))

(reformatter-define sqlformat
  :program (executable-find "sql-formatter")
  :args `("-l" ,my/sqlformatter-dialect))

(my-leader-def
  :keymaps '(sql-mode-map)
  "rr" #'sqlformat-buffer)

(use-package sparql-mode
  :mode "\\.sparql\\'"
  :straight t)

(use-package graphql-mode
  :mode (rx (| "gql" "grapql") eos)
  :straight t)

(defun my/doc-view-setup ()
  (display-line-numbers-mode -1)
  (undo-tree-mode -1))

(use-package doc-view
  :straight (:type built-in)
  :config
  (setq doc-view-resolution 300)
  (add-hook 'doc-view-mode-hook #'my/doc-view-setup)
  (general-define-key
   :states '(normal)
   :keymaps '(doc-view-mode-map)
   "j" #'doc-view-next-line-or-next-page
   "k" #'doc-view-previous-line-or-previous-page))

(use-package gnuplot
  :straight t
  :commands (gnuplot-mode gnuplot-make-buffer)
  :init
  (add-to-list 'auto-mode-alist '("\\.gp\\'" . gnuplot-mode))
  :config
  (general-define-key
   :keymaps 'gnuplot-mode-map
   "C-c C-c" #'gnuplot-send-buffer-to-gnuplot)
  (general-define-key
   :states '(normal)
   :keymaps 'gnuplot-mode-map
   "RET" #'gnuplot-send-buffer-to-gnuplot)
  (add-hook 'gnuplot-mode-hook #'smartparens-mode))

(use-package x509-mode
  :commands (x509-dwim)
  :straight (:host github :repo "jobbflykt/x509-mode"
                   :build (:not native-compile)))

(use-package lsp-java
  :straight t
  :after (lsp)
  :config
  (setq lsp-java-jdt-download-url "https://www.eclipse.org/downloads/download.php?file=/jdtls/milestones/1.23.0/jdt-language-server-1.23.0-202304271346.tar.gz"))

(add-hook 'java-mode-hook #'smartparens-mode)
;; (add-hook 'java-mode-hook #'hs-minor-mode)
(my/set-smartparens-indent 'java-mode)

(use-package go-mode
  :straight t
  :mode "\\.go\\'"
  :config
  (my/set-smartparens-indent 'go-mode)
  (add-hook 'go-mode-hook #'smartparens-mode)
  (add-hook 'go-mode-hook #'treesit-fold-mode))

(use-package csharp-mode
  :straight t
  :mode "\\.cs\\'"
  :disabled t
  :config
  (setq lsp-csharp-server-path (executable-find "omnisharp-wrapper"))
  (add-hook 'csharp-mode-hook #'csharp-tree-sitter-mode)
  (add-hook 'csharp-tree-sitter-mode-hook #'smartparens-mode)
  (add-hook 'csharp-mode-hook #'treesit-fold-mode)
  (my/set-smartparens-indent 'csharp-tree-sitter-mode))

(use-package csproj-mode
  :straight t
  :mode "\\.csproj\\'"
  :config
  (add-hook 'csproj-mode #'smartparens-mode))

(use-package haskell-mode
  :straight t
  :mode "\\.hs\\'")

(use-package lsp-haskell
  :straight t
  :after (lsp haskell-mode))

(use-package nix-mode
  :straight t
  :mode "\\.nix\\'"
  :config
  (add-hook 'nix-mode-hook #'smartparens-mode)
  (my/set-smartparens-indent 'nix-mode))

(use-package lua-mode
  :straight t
  :mode "\\.lua\\'"
  :hook (lua-mode . smartparens-mode))

(my/set-smartparens-indent 'lua-mode)

(provide 'sqrt-misc-programming)
