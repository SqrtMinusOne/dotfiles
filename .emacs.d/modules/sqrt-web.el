;;; -*- lexical-binding: t -*-
(defun my/should-run-emmet-p ()
  (and (bound-and-true-p emmet-mode)
       (or (and (derived-mode-p 'web-mode)
                (member (web-mode-language-at-pos) '("html" "css")))
           (not (derived-mode-p 'web-mode)))))

(use-package emmet-mode
  :straight t
  :hook ((vue-html-mode . emmet-mode)
         (svelte-mode . emmet-mode)
         (web-mode . emmet-mode)
         (html-mode . emmet-mode)
         (css-mode . emmet-mode)
         (scss-mode . emmet-mode))
  :config
  (defun my/emmet-or-tab (&optional arg)
    (interactive)
    (if (my/should-run-emmet-p)
        (or (emmet-expand-line arg)
            (emmet-go-to-edit-point 1)
            (indent-for-tab-command arg))
      (indent-for-tab-command arg)))
  (general-imap :keymaps 'emmet-mode-keymap
    "TAB" 'my/emmet-or-tab
    "<backtab>" 'emmet-prev-edit-point))

(use-package prettier
  :commands (prettier-prettify)
  :straight t
  :init
  (my-leader-def
    :keymaps '(js-mode-map
               web-mode-map
               typescript-mode-map
               typescript-ts-mode-map
               vue-mode-map
               svelte-mode-map)
    "rr" #'prettier-prettify))

(use-package typescript-mode
  :straight t
  :mode "\\.ts\\'"
  :init
  (add-hook 'typescript-mode-hook #'smartparens-mode)
  (add-hook 'typescript-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'typescript-mode-hook #'treesit-fold-mode)
  :config
  (my/set-smartparens-indent 'typescript-mode))

(add-hook 'js-mode-hook #'smartparens-mode)
(add-hook 'js-mode-hook #'treesit-fold-mode)
(my/set-smartparens-indent 'js-mode)

(use-package jest-test-mode
  :straight t
  :hook ((typescript-mode . jest-test-mode)
         (js-mode . jest-test-mode))
  :config
  (my-leader-def
    :keymaps 'jest-test-mode-map
    :infix "t"
    "t" #'jest-test-run-at-point
    "d" #'jest-test-debug-run-at-point
    "r" #'jest-test-run
    "a" #'jest-test-run-all-tests)
  (defmacro my/jest-test-with-debug-flags (form)
    "Execute FORM with debugger flags set."
    (declare (indent 0))
    `(let ((jest-test-options (seq-concatenate 'list jest-test-options (list "--runInBand") ))
           (jest-test-npx-options (seq-concatenate 'list jest-test-npx-options (list "--node-options" "--inspect-brk"))))
       ,form))
  (defun my/jest-test-debug ()
    "Run the test with an inline debugger attached."
    (interactive)
    (my/jest-test-with-debug-flags
      (jest-test-run)))
  (defun my/jest-test-debug-rerun-test ()
    "Run the test with an inline debugger attached."
    (interactive)
    (my/jest-test-with-debug-flags
      (jest-test-rerun-test)))
  (defun my/jest-test-debug-run-at-point ()
    "Run the test with an inline debugger attached."
    (interactive)
    (my/jest-test-with-debug-flags
      (jest-test-run-at-point)))
  (advice-add #'jest-test-debug :override #'my/jest-test-debug)
  (advice-add #'jest-test-debug-rerun-test :override #'my/jest-test-debug-rerun-test)
  (advice-add #'jest-test-debug-run-at-point
              :override #'my/jest-test-debug-run-at-point))

(defun my/jest-test-run-at-point-copy ()
  "Run the top level describe block of the current buffer's point."
  (interactive)
  (let ((filename (jest-test-find-file))
        (example  (jest-test-unit-at-point)))
    (if (and filename example)
        (jest-test-from-project-directory filename
          (let ((jest-test-options (seq-concatenate 'list jest-test-options (list "-t" example))))
            (kill-new (jest-test-command filename))))
      (message jest-test-not-found-message))))

(use-package web-mode
  :straight t
  :commands (web-mode)
  :init
  (add-to-list 'auto-mode-alist '("\\.svelte\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
  :config
  (add-hook 'web-mode-hook 'smartparens-mode)
  (add-hook 'web-mode-hook 'hs-minor-mode)
  (my/set-smartparens-indent 'web-mode)
  ;; (with-eval-after-load 'editorconfig
  ;;   (push
  ;;    'standard-indent
  ;;    (alist-get 'web-mode editorconfig-indentation-alist)))
  (setq web-mode-auto-pairs nil))

(setq my/web-mode-lsp-extensions
      `(,(rx ".svelte" eos)
        ,(rx ".vue" eos)))

(defun my/web-mode-lsp ()
  (when (seq-some
         (lambda (regex) (string-match-p regex (buffer-file-name)))
         my/web-mode-lsp-extensions)
    (lsp-deferred)))

(add-hook 'web-mode-hook #'my/web-mode-lsp)

(defun my/web-mode-vue-setup (&rest _)
  (let ((filename (buffer-file-name)))
    (when (and (stringp filename)
               (string-match-p (rx ".vue" eos) filename))
      (setq-local web-mode-script-padding 0)
      (setq-local web-mode-style-padding 0)
      (setq-local create-lockfiles nil)
      (setq-local web-mode-enable-auto-pairing nil))))

(add-hook 'web-mode-hook 'my/web-mode-vue-setup)
(add-hook 'editorconfig-after-apply-functions 'my/web-mode-vue-setup)

(add-hook 'scss-mode-hook #'smartparens-mode)
(add-hook 'scss-mode-hook #'hs-minor-mode)
(my/set-smartparens-indent 'scss-mode)

(use-package php-mode
  :straight t
  :mode "\\.php\\'"
  :config
  (add-hook 'php-mode-hook #'smartparens-mode)
  (add-hook 'php-mode-hook #'lsp)
  (my/set-smartparens-indent 'php-mode))

(use-package node-debug
  :if (file-exists-p "/home/pavel/10-19 Code/12 My Emacs Packages/12.21 node-debug.el/")
  :straight (:local-repo "/home/pavel/10-19 Code/12 My Emacs Packages/12.21 node-debug.el/")
  :commands (node-debug node-debug-connect node-debug-dispatch)
  :init
  (my-leader-def "dd" #'node-debug-dispatch))

(defun my/node-debug-exwm-perspective-target (buffer)
  "Return (FRAME . PERSPECTIVE-NAME) containing BUFFER, or nil."
  (when (and (bound-and-true-p persp-mode)
             (boundp 'exwm-workspace--list))
    (catch 'target
      (dolist (frame exwm-workspace--list)
        (when (frame-live-p frame)
          (maphash
           (lambda (name perspective)
             (when (memq buffer (persp-buffers perspective))
               (throw 'target (cons frame name))))
           (perspectives-hash frame)))))))

(defun my/node-debug-display-buffer (buffer location select)
  "Reuse BUFFER's visible window or its EXWM perspective."
  (if-let ((window (or (get-buffer-window buffer (selected-frame))
                       (get-buffer-window buffer t))))
      (progn
        (unless (eq (window-frame window) (selected-frame))
          (exwm-workspace-switch (window-frame window) t))
        window)
    (if-let ((target (my/node-debug-exwm-perspective-target buffer)))
        (progn
          (unless (eq (car target) (selected-frame))
            (exwm-workspace-switch (car target) t))
          (persp-switch (cdr target))
          ;; Switching perspectives restores its window configuration, so
          ;; check again before replacing the selected window.
          (or (get-buffer-window buffer (selected-frame))
              (progn
                (persp-switch-to-buffer buffer)
                (selected-window))))
      (node-debug-source-default-display-buffer buffer location select))))

(setq node-debug-display-buffer-function #'my/node-debug-display-buffer)

(use-package rest-query
  :if (file-exists-p "/home/pavel/10-19 Code/12 My Emacs Packages/12.20 rest-query.el/")
  :straight (:local-repo "/home/pavel/10-19 Code/12 My Emacs Packages/12.20 rest-query.el/")
  :commands (rest-query rest-query-openapi rest-query-new rest-query-open-preset)
  :init
  (my-leader-def
    "dr" #'rest-query
    "dR" #'rest-query-open-preset)
  :config
  (my/persp-add-rule
    rest-query-openapi-mode 4 "api"
    rest-query-request-mode 4 "api"
    rest-query-response-mode 4 "api"
    rest-query-workspaces-mode 4 "api"))

(setq rest-query-workspace-presets
      '(("portfolio-public" . "http://localhost:3019/portfolio/api/docs-public-json/")
        ("portfolio" . "http://localhost:3019/portfolio/api/docs-json/")
        ("is-mob" . "http://localhost:3023/is-mob/api/docs-json/")
        ("nav" . "http://localhost:8000/openapi.json")))

(provide 'sqrt-web)
