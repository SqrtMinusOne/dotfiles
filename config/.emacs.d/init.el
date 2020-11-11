;;; -------------------- Files --------------------
(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))

(defun load-user-file (file)
  "Load a file in current user's configuration directory."
  (interactive "f")
  (load-file (expand-file-name file user-init-dir)))

(add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))

;;; -------------------- Packages --------------------

;; straight.el package manager
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

;; install use-package
(straight-use-package 'use-package)

(eval-when-compile (require 'use-package))

;; General
(use-package general
  :straight t
  :config
  (general-evil-setup))

;; Vim emulation & editing
(use-package undo-tree
  :straight t
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-visualizer-timestamps t))

(use-package evil
  :straight t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (setq evil-search-module 'evil-search)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  ;; (setq evil-respect-visual-line-mode t)
  (evil-set-undo-system 'undo-tree)
  ;; (add-to-list 'evil-emacs-state-modes 'dired-mode)
  )

(use-package evil-numbers
  :straight t)

(use-package evil-surround
  :straight t
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :straight t
  :config
  (evil-commentary-mode))

(use-package evil-org
  :straight t
  :after (org evil-collection)
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme '(navigation insert textobjects additional calendar todo))))
  (add-to-list 'evil-emacs-state-modes 'org-agenda-mode)
  (require 'evil-org-agenda)
  (add-hook 'org-agenda-mode-hook
          (lambda ()
            (visual-line-mode -1)
            (toggle-truncate-lines 1)
            (display-line-numbers-mode 0)))
  (evil-org-agenda-set-keys))

(use-package htmlize
  :straight t)

(use-package jupyter
  :straight t
  :config
  ;; (add-to-list 'evil-emacs-state-modes 'jupyter-repl-mode)
  )

;; (use-package smart-backspace
;;   :straight t)

(use-package evil-quickscope
  :straight t
  :config
  :hook (
         (prog-mode . turn-on-evil-quickscope-mode)
         (LaTeX-mode . turn-on-evil-quickscope-mode)
         ))

;; Key helpers
(use-package which-key
  :config
  (setq which-key-idle-delay 0.3)
  (setq which-key-popup-type 'frame)
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  (set-face-attribute 'which-key-local-map-description-face nil
                      :weight 'bold)
  :straight t)

(use-package helm
  :init
  (require 'helm-config)
  (setq helm-split-window-in-side-p t)
  (setq helm-move-to-line-cycle-in-source t)
  :straight t
  :config
  (helm-mode 1)
  (helm-autoresize-mode 1))

(use-package helm-ag
  :straight t)

;; UI

(use-package all-the-icons
  :straight t)

(use-package solaire-mode
  :straight t
  :config
  (solaire-global-mode +1))

(use-package highlight-indent-guides
  :straight t
  :hook (
         (prog-mode . highlight-indent-guides-mode)
         (vue-mode . highlight-indent-guides-mode)
         (LaTeX-mode . highlight-indent-guides-mode))
  :config
  (setq highlight-indent-guides-method 'bitmap)
  (setq highlight-indent-guides-bitmap-function 'highlight-indent-guides--bitmap-line))

(use-package doom-themes
  :straight t
  :config
  (setq doom-themes-enable-bold t   
        doom-themes-enable-italic t)
  (load-theme 'doom-palenight t)
  (doom-themes-visual-bell-config)
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config))

(use-package treemacs
  :straight t
  :config
  (setq treemacs-follow-mode nil)
  (setq treemacs-follow-after-init nil)
  (setq treemacs-space-between-root-nodes nil)
  (treemacs-git-mode 'extended)
  (with-eval-after-load 'treemacs
    (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?)))

(use-package treemacs-evil
  :straight t)

(use-package treemacs-magit
  :after treemacs magit
  :straight t)

(use-package doom-modeline
  :straight t
  :config
  (doom-modeline-mode 1)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-buffer-state-icon nil))

(use-package rainbow-delimiters
  :straight t
  :hook (
         (prog-mode . rainbow-delimiters-mode)
         (LaTeX-mode . rainbow-delimiters-mode)))

;; Autocompletion

(use-package company
  :straight t
  :config
  (global-company-mode)
  (setq company-idle-delay 0.125)
  (setq company-show-numbers t))

(use-package company-box
  :straight t
  :hook (company-mode . company-box-mode))

;; Anaconda
(use-package conda
  :straight t
  :config
  (setq conda-anaconda-home (expand-file-name "~/Programs/miniconda3/"))
  (setq conda-env-home-directory (expand-file-name "~/Programs/miniconda3/"))
  (setq conda-env-subdirectory "envs")
  )

;; Language modes
(use-package typescript-mode
  :straight t)

(defun set-flycheck-eslint()
  "Override flycheck checker with eslint."
  (setq-local lsp-diagnostic-package :none)
  (setq-local flycheck-checker 'javascript-eslint))

(add-hook 'typescript-mode-hook
          #'set-flycheck-eslint)

(use-package vue-mode
  :straight t)

(add-hook 'vue-mode-hook
          #'set-flycheck-eslint)

(with-eval-after-load 'editorconfig
  (add-to-list 'editorconfig-indentation-alist
               '(vue-mode css-indent-offset
                          js-indent-level
                          sgml-basic-offset
                          ssass-tab-width
                          typescript-indent-level
                          )))

(use-package go-mode
  :straight t)

(add-hook 'vue-mode-hook (lambda () (set-face-background 'mmm-default-submode-face nil)))

(use-package tex
  :straight auctex
  :defer t
  :config
  (setq-default TeX-auto-save t)
  (setq-default TeX-parse-self t)
  (TeX-PDF-mode)
  (setq-default TeX-engine 'xetex)
  (setq-default TeX-command-extra-options "-shell-escape")
  (setq-default TeX-source-correlate-method 'synctex)
  (TeX-source-correlate-mode)
  (setq-default TeX-source-correlate-start-server t)
  (setq-default LaTeX-math-menu-unicode t)

  (setq-default font-latex-fontify-sectioning 1.3)

  (setq-default preview-scale-function 1.4)
  (assoc-delete-all "--" tex--prettify-symbols-alist)
  (assoc-delete-all "---" tex--prettify-symbols-alist)

  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (TeX-fold-mode 1)
              (outline-minor-mode)))
  
  (add-to-list 'TeX-view-program-selection
             '(output-pdf "Zathura")))

(add-hook 'LaTeX-mode-hook
          #'(lambda ()
              (lsp)
              (setq-local lsp-diagnostic-package :none)
              (setq-local flycheck-checker 'tex-chktex)))

(use-package clips-mode
  :straight t)

(use-package json-mode
  :straight t)

(use-package markdown-mode
  :straight t
  :config
  (setq markdown-command
      (concat
       "pandoc"
       " --from=markdown --to=html"
       " --standalone --mathjax --highlight-style=pygments"
       " --css=pandoc.css"
       " --quiet"
       ))
  (setq markdown-live-preview-delete-export 'delete-on-export)
  (setq markdown-asymmetric-header t)
  (setq markdown-open-command "/home/pavel/bin/scripts/vmd-sep")
  )

;; (use-package livedown
;;   :straight (:host github :repo "shime/emacs-livedown")
;;   :commands livedown-preview
;;   :config
;;   (setq livedown-browser "qutebrowser"))

(use-package plantuml-mode
  :straight t
  :config
  (setq plantuml-executable-path "/usr/bin/plantuml")
  (setq plantuml-default-exec-mode 'executable)
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  (add-to-list 'auto-mode-alist '("\\.uml\\'" . plantuml-mode))
  )

(use-package svelte-mode
  :straight t)

(add-hook 'svelte-mode-hook
          'set-flycheck-eslint)

(use-package php-mode
  :straight t)

(use-package yaml-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

;; LSP
(use-package lsp-mode
  :straight t
  :hook (
         (typescript-mode . lsp)
         (vue-mode . lsp)
         (go-mode . lsp)
         (svelte-mode . lsp)
         (python-mode . lsp)) 
  :commands lsp
  :config
  (setq lsp-idle-delay 1)
  (setq lsp-eslint-server-command '("node" "/home/pavel/.emacs.d/.cache/lsp/eslint/unzipped/extension/server/out/eslintServer.js" "--stdio"))
  (setq lsp-signature-render-documentation nil)
  (add-to-list 'lsp-language-id-configuration '(svelte-mode . "svelte"))
  )

(use-package flycheck
  :straight t
  :config
  (global-flycheck-mode)
  (setq flycheck-check-syntax-automatically '(save idle-buffer-switch mode-enabled))
  (add-hook 'evil-insert-state-exit-hook
            '(lambda ()
               (if flycheck-checker
                   (flycheck-buffer))
               ))
  (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t)))

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-delay 2)
  (setq lsp-ui-sideline-show-hover nil)
  )

(use-package helm-lsp
  :straight t
  :commands helm-lsp-workspace-symbol)

;; (use-package origami
;;   :straight t
;;   :hook (prog-mode . origami-mode))

;; (use-package lsp-origami
;;   :straight t
;;   :config
;;   (add-hook 'lsp-after-open-hook #'lsp-origami-try-enable))

(use-package lsp-treemacs
  :straight t
  :commands lsp-treemacs-errors-list)

;; Misc
(use-package magit
  :straight t
  :config
  (setq magit-blame-styles
        '((margin
           (margin-format    . ("%a %A %s"))
           (margin-width     . 42)
           (margin-face      . magit-blame-margin)
           (margin-body-face . (magit-blame-dimmed)))
          (headings
           (heading-format   . "%-20a %C %s\n"))
          (highlight
           (highlight-face   . magit-blame-highlight))
          (lines
           (show-lines       . t)
           (show-message     . t)))
        ))

(use-package git-gutter
  :straight t
  :config
  (global-git-gutter-mode +1))

(use-package evil-magit
  :straight t)

(use-package visual-fill-column
  :straight t
  :config
  (add-hook 'visual-fill-column-mode-hook
            (lambda () (setq visual-fill-column-center-text t))))

(use-package avy
  :straight t)

(use-package wakatime-mode
  :straight t
  :config
  (global-wakatime-mode))

(use-package editorconfig
  :straight t
  :config
  (editorconfig-mode 1))

(use-package vterm
  :straight t
  :config
  (setq vterm-kill-buffer-on-exit t))

;; (use-package ranger
;;   :straight t)

(use-package yasnippet
  :straight t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :straight t)

;; Elfeed
;; (use-package elfeed
;;   :straight t
;;   :config
;;   (add-to-list 'evil-emacs-state-modes 'elfeed-search-mode)
;;   (add-to-list 'evil-emacs-state-modes 'elfeed-show-mode)
;;   (setq browse-url-browser-function 'eww-browse-url))

;; (use-package elfeed-org
;;   :straight t
;;   :config
;;   (setq rmh-elfeed-org-files (list (expand-file-name "~/Documents/org-mode/rss.org")))
;;   (elfeed-org))

(use-package dired+
  :straight t
  :init
  (setq diredp-hide-details-initially-flag nil))

(use-package dired-single
  :straight t)

(use-package all-the-icons-dired
  :straight t
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; Should be last
(use-package evil-collection
  :straight t
  :config
  (evil-collection-init '(eww dired)))

;; (use-package projectile
;;   :straight t)
;; 
;; (use-package treemacs-projectile
;;   :straight t)

;;; -------------------- Display settings --------------------
;; Disable GUI elements
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Tab bar
(setq tab-bar-show 1)
(setq tab-bar-tab-hints t)
(setq tab-bar-tab-name-function 'tab-bar-tab-name-current-with-count)

;; Prettify symbols
(global-prettify-symbols-mode)

;; No start screen
(setq inhibit-startup-screen t)
;; Visual bell
(setq visible-bell 0)

;; y or n instead of yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Hide mouse cursor while typing
(setq make-pointer-invisible t)

;; Font
(set-frame-font "JetBrainsMono Nerd Font 10" nil t)
;; (load-user-file "jetbrains-ligatures.el")

;; Line numbers
(global-display-line-numbers-mode 1)
(line-number-mode nil)
(setq display-line-numbers-type 'relative)
(column-number-mode)

;; Parenteses
(show-paren-mode 1)

;; Wrap
(setq word-wrap 1)
(global-visual-line-mode t)

;; Hightlight line
(global-hl-line-mode 1)

;; Show error buffer
(add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (side            . bottom)
               (reusable-frames . visible)
               (window-height   . 0.33)))

;; Vterm buffer
(add-to-list 'display-buffer-alist
             `(,"vterm-subterminal.*"
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (side . bottom)
               (reusable-frames . visible)
               (window-height . 0.33)))



;;; -------------------- Keyboard --------------------
(load-user-file "zoom.el")

(electric-pair-mode)

(defvar my-intercept-mode-map (make-sparse-keymap)
  "High precedence keymap.")

(define-minor-mode my-intercept-mode
  "Global minor mode for higher precedence evil keybindings."
  :global t)

(my-intercept-mode)

(dolist (state '(normal visual insert))
  (evil-make-intercept-map
   (evil-get-auxiliary-keymap my-intercept-mode-map state t t)
   state))

(general-define-key
 :keymaps 'my-intercept-mode-map
 :states '(normal emacs)
 "gt" 'tab-bar-switch-to-next-tab
 "gT" 'tab-bar-switch-to-prev-tab
 "gn" 'tab-bar-new-tab
 )

(general-create-definer my-leader-def
  :prefix "SPC"
  :keymaps 'my-intercept-mode-map
  :states '(normal motion emacs))

(general-def :states '(normal motion emacs) "SPC" nil)

(general-def :states '(normal insert visual)
  "<home>" 'beginning-of-line
  "<end>" 'end-of-line)

(my-leader-def
  :infix "h"
  "RET" 'view-order-manuals
  "." 'display-local-help
  "?" 'help-for-help
  "C" 'describe-coding-system
  "F" 'Info-goto-emacs-command-node
  "I" 'describe-input-method
  "K" 'Info-goto-emacs-key-command-node
  "L" 'describe-language-environment
  "P" 'describe-package
  "S" 'info-lookup-symbol
  "a" 'helm-apropos
  "b" 'describe-bindings
  "c" 'describe-key-briefly
  "d" 'apropos-documentation
  "e" 'view-echo-area-messages
  "f" 'describe-function
  "g" 'describe-gnu-project
  "h" 'view-hello-file
  "i" 'info
  "k" 'describe-key
  "l" 'view-lossage
  "m" 'describe-mode
  "n" 'view-emacs-news
  "o" 'describe-symbol
  "p" 'finder-by-keyword
  "q" 'help-quit
  "r" 'info-emacs-manual
  "s" 'describe-syntax
  "t" 'help-with-tutorial
  "v" 'describe-variable
  "w" 'where-is
  "<f1>" 'help-for-help
  "C-\\" 'describe-input-method
  "C-a" 'about-emacs
  "C-c" 'describe-copying
  "C-d" 'view-emacs-debugging
  "C-e" 'view-external-packages
  "C-f" 'view-emacs-FAQ
  "C-h" 'help-for-help
  "C-n" 'view-emacs-news
  "C-o" 'describe-distribution
  "C-p" 'view-emacs-problems
  "C-s" 'search-forward-help-for-help
  "C-t" 'view-emacs-todo
  "C-w" 'describe-no-warranty
  )

(my-leader-def "?" 'which-key-show-top-level)

;; Escape
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(general-define-key
 :keymaps '(normal visual global)
 [escape] 'keyboard-quit)

(general-define-key
 :keymaps '(minibuffer-local-map
            minibuffer-local-ns-map
            minibuffer-local-completion-map
            minibuffer-local-must-match-map
            minibuffer-local-isearch-map)
 [escape] 'minibuffer-keyboard-quit)

;; Vterm
(defun toggle-vterm-subteminal ()
  "Toogle subteminal."
  (interactive)
  (let
      ((vterm-window
        (seq-find
         (lambda (window)
           (string-match
            "vterm-subterminal.*"
            (buffer-name (window-buffer window)))
           )
         (window-list))))
    (if vterm-window
        (if (eq (get-buffer-window (current-buffer)) vterm-window)
            (kill-buffer (current-buffer))
          (select-window vterm-window)
          )
      (vterm-other-window "vterm-subterminal")
      )
    )
  )

(general-nmap "`" 'toggle-vterm-subteminal)
(general-nmap "~" 'vterm)

(general-define-key
 :keymaps 'vterm-mode-map
 "M-q" 'vterm-send-escape)

;; Indent
(setq tab-always-indent nil)

;; Smart backspace
; (general-imap [?\C-?] 'smart-backspace)
; (general-imap [(shift backspace)] 'backward-delete-char)

;; Helm
(define-key evil-ex-map "b" 'helm-buffers-list)
(general-define-key "M-x" 'helm-M-x)
(general-define-key "C-s" 'helm-occur)
(general-define-key "C-h a" 'helm-apropos)

;; Buffer switch
(general-define-key "C-<right>" 'evil-window-right)
(general-define-key "C-<left>" 'evil-window-left)
(general-define-key "C-<up>" 'evil-window-up)
(general-define-key "C-<down>" 'evil-window-down)

(general-define-key "C-h" 'evil-window-left)
(general-define-key "C-l" 'evil-window-right)
(general-define-key "C-k" 'evil-window-up)
(general-define-key "C-j" 'evil-window-down)

;; Treemacs
(general-define-key
 :keymaps '(normal override global)
 "C-n" 'treemacs)

(general-nmap "C-o" 'lsp-treemacs-symbols)

(general-define-key
 :keymaps '(treemacs-mode-map) [mouse-1] #'treemacs-single-click-expand-action)

;; Company
(general-imap "C-SPC" 'company-complete)

;; Tabs
(general-nmap "gn" 'tab-new)
(general-nmap "gN" 'tab-close)

;; Org-mode
(general-define-key
 :keymaps 'org-mode-map
 "C-c d" 'org-decrypt-entry
 "C-c e" 'org-encrypt-entry)

(general-define-key
 :keymaps 'org-agenda-mode-map
 "M-]" 'org-agenda-later
 "M-[" 'org-agenda-earlier)

(general-imap :keymaps 'org-mode-map "RET" 'evil-org-return)
(general-nmap :keymaps 'org-mode-map "RET" 'org-ctrl-c-ctrl-c)

;; Markdown
(general-define-key
 :keymaps 'markdown-mode-map
 "M-<left>" 'markdown-promote
 "M-<right>" 'markdown-demote
 )

;; PlantUML
(general-nmap
  :keymaps 'plantuml-mode-map
  "RET" 'plantuml-preview)

;; Image view
(general-define-key
 :keymaps 'image-mode-map
 "q" 'kill-this-buffer)

;; LaTeX
(general-nmap
  :keymaps '(LaTeX-mode-map latex-mode-map)
  "RET" 'TeX-command-run-all
  "C-c t" 'orgtbl-mode)

;; LSP
(my-leader-def
  "ld" 'lsp-ui-peek-find-definitions
  "lr" 'lsp-rename
  "lu" 'lsp-ui-peek-find-references
  "ls" 'lsp-ui-find-workspace-symbol
  "la" 'helm-lsp-code-actions
  "le" 'list-flycheck-errors)

;; Apps
(my-leader-def
  :infix "a"
  "a" 'org-agenda
  ;; "e" 'elfeed
  "r" 'jupyter-run-repl
  "w" 'eww
  "d" 'dired)

;; EWW

;; (add-to-list 'evil-emacs-state-modes 'eww-mode)

(general-define-key
 :keymaps 'eww-mode-map
 "+" 'text-scale-increase
 "-" 'text-scale-decrease)

;; avy
(general-nmap "\\\\w" 'avy-goto-word-0-below)
(general-nmap "\\\\b" 'avy-goto-word-0-above)

;; Origami
;; (general-nmap "TAB" 'origami-recursively-toggle-node)
; (my-leader-def
;   "of" 'origami-show-only-node)

;; HS & Folding
(add-hook 'prog-mode-hook #'hs-minor-mode)
(general-nmap "TAB" 'evil-toggle-fold)
(general-nmap :keymaps 'hs-minor-mode-map "ze" 'hs-hide-level)

;; Dired
(general-define-key
 :keymaps 'dired-mode-map
 [remap dired-find-file] 'dired-single-buffer
 [remap dired-mouse-find-file-other-window] 'dired-single-buffer-mouse
 [remap dired-up-directory] 'dired-single-up-directory)

;; Fuzzy search
(my-leader-def
  "fb" 'helm-buffers-list
  "fs" 'helm-lsp-workspace-symbol
  "fw" 'helm-lsp-global-workspace-symbol
  "fc" 'helm-show-kill-ring
  "fa" 'helm-do-ag-project-root
  "fm" 'helm-bookmarks
  "ff" 'project-find-file)

(my-leader-def "s" 'helm-occur)

(general-nmap "C-p" 'project-find-file)

(my-leader-def
  "tw" 'treemacs-switch-workspace
  "te" 'treemacs-edit-workspaces)

(my-leader-def
  "e" 'conda-env-activate)

(my-leader-def
  "u" 'undo-tree-visualize)

(my-leader-def
  "m" 'magit)

(general-imap "M-TAB" 'company-yasnippet)

;; Transparency
;; (defun toggle-transparency ()
;;    (interactive)
;;    (let ((alpha (frame-parameter nil 'alpha)))
;;      (set-frame-parameter
;;       nil 'alpha
;;       (if (eql (cond ((numberp alpha) alpha)
;;                      ((numberp (cdr alpha)) (cdr alpha))
;;                      ((numberp (cadr alpha)) (cadr alpha)))
;;                100)
;;           '(95 . 95) '(100 . 100)))))
;; (my-leader-def "dt" 'toggle-transparency)


;;; -------------------- Misc --------------------
;; UTF-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; Scrolling
(setq scroll-conservatively scroll-margin)
(setq scroll-step 1)
(setq scroll-preserve-screen-position t)
(setq scroll-error-top-bottom t)
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-inhibit-click-time nil)

;; Clipboard
(setq select-enable-clipboard t)
(setq mouse-yank-at-point t)

;; Backups (no)
(setq backup-inhibited t)
(setq auto-save-default nil)

;; Agenda
;; (setq org-agenda-sorting-strategy
;;       '((agenda time-up)
;;        (todo time-up)
;;        (tags time-up)
;;        (search time-up)))

;;; -------------------- Editing --------------------
;; undo-redo
(fset 'undo-auto-amalgamate 'ignore)
(setq undo-limit 6710886400)
(setq undo-strong-limit 100663296)
(setq undo-outer-limit 1006632960)

;; indent
(setq default-tab-width 4)
(setq tab-width 4)
(setq-default evil-indent-convert-tabs nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default evil-shift-round nil)

;;; -------------------- Misc --------------------
(defun my-edit-configuration ()
  "Open the init file."
  (interactive)
  (find-file user-init-file))

(general-define-key "C-c c" 'my-edit-configuration)

(setq gc-cons-threshold 8000000)
(setq read-process-output-max (* 1024 1024))

;; org mode
(setq org-startup-indented t)
(setq org-return-follows-link t)
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
(setq org-crypt-key nil)

(conda-env-activate "base")
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (jupyter . t)))

(org-babel-jupyter-override-src-block "python")

(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

(with-eval-after-load 'ox-latex
  (load-user-file "org-latex.el"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(css-indent-offset 2)
 '(custom-safe-themes
   '("c83c095dd01cde64b631fb0fe5980587deec3834dc55144a6e78ff91ebc80b19" "bf387180109d222aee6bb089db48ed38403a1e330c9ec69fe1f52460a8936b66" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" default))
 '(js-indent-level 2)
 '(org-agenda-files
   '("~/Documents/org-mode/Job/dig-traject.org" "~/Documents/org-mode/Personal/look-forward.org" "~/Documents/org-mode/ETU/sem-9.org"))
 '(sgml-basic-offset 2)
 '(wakatime-cli-path "/usr/bin/wakatime")
 '(wakatime-python-bin nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
