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
(use-package evil
  :straight t
  :config
  (evil-mode 1)
  (setq evil-search-module 'evil-search))
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

;; (use-package smart-backspace
;;   :straight t)

(use-package evil-quickscope
  :straight t
  :config
  (global-evil-quickscope-mode))

;; Key helpers
(use-package which-key
  :config
  (setq which-key-idle-delay 0.3)
  (setq which-key-popup-type 'frame)
  (which-key-mode)
  (which-key-setup-minibuffer)
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

;; UI

(use-package all-the-icons
  :straight t)

(use-package solaire-mode
  :straight t
  :config
  (solaire-global-mode +1))

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
  (setq treemacs-follow-after-init nil))

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
  :hook (prog-mode . rainbow-delimiters-mode))

;; Autocompletion

(use-package company
  :straight t
  :config
  (global-company-mode)
  (setq company-idle-delay 0.2)
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

(use-package vue-mode
  :straight t)

(add-hook 'vue-mode-hook (lambda () (set-face-background 'mmm-default-submode-face nil)))

;; LSP
(use-package lsp-mode
  :straight t
  :hook (
         (typescript-mode . lsp)
         (vue-mode . lsp)
         )
  :commands lsp)

(use-package flycheck
  :straight t
  :config
  (global-flycheck-mode))

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode)

(use-package helm-lsp
  :straight t
  :commands helm-lsp-workspace-symbol)

(use-package lsp-treemacs
  :straight t
  :commands lsp-treemacs-errors-list)

;; Misc
(use-package magit
  :straight t)

(use-package avy
  :straight t)

(use-package undo-tree
  :straight t
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-visualizer-timestamps t))

(use-package wakatime-mode
  :straight t
  :config
  (global-wakatime-mode))

;;; -------------------- Display settings --------------------
;; Disable GUI elements
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

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
; (load-user-file "jetbrains-ligatures.el")

;; Theme
;; (load-user-file "palenight-theme.el")
;; (load-theme 'palenight t)

;; Line numbers
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)
(column-number-mode)

;; Parenteses
(show-paren-mode 1)

;; Wrap
;; (visual-line-mode t)
(setq word-wrap 1)

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

;;; -------------------- Keyboard --------------------
(load-user-file "zoom.el")

(electric-pair-mode)

(general-create-definer my-leader-def
  :prefix "SPC"
  :states '(normal motion emacs))

(general-def :states '(normal motion emacs) "SPC" nil)

(my-leader-def "h" (general-simulate-key "C-h" :name help-menu))

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

;; Treemacs
(general-define-key
 :keymaps '(normal override global)
 "C-n" 'treemacs)

(general-define-key
 :keymaps '(treemacs-mode-map) [mouse-1] #'treemacs-single-click-expand-action)

;; Company
(general-imap "C-SPC" 'company-complete)

;; Tabs
(general-nmap "gn" 'tab-new)
(general-nmap "gN" 'tab-close)

;; LSP
(my-leader-def
  "ld" 'lsp-ui-peek-find-definitions
  "lr" 'lsp-ui-peek-find-references
  "ls" 'lsp-ui-find-workspace-symbol
  "la" 'helm-lsp-code-actions
  "le" 'list-flycheck-errors)

;; avy
(general-nmap "\\\\w" 'avy-goto-word-0-below)
(general-nmap "\\\\b" 'avy-goto-word-0-above)

;; Fuzzy search

(my-leader-def
  "fb" 'helm-buffers-list
  "fs" 'helm-lsp-workspace-symbol
  "fw" 'helm-lsp-global-workspace-symbol
  "fc" 'helm-show-kill-ring
  "fa" 'helm-do-grep-ag
  "ff" 'project-find-file)

(general-nmap "C-p" 'project-find-file)

(my-leader-def
  "tw" 'treemacs-switch-workspace
  "te" 'treemacs-edit-workspaces)

(my-leader-def
  "c" 'conda-env-activate)

(my-leader-def
  "u" 'undo-tree-visualize)

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
(setq max-list-eval-depth 8000)
