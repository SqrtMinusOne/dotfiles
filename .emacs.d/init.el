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

(setq my/lowpower (string= (system-name) "pntk"))

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

(setenv "IS_EMACS" "true")

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

(use-package conda
  :straight t
  :config
  (setq conda-anaconda-home (expand-file-name "~/Programs/miniconda3/"))
  (setq conda-env-home-directory (expand-file-name "~/Programs/miniconda3/"))
  (setq conda-env-subdirectory "envs")
  )
  
(conda-env-activate "base")

(setq gc-cons-threshold 80000000)
(setq read-process-output-max (* 1024 1024))

(use-package general
  :straight t
  :config
  (general-evil-setup))

(use-package which-key
  :config
  (setq which-key-idle-delay (if my/lowpower 1 0.3))
  (setq which-key-popup-type 'frame)
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  (set-face-attribute 'which-key-local-map-description-face nil
                      :weight 'bold)
  :straight t)

(defun my-edit-configuration ()
  "Open the init file."
  (interactive)
  (find-file "~/.emacs.d/emacs.org"))

(general-define-key "C-c c" 'my-edit-configuration)

(general-def :states '(normal insert visual)
  "<home>" 'beginning-of-line
  "<end>" 'end-of-line)

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

(use-package evil
  :straight t
  :init
  (setq evil-want-integration t)
  (setq evil-want-C-u-scroll t)
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
  
(use-package evil-collection
  :straight t
  :config
  (evil-collection-init '(eww dired company vterm flycheck profiler cider explain-pause-mode notmuch custom)))
  
(use-package evil-quickscope
  :straight t
  :config
  :hook (
         (prog-mode . turn-on-evil-quickscope-mode)
         (LaTeX-mode . turn-on-evil-quickscope-mode)
         ))

(general-create-definer my-leader-def
  :prefix "SPC"
  :keymaps 'override
  :states '(normal motion emacs))
  
(general-def
  :keymaps 'override
  :states '(normal motion emacs insert visual)
  "M-u" 'universal-argument
)

(general-def :states '(normal motion emacs) "SPC" nil)

(my-leader-def "?" 'which-key-show-top-level)
(my-leader-def "E" 'eval-expression)
(my-leader-def "Ps" 'profiler-start)
(my-leader-def "Pe" 'profiler-stop)
(my-leader-def "Pp" 'profiler-report)

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

(general-define-key
  :keymaps 'override
  "C-<right>" 'evil-window-right
  "C-<left>" 'evil-window-left
  "C-<up>" 'evil-window-up
  "C-<down>" 'evil-window-down
  "C-h" 'evil-window-left
  "C-l" 'evil-window-right
  "C-k" 'evil-window-up
  "C-j" 'evil-window-down
  "C-x h" 'previous-buffer
  "C-x l" 'next-buffer
  )

;; (use-package evil-mc
;;   :straight t
;;   :config
;;   (define-key evil-mc-key-map (kbd "C-n") nil)
;;   (define-key evil-mc-key-map (kbd "C-p") nil)
;;   (define-key evil-mc-key-map (kbd "g") nil)
;;   (evil-define-key 'normal evil-mc-key-map
;;     (kbd "C-n") nil
;;     (kbd "g") nil
;;     (kbd "C-p") nil
;;   )
;;   (evil-define-key 'visual evil-mc-key-map
;;     "A" #'evil-mc-make-cursor-in-visual-selection-end
;;     "I" #'evil-mc-make-cursor-in-visual-selection-beg
;;     (kbd "C-n") nil
;;     (kbd "g") nil
;;     (kbd "C-p") nil
;;   )
;;   (global-evil-mc-mode 1))
;;   
;; (general-nmap "gr" evil-mc-cursors-map)

;; (use-package multiple-cursors
;;   :straight t)
;;  
;; (general-vmap
;;   "I" #'mc/edit-lines
;; )

(use-package undo-tree
  :straight t
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-visualizer-timestamps t))
  
(my-leader-def
  "u" 'undo-tree-visualize)
  
  (fset 'undo-auto-amalgamate 'ignore)
(setq undo-limit 6710886400)
(setq undo-strong-limit 100663296)
(setq undo-outer-limit 1006632960)

(use-package ivy
  :straight t
  :config
  (setq ivy-use-virtual-buffers t)
  (ivy-mode))

(use-package counsel
  :straight t
  :config
  (counsel-mode))
  
(use-package swiper
  :straight t)
  
(use-package ivy-rich
  :straight t
  :config
  (ivy-rich-mode 1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))
  
(my-leader-def
  :infix "f"
  "b" 'ivy-switch-buffer
  "e" 'conda-env-activate
  "f" 'project-find-file
  "c" 'counsel-yank-pop
  "a" 'counsel-rg
  "A" 'counsel-ag
)

(my-leader-def "SPC" 'ivy-resume)
(my-leader-def "s" 'swiper-isearch
               "S" 'swiper-all)

(general-define-key
  :keymaps '(ivy-minibuffer-map swiper-map)
  "M-j" 'ivy-next-line
  "M-k" 'ivy-previous-line
  "<C-return>" 'ivy-call
  "M-RET" 'ivy-immediate-done
  [escape] 'minibuffer-keyboard-quit)

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
  
(general-define-key
 :keymaps '(normal override global)
 "C-n" 'treemacs)

(general-define-key
 :keymaps '(treemacs-mode-map) [mouse-1] #'treemacs-single-click-expand-action)
 
(my-leader-def
  "tw" 'treemacs-switch-workspace
  "te" 'treemacs-edit-workspaces)

(use-package projectile
  :straight t
  :config
  (projectile-mode +1)
  (setq projectile-project-search-path '("~/Code" "~/Documents")))
  
;; (use-package helm-projectile
;;   :straight t
;;   :config
;;   (setq projectile-completion-system 'helm))

(use-package counsel-projectile
  :straight t)

(use-package treemacs-projectile
  :straight t)
  
(my-leader-def
  "p" 'projectile-command-map
  ;; "fa" 'helm-projectile-rg
  ;; "fA" 'helm-projectile-ag
  )
  
;; (general-nmap "C-p" 'helm-projectile-find-file)
(general-nmap "C-p" 'counsel-projectile-find-file)

(use-package company
  :straight t
  :config
  (global-company-mode)
  (setq company-idle-delay (if my/lowpower 0.5 0.125))
  (setq company-dabbrev-downcase nil)
  (setq company-show-numbers t))

(use-package company-box
  :straight t
  :if (not my/lowpower)
  :hook (company-mode . company-box-mode))
  
(general-imap "C-SPC" 'company-complete)

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
  
(my-leader-def
  "m" 'magit
  "M" 'magit-file-dispatch)

;; (use-package better-jumper
;;   :straight t
;;   :config
;;   (better-jumper-mode +1)
;;   (setq better-jumper-add-jump-behavior 'replace))
;; 
;; (general-nmap
;;   "go" 'better-jumper-jump-forward
;;   "gp" 'better-jumper-jump-backward)

;; (use-package smart-backspace
;;   :straight t)

;; (general-imap [?\C-?] 'smart-backspace)
;; (general-imap [(shift backspace)] 'backward-delete-char)

(use-package visual-fill-column
  :straight t
  :config
  (add-hook 'visual-fill-column-mode-hook
            (lambda () (setq visual-fill-column-center-text t))))

(use-package smartparens
  :straight t)

(use-package aggressive-indent
  :straight t)

(setq tab-always-indent nil)

(setq default-tab-width 4)
(setq tab-width 4)
(setq-default evil-indent-convert-tabs nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default evil-shift-round nil)

(use-package winner-mode
  :ensure nil
  :config
  (winner-mode)
  :bind (:map evil-window-map
    ("u" . winner-undo)
    ("U" . winner-redo)
  ))

(use-package editorconfig
  :straight t
  :config
  (editorconfig-mode 1))

(use-package avy
  :straight t)
  
(general-nmap "\\\\w" 'avy-goto-word-0-below)
(general-nmap "\\\\b" 'avy-goto-word-0-above)

(use-package yasnippet
  :straight t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :straight t)
  
(general-imap "M-TAB" 'company-yasnippet)

(add-hook 'prog-mode-hook #'hs-minor-mode)
(general-nmap "TAB" 'evil-toggle-fold)
(general-nmap :keymaps 'hs-minor-mode-map "ze" 'hs-hide-level)

(use-package wakatime-mode
  :straight t
  :config
  (global-wakatime-mode))

(use-package request
  :straight t)
  
(use-package activity-watch-mode
  :straight t
  :config
  (global-activity-watch-mode))

(use-package dired
  :ensure nil
  :custom ((dired-listing-switches "-alh --group-directories-first"))
  :config
  (setq dired-dwim-target t)
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (add-hook 'dired-mode-hook
    (lambda ()
      (setq truncate-lines t)
      (visual-line-mode nil)))
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer
    "=" 'dired-narrow
    "-" 'dired-create-empty-file
    (kbd "<left>") 'dired-single-up-directory
    (kbd "<right>") 'dired-single-buffer))
    
(use-package dired+
  :straight t
  :init
  (setq diredp-hide-details-initially-flag nil))

(use-package dired-single
  :straight t)

(use-package all-the-icons-dired
  :straight t
  :if (not my/lowpower)
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  (advice-add 'dired-add-entry :around #'all-the-icons-dired--refresh-advice)
  (advice-add 'dired-remove-entry :around #'all-the-icons-dired--refresh-advice))
  
(use-package dired-open
  :straight t)
  
(use-package dired-narrow
  :straight t)
  
(my-leader-def "ad" 'dired)

(general-define-key
  :keymaps 'dired-mode-map
  [remap dired-find-file] 'dired-single-buffer
  [remap dired-mouse-find-file-other-window] 'dired-single-buffer-mouse
  [remap dired-up-directory] 'dired-single-up-directory
  "M-<return>" 'dired-open-xdg)
  
(general-define-key
  :keymaps 'dired-narrow-map
  [escape] 'keyboard-quit)

(use-package vterm
  :straight t
  :config
  (setq vterm-kill-buffer-on-exit t))
  
(add-to-list 'display-buffer-alist
             `(,"vterm-subterminal.*"
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (side . bottom)
               (reusable-frames . visible)
               (window-height . 0.33)))
               
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

(add-hook 'vterm-mode-hook
          (lambda ()
            (setq-local global-display-line-numbers-mode nil)
            (display-line-numbers-mode 0)
            ))

(general-define-key
  :keymaps 'vterm-mode-map
  "M-q" 'vterm-send-escape
  
  "C-h" 'evil-window-left
  "C-l" 'evil-window-right
  "C-k" 'evil-window-up
  "C-j" 'evil-window-down
  
  "C-<right>" 'evil-window-right
  "C-<left>" 'evil-window-left
  "C-<up>" 'evil-window-up
  "C-<down>" 'evil-window-down
  
  "M-<left>" 'vterm-send-left
  "M-<right>" 'vterm-send-right
  "M-<up>" 'vterm-send-up
  "M-<down>" 'vterm-send-down)
  
(general-imap
  :keymaps 'vterm-mode-map
  "C-r" 'vterm-send-C-r
  "C-k" 'vterm-send-C-k
  "C-j" 'vterm-send-C-j
  "M-l" 'vterm-send-right
  "M-h" 'vterm-send-left)

(straight-override-recipe
   '(org :repo "emacsmirror/org" :no-build t))

(use-package org
  :straight t)

(use-package evil-org
  :straight t
  :after (org evil-collection)
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'org-mode-hook #'smartparens-mode)
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

(use-package jupyter
  :straight t
  :config
  ;; (add-to-list 'evil-emacs-state-modes 'jupyter-repl-mode)
  )
  
(my-leader-def "ar" 'jupyter-run-repl)

;; (use-package ob-typescript
;;   :straight t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   ;; (typescript .t)
   (jupyter . t)))

(org-babel-jupyter-override-src-block "python")

(use-package ob-async
  :straight t
  :config
  (setq ob-async-no-async-languages-alist '("python" "jupyter-python")))

(use-package org-latex-impatient
  :straight (
    :repo "yangsheng6810/org-latex-impatient"
    :branch "master"
    :host github)
  :hook (org-mode . org-latex-impatient-mode)
  :init
  (setq org-latex-impatient-tex2svg-bin
        "/home/pavel/Programs/miniconda3/lib/node_modules/mathjax-node-cli/bin/tex2svg")
  (setq org-latex-impatient-scale 2)
  (setq org-latex-impatient-delay 1)
  (setq org-latex-impatient-border-color "#ffffff")
)

(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.75))
(setq org-highlight-latex-and-related '(native script entities))

;; (use-package htmlize
;;   :straight t)
  
(defun my/setup-org-latex ()
  (setq org-latex-compiler "xelatex")
  (add-to-list 'org-latex-classes
                 '("extarticle"
                   "\\documentclass[a4paper, 14pt]{extarticle}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
  )
)
  
(with-eval-after-load 'ox-latex
  (my/setup-org-latex))

(general-define-key
 :keymaps 'org-mode-map
 "C-c d" 'org-decrypt-entry
 "C-c e" 'org-encrypt-entry
  "M-p" 'org-latex-preview
 )

(general-define-key
 :keymaps 'org-agenda-mode-map
 "M-]" 'org-agenda-later
 "M-[" 'org-agenda-earlier)

(general-imap :keymaps 'org-mode-map "RET" 'evil-org-return)
(general-nmap :keymaps 'org-mode-map "RET" 'org-ctrl-c-ctrl-c)

(my-leader-def
    "aa" 'org-agenda
    "ao" 'org-switchb)

(defun my/org-link-copy (&optional arg)
  "Extract URL from org-mode link and add it to kill ring."
  (interactive "P")
  (let* ((link (org-element-lineage (org-element-context) '(link) t))
          (type (org-element-property :type link))
          (url (org-element-property :path link))
          (url (concat type ":" url)))
    (kill-new url)
    (message (concat "Copied URL: " url))))
    
(general-nmap :keymaps 'org-mode-map
    "C-x C-l" 'my/org-link-copy)

(use-package org-superstar
  :straight t
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
)

(setq org-startup-indented t)

(setq org-return-follows-link t)
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
(setq org-crypt-key nil)

(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
(add-hook 'org-mode-hook (lambda () (rainbow-delimiters-mode 0)))

;; Disable GUI elements
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Transparency
(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))

;; Prettify symbols
;; (global-prettify-symbols-mode)

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
(setq display-line-numbers-type 'visual)
(column-number-mode)

;; Parenteses
(show-paren-mode 1)

;; Wrap
(setq word-wrap 1)
(global-visual-line-mode t)

;; Hightlight line
(global-hl-line-mode 1)

(setq frame-title-format
    '(""
      "emacs"
      (:eval
       (let ((project-name (projectile-project-name)))
         (if (not (string= "-" project-name))
           (format ":%s@%s" project-name (system-name))
           (format "@%s" (system-name)))))
       ))

(general-define-key
 :keymaps 'override
 :states '(normal emacs)
 "gt" 'tab-bar-switch-to-next-tab
 "gT" 'tab-bar-switch-to-prev-tab
 "gn" 'tab-bar-new-tab
 )
 
(setq tab-bar-show 1)
(setq tab-bar-tab-hints t)
(setq tab-bar-tab-name-function 'tab-bar-tab-name-current-with-count)

;; Tabs
(general-nmap "gn" 'tab-new)
(general-nmap "gN" 'tab-close)

(use-package doom-modeline
  :straight t
  :init
  (setq doom-modeline-env-enable-python nil)
  :config
  (doom-modeline-mode 1)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-buffer-state-icon nil))

(use-package emojify
  :straight t
  :if (not my/lowpower)
  :hook (after-init . global-emojify-mode))

(use-package all-the-icons
  :straight t)

;; (use-package dashboard
;;   :straight t
;;   :config
;;   (dashboard-setup-startup-hook))

;; (use-package solaire-mode
;;   :straight t
;;   :config
;;   (solaire-global-mode +1))

(use-package auto-dim-other-buffers
  :straight t
  :if (display-graphic-p)
  :config
  (set-face-attribute 'auto-dim-other-buffers-face nil
                      :background "#212533")
  (auto-dim-other-buffers-mode t))
  
(use-package doom-themes
  :straight t
  :config
  (setq doom-themes-enable-bold t   
        doom-themes-enable-italic t)
  (load-theme 'doom-palenight t)
  (doom-themes-visual-bell-config)
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config))

(use-package highlight-indent-guides
  :straight t
  :if (not my/lowpower)
  :hook (
         (prog-mode . highlight-indent-guides-mode)
         (vue-mode . highlight-indent-guides-mode)
         (LaTeX-mode . highlight-indent-guides-mode))
  :config
  (setq highlight-indent-guides-method 'bitmap)
  (setq highlight-indent-guides-bitmap-function 'highlight-indent-guides--bitmap-line))
  
(use-package rainbow-delimiters
  :straight t
  :if (not my/lowpower)
  :config
  (add-hook 'org-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package ligature
  :straight (:host github :repo "mickeynp/ligature.el")
  :config
  (ligature-set-ligatures 'prog-mode
    '("--" "---" "==" "===" "!=" "!==" "=!=" "=:=" "=/=" "<="
    ">=" "&&" "&&&" "&=" "++" "+++" "***" ";;" "!!" "??"
    "?:" "?." "?=" "<:" ":<" ":>" ">:" "<>" "<<<" ">>>"
    "<<" ">>" "||" "-|" "_|_" "|-" "||-" "|=" "||=" "##"
    "###" "####" "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:"
    "#!" "#=" "^=" "<$>" "<$" "$>" "<+>" "<+" "+>" "<*>"
    "<*" "*>" "</" "</>" "/>" "<!--" "<#--" "-->" "->" "->>"
    "<<-" "<-" "<=<" "=<<" "<<=" "<==" "<=>" "<==>" "==>" "=>"
    "=>>" ">=>" ">>=" ">>-" ">-" ">--" "-<" "-<<" ">->" "<-<"
    "<-|" "<=|" "|=>" "|->" "<->" "<~~" "<~" "<~>" "~~" "~~>"
    "~>" "~-" "-~" "~@" "[||]" "|]" "[|" "|}" "{|" "[<"
    ">]" "|>" "<|" "||>" "<||" "|||>" "<|||" "<|>" "..." ".."
    ".=" ".-" "..<" ".?" "::" ":::" ":=" "::=" ":?" ":?>"
    "//" "///" "/*" "*/" "/=" "//=" "/==" "@_" "__"))
  (global-ligature-mode t))

(defun zoom-in ()
  "Increase font size by 10 points"
  (interactive)
  (set-face-attribute 'default nil
                      :height
                      (+ (face-attribute 'default :height)
                         10)))

(defun zoom-out ()
  "Decrease font size by 10 points"
  (interactive)
  (set-face-attribute 'default nil
                      :height
                      (- (face-attribute 'default :height)
                         10)))

;; change font size, interactively
(global-set-key (kbd "C-+") 'zoom-in)
(global-set-key (kbd "C-=") 'zoom-out)

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

(setq scroll-conservatively scroll-margin)
(setq scroll-step 1)
(setq scroll-preserve-screen-position t)
(setq scroll-error-top-bottom t)
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-inhibit-click-time nil)

(setq select-enable-clipboard t)
(setq mouse-yank-at-point t)

(setq backup-inhibited t)
(setq auto-save-default nil)

(use-package notmuch
  :ensure nil)
  
(my-leader-def "am" 'notmuch)

(use-package lsp-mode
  :straight t
  :hook (
         (typescript-mode . lsp)
         (vue-mode . lsp)
         (go-mode . lsp)
         (svelte-mode . lsp)
         (python-mode . lsp)
         (json-mode . lsp)
         (haskell-mode . lsp)
         (haskell-literate-mode . lsp)) 
  :commands lsp
  :config
  (setq lsp-idle-delay 1)
  (setq lsp-eslint-server-command '("node" "/home/pavel/.emacs.d/.cache/lsp/eslint/unzipped/extension/server/out/eslintServer.js" "--stdio"))
  (setq lsp-eslint-run "onSave")
  (setq lsp-signature-render-documentation nil)
 ;  (lsp-headerline-breadcrumb-mode nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (add-to-list 'lsp-language-id-configuration '(svelte-mode . "svelte"))
  )
  
(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-delay 2)
  (setq lsp-ui-sideline-show-hover nil))

;; (use-package helm-lsp
;;   :straight t
;;   :commands helm-lsp-workspace-symbol)

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

(my-leader-def
  "ld" 'lsp-ui-peek-find-definitions
  "lr" 'lsp-rename
  "lu" 'lsp-ui-peek-find-references
  "ls" 'lsp-ui-find-workspace-symbol
  ;; "la" 'helm-lsp-code-actions
  "le" 'list-flycheck-errors)

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
  (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t))
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.33)))
  )

(use-package dap-mode
  :straight t
  :defer t
  :init
  (setq lsp-enable-dap-auto-configure nil)
  :config

  (setq dap-ui-variable-length 100)
  (require 'dap-node)
  (dap-node-setup)

  (require 'dap-chrome)
  (dap-chrome-setup)
  
  (require 'dap-python)
  
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-ui-controls-mode 1))

(my-leader-def
  :infix "d"
  "d" 'dap-debug
  "b" 'dap-breakpoint-toggle
  "c" 'dap-breakpoint-condition
  "wl" 'dap-ui-locals
  "wb" 'dap-ui-breakpoints
  "wr" 'dap-ui-repl
  "ws" 'dap-ui-sessions
  "we" 'dap-ui-expressions
  )

(my-leader-def
  :infix "d"
  :keymaps 'dap-mode-map
  "h" 'dap-hydra
  )
  
(defun my/dap-yank-value-at-point (node)
  (interactive (list (treemacs-node-at-point)))
  (kill-new (message (plist-get (button-get node :item) :value))))

(use-package typescript-mode
  :straight t)
  
(add-hook 'typescript-mode-hook #'smartparens-mode)
(defun my/set-smartparens-indent (mode)
  (sp-local-pair mode "{" nil :post-handlers '(("|| " "SPC") ("||\n[i]" "RET")))
  (sp-local-pair mode "[" nil :post-handlers '(("|| " "SPC") ("||\n[i]" "RET")))
)
(my/set-smartparens-indent 'typescript-mode)

(defun set-flycheck-eslint()
  "Override flycheck checker with eslint."
  (setq-local lsp-diagnostic-package :none)
  (setq-local flycheck-checker 'javascript-eslint))

;; (add-hook 'typescript-mode-hook
;;           #'set-flycheck-eslint)

(add-hook 'js-mode-hook #'smartparens-mode)
(my/set-smartparens-indent 'js-mode)

(use-package vue-mode
  :straight t)
  
;; (add-hook 'vue-mode-hook
;;          #'set-flycheck-eslint)

(add-hook 'vue-mode-hook #'hs-minor-mode)
(add-hook 'vue-mode-hook #'smartparens-mode)
(my/set-smartparens-indent 'vue-mode)
         
(with-eval-after-load 'editorconfig
  (add-to-list 'editorconfig-indentation-alist
               '(vue-mode css-indent-offset
                          js-indent-level
                          sgml-basic-offset
                          ssass-tab-width
                          typescript-indent-level
                          )))

(add-hook 'vue-mode-hook (lambda () (set-face-background 'mmm-default-submode-face nil)))
(add-hook 'vue-html-mode 'emmet-mode)

(use-package svelte-mode
  :straight t)

(add-hook 'svelte-mode-hook
          'set-flycheck-eslint)
          
(add-hook 'svelte-mode-hook #'smartparens-mode)
(my/set-smartparens-indent 'svelte-mode)

(add-hook 'scss-mode-hook #'smartparens-mode)
(my/set-smartparens-indent 'scss-mode)

(use-package php-mode
  :straight t)

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
              (unless (string-match "\.hogan\.tex$" (buffer-name))
                (lsp))
              (setq-local lsp-diagnostic-package :none)
              (setq-local flycheck-checker 'tex-chktex)))
              
(add-hook 'LaTeX-mode-hook #'rainbow-delimiters-mode)
(add-hook 'LaTeX-mode-hook #'smartparens-mode)
(add-hook 'LaTeX-mode-hook #'prettify-symbols-mode)

(my/set-smartparens-indent 'LaTeX-mode)

(general-nmap
  :keymaps '(LaTeX-mode-map latex-mode-map)
  "RET" 'TeX-command-run-all
  "C-c t" 'orgtbl-mode)

(defun my/import-sty ()
  (interactive)
  (insert 
   (apply #'concat
          (cl-mapcar
           (lambda (file) (concat "\\usepackage{" (file-name-sans-extension (file-relative-name file default-directory)) "}\n"))
           (sort 
            (seq-filter
             (lambda (file) (if (string-match ".*\.sty$" file) 1 nil))
             (directory-files
              (seq-some
               (lambda (dir)
                 (if (and
                      (f-directory-p dir)
                      (seq-some
                       (lambda (file) (string-match ".*\.sty$" file))
                       (directory-files dir))
                      ) dir nil))
               (list "./styles" "../styles/" "." "..")) :full)
             )
            (lambda (f1 f2)
              (pcase f1
                ("gostBibTex.sty" 2)
                ("russianlocale.sty" 1)
                (_ nil)))
            ))))
  )

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
  (setq markdown-open-command "/home/pavel/bin/scripts/chromium-sep")
  )

;; (use-package livedown
;;   :straight (:host github :repo "shime/emacs-livedown")
;;   :commands livedown-preview
;;   :config
;;   (setq livedown-browser "qutebrowser"))

(general-define-key
  :keymaps 'markdown-mode-map
  "M-<left>" 'markdown-promote
  "M-<right>" 'markdown-demote)
  
(add-hook 'markdown-mode-hook #'smartparens-mode)
;; (my/set-smartparens-indent 'js-mode)

(use-package plantuml-mode
  :straight t
  :config
  (setq plantuml-executable-path "/usr/bin/plantuml")
  (setq plantuml-default-exec-mode 'executable)
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  (add-to-list 'auto-mode-alist '("\\.uml\\'" . plantuml-mode))
  )
  
(general-nmap
  :keymaps 'plantuml-mode-map
  "RET" 'plantuml-preview)
  
(add-hook 'plantuml-mode-hook #'smartparens-mode)

(use-package langtool
  :straight t
  :config
  (setq langtool-language-tool-server-jar "/home/pavel/Programs/LanguageTool-5.1/languagetool-server.jar")
  (setq langtool-mother-tongue "ru"))
  
(my-leader-def
  :infix "L"
  "c" 'langtool-check
  "s" 'langtool-server-stop
  "d" 'langtool-check-done
  "n" 'langtool-goto-next-error
  "p" 'langtool-goto-previous-error
)

(add-hook 'python-mode-hook #'smartparens-mode)

(use-package clojure-mode
  :straight t)
  
(add-hook 'clojure-mode-hook #'smartparens-strict-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)

(use-package cider
  :straight t)

(use-package json-mode
  :straight t)
  
(add-hook 'json-mode #'smartparens-mode)
(my/set-smartparens-indent 'json-mode)

(use-package yaml-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(use-package go-mode
  :straight t)

(use-package fish-mode
  :straight t)
  
(add-hook 'fish-mode-hook #'smartparens-mode)

(add-hook 'sh-mode-hook #'smartparens-mode)

(use-package clips-mode
  :straight t)

(use-package haskell-mode
  :straight t)
  
(use-package lsp-haskell
  :straight t)

(use-package dockerfile-mode
  :straight t)

(general-define-key
 :keymaps 'image-mode-map
 "q" 'kill-this-buffer)

(setq remote-file-name-inhibit-cache nil)
(setq tramp-default-method "ssh")
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
                    vc-ignore-dir-regexp
                    tramp-file-name-regexp))
(setq tramp-verbose 6)

(my-leader-def "aw" 'eww)

(general-define-key
 :keymaps 'eww-mode-map
 "+" 'text-scale-increase
 "-" 'text-scale-decrease)

(use-package snow
  :straight (:repo "alphapapa/snow.el" :host github))

(use-package explain-pause-mode
  :straight (explain-pause-mode :type git :host github :repo "lastquestion/explain-pause-mode"))
