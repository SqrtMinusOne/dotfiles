(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; (setq use-package-verbose t)

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
 ;; (setq use-package-verbose t)

(setq gc-cons-threshold 80000000)
(setq read-process-output-max (* 1024 1024))

(add-hook 'emacs-startup-hook
          (lambda ()
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              (add-hook 'after-focus-change-function 'garbage-collect))))

(setq my/lowpower (string= (system-name) "azure"))

(setq my/slow-ssh (string= (getenv "IS_TRAMP") "true"))

(when my/lowpower
  (setq comp-async-jobs-number 1))

(use-package conda
  :straight t
  :if (executable-find "conda")
  :config
  (setq conda-anaconda-home (string-replace "/bin/conda" "" (executable-find "conda")))
  (setq conda-env-home-directory (expand-file-name "~/.conda/"))
  (setq conda-env-subdirectory "envs")
  (setenv "INIT_CONDA" "true")
  (advice-add 'conda-env-activate :after
              (lambda (&rest _) (setenv "EMACS_CONDA_ENV" conda-env-current-name)))
  (unless (getenv "CONDA_DEFAULT_ENV")
    (conda-env-activate "general")))

(setenv "IS_EMACS" "true")

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

(use-package no-littering
  :straight t)

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

(use-package evil-surround
  :straight t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :straight t
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-quickscope
  :straight t
  :after evil
  :config
  :hook ((prog-mode . turn-on-evil-quickscope-mode)
         (LaTeX-mode . turn-on-evil-quickscope-mode)
         (org-mode . turn-on-evil-quickscope-mode)))

(use-package evil-numbers
  :straight t
  :commands (evil-numbers/inc-at-pt evil-numbers/dec-at-pt)
  :init
  (general-nmap
    "g+" 'evil-numbers/inc-at-pt
    "g-" 'evil-numbers/dec-at-pt))

(use-package evil-lion
  :straight t
  :config
  (setq evil-lion-left-align-key (kbd "g a"))
  (setq evil-lion-right-align-key (kbd "g A"))
  (evil-lion-mode))

(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init
   '(eww
     proced
     emms
     pass
     calendar
     dired
     debug
     guix
     calc
     docker
     ibuffer
     geiser
     pdf
     info
     elfeed
     edebug
     bookmark
     company
     vterm
     flycheck
     profiler
     cider
     explain-pause-mode
     notmuch
     custom
     xref
     eshell
     helpful
     compile
     comint
     magit
     prodigy)))

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

(general-def :states '(normal insert visual)
  "<home>" 'beginning-of-line
  "<end>" 'end-of-line)

(general-create-definer my-leader-def
  :keymaps 'override
  :prefix "SPC"
  :states '(normal motion emacs))


(general-def :states '(normal motion emacs) "SPC" nil)

(my-leader-def "?" 'which-key-show-top-level)
(my-leader-def "E" 'eval-expression)

(general-def
  :keymaps 'universal-argument-map
  "M-u" 'universal-argument-more)
(general-def
  :keymaps 'override
  :states '(normal motion emacs insert visual)
  "M-u" 'universal-argument)

(my-leader-def "Ps" 'profiler-start)
(my-leader-def "Pe" 'profiler-stop)
(my-leader-def "Pp" 'profiler-report)

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
  "C-x l" 'next-buffer)

(winner-mode 1)
(define-key evil-window-map (kbd "u") 'winner-undo)
(define-key evil-window-map (kbd "U") 'winner-redo)

(general-nmap
  "gD" 'xref-find-definitions-other-window
  "gr" 'xref-find-references)

(my-leader-def
  "fx" 'xref-find-apropos)

(general-nmap :keymaps '(hs-minor-mode-map outline-minor-mode-map)
  "ze" 'hs-hide-level
  "TAB" 'evil-toggle-fold)

(defun my/zoom-in ()
  "Increase font size by 10 points"
  (interactive)
  (set-face-attribute 'default nil
                      :height
                      (+ (face-attribute 'default :height)
                         10)))

(defun my/zoom-out ()
  "Decrease font size by 10 points"
  (interactive)
  (set-face-attribute 'default nil
                      :height
                      (- (face-attribute 'default :height)
                         10)))

;; change font size, interactively
(global-set-key (kbd "C-+") 'my/zoom-in)
(global-set-key (kbd "C-=") 'my/zoom-out)

(use-package visual-fill-column
  :straight t
  :config
  (add-hook 'visual-fill-column-mode-hook
            (lambda () (setq visual-fill-column-center-text t))))

(use-package smartparens
  :straight t)

(use-package aggressive-indent
  :commands (aggressive-indent-mode)
  :straight t)

(setq my/trailing-whitespace-modes '(markdown-mode))

(require 'cl-extra)

(add-hook 'before-save-hook
          (lambda ()
            (unless (cl-some #'derived-mode-p my/trailing-whitespace-modes)
              (delete-trailing-whitespace))))

(use-package expand-region
  :straight t
  :commands (er/expand-region)
  :init
  (general-nmap "+" 'er/expand-region))

(setq tab-always-indent nil)

(setq-default default-tab-width 4)
(setq-default tab-width 4)
(setq-default evil-indent-convert-tabs nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default evil-shift-round nil)

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

(use-package undo-tree
  :straight t
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-visualizer-timestamps t)

  (my-leader-def "u" 'undo-tree-visualize)
  (fset 'undo-auto-amalgamate 'ignore)
  (setq undo-limit 6710886400)
  (setq undo-strong-limit 100663296)
  (setq undo-outer-limit 1006632960))

(use-package helpful
  :straight t
  :commands (helpful-callable
             helpful-variable
             helpful-key
             helpful-macro
             helpful-function
             helpful-command))

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
  "f" 'helpful-function
  "g" 'describe-gnu-project
  "h" 'view-hello-file
  "i" 'info
  "k" 'helpful-key
  "l" 'view-lossage
  "m" 'describe-mode
  "n" 'view-emacs-news
  "o" 'describe-symbol
  "p" 'finder-by-keyword
  "q" 'help-quit
  "r" 'info-emacs-manual
  "s" 'describe-syntax
  "t" 'help-with-tutorial
  "v" 'helpful-variable
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
  "C-w" 'describe-no-warranty)

(use-package ivy
  :straight t
  :config
  (setq ivy-use-virtual-buffers t)
  (ivy-mode))

(use-package counsel
  :straight t
  :after ivy
  :config
  (counsel-mode))

(use-package swiper
  :defer t
  :straight t)

(use-package ivy-rich
  :straight t
  :after ivy
  :config
  (ivy-rich-mode 1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package ivy-prescient
  :straight t
  :after counsel
  :config
  (ivy-prescient-mode +1)
  (setq ivy-prescient-retain-classic-highlighting t)
  (prescient-persist-mode 1)
  (setq ivy-prescient-sort-commands
        '(:not swiper
               swiper-isearch
               ivy-switch-buffer
               ;; ivy-resume
               ;; ivy--restore-session
               lsp-ivy-workspace-symbol
               counsel-grep
               ;; counsel-find-file
               counsel-git-grep
               counsel-rg
               counsel-ag
               counsel-ack
               counsel-fzf
               counsel-pt
               counsel-imenu
               counsel-yank-pop
               counsel-recentf
               counsel-buffer-or-recentf
               proced-filter-interactive
               proced-sort-interactive))
  ;; Do not use prescient in find-file
  (ivy--alist-set 'ivy-sort-functions-alist #'read-file-name-internal #'ivy-sort-file-function-default))

(my-leader-def
  :infix "f"
  ;; "b" 'counsel-switch-buffer
  "b" 'persp-ivy-switch-buffer
  "e" 'conda-env-activate
  "f" 'project-find-file
  "c" 'counsel-yank-pop
  "a" 'counsel-rg
  "A" 'counsel-ag)

(general-imap
  "C-y" 'counsel-yank-pop)

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
  :commands (treemacs treemacs-switch-workspace treemacs-edit-workspace)
  :config
  (setq treemacs-follow-mode nil)
  (setq treemacs-follow-after-init nil)
  (setq treemacs-space-between-root-nodes nil)
  (treemacs-git-mode 'extended)
  (with-eval-after-load 'treemacs
    (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?)))

(use-package treemacs-evil
  :after (treemacs evil)
  :straight t)

(use-package treemacs-magit
  :after (treemacs magit)
  :straight t)

(general-define-key
 :keymaps '(normal override global)
 "C-n" 'treemacs)

(general-define-key
 :keymaps '(treemacs-mode-map) [mouse-1] #'treemacs-single-click-expand-action)

(my-leader-def
  "tw" 'treemacs-switch-workspace
  "te" 'treemacs-edit-workspaces)

(defun my/treemacs-open-dired ()
  "Open dired at given treemacs node"
  (interactive)
  (let (path (treemacs--prop-at-point :path))
    (dired path)))

(defun my/treemacs-open-vterm ()
  "Open vterm at given treemacs node"
  (interactive)
  (let ((default-directory (file-name-directory (treemacs--prop-at-point :path))))
    (vterm)))

(with-eval-after-load 'treemacs
  (general-define-key
   :keymaps 'treemacs-mode-map
   :states '(treemacs)
   "gd" 'my/treemacs-open-dired
   "gt" 'my/treemacs-open-vterm
   "`" 'my/treemacs-open-vterm))

(use-package projectile
  :straight t
  :config
  (projectile-mode +1)
  (setq projectile-project-search-path '("~/Code" "~/Documents"))
  (defadvice projectile-project-root (around ignore-remote first activate)
    (unless (file-remote-p default-directory) ad-do-it)))

(use-package counsel-projectile
  :after (counsel projectile)
  :straight t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :straight t)

(my-leader-def
  "p" 'projectile-command-map)

(general-nmap "C-p" 'counsel-projectile-find-file)

(use-package company
  :straight t
  :config
  (global-company-mode)
  (setq company-idle-delay (if my/lowpower 0.5 0.125))
  (setq company-dabbrev-downcase nil)
  (setq company-show-numbers t))

(general-imap "C-SPC" 'company-complete)

(use-package company-box
  :straight t
  :if (not my/lowpower)
  :after (company)
  :hook (company-mode . company-box-mode))

(use-package magit
  :straight t
  :commands (magit-status magit-file-dispatch)
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
           (show-message     . t)))))

(use-package git-gutter
  :straight t
  :if (not my/slow-ssh)
  :config
  (global-git-gutter-mode +1))

(my-leader-def
  "m" 'magit
  "M" 'magit-file-dispatch)

(use-package editorconfig
  :straight t
  :config
  (unless my/slow-ssh (editorconfig-mode 1))
  (add-to-list 'editorconfig-indentation-alist
               '(emmet-mode emmet-indentation)))

(use-package yasnippet-snippets
  :straight t)

(use-package yasnippet
  :straight t
  :config
  (setq yas-triggers-in-field t)
  (yas-global-mode 1))

(general-imap "M-TAB" 'company-yasnippet)

(use-package wakatime-mode
  :straight t
  :config
  (advice-add 'wakatime-init :after (lambda () (setq wakatime-cli-path "/home/pavel/bin/wakatime")))
  (global-wakatime-mode))

(use-package request
  :straight t)

(use-package activity-watch-mode
  :straight t
  :config
  (global-activity-watch-mode))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; (set-frame-parameter (selected-frame) 'alpha '(90 . 90))
;; (add-to-list 'default-frame-alist '(alpha . (90 . 90)))

;; (global-prettify-symbols-mode)

(setq inhibit-startup-screen t)

(setq visible-bell 0)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq make-pointer-invisible t)

(global-display-line-numbers-mode 1)
(line-number-mode nil)
(setq display-line-numbers-type 'visual)
(column-number-mode)

(show-paren-mode 1)

(setq word-wrap 1)
(global-visual-line-mode t)

(global-hl-line-mode 1)

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

(deftheme my-theme)

(defun my/update-my-theme (&rest _)
  (custom-theme-set-faces
   'my-theme
   `(tab-bar-tab ((t (
                      :background ,(doom-color 'bg)
                      :foreground ,(doom-color 'yellow)
                      :underline ,(doom-color 'yellow)))))
   `(org-block ((t (:background ,(color-darken-name (doom-color 'bg) 3)))))
   `(org-block-begin-line ((t (
                               :background ,(color-darken-name (doom-color 'bg) 3)
                               :foreground ,(doom-color 'grey)))))
   `(auto-dim-other-buffers-face ((t (:background ,(color-darken-name (doom-color 'bg) 3)))))
   `(aweshell-alert-buffer-face ((t (:foreground ,(doom-color 'red) :weight bold))))
   `(aweshell-alert-command-face ((t (:foreground ,(doom-color 'yellow) :weight bold))))
   `(epe-pipeline-delimiter-face ((t (:foreground ,(doom-color 'green)))))
   `(epe-pipeline-host-face ((t (:foreground ,(doom-color 'blue)))))
   `(epe-pipeline-time-face ((t (:foreground ,(doom-color 'yellow)))))
   `(epe-pipeline-user-face ((t (:foreground ,(doom-color 'red)))))
   `(elfeed-search-tag-face ((t (:foreground ,(doom-color 'yellow))))))
  (custom-theme-set-variables
   'my-theme
   `(aweshell-invalid-command-color ,(doom-color 'red))
   `(aweshell-valid-command-color ,(doom-color 'green)))
  (enable-theme 'my-theme))

(advice-add 'load-theme :after #'my/update-my-theme)
(when (fboundp 'doom-color)
  (my/update-my-theme))

(set-frame-font "JetBrainsMono Nerd Font 10" nil t)

(setq-default frame-title-format
              '(""
                "emacs"
                (:eval
                 (let ((project-name (projectile-project-name)))
                   (if (not (string= "-" project-name))
                       (format ":%s@%s" project-name (system-name))
                     (format "@%s" (system-name)))))))

(use-package perspective
  :straight t
  :init
  ;; (setq persp-show-modestring 'header)
  (setq persp-sort 'created)
  :config
  (persp-mode)
  (my-leader-def "x" 'perspective-map)
  (general-define-key
   :keymaps 'override
   :states '(normal emacs)
   "gt" 'persp-next
   "gT" 'persp-prev
   "gn" 'persp-switch
   "gN" 'persp-kill)
  (general-define-key
   :keymaps 'perspective-map
   "b" 'persp-ivy-switch-buffer
   "x" 'persp-ivy-switch-buffer
   "u" 'persp-ibuffer))

(defun my/persp-move-window-and-switch ()
  (interactive)
  (let* ((buffer (current-buffer)))
    (call-interactively #'persp-switch)
    (persp-set-buffer (buffer-name buffer))
    (switch-to-buffer buffer)))

(defun my/persp-copy-window-and-switch ()
  (interactive)
  (let* ((buffer (current-buffer)))
    (call-interactively #'persp-switch)
    (persp-add-buffer (buffer-name buffer))
    (switch-to-buffer buffer)))

(with-eval-after-load 'perspective
  (general-define-key
   :keymaps 'perspective-map
   "m" #'my/persp-move-window-and-switch
   "f" #'my/persp-copy-window-and-switch))

(setq my/project-title-separators "[-_ ]")

(setq my/project-names-override-alist
      '((".password-store" . "pass")))

(defun my/shorten-project-name-elem (elem crop)
  (if (string-match "^\\[.*\\]$" elem)
      (concat "["
              (my/shorten-project-name-elem (substring elem 1 (- (length elem) 1)) crop)
              "]")
    (let* ((prefix (car (s-match my/project-title-separators elem)))
           (rest
            (substring
             (if prefix
                 (substring elem (length prefix))
               elem)
             0 (if crop 1 nil))))
      (concat prefix rest))))

(defun my/shorten-project-name (project-name)
  (or
   (cdr (assoc project-name my/project-names-override-alist))
   (let ((elems (s-slice-at my/project-title-separators project-name)))
     (concat
      (apply
       #'concat
       (cl-mapcar (lambda (elem) (my/shorten-project-name-elem elem t)) (butlast elems)))
      (my/shorten-project-name-elem (car (last elems)) nil)))))

(defun my/tab-bar-name-function ()
  (let ((project-name (projectile-project-name)))
    (if (string= "-" project-name)
        (tab-bar-tab-name-current-with-count)
      (concat "[" (my/shorten-project-name project-name) "] "
              (replace-regexp-in-string "<.*>" "" (tab-bar-tab-name-current-with-count))))))

(setq tab-bar-tab-name-function #'my/tab-bar-name-function)

(use-package doom-modeline
  :straight t
  :init
  (setq doom-modeline-env-enable-python nil)
  (setq doom-modeline-env-enable-go nil)
  :config
  (doom-modeline-mode 1)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-buffer-state-icon nil))

(use-package emojify
  :straight t
  :if (not my/lowpower)
  :hook (after-init . global-emojify-mode))

(use-package ligature
  :straight (:host github :repo "mickeynp/ligature.el")
  :config
  (ligature-set-ligatures
   '(
     typescript-mode
     js2-mode
     vue-mode
     svelte-mode
     scss-mode
     php-mode
     python-mode
     js-mode
     markdown-mode
     clojure-mode
     go-mode
     sh-mode
     haskell-mode)
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

(use-package all-the-icons
  :straight t)

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :straight t)

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
  :hook ((prog-mode . rainbow-delimiters-mode))
  ;; :commands (rainbow-delimiters-mode)
  ;; :init
  ;; (add-hook 'prog-mode-hook
  ;;           (lambda ()
  ;;             (unless (org-in-src-block-p)
  ;;               (rainbow-delimiters-mode))))
  )

(use-package rainbow-mode
  :commands (rainbow-mode)
  :straight t)

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
    "~" 'vterm
    (kbd "<left>") 'dired-single-up-directory
    (kbd "<right>") 'dired-single-buffer)
  (general-define-key
   :keymaps 'dired-mode-map
   [remap dired-find-file] 'dired-single-buffer
   [remap dired-mouse-find-file-other-window] 'dired-single-buffer-mouse
   [remap dired-up-directory] 'dired-single-up-directory
   "M-<return>" 'dired-open-xdg))

(defun my/dired-home ()
  "Open dired at $HOME"
  (interactive)
  (dired (expand-file-name "~")))

(my-leader-def
  "ad" #'dired
  "aD" #'my/dired-home)

(use-package dired+
  :straight t
  :init
  (setq diredp-hide-details-initially-flag nil)
  :config
  (defun dired-do-delete (&optional arg)
    "Delete all marked (or next ARG) files.
  `dired-recursive-deletes' controls whether deletion of
  non-empty directories is allowed."
    ;; This is more consistent with the file marking feature than
    ;; dired-do-flagged-delete.
    (interactive "P")
    (let (markers)
      (dired-internal-do-deletions
       (nreverse
        ;; this may move point if ARG is an integer
        (dired-map-over-marks (cons (dired-get-filename)
                                    (let ((m (point-marker)))
                                      (push m markers)
                                      m))
                              arg))
       arg t)
      (dolist (m markers) (set-marker m nil)))))

(use-package dired-single
  :after dired
  :straight t)

(use-package all-the-icons-dired
  :straight t
  :if (not (or my/lowpower my/slow-ssh))
  :hook (dired-mode . all-the-icons-dired-mode)
  :config
  (advice-add 'dired-add-entry :around #'all-the-icons-dired--refresh-advice)
  (advice-add 'dired-remove-entry :around #'all-the-icons-dired--refresh-advice)
  (advice-add 'dired-kill-subdir :around #'all-the-icons-dired--refresh-advice))

(use-package dired-open
  :straight t
  :commands (dired-open-xdg))

(use-package dired-narrow
  :straight t
  :commands (dired-narrow)
  :config
  (general-define-key
   :keymaps 'dired-narrow-map
   [escape] 'keyboard-quit))

(defun my/dired-open-this-subdir ()
  (interactive)
  (dired (dired-current-directory)))

(defun my/dired-kill-all-subdirs ()
  (interactive)
  (let ((dir dired-directory))
    (kill-buffer (current-buffer))
    (dired dir)))

(with-eval-after-load 'dired
  (evil-collection-define-key 'normal 'dired-mode-map
    "s" nil
    "ss" 'dired-maybe-insert-subdir
    "sl" 'dired-maybe-insert-subdir
    "sq" 'dired-kill-subdir
    "sk" 'dired-prev-subdir
    "sj" 'dired-next-subdir
    "sS" 'my/dired-open-this-subdir
    "sQ" 'my/dired-kill-all-subdirs
    (kbd "TAB") 'dired-hide-subdir))

(setq tramp-verbose 1)

(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

(with-eval-after-load 'tramp
  (setq tramp-remote-path
        (append tramp-remote-path
                '(tramp-own-remote-path))))

(defun my/dired-bookmark-open ()
  (interactive)
  (unless (boundp 'my/dired-bookmarks)
    (load (concat user-emacs-directory "dired-bookmarks")))
  (let ((bookmarks
         (mapcar
          (lambda (el) (cons (format "%-30s %s" (car el) (cdr el)) (cdr el)))
          my/dired-bookmarks)))
    (dired
     (cdr
      (assoc
       (completing-read "Dired: " bookmarks nil nil "^")
       bookmarks)))))

(use-package vterm
  ;; :straight t
  :commands (vterm vterm-other-window)
  :config
  (setq vterm-kill-buffer-on-exit t)

  (add-hook 'vterm-mode-hook
            (lambda ()
              (setq-local global-display-line-numbers-mode nil)
              (display-line-numbers-mode 0)))

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
    "M-h" 'vterm-send-left))

(add-to-list 'display-buffer-alist
             `(,"vterm-subterminal.*"
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (side . bottom)
               (reusable-frames . visible)
               (window-height . 0.33)))

(defun my/toggle-vterm-subteminal ()
  "Toogle subteminal."
  (interactive)
  (let
      ((vterm-window
        (seq-find
         (lambda (window)
           (string-match
            "vterm-subterminal.*"
            (buffer-name (window-buffer window))))
         (window-list))))
    (if vterm-window
        (if (eq (get-buffer-window (current-buffer)) vterm-window)
            (kill-buffer (current-buffer))
          (select-window vterm-window))
      (vterm-other-window "vterm-subterminal"))))
(unless my/slow-ssh
  (general-nmap "`" 'my/toggle-vterm-subteminal)
  (general-nmap "~" 'vterm))

(defun my/vterm-get-pwd ()
  (if vterm--process
      (file-truename (format "/proc/%d/cwd" (process-id vterm--process)))
    default-directory))

(defun my/vterm-dired-other-window ()
  "Open dired in vterm pwd in other window"
  (interactive)
  (dired-other-window (my/vterm-get-pwd)))

(defun my/vterm-dired-replace ()
  "Replace vterm with dired"
  (interactive)
  (let ((pwd (my/vterm-get-pwd)))
    (kill-process vterm--process)
    (dired pwd)))

(with-eval-after-load 'vterm
  (general-define-key
   :keymap 'vterm-mode-map
   :states '(normal)
   "gd" #'my/vterm-dired-other-window
   "gD" #'my/vterm-dired-replace))

(defun my/configure-eshell ()
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)
  (setq eshell-history-size 10000)
  (setq eshell-hist-ingnoredups t)
  (setq eshell-buffer-maximum-lines 10000)

  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (evil-collection-define-key 'normal 'eshell-mode-map
    (kbd "C-h") 'evil-window-left
    (kbd "C-l") 'evil-window-right
    (kbd "C-k") 'evil-window-up
    (kbd "C-j") 'evil-window-down))

(use-package eshell
  :ensure nil
  :after evil-collection
  :commands (eshell)
  :config
  (add-hook 'eshell-first-time-mode-hook 'my/configure-eshell 90)
  (when my/slow-ssh
    (add-hook 'eshell-mode-hook
              (lambda ()
                (setq-local company-idle-delay 1000))))
  (setq eshell-banner-message ""))

(use-package aweshell
  :straight (:repo "manateelazycat/aweshell" :host github)
  :after eshell
  :config
  (setq eshell-highlight-prompt nil)
  (setq eshell-prompt-function 'epe-theme-pipeline))

(use-package eshell-info-banner
  :defer t
  :if (not my/slow-ssh)
  :straight (eshell-info-banner :type git
                                :host github
                                :repo "phundrak/eshell-info-banner.el")
  :hook (eshell-banner-load . eshell-info-banner-update-banner))

(when my/slow-ssh
  (general-nmap "`" 'aweshell-dedicated-toggle)
  (general-nmap "~" 'eshell))

(use-package org
  :straight t
  :defer t
  :config
  (setq org-startup-indented t)
  (setq org-return-follows-link t)
  (setq org-src-tab-acts-natively nil)
  (add-hook 'org-mode-hook 'smartparens-mode)
  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (visual-line-mode -1)
              (toggle-truncate-lines 1)
              (display-line-numbers-mode 0)))
  (add-hook 'org-mode-hook
            (lambda ()
              (rainbow-delimiters-mode -1)))
  (require 'org-crypt)
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  (setq org-crypt-key "C1EC867E478472439CC82410DE004F32AFA00205")
  (use-package jupyter
    :straight t
    :init
    (my-leader-def "ar" 'jupyter-run-repl))
  (use-package ob-hy
    :straight t)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (sql . t)
     ;; (typescript .t)
     (hy . t)
     (shell . t)
     (octave . t)
     (jupyter . t)))
  
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
  (org-babel-jupyter-override-src-block "python")
  (org-babel-jupyter-override-src-block "hy")
  (add-hook 'org-src-mode-hook
            (lambda ()
              ;; (hs-minor-mode -1)
              ;; (electric-indent-local-mode -1)
              ;; (rainbow-delimiters-mode -1)
              (highlight-indent-guides-mode -1)))
  (setq my/org-latex-scale 1.75)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale my/org-latex-scale))
  (setq my/latex-preview-header "\\documentclass{article}
  \\usepackage[usenames]{color}
  \\usepackage{graphicx}
  \\usepackage{grffile}
  \\usepackage{longtable}
  \\usepackage{wrapfig}
  \\usepackage{rotating}
  \\usepackage[normalem]{ulem}
  \\usepackage{amsmath}
  \\usepackage{textcomp}
  \\usepackage{amssymb}
  \\usepackage{capt-of}
  \\usepackage{hyperref}
  \\pagestyle{empty}")
  
  (setq org-preview-latex-process-alist
        (mapcar
         (lambda (item)
           (cons
            (car item)
            (plist-put (cdr item) :latex-header my/latex-preview-header)))
         org-preview-latex-process-alist))
  (if (not my/lowpower)
      (setq org-agenda-category-icon-alist
            `(("inbox" ,(list (all-the-icons-faicon "inbox")) nil nil :ascent center)
              ("work" ,(list (all-the-icons-faicon "cog")) nil nil :ascent center)
              ("education" ,(list (all-the-icons-material "build")) nil nil :ascent center)
              ("personal" ,(list (all-the-icons-faicon "music")) nil nil :ascent center)
              ("misc" ,(list (all-the-icons-material "archive")) nil nil :ascent center)
              ;; ("lesson" ,(list (all-the-icons-faicon "book")) nil nil :ascent center)
              ;; ("meeting" ,(list (all-the-icons-material "chat")) nil nil :ascent center)
              ;; ("event" ,(list (all-the-icons-octicon "clock")) nil nil :ascent center)
              ("." ,(list (all-the-icons-faicon "circle-o")) nil nil :ascent center))))
  (general-define-key
   :keymaps 'org-mode-map
   "C-c d" 'org-decrypt-entry
   "C-c e" 'org-encrypt-entry
   "M-p" 'org-latex-preview
   "M-o" 'org-redisplay-inline-images)
  
  (general-define-key
   :keymaps 'org-mode-map
   :states '(normal emacs)
   "L" 'org-shiftright
   "H" 'org-shiftleft
   "S-<next>" 'org-next-visible-heading
   "S-<prior>" 'org-previous-visible-heading
   "M-0" 'org-next-visible-heading
   "M-9" 'org-previous-visible-heading
   "M-]" 'org-babel-next-src-block
   "M-[" 'org-babel-previous-src-block)
  
  (general-define-key
   :keymaps 'org-agenda-mode-map
   "M-]" 'org-agenda-later
   "M-[" 'org-agenda-earlier)
  
  ;; (general-imap :keymaps 'org-mode-map "RET" 'evil-org-return)
  (general-nmap :keymaps 'org-mode-map "RET" 'org-ctrl-c-ctrl-c)
  
  (my-leader-def "aa" 'org-agenda)
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
  (setq org-directory (expand-file-name "~/Documents/org-mode"))
  (setq org-agenda-files '("inbox.org" "projects.org" "work.org"))
  ;; (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-capture-templates
        `(("i" "Inbox" entry  (file "inbox.org")
           ,(concat "* TODO %?\n"
                    "/Entered on/ %U"))
          ("e" "email" entry (file "inbox.org")
           ,(concat "* TODO %:from %:subject \n"
                    "/Entered on/ %U\n"
                    "/Received on/ %:date-timestamp-inactive\n"
                    "%a\n"))
          ("f" "elfeed" entry (file "inbox.org")
           ,(concat "* TODO %:elfeed-entry-title\n"
                    "/Entered on/ %U\n"
                    "%a\n"))))
  (add-to-list 'org-global-properties
               '("Effort_ALL" . "0 0:05 0:10 0:15 0:30 0:45 1:00 2:00 4:00"))
  (setq org-log-done 'time))

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
(setq org-crypt-key "C1EC867E478472439CC82410DE004F32AFA00205")

(use-package org-contrib
  :straight (org-contrib
             :type git
             :host nil
             :repo "https://git.sr.ht/~bzg/org-contrib"
             :build t)
  :after (org)
  :config
  (require 'ox-extra)
  (require 'ol-notmuch)
  (ox-extras-activate '(latex-header-blocks ignore-headlines)))

(use-package evil-org
  :straight t
  :hook (org-mode . evil-org-mode)
  :config
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme '(navigation insert textobjects additional calendar todo))))
  (add-to-list 'evil-emacs-state-modes 'org-agenda-mode)
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package jupyter
  :straight t
  :init
  (my-leader-def "ar" 'jupyter-run-repl))

(defun my/jupyter-refresh-kernelspecs ()
  "Refresh Jupyter kernelspecs"
  (interactive)
  (jupyter-available-kernelspecs t))

(defun my/jupyter-refesh-langs ()
  "Refresh Jupyter languages"
  (interactive)
  (org-babel-jupyter-aliases-from-kernelspecs t))

(use-package ob-hy
  :straight t)

(setq my/org-view-html-tmp-dir "/tmp/org-html-preview/")

(use-package f
  :straight t)

(defun my/org-view-html ()
  (interactive)
  (let ((elem (org-element-at-point))
        (temp-file-path (concat my/org-view-html-tmp-dir (number-to-string (random (expt 2 32))) ".html")))
    (cond
     ((not (eq 'export-block (car elem)))
      (message "Not in an export block!"))
     ((not (string-equal (plist-get (car (cdr elem)) :type) "HTML"))
      (message "Export block is not HTML!"))
     (t (progn
          (f-mkdir my/org-view-html-tmp-dir)
          (f-write (plist-get (car (cdr elem)) :value) 'utf-8 temp-file-path)
          (start-process "org-html-preview" nil "xdg-open" temp-file-path))))))

(use-package ob-async
  :straight t
  :after (org)
  :config
  (setq ob-async-no-async-languages-alist '("python" "hy" "jupyter-python" "jupyter-octave")))

(setq my/jupyter-runtime-folder (expand-file-name "~/.local/share/jupyter/runtime"))

(defun my/get-open-ports ()
  (mapcar
   #'string-to-number
   (split-string (shell-command-to-string "ss -tulpnH | awk '{print $5}' | sed -e 's/.*://'") "\n")))

(defun my/list-jupyter-kernel-files ()
  (mapcar
   (lambda (file) (cons (car file) (cdr (assq 'shell_port (json-read-file (car file))))))
   (sort
    (directory-files-and-attributes my/jupyter-runtime-folder t ".*kernel.*json$")
    (lambda (x y) (not (time-less-p (nth 6 x) (nth 6 y)))))))

(defun my/select-jupyter-kernel ()
  (let ((ports (my/get-open-ports))
        (files (my/list-jupyter-kernel-files)))
    (completing-read
     "Jupyter kernels: "
     (seq-filter
      (lambda (file)
        (member (cdr file) ports))
      files))))

(defun my/insert-jupyter-kernel ()
  "Insert a path to an active Jupyter kernel into the buffer"
  (interactive)
  (insert (my/select-jupyter-kernel)))

(defun my/jupyter-connect-repl ()
  "Open an emacs-jupyter REPL, connected to a Jupyter kernel"
  (interactive)
  (jupyter-connect-repl (my/select-jupyter-kernel) nil nil nil t))

(defun my/jupyter-qtconsole ()
  "Open Jupyter QtConsole, connected to a Jupyter kernel"
  (interactive)
  (start-process "jupyter-qtconsole" nil "setsid" "jupyter" "qtconsole" "--existing"
                 (file-name-nondirectory (my/select-jupyter-kernel))))

(defun my/jupyter-cleanup-kernels ()
  (interactive)
  (let* ((ports (my/get-open-ports))
         (files (my/list-jupyter-kernel-files))
         (to-delete (seq-filter
                     (lambda (file)
                       (not (member (cdr file) ports)))
                     files)))
    (when (and (length> to-delete 0)
               (y-or-n-p (format "Delete %d files?" (length to-delete))))
      (dolist (file to-delete)
        (delete-file (car file))))))

(defun my/jupyter-org-scalar (value)
  (cond
   ((stringp value) value)
   (t (jupyter-org-scalar value))))

(define-minor-mode my/emacs-jupyter-raw-output
  "Make emacs-jupyter do raw output")

(defun my/jupyter-org-scalar-around (fun value)
  (if my/emacs-jupyter-raw-output
      (my/jupyter-org-scalar value)
    (funcall fun value)))

(advice-add 'jupyter-org-scalar :around #'my/jupyter-org-scalar-around)

(defun my/org-strip-results (data)
  (replace-regexp-in-string ":\\(RESULTS\\|END\\):\n" "" data))

(defun my/org-caption-wrap (data &optional name caption attrs strip-drawer src-wrap)
  (let* ((data-s (if (and strip-drawer (not (string-empty-p strip-drawer)))
                     (my/org-strip-results data)
                   data))
         (drawer-start (if (string-match-p "^:RESULTS:.*" data-s) 10 0)))
    (concat
     (substring data-s 0 drawer-start)
     (and name (not (string-empty-p name)) (concat "#+NAME:" name "\n"))
     (and caption (not (string-empty-p caption)) (concat "#+CAPTION:" caption "\n"))
     (and attrs (not (string-empty-p attrs)) (concat "#+ATTR_LATEX:" attrs "\n"))
     (if (and src-wrap (not (string-empty-p src-wrap)))
         (concat "#+begin_src " src-wrap "\n"
                 (substring data-s drawer-start)
                 (when (not (string-match-p ".*\n" data-s)) "\n")
                 "#+end_src")
       (substring data-s drawer-start)))))

(my-leader-def "oc" 'org-capture)
(my-leader-def "oa" 'org-agenda)

(setq org-refile-targets
      '(("projects.org" :maxlevel . 2)
        ("work.org" :maxlevel . 2)))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)

(defun my/org-scheduled-get-time ()
  (let ((scheduled (org-get-scheduled-time (point))))
    (if scheduled
        (format-time-string "%Y-%m-%d" scheduled)
      "")))

(setq org-agenda-custom-commands
      `(("p" "My outline"
         ((agenda "")
          (todo "NEXT"
                ((org-agenda-prefix-format "  %i %-12:c [%e] ")
                 (org-agenda-overriding-header "Next tasks")))
          (tags-todo "inbox"
                     ((org-agenda-overriding-header "Inbox")
                      (org-agenda-prefix-format " %i %-12:c")
                      (org-agenda-hide-tags-regexp ".")))
          (tags-todo "+waitlist+SCHEDULED<=\"<+14d>\""
                     ((org-agenda-overriding-header "Waitlist")
                      (org-agenda-hide-tags-regexp "waitlist")
                      (org-agenda-prefix-format " %i %-12:c %-12(my/org-scheduled-get-time)")))))
        ("tp" "Personal tasks"
         ((tags-todo "personal"
                     ((org-agenda-prefix-format "  %i %-12:c [%e] ")))))))

(use-package org-journal
  :straight t
  :after org
  :config
  (setq org-journal-dir (concat org-directory "/journal"))
  (setq org-journal-file-type 'weekly)
  (setq org-journal-file-format "%Y-%m-%d.org")
  (setq org-journal-date-format "%A, %Y-%m-%d")
  (setq org-journal-enable-encryption t))

(my-leader-def
  :infix "oj"
  "j" 'org-journal-new-entry
  "o" 'org-journal-open-current-journal-file
  "s" 'org-journal-search)

(use-package emacsql-sqlite
  :defer t
  :straight (:type built-in))

(use-package org-roam
  :straight t
  :after org
  :init
  (setq org-roam-directory (concat org-directory "/roam"))
  (setq org-roam-file-extensions '("org"))
  (setq org-roam-v2-ack t)
  (setq orb-insert-interface 'ivy-bibtex)
  :config
  (org-roam-setup)
  (setq org-roam-capture-templates
        `(("d" "default" plain "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)))
  (require 'org-roam-protocol))

(my-leader-def
  :infix "or"
  "i" 'org-roam-node-insert
  "r" 'org-roam-node-find
  "g" 'org-roam-graph
  "c" 'org-roam-capture
  "b" 'org-roam-buffer-toggle)

(with-eval-after-load 'org
  (my-leader-def
    :keymap 'org-mode-map
    :infix "or"
    "t" 'org-roam-tag-add
    "T" 'org-toam-tag-remove)
  (general-define-key
   :keymap 'org-mode-map
   "C-c i" 'org-id-get-create
   "C-c l o" 'org-roam-node-insert))

(use-package org-ref
  :straight (:files (:defaults (:exclude "*helm*")))
  :init
  (setq org-ref-completion-library 'org-ref-ivy-cite)
  (setq bibtex-dialect 'biblatex)
  (setq org-ref-default-bibliography '("~/Documents/org-mode/bibliography.bib"))
  (setq reftex-default-bibliography org-ref-default-bibliography)
  (setq bibtex-completion-bibliography org-ref-default-bibliography)
  :after (org)
  :config
  (general-define-key
   :keymaps 'org-mode-map
   "C-c l l" 'org-ref-ivy-insert-cite-link
   "C-c l r" 'org-ref-ivy-insert-ref-link
   "C-c l h" 'org-ref-cite-hydra/body)
  (general-define-key
   :keymaps 'bibtex-mode-map
   "M-RET" 'org-ref-bibtex-hydra/body)
  (add-to-list 'orhc-candidate-formats
               '("online" . "  |${=key=}| ${title} ${url}")))

(use-package org-roam-bibtex
  :straight (:host github :repo "org-roam/org-roam-bibtex")
  :after (org-roam org-ref)
  :disabled
  :config
  (org-roam-bibtex-mode))

(use-package org-latex-impatient
  :straight (:repo "yangsheng6810/org-latex-impatient"
                   :branch "master"
                   :host github)
  :hook (org-mode . org-latex-impatient-mode)
  :disabled
  :init
  (setq org-latex-impatient-tex2svg-bin
        "/home/pavel/Programs/miniconda3/lib/node_modules/mathjax-node-cli/bin/tex2svg")
  (setq org-latex-impatient-scale 1.75)
  (setq org-latex-impatient-delay 1)
  (setq org-latex-impatient-border-color "#ffffff"))

(defun my/enable-org-latex ()
  (interactive)
  (customize-set-variable 'org-highlight-latex-and-related '(native))
  (add-hook 'org-mode-hook (lambda () (yas-activate-extra-mode 'LaTeX-mode)))
  (sp-local-pair 'org-mode "$" "$")
  (sp--remove-local-pair "'"))

(use-package org-superstar
  :straight t
  :hook (org-mode . org-superstar-mode))

(use-package ox-hugo
  :straight t
  :after ox)

(use-package ox-ipynb
  :straight (:host github :repo "jkitchin/ox-ipynb")
  :after ox)

(use-package htmlize
  :straight t
  :after ox
  :config
  (setq org-html-htmlize-output-type 'css))

(defun my/setup-org-latex ()
  (setq org-latex-compiler "xelatex") ;; Probably not necessary
  (setq org-latex-pdf-process '("latexmk -outdir=%o %f")) ;; Use latexmk
  (setq org-latex-listings 'minted) ;; Use minted to highlight source code
  (setq org-latex-minted-options    ;; Some minted options I like
        '(("breaklines" "true")
          ("tabsize" "4")
          ("autogobble")
          ("linenos")
          ("numbersep" "0.5cm")
          ("xleftmargin" "1cm")
          ("frame" "single")))
  ;; Use extarticle without the default packages
  (add-to-list 'org-latex-classes
               '("org-plain-extarticle"
                 "\\documentclass{extarticle}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  ;; Use beamer without the default packages
  (add-to-list 'org-latex-classes
               '("org-latex-beamer"
                 "\\documentclass{beamer}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
                 ("beamer" "\\documentclass[presentation]{beamer}"
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))))

;; Make sure to eval the function when org-latex-classes list already exists
(with-eval-after-load 'ox-latex
  (my/setup-org-latex))

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

(use-package hide-mode-line
  :straight t
  :after (org-present))

(defun my/present-next-with-latex ()
  (interactive)
  (org-present-next)
  (org-latex-preview '(16)))

(defun my/present-prev-with-latex ()
  (interactive)
  (org-present-prev)
  (org-latex-preview '(16)))

(use-package org-present
  :straight (:host github :repo "rlister/org-present")
  :commands (org-present)
  :config
  (general-define-key
   :keymaps 'org-present-mode-keymap
   "<next>" 'my/present-next-with-latex
   "<prior>" 'my/present-prev-with-latex)
  (add-hook 'org-present-mode-hook
            (lambda ()
              (blink-cursor-mode 0)
              (org-present-big)
              ;; (org-display-inline-images)
              (org-present-hide-cursor)
              (org-present-read-only)
              (display-line-numbers-mode 0)
              (hide-mode-line-mode +1)
              (setq-local org-format-latex-options
                          (plist-put org-format-latex-options
                                     :scale (* org-present-text-scale my/org-latex-scale 0.5)))
              (org-latex-preview '(16))
              (tab-bar-mode 0)))
  (add-hook 'org-present-mode-quit-hook
            (lambda ()
              (blink-cursor-mode 1)
              (org-present-small)
              ;; (org-remove-inline-images)
              (org-present-show-cursor)
              (org-present-read-write)
              (display-line-numbers-mode 1)
              (hide-mode-line-mode 0)
              (setq-local org-format-latex-options (plist-put org-format-latex-options :scale my/org-latex-scale))
              (org-latex-preview '(64))
              (tab-bar-mode 1))))

(use-package org-make-toc
  :after (org)
  :commands
  (org-make-toc
   org-make-toc-insert
   org-make-toc-set
   org-make-toc-at-point)
  :straight t)

(defun my/extract-guix-dependencies (&optional category)
  (let ((dependencies '()))
    (org-table-map-tables
     (lambda ()
       (let* ((table
               (seq-filter
                (lambda (q) (not (eq q 'hline)))
                (org-table-to-lisp)))
              (dep-name-index
               (cl-position
                nil
                (mapcar #'substring-no-properties (nth 0 table))
                :test (lambda (_ elem)
                        (string-match-p "[G|g]uix.*dep" elem))))
              (category-name-index
               (cl-position
                nil
                (mapcar #'substring-no-properties (nth 0 table))
                :test (lambda (_ elem)
                        (string-match-p ".*[C|c]ategory.*" elem))))
              (disabled-name-index
               (cl-position
                nil
                (mapcar #'substring-no-properties (nth 0 table))
                :test (lambda (_ elem)
                        (string-match-p ".*[D|d]isabled.*" elem)))))
         (when dep-name-index
           (dolist (elem (cdr table))
             (when
                 (and
                  ;; Category
                  (or
                   ;; Category not set and not present in the table
                   (and
                    (or (not category) (string-empty-p category))
                    (not category-name-index))
                   ;; Category is set and present in the table
                   (and
                    category-name-index
                    (not (string-empty-p category))
                    (string-match-p category (nth category-name-index elem))))
                  ;; Not disabled
                  (or
                   (not disabled-name-index)
                   (string-empty-p (nth disabled-name-index elem))))
               (add-to-list
                'dependencies
                (substring-no-properties (nth dep-name-index elem)))))))))
    dependencies))

(defun my/format-guix-dependencies (&optional category)
  (mapconcat
   (lambda (e) (concat "\"" e "\""))
   (my/extract-guix-dependencies category)
   "\n"))

(setq my/org-config-files
      '("/home/pavel/Emacs.org"
        "/home/pavel/Desktop.org"
        "/home/pavel/Console.org"
        "/home/pavel/Guix.org"
        "/home/pavel/Mail.org"))

(add-hook 'org-mode-hook
          (lambda ()
            (when (member (buffer-file-name) my/org-config-files)
              (setq-local org-confirm-babel-evaluate nil))))

(use-package lsp-mode
  :straight t
  :if (not my/slow-ssh)
  :hook (
         (typescript-mode . lsp)
         (vue-mode . lsp)
         (go-mode . lsp)
         (svelte-mode . lsp)
         ;; (python-mode . lsp)
         (json-mode . lsp)
         (haskell-mode . lsp)
         (haskell-literate-mode . lsp)
         (java-mode . lsp)
         ;; (csharp-mode . lsp)
         )
  :commands lsp
  :config
  (setq lsp-idle-delay 1)
  (setq lsp-eslint-server-command '("node" "/home/pavel/.emacs.d/.cache/lsp/eslint/unzipped/extension/server/out/eslintServer.js" "--stdio"))
  (setq lsp-eslint-run "onSave")
  (setq lsp-signature-render-documentation nil)
  ;; (lsp-headerline-breadcrumb-mode nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (add-to-list 'lsp-language-id-configuration '(svelte-mode . "svelte")))

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
  ;; (add-hook 'evil-insert-state-exit-hook
  ;;           (lambda ()
  ;;             (if flycheck-checker
  ;;                 (flycheck-buffer))
  ;;             ))
  (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t))
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.33))))

(defun my/tree-sitter-if-not-mmm ()
  (when (not (and (boundp 'mmm-temp-buffer-name)
                  (string-equal mmm-temp-buffer-name (buffer-name))))
    (tree-sitter-mode)
    (tree-sitter-hl-mode)))

(use-package tree-sitter
  :straight t
  :hook ((typescript-mode . my/tree-sitter-if-not-mmm)
         (js-mode . my/tree-sitter-if-not-mmm)
         (python-mode . tree-sitter-mode)
         (python-mode . tree-sitter-hl-mode)
         (csharp-mode . tree-sitter-mode)))

(use-package tree-sitter-langs
  :straight t
  :after tree-sitter)

(defun my/set-smartparens-indent (mode)
  (sp-local-pair mode "{" nil :post-handlers '(("|| " "SPC") ("||\n[i]" "RET")))
  (sp-local-pair mode "[" nil :post-handlers '(("|| " "SPC") ("||\n[i]" "RET")))
  (sp-local-pair mode "(" nil :post-handlers '(("|| " "SPC") ("||\n[i]" "RET"))))

(defun my/set-flycheck-eslint()
  "Override flycheck checker with eslint."
  (setq-local lsp-diagnostic-package :none)
  (setq-local flycheck-checker 'javascript-eslint))

(use-package emmet-mode
  :straight t
  :hook ((vue-html-mode . emmet-mode)
         (svelte-mode . emmet-mode)
         (html-mode . emmet-mode)
         (css-mode . emmet-mode)
         (scss-mode . emmet-mode))
  :config
  ;; (setq emmet-indent-after-insert nil)
  (setq my/emmet-mmm-submodes '(vue-html-mode css-mode))
  (defun my/emmet-or-tab (&optional arg)
    (interactive)
    (if (and
         (boundp 'mmm-current-submode)
         mmm-current-submode
         (not (member mmm-current-submode my/emmet-mmm-submodes)))
        (indent-for-tab-command arg)
      (or (emmet-expand-line arg)
          (emmet-go-to-edit-point 1)
          (indent-for-tab-command arg))))
  (general-imap :keymaps 'emmet-mode-keymap
    "TAB" 'my/emmet-or-tab
    "<backtab>" 'emmet-prev-edit-point))

(use-package prettier
  :commands (prettier-prettify)
  :straight t
  :init
  (my-leader-def
    :keymaps '(js-mode-map typescript-mode-map vue-mode-map svelte-mode-map)
    "rr" #'prettier-prettify))

(use-package typescript-mode
  :straight t
  :mode "\\.ts\\'"
  :config
  (add-hook 'typescript-mode-hook #'smartparens-mode)
  (add-hook 'typescript-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'typescript-mode-hook #'hs-minor-mode)
  (my/set-smartparens-indent 'typescript-mode))

(add-hook 'js-mode-hook #'smartparens-mode)
(add-hook 'js-mode-hook #'hs-minor-mode)
(my/set-smartparens-indent 'js-mode)

(use-package jest-test-mode
  :straight t
  :hook ((typescript-mode . jest-test-mode)
         (js-mode . jest-test-mode))
  :config
  (my-leader-def
    :keymaps 'jest-test-mode-map
    :infix "t"
    "t" 'jest-test-run-at-point
    "r" 'jest-test-run
    "a" 'jest-test-run-all-tests))

(use-package vue-mode
  :straight t
  :mode "\\.vue\\'"
  :config
  (add-hook 'vue-mode-hook #'hs-minor-mode)
  (add-hook 'vue-mode-hook #'smartparens-mode)
  (my/set-smartparens-indent 'vue-mode)
  (add-hook 'vue-mode-hook (lambda () (set-face-background 'mmm-default-submode-face nil)))
  (defun mmm-syntax-propertize-function (start stop)
    (let ((saved-mode mmm-current-submode)
          (saved-ovl  mmm-current-overlay))
      (mmm-save-changed-local-variables
       mmm-current-submode mmm-current-overlay)
      (unwind-protect
          (mapc (lambda (elt)
                  (let* ((mode (car elt))
                         (func (get mode 'mmm-syntax-propertize-function))
                         (beg (cadr elt)) (end (nth 2 elt))
                         (ovl (nth 3 elt))
                         syntax-ppss-cache
                         syntax-ppss-last)
                    (goto-char beg)
                    (mmm-set-current-pair mode ovl)
                    (mmm-set-local-variables mode mmm-current-overlay)
                    (save-restriction
                      (if mmm-current-overlay
                          (narrow-to-region (overlay-start mmm-current-overlay)
                                            (overlay-end mmm-current-overlay))
                        (narrow-to-region beg end))
                      (cond
                       (func
                        (funcall func beg end))
                       (font-lock-syntactic-keywords
                        (let ((syntax-propertize-function nil))
                          (font-lock-fontify-syntactic-keywords-region beg end))))
                      (run-hook-with-args 'mmm-after-syntax-propertize-functions
                                          mmm-current-overlay mode beg end))))
                (mmm-regions-in start stop))
        (mmm-set-current-pair saved-mode saved-ovl)
        (mmm-set-local-variables (or saved-mode mmm-primary-mode) saved-ovl)))))

(with-eval-after-load 'editorconfig
  (add-to-list 'editorconfig-indentation-alist
               '(vue-mode css-indent-offset
                          js-indent-level
                          sgml-basic-offset
                          ssass-tab-width
                          typescript-indent-level
                          emmet-indentation
                          vue-html-extra-indent)))

(use-package svelte-mode
  :straight t
  :mode "\\.svelte\\'"
  :config
  (add-hook 'svelte-mode-hook 'my/set-flycheck-eslint)
  (add-hook 'svelte-mode-hook #'smartparens-mode)
  (my/set-smartparens-indent 'svelte-mode)
  ;; I have my own Emmet
  (setq lsp-svelte-plugin-css-completions-emmet nil)
  (setq lsp-svelte-plugin-html-completions-emmet nil))

(add-hook 'scss-mode-hook #'smartparens-mode)
(add-hook 'scss-mode-hook #'hs-minor-mode)
(my/set-smartparens-indent 'scss-mode)

(use-package php-mode
  :straight t
  :mode "\\.php\\'")

(use-package tex
  :straight auctex
  :defer t
  :config
  (setq-default TeX-auto-save t)
  (setq-default TeX-parse-self t)
  (TeX-PDF-mode)
  ;; Use XeLaTeX & stuff
  (setq-default TeX-engine 'xetex)
  (setq-default TeX-command-extra-options "-shell-escape")
  (setq-default TeX-source-correlate-method 'synctex)
  (TeX-source-correlate-mode)
  (setq-default TeX-source-correlate-start-server t)
  (setq-default LaTeX-math-menu-unicode t)

  (setq-default font-latex-fontify-sectioning 1.3)

  ;; Scale preview for my DPI
  (setq-default preview-scale-function 1.4)
  (when (boundp 'tex--prettify-symbols-alist)
    (assoc-delete-all "--" tex--prettify-symbols-alist)
    (assoc-delete-all "---" tex--prettify-symbols-alist))

  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (TeX-fold-mode 1)
              (outline-minor-mode)))

  (add-to-list 'TeX-view-program-selection
               '(output-pdf "Zathura"))

  ;; Do not run lsp within templated TeX files
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (unless (string-match "\.hogan\.tex$" (buffer-name))
                (lsp))
              (setq-local lsp-diagnostic-package :none)
              (setq-local flycheck-checker 'tex-chktex)))

  (add-hook 'LaTeX-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'LaTeX-mode-hook #'smartparens-mode)
  (add-hook 'LaTeX-mode-hook #'prettify-symbols-mode)

  (my/set-smartparens-indent 'LaTeX-mode)
  (require 'smartparens-latex)

  (general-nmap
    :keymaps '(LaTeX-mode-map latex-mode-map)
    "RET" 'TeX-command-run-all
    "C-c t" 'orgtbl-mode)

  (setq my/greek-alphabet
        '(("a" . "\\alpha")
          ("b" . "\\beta" )
          ("g" . "\\gamma")
          ("d" . "\\delta")
          ("e" . "\\epsilon")
          ("z" . "\\zeta")
          ("h" . "\\eta")
          ("o" . "\\theta")
          ("i" . "\\iota")
          ("k" . "\\kappa")
          ("l" . "\\lambda")
          ("m" . "\\mu")
          ("n" . "\\nu")
          ("x" . "\\xi")
          ("p" . "\\pi")
          ("r" . "\\rho")
          ("s" . "\\sigma")
          ("t" . "\\tau")
          ("u" . "\\upsilon")
          ("f" . "\\phi")
          ("c" . "\\chi")
          ("v" . "\\psi")
          ("g" . "\\omega")))
  
  (setq my/latex-greek-prefix "'")
  
  ;; The same for capitalized letters
  (dolist (elem my/greek-alphabet)
    (let ((key (car elem))
          (value (cdr elem)))
      (when (string-equal key (downcase key))
        (add-to-list 'my/greek-alphabet
                     (cons
                      (capitalize (car elem))
                      (concat
                       (substring value 0 1)
                       (capitalize (substring value 1 2))
                       (substring value 2)))))))
  
  (yas-define-snippets
   'latex-mode
   (mapcar
    (lambda (elem)
      (list (concat my/latex-greek-prefix (car elem)) (cdr elem) (concat "Greek letter " (car elem))))
    my/greek-alphabet))
  (setq my/english-alphabet
        '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))
  
  (dolist (elem my/english-alphabet)
    (when (string-equal elem (downcase elem))
      (add-to-list 'my/english-alphabet (upcase elem))))
  
  (setq my/latex-mathbb-prefix "`")
  
  (yas-define-snippets
   'latex-mode
   (mapcar
    (lambda (elem)
      (list (concat my/latex-mathbb-prefix elem) (concat "\\mathbb{" elem "}") (concat "Mathbb letter " elem)))
    my/english-alphabet))
  (setq my/latex-math-symbols
        '(("x" . "\\times")
          ("." . "\\cdot")
          ("v" . "\\forall")
          ("s" . "\\sum_{$1}^{$2}$0")
          ("p" . "\\prod_{$1}^{$2}$0")
          ("d" . "\\partial")
          ("e" . "\\exists")
          ("i" . "\\int_{$1}^{$2}$0")
          ("c" . "\\cap")
          ("u" . "\\cup")
          ("0" . "\\emptyset")
          ("^" . "\\widehat{$1}$0")
          ("_" . "\\overline{$1}$0")
          ("~" . "\\sim")
          ("|" . "\\mid")
          ("_|" . "\\perp")))
  
  (setq my/latex-math-prefix ";")
  
  (yas-define-snippets
   'latex-mode
   (mapcar
    (lambda (elem)
      (let ((key (car elem))
            (value (cdr elem)))
        (list (concat my/latex-math-prefix key) value (concat "Math symbol " value))))
    my/latex-math-symbols))
  (setq my/latex-section-snippets
        '(("ch" . "\\chapter{$1}")
          ("sec" . "\\section{$1}")
          ("ssec" . "\\subsection{$1}")
          ("sssec" . "\\subsubsection{$1}")
          ("par" . "\\paragraph{$1}}")))
  
  (setq my/latex-section-snippets
        (mapcar
         (lambda (elem)
           `(,(car elem)
             ,(cdr elem)
             ,(progn
                (string-match "[a-z]+" (cdr elem))
                (match-string 0 (cdr elem)))))
         my/latex-section-snippets))
  
  (dolist (elem my/latex-section-snippets)
    (let* ((key (nth 0 elem))
           (value (nth 1 elem))
           (desc (nth 2 elem))
           (star-index (string-match "\{\$1\}" value)))
      (add-to-list 'my/latex-section-snippets
                   `(,(concat key "*")
                     ,(concat
                       (substring value 0 star-index)
                       "*"
                       (substring value star-index))
                     ,(concat desc " with *")))
      (add-to-list 'my/latex-section-snippets
                   `(,(concat key "l")
                     ,(concat value "%\n\\label{sec:$2}")
                     ,(concat desc " with label")))))
  
  (dolist (elem my/latex-section-snippets)
    (setf (nth 1 elem) (concat (nth 1 elem) "\n$0")))
  
  (yas-define-snippets
   'latex-mode
   my/latex-section-snippets))

(use-package ivy-bibtex
  :commands (ivy-bibtex)
  :straight t
  :init
  (my-leader-def "fB" 'ivy-bibtex))

(add-hook 'bibtex-mode 'smartparens-mode)

(defun my/list-sty ()
  (reverse
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
       (list "./styles" "../styles/" "." "..")) :full))
    (lambda (f1 f2)
      (let ((f1b (file-name-base f1))
            (f1b (file-name-base f2)))
        (cond
         ((string-match-p ".*BibTex" f1) t)
         ((and (string-match-p ".*Locale" f1) (not (string-match-p ".*BibTex" f2))) t)
         ((string-match-p ".*Preamble" f2) t)
         (t (string-lessp f1 f2))))))))

(defun my/import-sty ()
  (interactive)
  (insert
   (apply #'concat
          (cl-mapcar
           (lambda (file) (concat "\\usepackage{" (file-name-sans-extension (file-relative-name file default-directory)) "}\n"))
           (my/list-sty)))))

(defun my/import-sty-org ()
  (interactive)
  (insert
   (apply #'concat
          (cl-mapcar
           (lambda (file) (concat "#+LATEX_HEADER: \\usepackage{" (file-name-sans-extension (file-relative-name file default-directory)) "}\n"))
           (my/list-sty)))))

(setq my/greek-alphabet
      '(("a" . "\\alpha")
        ("b" . "\\beta" )
        ("g" . "\\gamma")
        ("d" . "\\delta")
        ("e" . "\\epsilon")
        ("z" . "\\zeta")
        ("h" . "\\eta")
        ("o" . "\\theta")
        ("i" . "\\iota")
        ("k" . "\\kappa")
        ("l" . "\\lambda")
        ("m" . "\\mu")
        ("n" . "\\nu")
        ("x" . "\\xi")
        ("p" . "\\pi")
        ("r" . "\\rho")
        ("s" . "\\sigma")
        ("t" . "\\tau")
        ("u" . "\\upsilon")
        ("f" . "\\phi")
        ("c" . "\\chi")
        ("v" . "\\psi")
        ("g" . "\\omega")))

(setq my/latex-greek-prefix "'")

;; The same for capitalized letters
(dolist (elem my/greek-alphabet)
  (let ((key (car elem))
        (value (cdr elem)))
    (when (string-equal key (downcase key))
      (add-to-list 'my/greek-alphabet
                   (cons
                    (capitalize (car elem))
                    (concat
                     (substring value 0 1)
                     (capitalize (substring value 1 2))
                     (substring value 2)))))))

(yas-define-snippets
 'latex-mode
 (mapcar
  (lambda (elem)
    (list (concat my/latex-greek-prefix (car elem)) (cdr elem) (concat "Greek letter " (car elem))))
  my/greek-alphabet))

(setq my/english-alphabet
      '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))

(dolist (elem my/english-alphabet)
  (when (string-equal elem (downcase elem))
    (add-to-list 'my/english-alphabet (upcase elem))))

(setq my/latex-mathbb-prefix "`")

(yas-define-snippets
 'latex-mode
 (mapcar
  (lambda (elem)
    (list (concat my/latex-mathbb-prefix elem) (concat "\\mathbb{" elem "}") (concat "Mathbb letter " elem)))
  my/english-alphabet))

(setq my/latex-math-symbols
      '(("x" . "\\times")
        ("." . "\\cdot")
        ("v" . "\\forall")
        ("s" . "\\sum_{$1}^{$2}$0")
        ("p" . "\\prod_{$1}^{$2}$0")
        ("d" . "\\partial")
        ("e" . "\\exists")
        ("i" . "\\int_{$1}^{$2}$0")
        ("c" . "\\cap")
        ("u" . "\\cup")
        ("0" . "\\emptyset")
        ("^" . "\\widehat{$1}$0")
        ("_" . "\\overline{$1}$0")
        ("~" . "\\sim")
        ("|" . "\\mid")
        ("_|" . "\\perp")))

(setq my/latex-math-prefix ";")

(yas-define-snippets
 'latex-mode
 (mapcar
  (lambda (elem)
    (let ((key (car elem))
          (value (cdr elem)))
      (list (concat my/latex-math-prefix key) value (concat "Math symbol " value))))
  my/latex-math-symbols))

(setq my/latex-section-snippets
      '(("ch" . "\\chapter{$1}")
        ("sec" . "\\section{$1}")
        ("ssec" . "\\subsection{$1}")
        ("sssec" . "\\subsubsection{$1}")
        ("par" . "\\paragraph{$1}}")))

(setq my/latex-section-snippets
      (mapcar
       (lambda (elem)
         `(,(car elem)
           ,(cdr elem)
           ,(progn
              (string-match "[a-z]+" (cdr elem))
              (match-string 0 (cdr elem)))))
       my/latex-section-snippets))

(dolist (elem my/latex-section-snippets)
  (let* ((key (nth 0 elem))
         (value (nth 1 elem))
         (desc (nth 2 elem))
         (star-index (string-match "\{\$1\}" value)))
    (add-to-list 'my/latex-section-snippets
                 `(,(concat key "*")
                   ,(concat
                     (substring value 0 star-index)
                     "*"
                     (substring value star-index))
                   ,(concat desc " with *")))
    (add-to-list 'my/latex-section-snippets
                 `(,(concat key "l")
                   ,(concat value "%\n\\label{sec:$2}")
                   ,(concat desc " with label")))))

(dolist (elem my/latex-section-snippets)
  (setf (nth 1 elem) (concat (nth 1 elem) "\n$0")))

(yas-define-snippets
 'latex-mode
 my/latex-section-snippets)

(use-package markdown-mode
  :straight t
  :mode "\\.md\\'"
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
  (add-hook 'markdown-mode-hook #'smartparens-mode)
  (general-define-key
   :keymaps 'markdown-mode-map
   "M-<left>" 'markdown-promote
   "M-<right>" 'markdown-demote))

;; (use-package livedown
;;   :straight (:host github :repo "shime/emacs-livedown")
;;   :commands livedown-preview
;;   :config
;;   (setq livedown-browser "qutebrowser"))

(use-package plantuml-mode
  :straight t
  :mode "(\\.\\(plantuml?\\|uml\\|puml\\)\\'"
  :config
  (setq plantuml-executable-path "/usr/bin/plantuml")
  (setq plantuml-default-exec-mode 'executable)
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  (add-to-list 'auto-mode-alist '("\\.uml\\'" . plantuml-mode))
  (add-hook 'plantuml-mode-hook #'smartparens-mode))

(general-nmap
  :keymaps 'plantuml-mode-map
  "RET" 'plantuml-preview)

(use-package langtool
  :straight t
  :commands (langtool-check)
  :config
  (setq langtool-language-tool-server-jar "/home/pavel/Programs/LanguageTool-5.1/languagetool-server.jar")
  (setq langtool-mother-tongue "ru")
  (setq langtool-default-language "en-US"))

(my-leader-def
  :infix "L"
  "c" 'langtool-check
  "s" 'langtool-server-stop
  "d" 'langtool-check-done
  "n" 'langtool-goto-next-error
  "p" 'langtool-goto-previous-error
  "l" 'langtool-correct-buffer)

(use-package lispy
  :commands (lispy-mode)
  :straight t)

(use-package lispyville
  :hook (lispy-mode . lispyville-mode)
  :straight t)

(sp-with-modes sp-lisp-modes
  (sp-local-pair "'" nil :actions nil))

(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
;; (add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)
(add-hook 'emacs-lisp-mode-hook #'lispy-mode)

(add-hook 'lisp-mode-hook #'aggressive-indent-mode)
;; (add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)
(add-hook 'lisp-mode-hook #'lispy-mode)

(use-package clojure-mode
  :straight t
  :mode "\\.clj[sc]?\\'"
  :config
  ;; (add-hook 'clojure-mode-hook #'smartparens-strict-mode)
  (add-hook 'clojure-mode-hook #'lispy-mode)
  (add-hook 'clojure-mode-hook #'aggressive-indent-mode))

(use-package cider
  :mode "\\.clj[sc]?\\'"
  :straight t)

(use-package hy-mode
  :straight t
  :mode "\\.hy\\'"
  :config
  (add-hook 'hy-mode-hook #'lispy-mode)
  (add-hook 'hy-mode-hook #'aggressive-indent-mode))

(use-package geiser
  :straight t
  :if (not my/lowpower)
  :config
  (setq geiser-default-implementation 'guile))

(use-package geiser-guile
  :straight t
  :after geiser)

(add-hook 'scheme-mode-hook #'aggressive-indent-mode)
(add-hook 'scheme-mode-hook #'lispy-mode)

(use-package clips-mode
  :straight t
  :mode "\\.cl\\'"
  :config
  (add-hook 'clips-mode 'lispy-mode))

(setq my/pipenv-python-alist '())

(defun my/get-pipenv-python ()
  (let ((default-directory (projectile-project-root)))
    (if (file-exists-p "Pipfile")
        (let ((asc (assoc default-directory my/pipenv-python-alist)))
          (if asc
              (cdr asc)
            (let ((python-executable
                   (string-trim (shell-command-to-string "PIPENV_IGNORE_VIRTUALENVS=1 pipenv run which python"))))
              (if (string-match-p ".*not found.*" python-executable)
                  (message "Pipfile found, but not pipenv executable!")
                (message (format "Found pipenv python: %s" python-executable))
                (add-to-list 'my/pipenv-python-alist (cons default-directory python-executable))
                python-executable))))
      "python")))

(use-package lsp-pyright
  :straight t
  :defer t
  :if (not my/slow-ssh)
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (setq-local lsp-pyright-python-executable-cmd (my/get-pipenv-python))
                         (lsp))))

(add-hook 'python-mode-hook #'smartparens-mode)
(add-hook 'python-mode-hook #'hs-minor-mode)

(use-package pipenv
  :straight t
  :hook (python-mode . pipenv-mode)
  :if (not my/slow-ssh)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended))

(use-package yapfify
  :straight (:repo "JorisE/yapfify" :host github)
  :commands (yapfify-region
             yapfify-buffer
             yapfify-region-or-buffer
             yapf-mode))

(use-package py-isort
  :straight t
  :commands (py-isort-buffer py-isort-region))

(my-leader-def
  :keymaps 'python-mode-map
  "rr" (lambda ()
         (interactive)
         (unless (and (fboundp #'org-src-edit-buffer-p) (org-src-edit-buffer-p))
           (py-isort-buffer))
         (yapfify-buffer)))

(use-package sphinx-doc
  :straight t
  :hook (python-mode . sphinx-doc-mode)
  :config
  (my-leader-def
    :keymaps 'sphinx-doc-mode-map
    "rd" 'sphinx-doc))

(defun my/set-pipenv-pytest ()
  (setq-local
   python-pytest-executable
   (concat (my/get-pipenv-python) " -m pytest")))

(use-package python-pytest
  :straight t
  :commands (python-pytest-dispatch)
  :init
  (my-leader-def
    :keymaps 'python-mode-map
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
  :commands (code-cells-mode))

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

(use-package lsp-java
  :straight t
  :after (lsp)
  :config
  (setq lsp-java-jdt-download-url "https://download.eclipse.org/jdtls/milestones/0.57.0/jdt-language-server-0.57.0-202006172108.tar.gz"))

(add-hook 'java-mode-hook #'smartparens-mode)
;; (add-hook 'java-mode-hook #'hs-minor-mode)
(my/set-smartparens-indent 'java-mode)

(use-package go-mode
  :straight t
  :mode "\\.go\\'"
  :config
  (my/set-smartparens-indent 'go-mode)
  (add-hook 'go-mode-hook #'smartparens-mode)
  (add-hook 'go-mode-hook #'hs-minor-mode))

(use-package csharp-mode
  :straight t
  :mode "\\.cs\\'"
  :config
  (setq lsp-csharp-server-path (executable-find "omnisharp-wrapper"))
  (add-hook 'csharp-mode-hook #'csharp-tree-sitter-mode)
  (add-hook 'csharp-tree-sitter-mode-hook #'smartparens-mode)
  (add-hook 'csharp-mode-hook #'hs-minor-mode)
  (my/set-smartparens-indent 'csharp-tree-sitter-mode))

(use-package csproj-mode
  :straight t
  :mode "\\.csproj\\'"
  :config
  (add-hook 'csproj-mode #'smartparens-mode))

(use-package fish-mode
  :straight t
  :mode "\\.fish\\'"
  :config
 (add-hook 'fish-mode-hook #'smartparens-mode))

(add-hook 'sh-mode-hook #'smartparens-mode)

(use-package haskell-mode
  :straight t
  :mode "\\.hs\\'")

(use-package lsp-haskell
  :straight t
  :after (lsp haskell-mode))

(use-package json-mode
  :straight t
  :mode "\\.json\\'"
  :config
  (add-hook 'json-mode #'smartparens-mode)
  (add-hook 'json-mode #'hs-minor-mode)
  (my/set-smartparens-indent 'json-mode))

(use-package yaml-mode
  :straight t
  :mode "\\.yml\\'"
  :config
  (add-hook 'yaml-mode-hook 'smartparens-mode)
  (add-hook 'yaml-mode-hook 'highlight-indent-guides-mode)
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(use-package dotenv-mode
  :straight t
  :mode "\\.env\\..*\\'")

(use-package csv-mode
  :straight t
  :mode "\\.csv\\'")

(use-package dockerfile-mode
  :mode "Dockerfile\\'"
  :straight t
  :config
  (add-hook 'dockerfile-mode 'smartparens-mode))

(defun my/edit-configuration ()
  "Open the init file."
  (interactive)
  (find-file "~/Emacs.org"))

;; (defun my/edit-exwm-configuration ()
;;   "Open the exwm config file."
;;   (interactive)
;;   (find-file "~/.emacs.d/exwm.org"))

(general-define-key "C-c c" 'my/edit-configuration)
;; (general-define-key "C-c C" 'my/edit-exwm-configuration)
(my-leader-def "cc" 'my/edit-configuration)

(with-eval-after-load 'tramp
  (add-to-list 'tramp-methods
               `("yadm"
                 (tramp-login-program "yadm")
                 (tramp-login-args (("enter")))
                 (tramp-login-env (("SHELL") "/bin/sh"))
                 (tramp-remote-shell "/bin/sh")
                 (tramp-remote-shell-args ("-c")))))


(defun my/yadm-magit ()
  (interactive)
  (magit-status "/yadm::"))

(my-leader-def "cm" 'my/yadm-magit)

(defun my/open-yadm-file ()
  "Open a file managed by yadm"
  (interactive)
  (find-file
   (concat
    (file-name-as-directory (getenv "HOME"))
    (completing-read
     "yadm files: "
     (split-string
      (shell-command-to-string "yadm ls-files $HOME --full-name") "\n")))))

(general-define-key "C-c f" 'my/open-yadm-file)
(my-leader-def "cf" 'my/open-yadm-file)

(load-file (expand-file-name "mail.el" user-emacs-directory))

(use-package elfeed
  :straight (:repo "SqrtMinusOne/elfeed" :host github)
  :commands (elfeed)
  :init
  (my-leader-def "ae" 'elfeed)
  :config
  (setq elfeed-db-directory "~/.elfeed")
  (setq elfeed-enclosure-default-dir (expand-file-name "~"))
  (advice-add #'elfeed-insert-html
              :around
              (lambda (fun &rest r)
                (let ((shr-use-fonts nil))
                  (apply fun r))))
  (evil-collection-define-key 'normal 'elfeed-search-mode-map
    "o" #'my/elfeed-search-filter-source
    "c" #'elfeed-search-clear-filter
    "gl" (lambda () (interactive) (elfeed-search-set-filter "+later")))
  (evil-collection-define-key 'normal 'elfeed-show-mode-map
    "ge" #'my/elfeed-show-visit-eww))

(use-package elfeed-org
  :straight t
  :after (elfeed)
  :config
  (setq rmh-elfeed-org-files '("~/.emacs.d/elfeed.org"))
  (elfeed-org))

(defun my/elfeed-search-filter-source (entry)
  "Filter elfeed search buffer by the feed under cursor."
  (interactive (list (elfeed-search-selected :ignore-region)))
  (when (elfeed-entry-p entry)
    (elfeed-search-set-filter
     (concat
      "@6-months-ago "
      "+unread "
      "="
      (replace-regexp-in-string
       (rx "?" (* not-newline) eos)
       ""
       (elfeed-feed-url (elfeed-entry-feed entry)))))))

(defun my/elfeed-show-visit-eww ()
  "Visit the current entry in eww"
  (interactive)
  (let ((link (elfeed-entry-link elfeed-show-entry)))
    (when link
      (eww link))))

(setq my/youtube-dl-quality-list
      '("bestvideo[height<=720]+bestaudio/best[height<=720]"
        "bestvideo[height<=480]+bestaudio/best[height<=480]"
        "bestvideo[height<=1080]+bestaudio/best[height<=1080]"))

(setq my/youtube-dl-quality-default "bestvideo[height<=720]+bestaudio/best[height<=720]")

(defun my/open-youtube-video (link)
  "Open Youtube URL with mpv"
  (interactive "MURL: ")
  (let ((quality (completing-read "Quality: " my/youtube-dl-quality-list nil t))
        (watch-id (cadr
                   (assoc "watch?v"
                          (url-parse-query-string
                           (substring
                            (url-filename
                             (url-generic-parse-url link))
                            1))))))
    (if (not watch-id)
        (message "Can't find youtube link")
      (let ((yt-link (concat "https://www.youtube.com/watch?v=" watch-id))
            (watch-name (concat "mpv-" watch-id)))
        (start-process watch-name watch-name "mpv" yt-link
                       (format "--ytdl-format=%s" quality))))))

(defun my/elfeed-open-mpv ()
  (interactive)
  "Open MPV for the current entry"
  (my/open-youtube-video (elfeed-entry-link elfeed-show-entry)))

(with-eval-after-load 'elfeed
  (evil-collection-define-key 'normal 'elfeed-show-mode-map
    "gm" #'my/elfeed-open-mpv))

(use-package emms
  :straight t
  :commands (emms-smart-browse emms-browser)
  :init
  (my-leader-def
    :infix "as"
    "s" 'emms-smart-browse
    "b" 'emms-browser
    "p" 'emms-pause
    "q" 'emms-stop
    "h" 'emms-previous
    "l" 'emms-next
    "u" 'emms-player-mpd-connect)
  (setq emms-mode-line-icon-enabled-p nil)
  :config
  (require 'emms-setup)
  (require 'emms-player-mpd)
  (emms-all)
  ;; MPD setup
  (setq emms-source-file-default-directory (expand-file-name "~/Music/"))
  (add-to-list 'emms-info-functions 'emms-info-mpd)
  (add-to-list 'emms-player-list 'emms-player-mpd)
  (setq emms-player-mpd-server-name "localhost")
  (setq emms-player-mpd-server-port "6600")
  (setq emms-player-mpd-music-directory "~/Music")
  (emms-player-mpd-connect)
  ;; Clear MPD playlist on clearing EMMS playlist
  ;; IDK if this is fine for MPD playlists, I don't use them anyhow
  (add-hook 'emms-playlist-cleared-hook 'emms-player-mpd-clear)
  ;; evil-lion shadows ga bindings
  (add-hook 'emms-browser-mode-hook
            (lambda () (evil-lion-mode -1)))
  ;; I have everything I need in polybar
  (emms-mode-line-mode -1)
  (emms-playing-time-display-mode -1)
  (defun emms-info-mpd-process (track info)
    (dolist (data info)
      (let ((name (car data))
            (value (cdr data)))
        (setq name (cond ((string= name "artist") 'info-artist)
                         ((string= name "albumartist") 'info-albumartist)
                         ((string= name "composer") 'info-composer)
                         ((string= name "performer") 'info-performer)
                         ((string= name "title") 'info-title)
                         ((string= name "album") 'info-album)
                         ((string= name "track") 'info-tracknumber)
                         ((string= name "disc") 'info-discnumber)
                         ((string= name "date") 'info-year)
                         ((string= name "genre") 'info-genre)
                         ((string= name "time")
                          (setq value (string-to-number value))
                          'info-playing-time)
                         (t nil)))
        (when name
          (emms-track-set track name value)))))
  (defun emms-player-mpd-get-alists (info)
    "Turn the given parsed INFO from MusicPD into an list of alists.
  
  The list will be in reverse order."
    (when (and info
               (null (car info))          ; no error has occurred
               (cdr info))                ; data exists
      (let ((alists nil)
            (alist nil)
            cell)
        (dolist (line (cdr info))
          (when (setq cell (emms-player-mpd-parse-line line))
            (if (member (car cell) '("file" "directory" "playlist"))
                (setq alists (cons alist alists)
                      alist (list cell))
              (setq alist (cons cell alist)))))
        (when alist
          (setq alists (cons alist alists)))
        alists))))

(with-eval-after-load 'emms-browser
  (evil-collection-define-key 'normal 'emms-browser-mode-map
    "q" 'quit-window))

(with-eval-after-load 'emms
  (evil-collection-define-key 'normal 'emms-playlist-mode-map
    "q" 'quit-window))

(defun my/toggle-shr-use-fonts ()
  "Toggle the shr-use-fonts variable in buffer"
  (interactive)
  (setq-local shr-use-fonts (not shr-use-fonts)))

(my-leader-def "aw" 'eww)

(general-define-key
 :keymaps 'eww-mode-map
 "+" 'text-scale-increase
 "-" 'text-scale-decrease)

(my-leader-def "ai" #'erc-tls)

(use-package erc-hl-nicks
  :hook (erc-mode . erc-hl-nicks-mode)
  :straight t)

(setq erc-server "sqrtminusone.xyz")
(setq erc-port 1984)
(setq erc-nick "sqrtminusone")
(setq erc-user-full-name "Pavel Korytov")
(setq erc-track-shorten-start 8)

(setq erc-kill-buffer-on-part t)

(use-package znc
  :straight t
  :after (erc))

(use-package google-translate
  :straight t
  :functions (my-google-translate-at-point google-translate--search-tkk)
  :custom
  (google-translate-backend-method 'curl)
  :config
  (require 'facemenu)
  (defun google-translate--search-tkk ()
    "Search TKK."
    (list 430675 2721866130))
  (defun my-google-translate-at-point()
    "reverse translate if prefix"
    (interactive)
    (if current-prefix-arg
        (google-translate-at-point)
      (google-translate-at-point-reverse)))
  (setq google-translate-translation-directions-alist
        '(("en" . "ru")
          ("ru" . "en"))))

(my-leader-def
  "atp" 'google-translate-at-point
  "atP" 'google-translate-at-point-reverse
  "atq" 'google-translate-query-translate
  "atQ" 'google-translate-query-translate-reverse
  "att" 'google-translate-smooth-translate)

(use-package elcord
  :straight t
  :if (and (or
            (string= (system-name) "indigo")
            (string= (system-name) "eminence"))
           (not my/slow-ssh))
  :config
  (elcord-mode)
  (add-to-list 'elcord-boring-buffers-regexp-list
               (rx bos (+ num) "-" (+ num) "-" (+ num) ".org" eos))
  (add-to-list 'elcord-boring-buffers-regexp-list
               (rx bos (= 14 num) "-" (* not-newline) ".org" eos)))

(use-package tldr
  :straight t
  :commands (tldr)
  :config
  (setq tldr-source-zip-url "https://github.com/tldr-pages/tldr/archive/refs/heads/main.zip")

  (defun tldr-update-docs ()
    (interactive)
    (shell-command-to-string (format "curl -L %s --output %s" tldr-source-zip-url tldr-saved-zip-path))
    (when (file-exists-p "/tmp/tldr")
      (delete-directory "/tmp/tldr" t))
    (shell-command-to-string (format "unzip -d /tmp/tldr/ %s" tldr-saved-zip-path) nil nil)
    (when (file-exists-p tldr-directory-path)
      (delete-directory tldr-directory-path 'recursive 'no-trash))
    (shell-command-to-string (format "mv %s %s" "/tmp/tldr/tldr-main" tldr-directory-path))))

(my-leader-def "hT" 'tldr)

(setq Man-width-max 180)
(my-leader-def "hM" 'man)

(evil-collection-define-key 'normal 'Info-mode-map
  (kbd "RET") 'Info-follow-nearest-node)

(defun my/man-fix-width (&rest _)
  (setq-local Man-width (- (window-width) 4)))

(advice-add #'Man-update-manpage :before #'my/man-fix-width)

(use-package pass
  :straight t
  :commands (pass)
  :init
  (my-leader-def "ak" #'pass)
  :config
  (setq pass-show-keybindings nil))

(use-package docker
  :straight t
  :commands (docker)
  :init
  (my-leader-def "ao" 'docker))

(setq my/selected-docker-directory nil)

(defun my/docker-override-dir (fun &rest args)
  (let ((default-directory (or my/selected-docker-directory default-directory)))
    (setq my/selected-docker-directory nil)
    (apply fun args)))

(with-eval-after-load 'docker
  (advice-add #'docker-compose-run-docker-compose-async :around #'my/docker-override-dir)
  (advice-add #'docker-compose-run-docker-compose :around #'my/docker-override-dir)
  (advice-add #'docker-run-docker-async :around #'my/docker-override-dir)
  (advice-add #'docker-run-docker :around #'my/docker-override-dir))

(defun my/docker-from-dir ()
  (interactive)
  (when (not (boundp 'my/docker-directories))
    (load (concat user-emacs-directory "prodigy-config")))
  (let* ((directories
          (mapcar
           (lambda (el) (cons (format "%-30s %s" (car el) (cdr el)) (cdr el)))
           my/docker-directories))
         (selected-directory
          (cdr (assoc (completing-read "Docker: " directories nil nil "^")
                      directories))))
    (setq my/selected-docker-directory selected-directory)
    (docker)))

(my-leader-def "aO" 'my/docker-from-dir)

(use-package prodigy
  :straight t
  :commands (prodigy)
  :init
  (my-leader-def "ap" 'prodigy)
  :config
  (when (not (boundp 'my/docker-directories))
    (load (concat user-emacs-directory "prodigy-config")))
  (evil-collection-define-key 'normal 'prodigy-view-mode-map
   (kbd "C-h") 'evil-window-left
   (kbd "C-l") 'evil-window-right
   (kbd "C-k") 'evil-window-up
   (kbd "C-j") 'evil-window-down))

(my-leader-def "ah" 'proced)
(add-hook 'proced-mode-hook (lambda ()
                              (visual-line-mode -1)
                              (setq-local truncate-lines t)))

(use-package guix
  :straight t
  :commands (guix)
  :init
  (my-leader-def "ag" 'guix))

(use-package pomidor
  :straight t
  :commands (pomidor)
  :init
  (my-leader-def "aP" #'pomidor)
  :config
  (setq pomidor-sound-tick nil)
  (setq pomidor-sound-tack nil)
  (evil-collection-define-key 'normal 'pomidor-mode-map
    (kbd "q") #'quit-window
    (kbd "Q") #'pomidor-quit
    (kbd "R") #'pomidor-reset
    (kbd "h") #'pomidor-hold
    (kbd "H") #'pomidor-unhold
    (kbd "RET") #'pomidor-stop
    (kbd "M-RET") #'pomidor-break))

(setq calendar-date-style 'iso) ;; YYYY/mm/dd
(setq calendar-week-start-day 1)
(setq calendar-time-display-form '(24-hours ":" minutes))

(setq calendar-latitude 59.9375)
(setq calendar-longitude 30.308611)

(use-package screenshot
  :straight (:repo "tecosaur/screenshot" :host github :files ("screenshot.el"))
  :commands (screenshot)
  :init
  (my-leader-def "S" 'screenshot))

(use-package snow
  :straight (:repo "alphapapa/snow.el" :host github)
  :commands (snow))

(use-package zone
  :ensure nil
  :config
  (setq original-zone-programs (copy-sequence zone-programs)))

(defun my/zone-with-select ()
  (interactive)
  (ivy-read "Zone programs"
            (cl-pairlis
             (cl-mapcar 'symbol-name original-zone-programs)
             original-zone-programs)
            :action (lambda (elem)
                      (setq zone-programs (vector (cdr elem)))
                      (zone))))
