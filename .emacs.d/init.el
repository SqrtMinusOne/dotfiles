;;; -*- lexical-binding: t -*-

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

(setq my/slow-ssh
      (or
       (string= (getenv "IS_TRAMP") "true")))

(setq my/remote-server
      (or (string= (getenv "IS_REMOTE") "true")
          (string= (system-name) "dev-digital")
          (string= (system-name) "violet")
          (string= (system-name) "viridian")))

(setq my/is-termux (string-match-p (rx (* nonl) "com.termux" (* nonl)) (getenv "HOME")))

(setenv "IS_EMACS" "true")

(defmacro with-eval-after-load-norem (file &rest body)
  (declare (indent 1) (debug (form def-body)))
  `(unless my/remote-server
     (with-eval-after-load ,file
       ,@body)))

(setq my/emacs-started nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done))
          (setq my/emacs-started t))

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

(defun my/get-ram-usage-async (callback)
  (let* ((temp-buffer (generate-new-buffer "*ps*"))
         (proc (start-process "ps" temp-buffer "ps"
                              "-p" (number-to-string (emacs-pid)) "-o" "rss")))
    (set-process-sentinel
     proc
     (lambda (process _msg)
       (when (eq (process-status process) 'exit)
         (let* ((output (with-current-buffer temp-buffer
                          (buffer-string)))
                (usage (string-to-number (nth 1 (split-string output "\n")))))
           (ignore-errors
             (funcall callback usage)))
         (kill-buffer temp-buffer))))))

(defun my/ram-usage ()
  (interactive)
  (my/get-ram-usage-async
   (lambda (data)
     (message "%f Gb" (/ (float data) 1024 1024)))))

(use-package conda
  :straight t
  :if (executable-find "conda")
  :config
  (setq conda-anaconda-home (string-replace "/bin/conda" "" (executable-find "conda")))
  (setq conda-env-home-directory (expand-file-name "~/.conda/"))
  (setq conda-env-subdirectory "envs")

  (advice-add 'conda-env-activate :after
              (lambda (&rest _)
                (setenv "EMACS_CONDA_ENV" conda-env-current-name)
                (setenv "INIT_CONDA" "true")))
  (advice-add 'conda-env-deactivate :after
              (lambda (&rest _)
                (setenv "EMACS_CONDA_ENV" nil)
                (setenv "INIT_CONDA" nil)))
  (unless (getenv "CONDA_DEFAULT_ENV")
    (conda-env-activate "general")))

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

(setq auth-source-debug nil)

(let ((private-file (expand-file-name "private.el" user-emacs-directory)))
  (when (file-exists-p private-file)
    (load-file private-file)))

(use-package no-littering
  :straight t)

(setq confirm-kill-emacs 'y-or-n-p)

(use-package general
  :straight t
  :config
  (general-evil-setup))

(use-package which-key
  :config
  (setq which-key-idle-delay 0.3)
  (setq which-key-popup-type 'frame)
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  (set-face-attribute 'which-key-local-map-description-face nil
                      :weight 'bold)
  :straight t)

(defun my/dump-bindings-recursive (prefix &optional level)
  (dolist (key (which-key--get-bindings (kbd prefix)))
    (when level
      (insert (make-string level ? )))
    (insert (apply #'format "%s%s%s\n" key))
    (when (string-match-p
           (rx bos "+" (* nonl))
           (substring-no-properties (elt key 2)))
      (my/dump-bindings-recursive
       (concat prefix " " (substring-no-properties (car key)))
       (+ 2 (or level 0))))))

(defun my/dump-bindings (prefix)
  "Dump keybindings starting with PREFIX in a tree-like form."
  (interactive "sPrefix: ")
  (with-current-buffer (get-buffer-create "bindings")
    (point-max)
    (erase-buffer)
    (save-excursion
      (my/dump-bindings-recursive prefix)))
  (switch-to-buffer-other-window "bindings"))

(use-package evil
  :straight t
  :init
  (setq evil-want-integration t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-keybinding nil)
  (setq evil-search-module 'evil-search)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  (unless (display-graphic-p)
    (setq evil-want-C-i-jump nil))
  :config
  (evil-mode 1)
  ;; (setq evil-respect-visual-line-mode t)
  (evil-set-undo-system 'undo-tree))

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

(use-package evil-matchit
  :straight t
  :disabled
  :config
  (global-evil-matchit-mode 1))

(defun my/evil-ex-search-word-forward-other-window (count &optional symbol)
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     evil-symbol-word-search))
  (save-excursion
    (evil-ex-start-word-search nil 'forward count symbol))
  (other-window 1)
  (evil-ex-search-next))

(general-define-key
 :states '(normal)
 "&" #'my/evil-ex-search-word-forward-other-window)

(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init
   '(eww
     devdocs
     proced
     emms
     pass
     calendar
     dired
     ivy
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
     git-timemachine
     magit
     prodigy
     slime
     forge)))

(use-package avy
  :straight t
  :config
  (setq avy-timeout-seconds 0.5)
  (setq avy-ignored-modes
        '(image-mode doc-view-mode pdf-view-mode exwm-mode))
  (general-define-key
   :states '(normal motion)
   "-" nil
   "--" #'avy-goto-char-2
   "-=" #'avy-goto-symbol-1))

(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(defun my/escape-key ()
  (interactive)
  (evil-ex-nohighlight)
  (keyboard-quit))

(general-define-key
 :keymaps '(normal visual global)
 [escape] #'my/escape-key)

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

(general-def :states '(normal motion emacs)
  "SPC" nil
  "M-SPC" (general-key "SPC"))

(general-def :states '(insert)
  "M-SPC" (general-key "SPC" :state 'normal))

(my-leader-def "?" 'which-key-show-top-level)
(my-leader-def "E" 'eval-expression)

(general-def :states '(insert)
  "<f1> e" #'eval-expression)

(my-leader-def
  "SPC" '(:wk "second level")
  "SPC x" '(:wk "ctl-x")
  "SPC x" ctl-x-map)

(my-leader-def
  "a" '(:which-key "apps"))

(general-def
  :keymaps 'universal-argument-map
  "M-u" 'universal-argument-more)
(general-def
  :keymaps 'override
  :states '(normal motion emacs insert visual)
  "M-u" 'universal-argument)

(my-leader-def
  :infix "P"
  "" '(:which-key "profiler")
  "s" 'profiler-start
  "e" 'profiler-stop
  "p" 'profiler-report)

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

(general-define-key
 :keymaps 'evil-window-map
 "x" 'kill-buffer-and-window
 "d" 'kill-current-buffer)

(winner-mode 1)

(general-define-key
 :keymaps 'evil-window-map
 "u" 'winner-undo
 "U" 'winner-redo)

(my-leader-def
  :infix "b"
  "" '(:which-key "buffers")
  "s" '((lambda () (interactive) (switch-to-buffer (persp-scratch-buffer)))
        :which-key "*scratch*")
  "m" '((lambda () (interactive) (persp-switch-to-buffer "*Messages*"))
        :which-key "*Messages*")
  "l" 'next-buffer
  "h" 'previous-buffer
  "k" 'kill-buffer
  "b" 'persp-ivy-switch-buffer
  "r" 'revert-buffer
  "u" 'ibuffer)

(general-nmap
  "gD" 'xref-find-definitions-other-window
  "gr" 'xref-find-references
  "gd" 'evil-goto-definition)

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
                      (+ (face-attribute 'default :height) 10)))

(defun my/zoom-out ()
  "Decrease font size by 10 points"
  (interactive)
  (set-face-attribute 'default nil
                      :height
                      (- (face-attribute 'default :height) 10)))

;; change font size, interactively
(global-set-key (kbd "C-+") 'my/zoom-in)
(global-set-key (kbd "C-=") 'my/zoom-out)

(unless my/remote-server
  (add-hook 'after-init-hook #'server-start))

(defmacro i3-msg (&rest args)
  `(start-process "emacs-i3-windmove" nil "i3-msg" ,@args))

(defun my/emacs-i3-windmove (dir)
  (let ((other-window (windmove-find-other-window dir)))
    (if (or (null other-window) (window-minibuffer-p other-window))
        (i3-msg "focus" (symbol-name dir))
      (windmove-do-window-select dir))))

(defun my/emacs-i3-direction-exists-p (dir)
  (cl-some (lambda (dir)
          (let ((win (windmove-find-other-window dir)))
            (and win (not (window-minibuffer-p win)))))
        (pcase dir
          ('width '(left right))
          ('height '(up down)))))

(defun my/emacs-i3-move-window (dir)
  (let ((other-window (windmove-find-other-window dir))
        (other-direction (my/emacs-i3-direction-exists-p
                          (pcase dir
                            ('up 'width)
                            ('down 'width)
                            ('left 'height)
                            ('right 'height)))))
    (cond
     ((and other-window (not (window-minibuffer-p other-window)))
      (window-swap-states (selected-window) other-window))
     (other-direction
      (evil-move-window dir))
     (t (i3-msg "move" (symbol-name dir))))))

(defun my/emacs-i3-resize-window (dir kind value)
  (if (or (one-window-p)
          (not (my/emacs-i3-direction-exists-p dir)))
      (i3-msg "resize" (symbol-name kind) (symbol-name dir)
              (format "%s px or %s ppt" value value))
    (setq value (/ value 2))
    (pcase kind
      ('shrink
       (pcase dir
         ('width
          (evil-window-decrease-width value))
         ('height
          (evil-window-decrease-height value))))
      ('grow
       (pcase dir
         ('width
          (evil-window-increase-width value))
         ('height
          (evil-window-increase-height value)))))))

(use-package transpose-frame
  :straight t
  :commands (transpose-frame))

(defun my/emacs-i3-integration (command)
  (pcase command
    ((rx bos "focus")
     (my/emacs-i3-windmove
      (intern (elt (split-string command) 1))))
    ((rx bos "move")
     (my/emacs-i3-move-window
      (intern (elt (split-string command) 1))))
    ((rx bos "resize")
     (my/emacs-i3-resize-window
       (intern (elt (split-string command) 2))
       (intern (elt (split-string command) 1))
       (string-to-number (elt (split-string command) 3))))
    ("layout toggle split" (transpose-frame))
    ("split h" (evil-window-split))
    ("split v" (evil-window-vsplit))
    ("kill" (evil-quit))
    (- (i3-msg command))))

(use-package aggressive-indent
  :commands (aggressive-indent-mode)
  :straight t)

(setq my/trailing-whitespace-modes '(markdown-mode))

(require 'cl-extra)

(add-hook 'before-save-hook
          (lambda ()
            (unless (cl-some #'derived-mode-p my/trailing-whitespace-modes)
              (delete-trailing-whitespace))))

(setq tab-always-indent nil)

(setq-default default-tab-width 4)
(setq-default tab-width 4)
(setq-default evil-indent-convert-tabs nil)
(setq-default indent-tabs-mode nil)
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

(use-package yasnippet-snippets
  :disabled
  :straight t)

(use-package yasnippet
  :straight t
  :config
  (setq yas-snippet-dirs
        `(,(concat (expand-file-name user-emacs-directory) "snippets")
          ;; yasnippet-snippets-dir
          ))
  (setq yas-triggers-in-field t)
  (yas-global-mode 1)
  (my-leader-def
    :keymaps 'yas-minor-mode-map
    :infix "es"
    "" '(:wk "yasnippet")
    "n" #'yas-new-snippet
    "s" #'yas-insert-snippet
    "v" #'yas-visit-snippet-file))

(general-imap "M-TAB" 'company-yasnippet)

(use-package smartparens
  :straight t)

(use-package expand-region
  :straight t
  :commands (er/expand-region)
  :init
  (general-nmap "+" 'er/expand-region))

(use-package visual-fill-column
  :straight t
  :commands (visual-fill-column-mode)
  :config
  (add-hook 'visual-fill-column-mode-hook
            (lambda () (setq visual-fill-column-center-text t))))

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

(my-leader-def
  "p" '(:keymap projectile-command-map :which-key "projectile"))

(general-nmap "C-p" 'counsel-projectile-find-file)

(use-package magit
  :straight t
  :commands (magit-status magit-file-dispatch)
  :init
  (my-leader-def
    "m" 'magit
    "M" 'magit-file-dispatch)
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

(use-package forge
  :after magit
  :straight t
  :config
  (add-to-list 'forge-alist '("gitlab.etu.ru"
                              "gitlab.etu.ru/api/v4"
                              "gitlab.etu.ru"
                              forge-gitlab-repository)))

(use-package git-gutter
  :straight t
  :if (not my/slow-ssh)
  :config
  (global-git-gutter-mode +1))

(use-package git-timemachine
  :straight t
  :commands (git-timemachine))

(use-package editorconfig
  :straight t
  :config
  (unless my/slow-ssh (editorconfig-mode 1))
  (add-to-list 'editorconfig-indentation-alist
               '(emmet-mode emmet-indentation)))

(recentf-mode 1)

(save-place-mode 1)

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
               dap-switch-stack-frame
               my/dap-switch-stack-frame
               dap-switch-session
               dap-switch-thread
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
               proced-sort-interactive
               perspective-exwm-switch-perspective
               my/persp-ivy-switch-buffer-other-window
               lsp-execute-code-action
               dired-recent-open))
  ;; Do not use prescient in find-file
  (ivy--alist-set 'ivy-sort-functions-alist #'read-file-name-internal #'ivy-sort-file-function-default))

(my-leader-def
  :infix "f"
  "" '(:which-key "various completions")'
  ;; "b" 'counsel-switch-buffer
  "b" 'persp-ivy-switch-buffer
  "e" 'conda-env-activate
  "f" 'project-find-file
  "c" 'counsel-yank-pop
  "a" 'counsel-rg
  "A" 'counsel-ag)

(general-define-key
 :states '(insert normal)
 "C-y" 'counsel-yank-pop)

(my-leader-def "SPC SPC" 'ivy-resume)
(my-leader-def "s" 'swiper-isearch
  "S" 'swiper-all)

(general-define-key
 :keymaps '(ivy-minibuffer-map swiper-map)
 "M-j" 'ivy-next-line
 "M-k" 'ivy-previous-line
 "<C-return>" 'ivy-call
 "M-RET" 'ivy-immediate-done
 [escape] 'minibuffer-keyboard-quit)

(use-package company
  :straight t
  :config
  (global-company-mode)
  (setq company-idle-delay 0.125)
  (setq company-dabbrev-downcase nil)
  (setq company-show-numbers t))

(general-imap "C-SPC" 'company-complete)

(use-package company-box
  :straight t
  :if (display-graphic-p)
  :after (company)
  :hook (company-mode . company-box-mode))

(use-package helpful
  :straight t
  :commands (helpful-callable
             helpful-variable
             helpful-key
             helpful-macro
             helpful-function
             helpful-command))

(my-leader-def
  "h" '(:keymap help-map :which-key "help"))

(my-leader-def
  :infix "h"
  "" '(:which-key "help")
  "h" '(:keymap help-map :which-key "help-map")
  "f" 'helpful-function
  "k" 'helpful-key
  "v" 'helpful-variable
  "o" 'helpful-symbol)

(general-define-key
 :keymaps 'help-map
 "f" 'helpful-function
 "k" 'helpful-key
 "v" 'helpful-variable
 "o" 'helpful-symbol)

(use-package wakatime-mode
  :straight (:host github :repo "SqrtMinusOne/wakatime-mode")
  :if (not (or my/is-termux my/remote-server))
  :config
  (setq wakatime-ignore-exit-codes '(0 1 102))
  (advice-add 'wakatime-init :after (lambda () (setq wakatime-cli-path "/home/pavel/bin/wakatime-cli")))
  ;; (setq wakatime-cli-path (executable-find "wakatime"))
  (global-wakatime-mode))

(use-package request
  :straight t)

(use-package activity-watch-mode
  :straight t
  :if (not (or my/is-termux my/remote-server))
  :config
  (global-activity-watch-mode))

(unless my/is-termux
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1))

;; (set-frame-parameter (selected-frame) 'alpha '(90 . 90))
;; (add-to-list 'default-frame-alist '(alpha . (90 . 90)))

;; (global-prettify-symbols-mode)

(setq use-dialog-box nil)

(setq inhibit-startup-screen t)

(setq visible-bell 0)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq make-pointer-invisible t)

(show-paren-mode 1)

(global-hl-line-mode 1)

(global-display-line-numbers-mode 1)
(line-number-mode nil)
(setq display-line-numbers-type 'visual)
(column-number-mode)

(setq word-wrap 1)
(global-visual-line-mode 1)

(setq-default frame-title-format
              '(""
                "emacs"
                ;; (:eval
                ;;  (let ((project-name (projectile-project-name)))
                ;;    (if (not (string= "-" project-name))
                ;;        (format ":%s@%s" project-name (system-name))
                ;;      (format "@%s" (system-name)))))
                ))

(use-package olivetti
  :straight t
  :config
  (setq-default olivetti-body-width 86))

(use-package keycast
  :straight t
  :config
  (define-minor-mode keycast-mode
    "Keycast mode"
    :global t
    (if keycast-mode
        (progn
          (add-to-list 'global-mode-string '("" keycast-mode-line " "))
          (add-hook 'pre-command-hook 'keycast--update t) )
      (remove-hook 'pre-command-hook 'keycast--update)
      (setq global-mode-string (delete '("" keycast-mode-line " ") global-mode-string)))))

(use-package doom-themes
  :straight t
  :if (not my/is-termux)
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (if my/remote-server
      (load-theme 'doom-gruvbox t)
    (load-theme 'doom-palenight t))
  (doom-themes-visual-bell-config)
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config))

(defun my/color-join (r g b)
  "Build a color from R G B.
Inverse of `color-values'."
  (format "#%02x%02x%02x"
          (ash r -8)
          (ash g -8)
          (ash b -8)))

(defun my/color-blend (c1 c2 &optional alpha)
  "Blend the two colors C1 and C2 with ALPHA.
C1 and C2 are in the format of `color-values'.
ALPHA is a number between 0.0 and 1.0 which corresponds to the
influence of C1 on the result."
  (setq alpha (or alpha 0.5))
  (apply #'my/color-join
         (cl-mapcar
          (lambda (x y)
            (round (+ (* x alpha) (* y (- 1 alpha)))))
          c1 c2)))

(deftheme my-theme-1)

(defvar my/doom-theme-update-colors-hook nil)

(defmacro my/use-doom-colors (&rest data)
  `(progn
     (add-hook 'my/doom-theme-update-colors-hook
               (lambda ()
                 (custom-theme-set-faces
                  'my-theme-1
                  ,@(cl-loop for i in data collect
                             `(,'\`
                               (,(car i)
                                ((t (,@(cl-loop for (key value) on (cdr i) by #'cddr
                                                append `(,key (,'\, ,value))))))))))))
     (when (and (fboundp 'doom-color) my/emacs-started)
       (my/update-my-theme))))

(defun my/update-my-theme (&rest _)
  (run-hooks 'my/doom-theme-update-colors-hook)
  (enable-theme 'my-theme-1))

(unless my/is-termux
  (advice-add 'load-theme :after #'my/update-my-theme)
  (when (fboundp 'doom-color)
    (my/update-my-theme))
  (add-hook 'emacs-startup-hook #'my/update-my-theme))

(my/use-doom-colors
 (tab-bar-tab :background (doom-color 'bg)
              :foreground (doom-color 'yellow)
              :underline (doom-color 'yellow))
 (tab-bar :background nil :foreground nil))

(use-package auto-dim-other-buffers
  :straight t
  :if (display-graphic-p)
  :config
  (auto-dim-other-buffers-mode t)
  (my/use-doom-colors
   (auto-dim-other-buffers-face
    :background (color-darken-name (doom-color 'bg) 3))))

(defun my/toggle-dark-light-theme ()
  (interactive)
  (let ((is-dark (member 'doom-palenight custom-enabled-themes)))
    (if is-dark
        (progn
          (load-theme 'doom-one-light t)
          (disable-theme 'doom-palenight))
      (load-theme 'doom-palenight t)
      (disable-theme 'doom-one-light))))

(with-eval-after-load 'ansi-color
  (my/use-doom-colors
   (ansi-color-black
    :foreground (doom-color 'base2) :background (doom-color 'base0))
   (ansi-color-red
    :foreground (doom-color 'red) :background (doom-color 'red))
   (ansi-color-green
    :foreground (doom-color 'green) :background (doom-color 'green))
   (ansi-color-yellow
    :foreground (doom-color 'yellow) :background (doom-color 'yellow))
   (ansi-color-blue
    :foreground (doom-color 'dark-blue) :background (doom-color 'dark-blue))
   (ansi-color-magenta
    :foreground (doom-color 'violet) :background (doom-color 'violet))
   (ansi-color-cyan
    :foreground (doom-color 'dark-cyan) :background (doom-color 'dark-cyan))
   (ansi-color-white
    :foreground (doom-color 'base8) :background (doom-color 'base8))
   (ansi-color-bright-black
    :foreground (doom-color 'base5) :background (doom-color 'base5))
   (ansi-color-bright-red
    :foreground (doom-color 'orange) :background (doom-color 'orange))
   (ansi-color-bright-green
    :foreground (doom-color 'teal) :background (doom-color 'teal))
   (ansi-color-bright-yellow
    :foreground (doom-color 'yellow) :background (doom-color 'yellow))
   (ansi-color-bright-blue
    :foreground (doom-color 'blue) :background (doom-color 'blue))
   (ansi-color-bright-magenta
    :foreground (doom-color 'magenta) :background (doom-color 'magenta))
   (ansi-color-bright-cyan
    :foreground (doom-color 'cyan) :background (doom-color 'cyan))
   (ansi-color-bright-white
    :foreground (doom-color 'fg) :background (doom-color 'fg))))

(when (display-graphic-p)
  (if (x-list-fonts "JetBrainsMono Nerd Font")
      (set-frame-font "JetBrainsMono Nerd Font 10" nil t)
    (message "Install JetBrainsMono Nerd Font!")))

(use-package ligature
  :straight (:host github :repo "mickeynp/ligature.el")
  :if (display-graphic-p)
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
     haskell-mode
     web-mode)
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
  :if (display-graphic-p)
  :straight t)

(use-package highlight-indent-guides
  :straight t
  :if (not (or my/remote-server))
  :hook ((prog-mode . highlight-indent-guides-mode)
         (LaTeX-mode . highlight-indent-guides-mode))
  :config
  (setq highlight-indent-guides-method 'bitmap)
  (setq highlight-indent-guides-bitmap-function 'highlight-indent-guides--bitmap-line))

(use-package rainbow-delimiters
  :straight t
  :hook ((prog-mode . rainbow-delimiters-mode)))

(use-package rainbow-mode
  :commands (rainbow-mode)
  :straight t)

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :straight t)

(use-package doom-modeline
  :straight t
  ;; :if (not (display-graphic-p))
  :init
  (setq doom-modeline-env-enable-python nil)
  (setq doom-modeline-env-enable-go nil)
  (setq doom-modeline-buffer-encoding 'nondefault)
  (setq doom-modeline-hud t)
  (setq doom-modeline-persp-icon nil)
  (setq doom-modeline-persp-name nil)
  :config
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-irc nil)
  (setq doom-modeline-buffer-state-icon nil)
  (doom-modeline-mode 1))

(use-package perspective
  :straight t
  :init
  ;; (setq persp-show-modestring 'header)
  (setq persp-sort 'created)
  (setq persp-suppress-no-prefix-key-warning t)
  :config
  (persp-mode)
  (my-leader-def "x" '(:keymap perspective-map :which-key "perspective"))
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

(defun my/persp-ivy-switch-buffer-other-window (arg)
  (interactive "P")
  (declare-function ivy-switch-buffer-other-window "ivy.el")
  (persp--switch-buffer-ivy-counsel-helper
   arg
   (lambda ()
     (ivy-read "Switch to buffer in other window: " #'internal-complete-buffer
               :keymap ivy-switch-buffer-map
               :preselect (buffer-name (other-buffer (current-buffer)))
               :action #'ivy--switch-buffer-other-window-action
               :matcher #'ivy--switch-buffer-matcher
               :caller 'ivy-switch-buffer))))

(with-eval-after-load 'perspective
  (general-define-key
   :keymaps 'perspective-map
   "m" #'my/persp-move-window-and-switch
   "f" #'my/persp-copy-window-and-switch))

(defmacro my/command-in-persp (command-name persp-name workspace-index &rest args)
  `'((lambda ()
       (interactive)
       (when (and ,workspace-index (fboundp #'exwm-workspace-switch-create))
         (exwm-workspace-switch-create ,workspace-index))
       (persp-switch ,persp-name)
       (delete-other-windows)
       ,@args)
     :wk ,command-name))

(use-package treemacs
  :straight t
  :defer t
  :config
  ;; (setq treemacs-follow-mode nil)
  ;; (setq treemacs-follow-after-init nil)
  (setq treemacs-space-between-root-nodes nil)
  ;; (treemacs-git-mode 'extended)
  ;; (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?)
  (general-define-key
   :keymaps 'treemacs-mode-map
   [mouse-1] #'treemacs-single-click-expand-action
   "M-l" #'treemacs-root-down
   "M-h" #'treemacs-root-up
   "q" #'treemacs-quit)
  (general-define-key
   :keymaps 'treemacs-mode-map
   :states '(normal emacs)
   "q" 'treemacs-quit))

(use-package treemacs-evil
  :after (treemacs evil)
  :straight t)

(use-package lsp-mode
  :straight t
  :if (not (or my/slow-ssh my/is-termux my/remote-server))
  :hook (
         (typescript-mode . lsp)
         (js-mode . lsp)
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
  :init
  (setq lsp-keymap-prefix nil)
  :config
  (setq lsp-idle-delay 1)
  (setq lsp-eslint-server-command '("node" "/home/pavel/.emacs.d/.cache/lsp/eslint/unzipped/extension/server/out/eslintServer.js" "--stdio"))
  (setq lsp-eslint-run "onSave")
  (setq lsp-signature-render-documentation nil)
  ;; (lsp-headerline-breadcrumb-mode nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-diagnostics-enable nil)
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
  :after (lsp)
  :straight t
  :commands lsp-treemacs-errors-list)

(my-leader-def
  :infix "l"
  "" '(:which-key "lsp")
  "d" 'lsp-ui-peek-find-definitions
  "r" 'lsp-rename
  "u" 'lsp-ui-peek-find-references
  "s" 'lsp-ui-find-workspace-symbol
  "l" 'lsp-execute-code-action
  "e" 'list-flycheck-errors)

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
  :if (not my/remote-server)
  :hook ((typescript-mode . my/tree-sitter-if-not-mmm)
         (js-mode . my/tree-sitter-if-not-mmm)
         (python-mode . tree-sitter-mode)
         (python-mode . tree-sitter-hl-mode)
         (csharp-mode . tree-sitter-mode)))

(use-package tree-sitter-langs
  :straight t
  :after tree-sitter)

(use-package dap-mode
  :straight t
  :commands (dap-debug)
  :init
  (setq lsp-enable-dap-auto-configure nil)
  :config

  (setq dap-ui-variable-length 100)
  (setq dap-auto-show-output nil)
  (require 'dap-node)
  (dap-node-setup)

  (require 'dap-chrome)
  (dap-chrome-setup)

  (require 'dap-python)
  (require 'dap-php)

  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1))

(with-eval-after-load 'dap-mode
  (defmacro my/define-dap-ui-window-toggler (name)
    `(defun ,(intern (concat "my/dap-ui-toggle-" name)) ()
       ,(concat "Toggle DAP " name "buffer")
       (interactive)
       (if-let (window (get-buffer-window ,(intern (concat "dap-ui--" name "-buffer"))))
           (quit-window nil window)
         (,(intern (concat "dap-ui-" name))))))

  (my/define-dap-ui-window-toggler "locals")
  (my/define-dap-ui-window-toggler "expressions")
  (my/define-dap-ui-window-toggler "sessions")
  (my/define-dap-ui-window-toggler "breakpoints")
  (my/define-dap-ui-window-toggler "repl"))

(defhydra my/dap-hydra (:color pink :hint nil :foreign-keys run)
  "
^Stepping^         ^UI^                     ^Switch^                   ^Breakpoints^         ^Debug^                     ^Expressions
^^^^^^^^------------------------------------------------------------------------------------------------------------------------------------------
_n_: Next          _uc_: Controls           _ss_: Session              _bb_: Toggle          _dd_: Debug                 _ee_: Eval
_i_: Step in       _ue_: Expressions        _st_: Thread               _bd_: Delete          _dr_: Debug recent          _er_: Eval region
_o_: Step out      _ul_: Locals             _sf_: Stack frame          _ba_: Add             _dl_: Debug last            _es_: Eval thing at point
_c_: Continue      _ur_: REPL               _su_: Up stack frame       _bc_: Set condition   _de_: Edit debug template   _ea_: Add expression
_r_: Restart frame _uo_: Output             _sd_: Down stack frame     _bh_: Set hit count   _Q_:  Disconnect            _ed_: Remove expression
                 _us_: Sessions           _sF_: Stack frame filtered _bl_: Set log message                           _eu_: Refresh expressions
                 _ub_: Breakpoints                                                                               "

  ("n" dap-next)
  ("i" dap-step-in)
  ("o" dap-step-out)
  ("c" dap-continue)
  ("r" dap-restart-frame)
  ("uc" dap-ui-controls-mode)
  ("ue" my/dap-ui-toggle-expressions)
  ("ul" my/dap-ui-toggle-locals)
  ("ur" my/dap-ui-toggle-repl)
  ("uo" dap-go-to-output-buffer)
  ("us" my/dap-ui-toggle-sessions)
  ("ub" my/dap-ui-toggle-breakpoints)
  ("ss" dap-switch-session)
  ("st" dap-switch-thread)
  ("sf" dap-switch-stack-frame)
  ("sF" my/dap-switch-stack-frame)
  ("su" dap-up-stack-frame)
  ("sd" dap-down-stack-frame)
  ("bb" dap-breakpoint-toggle)
  ("ba" dap-breakpoint-add)
  ("bd" dap-breakpoint-delete)
  ("bc" dap-breakpoint-condition)
  ("bh" dap-breakpoint-hit-condition)
  ("bl" dap-breakpoint-log-message)
  ("dd" dap-debug)
  ("dr" dap-debug-recent)
  ("dl" dap-debug-last)
  ("de" dap-debug-edit-template)
  ("ee" dap-eval)
  ("ea" dap-ui-expressions-add)
  ("er" dap-eval-region)
  ("es" dap-eval-thing-at-point)
  ("ed" dap-ui-expressions-remove)
  ("eu" dap-ui-expressions-refresh)
  ("q" nil "quit" :color blue)
  ("Q" dap-disconnect :color red))

(my-leader-def "d" #'my/dap-hydra/body)

(defvar my/dap-mode-buffer-fixed nil)

(with-eval-after-load 'dap-mode
  (defmacro my/define-dap-tree-buffer-fixer (buffer-var buffer-name)
    `(defun ,(intern (concat "my/fix-dap-ui-" buffer-name "-buffer")) (&rest _)
       (with-current-buffer ,buffer-var
         (unless my/dap-mode-buffer-fixed
           (toggle-truncate-lines 1)
           (doom-modeline-set-modeline 'info)
           (setq-local my/dap-mode-buffer-fixed t)))))

  (my/define-dap-tree-buffer-fixer dap-ui--locals-buffer "locals")
  (my/define-dap-tree-buffer-fixer dap-ui--expressions-buffer "expressions")
  (my/define-dap-tree-buffer-fixer dap-ui--sessions-buffer "sessions")
  (my/define-dap-tree-buffer-fixer dap-ui--breakpoints-buffer "breakpoints")

  (advice-add 'dap-ui-locals :after #'my/fix-dap-ui-locals-buffer)
  (advice-add 'dap-ui-expressions :after #'my/fix-dap-ui-expressions-buffer)
  (advice-add 'dap-ui-sessions :after #'my/fix-dap-ui-sessions-buffer)
  (advice-add 'dap-ui-breakpoints :after #'my/fix-dap-ui-breakpoints-buffer))

(defun my/clear-bad-window-parameters ()
  "Clear window parameters that interrupt my workflow."
  (interactive)
  (let ((window (get-buffer-window (current-buffer))))
    (set-window-parameter window 'no-delete-other-windows nil)))

(defun my/dap-yank-value-at-point (node)
  (interactive (list (treemacs-node-at-point)))
  (kill-new (message (plist-get (button-get node :item) :value))))

(defun my/dap-display-value (node)
  (interactive (list (treemacs-node-at-point)))
  (let ((value (plist-get (button-get node :item) :value)))
    (when value
      (let ((buffer (generate-new-buffer "dap-value")))
        (with-current-buffer buffer
          (insert value))
        (select-window (display-buffer buffer))))))

(with-eval-after-load 'dap-mode
  (setq my/dap-stack-frame-filters
        `(("node_modules,node:internal" . ,(rx (or "node_modules" "node:internal")))
          ("node_modules" . ,(rx (or "node_modules")))
          ("node:internal" . ,(rx (or "node:internal")))))

  (setq my/dap-stack-frame-current-filter (cdar my/dap-stack-frame-filters))

  (defun my/dap-stack-frame-filter-set ()
    (interactive)
    (setq my/dap-stack-frame-current-filter
          (cdr
           (assoc
            (completing-read "Filter: " my/dap-stack-frame-filters)
            my/dap-stack-frame-filters))))

  (defun my/dap-stack-frame-filter (frame)
    (when-let (path (dap--get-path-for-frame frame))
      (not (string-match my/dap-stack-frame-current-filter path)))))

(defun my/dap-switch-stack-frame ()
  "Switch stackframe by selecting another stackframe stackframes from current thread."
  (interactive)
  (when (not (dap--cur-session))
    (error "There is no active session"))

  (-if-let (thread-id (dap--debug-session-thread-id (dap--cur-session)))
      (-if-let (stack-frames
                (gethash
                 thread-id
                 (dap--debug-session-thread-stack-frames (dap--cur-session))))
          (let* ((index 0)
                 (stack-framces-filtered
                  (-filter
                   #'my/dap-stack-frame-filter
                   stack-frames))
                 (new-stack-frame
                  (dap--completing-read
                   "Select active frame: "
                   stack-framces-filtered
                   (-lambda ((frame &as &hash "name"))
                     (if-let (frame-path (dap--get-path-for-frame frame))
                         (format "%s: %s (in %s)"
                                 (cl-incf index) name frame-path)
                       (format "%s: %s" (cl-incf index) name)))
                   nil
                   t)))
            (dap--go-to-stack-frame (dap--cur-session) new-stack-frame))
        (->> (dap--cur-session)
             dap--debug-session-name
             (format "Current session %s is not stopped")
             error))
    (error "No thread is currently active %s" (dap--debug-session-name (dap--cur-session)))))

(defun my/exwm-perspective-find-buffer (path)
  "Find a buffer with PATH in all EXWM perspectives.

Returns (<buffer> . <workspace-index>) or nil."
  (let* ((buf (cl-loop for buf being buffers
                       if (and (buffer-file-name buf)
                               (f-equal-p (buffer-file-name buf) path))
                       return buf))
         (target-workspace
          (and buf
               (cl-loop for frame in exwm-workspace--list
                        if (with-selected-frame frame
                             (cl-loop for persp-name being the hash-keys of (perspectives-hash)
                                      if (member buf (persp-buffers
                                                      (gethash persp-name (perspectives-hash))))
                                      return persp-name))
                        return (cl-position frame exwm-workspace--list)))))
    (when target-workspace (cons buf target-workspace))))

(defun my/dap--go-to-stack-frame-override (debug-session stack-frame)
  "Make STACK-FRAME the active STACK-FRAME of DEBUG-SESSION."
  (with-lsp-workspace (dap--debug-session-workspace debug-session)
    (when stack-frame
      (-let* (((&hash "line" line "column" column "name" name) stack-frame)
              (path (dap--get-path-for-frame stack-frame)))
        (setf (dap--debug-session-active-frame debug-session) stack-frame)
        ;; If we have a source file with path attached, open it and
        ;; position the point in the line/column referenced in the
        ;; stack trace.
        (if (and path (file-exists-p path))
            (progn
              (let ((exwm-target (my/exwm-perspective-find-buffer path)))
                (if exwm-target
                    (progn
                      (unless (= (cdr exwm-target) exwm-workspace-current-index)
                        (exwm-workspace-switch (cdr exwm-target)))
                      (persp-switch-to-buffer (car exwm-target)))
                  (select-window (get-mru-window (selected-frame) nil))
                  (find-file path)))
              (goto-char (point-min))
              (forward-line (1- line))
              (forward-char column))
          (message "No source code for %s. Cursor at %s:%s." name line column))))
    (run-hook-with-args 'dap-stack-frame-changed-hook debug-session)))

(with-eval-after-load 'exwm
  (with-eval-after-load 'dap-mode
    (advice-add #'dap--go-to-stack-frame :override #'my/dap--go-to-stack-frame-override)))

;; (advice-remove #'dap--go-to-stack-frame #'my/dap--go-to-stack-frame-override)

(with-eval-after-load 'dap-mode
  (dap-register-debug-template
   "Node::Nest.js"
   (list :type "node"
         :request "attach"
         :name "Node::Attach"
         :port 9229
         :outFiles ["${workspaceFolder}/dist/**/*.js"]
         :sourceMaps t
         :program "${workspaceFolder}/src/app.ts"))
  (dap-register-debug-template
   "Node::Babel"
   (list :type "node"
         :request "attach"
         :name "Node::Attach"
         :port 9229
         :program "${workspaceFolder}/dist/bin/www.js")))

(use-package reformatter
  :straight t)

(defun my/copilot-tab ()
  (interactive)
  (or (copilot-accept-completion)
      (when (my/should-run-emmet-p) (my/emmet-or-tab))
      (when (and (eq evil-state 'normal)
                 (or hs-minor-mode outline-minor-mode))
        (evil-toggle-fold)
        t)
      (indent-for-tab-command)))

(use-package copilot
  :straight (:host github :repo "SqrtMinusOne/copilot.el" :files ("dist" "*.el"))
  :commands (copilot-mode)
  :if (not my/remote-server)
  :init
  (add-hook 'prog-mode-hook #'copilot-mode)
  :config
  (general-define-key
   :keymaps 'company-active-map
   "<backtab>" #'my/copilot-tab)
  (general-define-key
   :keymaps 'copilot-mode-map
   "<tab>" #'my/copilot-tab
   "M-j" #'copilot-accept-completion-by-line
   "M-l" #'copilot-accept-completion-by-word)
  (setq copilot-lispy-integration t))

(defun my/set-smartparens-indent (mode)
  (sp-local-pair mode "{" nil :post-handlers '(("|| " "SPC") ("||\n[i]" "RET")))
  (sp-local-pair mode "[" nil :post-handlers '(("|| " "SPC") ("||\n[i]" "RET")))
  (sp-local-pair mode "(" nil :post-handlers '(("|| " "SPC") ("||\n[i]" "RET"))))

(defun my/set-flycheck-eslint()
  "Override flycheck checker with eslint."
  (setq-local lsp-diagnostic-package :none)
  (setq-local flycheck-checker 'javascript-eslint))

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
    :keymaps '(js-mode-map web-mode-map typescript-mode-map vue-mode-map svelte-mode-map)
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
  (my/set-smartparens-indent 'web-mode))

(setq my/web-mode-lsp-extensions
      `(,(rx ".svelte" eos)
        ,(rx ".vue" eos)))

(defun my/web-mode-lsp ()
  (when (seq-some
         (lambda (regex) (string-match-p regex (buffer-name)))
         my/web-mode-lsp-extensions)
    (lsp-deferred)))

(add-hook 'web-mode-hook #'my/web-mode-lsp)

(defun my/web-mode-vue-setup (&rest _)
  (when (string-match-p (rx ".vue" eos) (buffer-name))
    (setq-local web-mode-script-padding 0)
    (setq-local web-mode-style-padding 0)
    (setq-local create-lockfiles nil)))

(add-hook 'web-mode-hook 'my/web-mode-vue-setup)
(add-hook 'editorconfig-after-apply-functions 'my/web-mode-vue-setup)

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
  (setq plantuml-executable-path "/home/pavel/.guix-extra-profiles/emacs/emacs/bin/plantuml")
  (setq plantuml-default-exec-mode 'executable)
  (setq plantuml-indent-level 2)
  (setq my/plantuml-indent-regexp-return "^\s*return\s+.+$")
  (add-to-list
   'plantuml-indent-regexp-end
   my/plantuml-indent-regexp-return)
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  (add-to-list 'auto-mode-alist '("\\.uml\\'" . plantuml-mode))
  (add-hook 'plantuml-mode-hook #'smartparens-mode))

(general-nmap
  :keymaps 'plantuml-mode-map
  "RET" 'plantuml-preview)

(use-package subed
  :straight (:host github :repo "rndusr/subed" :files ("subed/*.el")
                   :build (:not native-compile))
  :config
  (general-define-key
   :keymaps '(subed-mode-map subed-vtt-mode-map)
   :states '(normal)
   "gp" #'subed-mpv-toggle-pause))

(use-package langtool
  :straight t
  :commands (langtool-check)
  :config
  (setq langtool-language-tool-server-jar "/home/pavel/bin/LanguageTool-5.7/languagetool-server.jar")
  (setq langtool-mother-tongue "ru")
  (setq langtool-default-language "en-US"))

(my-leader-def
  :infix "L"
  "" '(:which-key "languagetool")
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

(use-package flycheck-package
  :straight t
  :after flycheck
  :config
  (flycheck-package-setup))

(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
;; (add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)
(add-hook 'emacs-lisp-mode-hook #'lispy-mode)

(use-package slime
  :straight t
  :commands (slime)
  :config
  (setq inferior-lisp-program "sbcl")
  (add-hook 'slime-repl-mode 'smartparens-mode))

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
  :after clojure-mode
  :straight t)

(use-package hy-mode
  :straight t
  :mode "\\.hy\\'"
  :config
  (add-hook 'hy-mode-hook #'lispy-mode)
  (add-hook 'hy-mode-hook #'aggressive-indent-mode))

(use-package geiser
  :straight t
  :commands (geiser run-geiser)
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

(use-package ein
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

(use-package json-mode
  :straight t
  :mode "\\.json\\'"
  :config
  (add-hook 'json-mode #'smartparens-mode)
  (add-hook 'json-mode #'hs-minor-mode)
  (my/set-smartparens-indent 'json-mode))

(use-package csv-mode
  :straight t
  :mode "\\.csv\\'")

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
  :config
  (add-hook 'jenkinsfile-mode-hook #'smartparens-mode)
  (my/set-smartparens-indent 'jenkinsfile-mode))

(use-package crontab-mode
  :straight t)

(add-hook 'sh-mode-hook #'smartparens-mode)

(use-package fish-mode
  :straight t
  :mode "\\.fish\\'"
  :config
 (add-hook 'fish-mode-hook #'smartparens-mode))

(use-package x509-mode
  :straight t)

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
  :args `("-l" ,my/sqlformatter-dialect, "-u"))

(my-leader-def
  :keymaps '(sql-mode-map)
  "rr" #'sqlformat-buffer)

(use-package sparql-mode
  :straight t)

(use-package org
  :straight t
  :if (not my/remote-server)
  :defer t
  :init
  (setq org-directory (expand-file-name "~/Documents/org-mode"))
  (unless (file-exists-p org-directory)
    (mkdir org-directory t))
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
              (rainbow-delimiters-mode -1))))

(with-eval-after-load-norem 'org
  (require 'org-crypt)
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  (setq org-crypt-key "C1EC867E478472439CC82410DE004F32AFA00205"))

(defun my/epa--select-keys-around (fun prompt keys)
  (if (= (seq-length keys) 1)
      keys
    (funcall fun prompt keys)))

(with-eval-after-load-norem 'epa
  (advice-add #'epa--select-keys :around #'my/epa--select-keys-around))

(unless my/remote-server
  (setq epa-file-encrypt-to '("DE004F32AFA00205")))

(use-package org-contrib
  :straight (org-contrib
             :type git
             :repo "https://git.sr.ht/~bzg/org-contrib"
             :files (:defaults (:exclude "lisp/org-contacts.el"))
             :build t)
  :after (org)
  :if (not my/remote-server)
  :config
  (require 'ox-extra)
  (require 'ol-notmuch)
  (ox-extras-activate '(latex-header-blocks ignore-headlines)))

(with-eval-after-load 'org
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("sq" . "src sql")))

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
  :after (org)
  :if (not my/remote-server)
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
  :after (org)
  :if (not my/remote-server)
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

(with-eval-after-load 'org
  (setq org-plantuml-executable-path "/home/pavel/.guix-extra-profiles/emacs/emacs/bin/plantuml")
  (setq org-plantuml-exec-mode 'plantuml)
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml)))

(use-package restclient
  :if (not my/remote-server)
  :straight t)

(use-package ob-restclient
  :after (org restclient)
  :if (not my/remote-server)
  :straight t)

(with-eval-after-load-norem 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (sql . t)
     ;; (typescript .t)
     (hy . t)
     (shell . t)
     (plantuml . t)
     (octave . t)
     (jupyter . t)
     (sparql . t)))

  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images))

(with-eval-after-load 'ob-jupyter
  (org-babel-jupyter-override-src-block "python")
  (org-babel-jupyter-override-src-block "hy"))

(add-hook 'org-src-mode-hook
          (lambda ()
            ;; (hs-minor-mode -1)
            ;; (electric-indent-local-mode -1)
            ;; (rainbow-delimiters-mode -1)
            (highlight-indent-guides-mode -1)))

(use-package ob-async
  :straight t
  :after (org)
  :config
  (setq ob-async-no-async-languages-alist '("python" "hy" "jupyter-python" "jupyter-octave" "restclient")))

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

(with-eval-after-load 'jupyter
  (advice-add 'jupyter-org-scalar :around #'my/jupyter-org-scalar-around))

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

(defun my/babel-ansi ()
  (when-let ((beg (org-babel-where-is-src-block-result nil nil)))
    (save-excursion
      (goto-char beg)
      (when (looking-at org-babel-result-regexp)
        (let ((end (org-babel-result-end))
              (ansi-color-context-region nil))
          (ansi-color-apply-on-region beg end))))))

(define-minor-mode org-babel-ansi-colors-mode
  "Apply ANSI color codes to Org Babel results."
  :global t
  :after-hook
  (if org-babel-ansi-colors-mode
      (add-hook 'org-babel-after-execute-hook #'my/babel-ansi)
    (remove-hook 'org-babel-after-execute-hook #'my/babel-ansi)))

(defun my/org-babel-execute-buffer-below (&optional arg)
  (interactive "P")
  (org-babel-eval-wipe-error-buffer)
  (let ((point (point)))
    (org-save-outline-visibility t
      (org-babel-map-executables nil
        (when (>= (point) point)
          (if (memq (org-element-type (org-element-context))
		            '(babel-call inline-babel-call))
              (org-babel-lob-execute-maybe)
            (org-babel-execute-src-block arg)))))))

(defun my/org-babel-execute-buffer-above (&optional arg)
  (interactive "P")
  (org-babel-eval-wipe-error-buffer)
  (let ((point (point)))
    (org-save-outline-visibility t
      (org-babel-map-executables nil
        (when (<= (point) point)
          (if (memq (org-element-type (org-element-context))
		            '(babel-call inline-babel-call))
              (org-babel-lob-execute-maybe)
            (org-babel-execute-src-block arg)))))))

(with-eval-after-load 'org
  (general-define-key
   :keymaps 'org-babel-map
   "B" #'my/org-babel-execute-buffer-below
   "A" #'my/org-babel-execute-buffer-above)

  (my-leader-def
    :keymaps 'org-mode-map
    "SPC b" '(:wk "org-babel")
    "SPC b" org-babel-map))

(defun my/org-prj-dir (path)
  (expand-file-name path (org-entry-get nil "PRJ-DIR" t)))

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
  :if (not my/remote-server)
  :commands (org-present)
  :config
  (general-define-key
   :keymaps 'org-present-mode-keymap
   "<next>" 'my/present-next-with-latex
   "<prior>" 'my/present-prev-with-latex)
  (setq org-present-mode-hook
        (list (lambda ()
                (blink-cursor-mode 0)
                (org-present-big)
                (org-bars-mode -1)
                ;; (org-display-inline-images)
                (org-present-hide-cursor)
                (org-present-read-only)
                (display-line-numbers-mode 0)
                (hide-mode-line-mode +1)
                (setq-local org-format-latex-options
                            (plist-put org-format-latex-options
                                       :scale (* org-present-text-scale my/org-latex-scale 0.5)))
                (org-latex-preview '(16))
                (setq-local olivetti-body-width 60)
                (olivetti-mode 1))))
  (setq org-present-mode-quit-hook
        (list (lambda ()
                (blink-cursor-mode 1)
                (org-present-small)
                (org-bars-mode 1)
                ;; (org-remove-inline-images)
                (org-present-show-cursor)
                (org-present-read-write)
                (display-line-numbers-mode 1)
                (hide-mode-line-mode 0)
                (setq-local org-format-latex-options (plist-put org-format-latex-options :scale my/org-latex-scale))
                (org-latex-preview '(64))
                (olivetti-mode -1)
                (setq-local olivetti-body-width (default-value 'olivetti-body-width))))))

(use-package org-make-toc
  :after (org)
  :if (not my/remote-server)
  :commands
  (org-make-toc
   org-make-toc-insert
   org-make-toc-set
   org-make-toc-at-point)
  :straight t)

(use-package org-attach-screenshot
  :commands (org-attach-screenshot)
  :straight t)

(use-package org-transclusion
  :after org
  :straight (:host github :repo "nobiot/org-transclusion")
  :config
  (add-to-list 'org-transclusion-extensions 'org-transclusion-indent-mode)
  (require 'org-transclusion-indent-mode)
  (general-define-key
   :keymaps '(org-transclusion-map)
   :states '(normal)
   "RET" #'org-transclusion-open-source
   "gr" #'org-transclusion-refresh)
  (general-define-key
   :keymaps '(org-mode-map)
   :states 'normal
   "C-c t a" #'org-transclusion-add
   "C-c t A" #'org-transclusion-add-all
   "C-c t t" #'org-transclusion-mode))

(with-eval-after-load-norem 'org
  (setq org-roam-directory (concat org-directory "/roam"))
  (setq org-agenda-files '("inbox.org"))
  ;; (setq org-default-notes-file (concat org-directory "/notes.org"))
  )

(my-leader-def
  :infix "o"
  "" '(:which-key "org-mode")
  "c" 'org-capture
  "a" 'org-agenda)

(setq org-refile-targets '())
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)

(defun my/generate-inbox-note-name ()
  (format
   "%s/inbox-notes/%s.org"
   org-directory
   (format-time-string "%Y%m%d%H%M%S")))

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
                  "%a\n"))
        ("n" "note" entry (file my/generate-inbox-note-name)
         ,(concat "* %?\n"
                  "/Entered on/ %U"))))

(with-eval-after-load-norem 'org
  (add-to-list 'org-global-properties
               '("Effort_ALL" . "0 0:05 0:10 0:15 0:30 0:45 1:00 2:00 4:00")))

(setq org-log-done 'time)

(use-package org-ql
  :after (org)
  :if (not my/remote-server)
  :straight (:fetcher github
                      :repo "alphapapa/org-ql"
                      :files (:defaults (:exclude "helm-org-ql.el"))))

(defun my/org-scheduled-get-time ()
  (let ((scheduled (org-get-scheduled-time (point))))
    (if scheduled
        (format-time-string "%Y-%m-%d" scheduled)
      "")))

(setq org-agenda-hide-tags-regexp (rx (or "org" "log" "log_here")))

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
                      (org-agenda-prefix-format " %i %-12:c %-12(my/org-scheduled-get-time)")))))))

(use-package org-journal
  :straight t
  :if (not my/remote-server)
  :init
  (my-leader-def
    :infix "oj"
    "" '(:which-key "org-journal")
    "j" 'org-journal-new-entry
    "o" 'org-journal-open-current-journal-file
    "s" 'org-journal-tags-status)
  :after org
  :config
  (setq org-journal-dir (concat org-directory "/journal"))
  (setq org-journal-file-type 'weekly)
  (setq org-journal-file-format "%Y-%m-%d.org")
  (setq org-journal-date-format "%A, %Y-%m-%d")
  (setq org-journal-enable-encryption t))

(use-package org-journal-tags
  :straight (:host github :repo "SqrtMinusOne/org-journal-tags")
  :after (org-journal)
  :if (not my/remote-server)
  :config
  (org-journal-tags-autosync-mode)
  (general-define-key
   :keymaps 'org-journal-mode-map
   "C-c t" #'org-journal-tags-insert-tag))

(use-package request
  :straight t
  :defer t)

(defvar my/weather-last-time 0)
(defvar my/weather-value nil)

(defun my/weather-get ()
  (when (> (- (time-convert nil 'integer) my/weather-last-time)
           (* 60 5))
    (request (format "https://wttr.in/%s" my/location)
      :params '(("format" . "%l:%20%C%20%t%20%w%20%p"))
      :sync t
      :parser (lambda () (url-unhex-string (buffer-string)))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setq my/weather-value data)
                  (setq my/weather-last-time (time-convert nil 'integer))))
      :error
      (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                     (message "Got error: %S" error-thrown)))))
  my/weather-value)

(defun my/set-journal-header ()
  (org-set-property "Emacs" emacs-version)
  (org-set-property "Hostname" system-name)
  (org-journal-tags-prop-apply-delta :add (list (format "host.%s" (system-name))))
  (when (boundp 'my/location)
    (org-set-property "Location" my/location)
    (when-let ((weather (my/weather-get)))
      (org-set-property "Weather" weather)))
  (when (boundp 'my/loc-tag)
    (org-journal-tags-prop-apply-delta :add (list my/loc-tag)))
  (when (fboundp 'emms-playlist-current-selected-track)
    (let ((track (emms-playlist-current-selected-track)))
      (when track
        (let ((album (cdr (assoc 'info-album track)))
              (artist (or (cdr (assoc 'info-albumartist track))
                          (cdr (assoc 'info-album track))))
              (title (cdr (assoc 'info-title track)))
              (string ""))
          (when artist
            (setq string (concat string "[" artist "] ")))
          (when album
            (setq string (concat string album " - ")))
          (when title
            (setq string (concat string title)))
          (when (> (length string) 0)
            (org-set-property "EMMS_Track" string)))))))

(add-hook 'org-journal-after-entry-create-hook
          #'my/set-journal-header)

(use-package emacsql-sqlite
  :defer t
  :if (not my/remote-server)
  :straight (:type built-in))

(use-package org-roam
  :straight (:host github :repo "org-roam/org-roam"
                   :files (:defaults "extensions/*.el"))
  :if (not my/remote-server)
  :after org
  :init
  (setq org-roam-file-extensions '("org"))
  (setq org-roam-v2-ack t)
  (setq orb-insert-interface 'ivy-bibtex)
  :config
  (org-roam-setup)
  (require 'org-roam-protocol))

(setq org-roam-capture-templates
      `(("d" "default" plain "%?"
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
         :unnarrowed t)
        ("e" "encrypted" plain "%?"
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org.gpg" "#+title: ${title}\n")
         :unnarrowed t)))

(with-eval-after-load 'org-roam
  (my-leader-def
    :infix "or"
    "" '(:which-key "org-roam")
    "i" 'org-roam-node-insert
    "r" 'org-roam-node-find
    "g" 'org-roam-graph
    "c" 'org-roam-capture
    "b" 'org-roam-buffer-toggle)
  (general-define-key
   :keymaps 'org-roam-mode-map
   :states '(normal)
   "TAB" #'magit-section-toggle
   "q" #'quit-window
   "k" #'magit-section-backward
   "j" #'magit-section-forward
   "gr" #'revert-buffer
   "RET" #'org-roam-buffer-visit-thing))

(with-eval-after-load 'org
  (my-leader-def
    :keymap 'org-mode-map
    :infix "or"
    "t" 'org-roam-tag-add
    "T" 'org-toam-tag-remove
    "s" 'org-roam-db-autosync-mode)
  (general-define-key
   :keymap 'org-mode-map
   "C-c i" 'org-id-get-create
   "C-c l o" 'org-roam-node-insert))

(use-package org-roam-ui
  :straight (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :if (not my/remote-server)
  :after org-roam
  ;; :hook (org-roam . org-roam-ui-mode)
  :init
  (my-leader-def "oru" #'org-roam-ui-mode))

(setq my/git-diff-status
      '(("A" . added)
        ("C" . copied)
        ("D" . deleted)
        ("M" . modified)
        ("R" . renamed)
        ("T" . type-changed)
        ("U" . unmerged)))

(defun my/get-files-status (rev)
  (let ((files (shell-command-to-string (concat "git diff --name-status " rev))))
    (mapcar
     (lambda (file)
       (let ((elems (split-string file "\t")))
         (cons
          (cdr (assoc (car elems) my/git-diff-status))
          (nth 1 elems))))
     (split-string files "\n" t))))

(defun my/org-changed-files-since-date (date)
  (let ((default-directory org-directory))
    (my/get-files-status (format "@{%s}" date))))

(setq my/org-review-roam-queries
      '((:status added
                 :tags (:include ("org"))
                 :title "New Project Entries")
        (:status changed
                 :tags (:include ("org"))
                 :title "Changed Project Entries")
        (:status added
                 :tags (:exclude ("org"))
                 :title "New Zettelkasten Entries")
        (:status changed
                 :tags (:exclude ("org"))
                 :title "Changed Zettelkasten Entries")))

(defun my/org-review-format-roam (changes)
  (cl-loop for query in my/org-review-roam-queries
           with nodes = (org-roam-node-list)
           with node-tags = (mapcar #'org-roam-node-tags nodes)
           for include-tags = (plist-get (plist-get query :tags) :include)
           for exclude-tags = (plist-get (plist-get query :tags) :exclude)
           ;; List of nodes filtered by :tags in query
           for filtered-nodes =
           (cl-loop for node in nodes
                    for tags in node-tags
                    if (and
                        (or (seq-empty-p include-tags)
                            (seq-intersection include-tags tags))
                        (or (seq-empty-p exclude-tags)
                            (not (seq-intersection exclude-tags tags))))
                    collect node)
           ;; List of changes filtered by :status in query
           for filtered-changes =
           (cl-loop for change in changes
                    if (and (eq (car change) (plist-get query :status))
                            (string-match-p (rx bos "roam") (cdr change)))
                    collect (cdr change))
           ;; Intersection of the two filtered lists
           for final-nodes =
           (cl-loop for node in filtered-nodes
                    for path = (file-relative-name (org-roam-node-file node)
                                                   org-directory)
                    if (member path filtered-changes)
                    collect node)
           ;; If the intersction list is not empty, format it to the result
           if final-nodes
           concat (format "** %s\n" (plist-get query :title))
           ;; FInal list of links, sorted by title
           and concat (cl-loop for node in (seq-sort
                                            (lambda (node1 node2)
                                              (string-lessp
                                               (org-roam-node-title node1)
                                               (org-roam-node-title node2)))
                                            final-nodes)
                               concat (format "- [[id:%s][%s]]\n"
                                              (org-roam-node-id node)
                                              (org-roam-node-title node)))))

(setq my/org-ql-review-queries
      `(("Waitlist" scheduled scheduled
         (and
          (done)
          (tags-inherited "waitlist")))
        ("Personal tasks done" closed ,nil
         (and
          (tags-inherited "personal")
          (todo "DONE")))
        ("Attended meetings" closed scheduled
         (and
          (tags-inherited "meeting")
          (todo "PASSED")))
        ("Done project tasks" closed deadline
         (and
          (todo "DONE")
          (ancestors
           (heading "Tasks"))))))

(defun my/org-review-exec-ql (saved rev-date)
  (let ((query `(and
                 (,(nth 1 saved) :from ,rev-date)
                 ,(nth 3 saved))))
    (org-ql-query
      :select #'element
      :from (org-agenda-files)
      :where query
      :order-by (nth 2 saved))))

(defun my/org-review-format-element (elem)
  (concat
   (string-pad
    (plist-get (cadr elem) :raw-value)
    40)
   (when-let (scheduled (plist-get (cadr elem) :scheduled))
     (concat " [SCHEDULED: " (plist-get (cadr scheduled) :raw-value) "]"))
   (when-let (deadline (plist-get (cadr elem) :deadline))
     (concat " [DEADLINE: " (plist-get (cadr deadline) :raw-value) "]"))))

(defun my/org-review-format-queries (rev-date)
  (mapconcat
   (lambda (results)
     (concat "** " (car results) "\n"
             (string-join
              (mapcar (lambda (r) (concat "- " r)) (cdr results))
              "\n")
             "\n"))
   (seq-filter
    (lambda (result)
      (not (seq-empty-p (cdr result))))
    (mapcar
     (lambda (saved)
       (cons
        (car saved)
        (mapcar
         #'my/org-review-format-element
         (my/org-review-exec-ql saved rev-date))))
     my/org-ql-review-queries))
   "\n"))

(setq my/org-review-directory "review")

(defun my/get-last-review-date ()
  (->
   (substring
    (or
     (-max-by
      'string-greaterp
      (-filter
       (lambda (f) (not (or (string-equal f ".") (string-equal f ".."))))
       (directory-files (f-join org-roam-directory my/org-review-directory))))
     (format-time-string
      "%Y-%m-%d"
      (time-subtract
       (current-time)
       (seconds-to-time (* 60 60 24 14)))))
    0 10)
   (concat "T00:00:00-00:00")
   parse-time-string
   encode-time
   (time-add (seconds-to-time (* 60 60 24)))
   ((lambda (time)
      (format-time-string "%Y-%m-%d" time)))))

(setq my/org-review-capture-template
      `("r" "Review" plain
        ,(string-join
          '("#+title: %<%Y-%m-%d>: REVIEW"
            "#+category: REVIEW"
            "#+filetags: log review"
            "#+STARTUP: overview"
            ""
            "Last review date: %(org-timestamp-translate (org-timestamp-from-string (format \"<%s>\" (my/get-last-review-date))))"
            ""
            "* Roam"
            "%(my/org-review-format-roam (my/org-changed-files-since-date (my/get-last-review-date)))"
            "* Agenda"
            "%(my/org-review-format-queries (my/get-last-review-date))"
            "* Thoughts"
            "%?")
          "\n")
        :if-new (file "review/%<%Y-%m-%d>.org.gpg")))

(defun my/org-roam-capture-review ()
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
                     :templates `(,my/org-review-capture-template)))

(use-package org-ref
  :straight (:files (:defaults (:exclude "*helm*")))
  :if (not my/remote-server)
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
   "C-c l" #'org-ref-insert-link-hydra/body)
  (general-define-key
   :keymaps 'bibtex-mode-map
   "M-RET" 'org-ref-bibtex-hydra/body)
   (setq bibtex-completion-display-formats
     '((t . "${author:36} ${title:*} ${note:10} ${year:4} ${=has-pdf=:1}${=type=:7}"))))

(defun my/org-ref-select-bibliograhy ()
  (interactive)
  (setq-local org-ref-default-bibliography
              `(,(read-file-name "Bibliograhy: " nil nil t)))
  (setq-local reftex-default-bibliography org-ref-default-bibliography)
  (setq-local bibtex-completion-bibliography org-ref-default-bibliography))

(use-package org-roam-bibtex
  :straight (:host github :repo "org-roam/org-roam-bibtex")
  :after (org-roam org-ref)
  :disabled
  :config
  (org-roam-bibtex-mode))

(use-package org-contacts
  :straight (:type git
                   :repo "https://git.sr.ht/~bzg/org-contrib"
                   :files ("lisp/org-contacts.el")
                   :build (:not compile))
  :after (notmuch)
  :if (not my/remote-server)
  :commands (org-contacts)
  :config
  (require 'cl)
  (setq org-contacts-files (list
                            (concat org-directory "/contacts.org"))))

(defun my/export-org-tables-to-csv ()
  (interactive)
  (org-table-map-tables
   (lambda ()
     (when-let
         (name
          (plist-get (cadr (org-element-at-point)) :name))
       (org-table-export
        (concat
         (file-name-directory
          (buffer-file-name))
         name ".csv")
        "orgtbl-to-csv")))))

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

(with-eval-after-load-norem 'org
  (setq my/org-latex-scale 1.75)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale my/org-latex-scale)))

(with-eval-after-load-norem 'org
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
         org-preview-latex-process-alist)))

(use-package org-superstar
  :straight t
  :disabled
  :hook (org-mode . org-superstar-mode))

(use-package org-bars
  :straight (:repo "tonyaldon/org-bars" :host github)
  :if (display-graphic-p)
  :hook (org-mode . org-bars-mode))

(defun my/org-no-ellipsis-in-headlines ()
  (remove-from-invisibility-spec '(outline . t))
  (add-to-invisibility-spec 'outline))

(with-eval-after-load 'org-bars
  (add-hook 'org-mode-hook #'my/org-no-ellipsis-in-headlines)
  (when (eq major-mode 'org-mode)
    (my/org-no-ellipsis-in-headlines)))

(my/use-doom-colors
 (org-block :background (color-darken-name (doom-color 'bg) 3))
 (org-block-begin-line :background (color-darken-name (doom-color 'bg) 3)
                       :foreground (doom-color 'grey)))

(use-package ox-hugo
  :straight t
  :if (not my/remote-server)
  :after ox)

(use-package ox-ipynb
  :straight (:host github :repo "jkitchin/ox-ipynb")
  :if (not my/remote-server)
  :after ox)

(use-package htmlize
  :straight t
  :after ox
  :if (not my/remote-server)
  :config
  (setq org-html-htmlize-output-type 'css))

(defun my/setup-org-latex ()
  (setq org-latex-prefer-user-labels t)
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
  (add-to-list 'org-latex-classes
               '("org-plain-extreport"
                 "\\documentclass{extreport}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")))
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
(with-eval-after-load-norem 'ox-latex
  (my/setup-org-latex))

(with-eval-after-load-norem 'org
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

  (general-nmap :keymaps 'org-mode-map "RET" 'org-ctrl-c-ctrl-c))

(defun my/org-link-copy (&optional arg)
  "Extract URL from org-mode link and add it to kill ring."
  (interactive "P")
  (let* ((link (org-element-lineage (org-element-context) '(link) t))
         (type (org-element-property :type link))
         (url (org-element-property :path link))
         (url (concat type ":" url)))
    (kill-new url)
    (message (concat "Copied URL: " url))))

(with-eval-after-load-norem 'org
  (general-nmap :keymaps 'org-mode-map
    "C-x C-l" 'my/org-link-copy))

(defun my/org-babel-next-visible-src-block (arg)
  "Move to the next visible source block.

With ARG, repeats or can move backward if negative."
  (interactive "p")
  (let ((regexp org-babel-src-block-regexp))
    (if (< arg 0)
	    (beginning-of-line)
      (end-of-line))
    (while (and (< arg 0) (re-search-backward regexp nil :move))
      (unless (bobp)
	    (while (pcase (get-char-property-and-overlay (point) 'invisible)
		         (`(outline . ,o)
		          (goto-char (overlay-start o))
		          (re-search-backward regexp nil :move))
		         (_ nil))))
      (cl-incf arg))
    (while (and (> arg 0) (re-search-forward regexp nil t))
      (while (pcase (get-char-property-and-overlay (point) 'invisible)
	           (`(outline . ,o)
		        (goto-char (overlay-end o))
		        (re-search-forward regexp nil :move))
	           (_ (end-of-line) nil)))
      (re-search-backward regexp nil :move)
      (cl-decf arg))
    (if (> arg 0) (goto-char (point-max)) (beginning-of-line))))

(defun my/org-babel-previous-visible-src-block (arg)
  "Move to the prevous visible source block.

With ARG, repeats or can move backward if negative."
  (interactive "p")
  (my/org-babel-next-visible-src-block (- arg)))

(with-eval-after-load 'org
  (general-define-key
   :keymaps 'org-mode-map
   :states '(normal emacs)
   "M-]" #'my/org-babel-next-visible-src-block
   "M-[" #'my/org-babel-previous-visible-src-block))

(defun my/org-file-open ()
  (interactive)
  (let* ((default-directory org-directory)
         (project-files
          (seq-filter
           (lambda (f)
             (and
              (string-match-p (rx (* nonl) ".org" eos) f)
              (not (string-match-p (rx (| "journal" "roam" "review" "archive" "figured-out")) f))))
           (projectile-current-project-files))))
    (find-file
     (concat org-directory "/" (completing-read "Org file: " project-files)))))

(my-leader-def
  "o o" 'my/org-file-open)

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
  (general-define-key
   :states '(normal)
   :keymaps 'dired-mode-map
   "h" #'dired-up-directory
   "l" #'dired-find-file
   "=" #'dired-narrow
   "-" #'my/dired-create-empty-file-subtree
   "~" #'vterm
   "M-r" #'wdired-change-to-wdired-mode
   "<left>" #'dired-up-directory
   "<right>" #'dired-find-file
   "M-<return>" #'dired-open-xdg))

(defun my/dired-home ()
  "Open dired at $HOME"
  (interactive)
  (dired (expand-file-name "~")))

(my-leader-def
  "ad" #'dired)

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
  :commands (dired-recent-open)
  :config
  (dired-recent-mode)
  (general-define-key
   :keymaps 'dired-recent-mode-map
   "C-x C-d" nil)
  (my-leader-def
    "aD" '(dired-recent-open :wk "dired history")))

(use-package dired-single
  :after dired
  :disabled
  :straight t)

(use-package all-the-icons-dired
  :straight t
  :if (not (or my/slow-ssh (not (display-graphic-p))))
  :hook (dired-mode . (lambda ()
                        (unless (string-match-p "/gnu/store" default-directory)
                          (all-the-icons-dired-mode))))
  :config
  (advice-add 'dired-add-entry :around #'all-the-icons-dired--refresh-advice)
  (advice-add 'dired-remove-entry :around #'all-the-icons-dired--refresh-advice)
  (advice-add 'dired-kill-subdir :around #'all-the-icons-dired--refresh-advice))

(use-package dired-open
  :straight t
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
  :if (not my/slow-ssh)
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

(setq tramp-verbose 1)

(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

(when (or my/remote-server my/slow-ssh)
  (setq explicit-shell-file-name "/bin/bash"))

(with-eval-after-load 'tramp
  (setq tramp-remote-path
        (append tramp-remote-path
                '(tramp-own-remote-path))))

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

(use-package vterm
  ;; :straight t
  :commands (vterm vterm-other-window)
  :config
  (setq vterm-kill-buffer-on-exit t)

  (add-hook 'vterm-mode-hook
            (lambda ()
              (setq-local global-display-line-numbers-mode nil)
              (display-line-numbers-mode 0)))


  (advice-add 'evil-collection-vterm-insert
              :before (lambda (&rest args)
                        (ignore-errors
                          (apply #'vterm-reset-cursor-point args))))

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

  (general-define-key
   :keymaps 'vterm-mode-map
   :states '(normal insert)
   "<home>" 'vterm-beginning-of-line
   "<end>" 'vterm-end-of-line)

  (general-define-key
   :keymaps 'vterm-mode-map
   :states '(insert)
   "C-r" 'vterm-send-C-r
   "C-k" 'vterm-send-C-k
   "C-j" 'vterm-send-C-j
   "M-l" 'vterm-send-right
   "M-h" 'vterm-send-left
   "M-k" 'vterm-send-up
   "M-j" 'vterm-send-down))

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
   :keymaps 'vterm-mode-map
   :states '(normal)
   "gd" #'my/vterm-dired-other-window
   "gD" #'my/vterm-dired-replace))

(use-package with-editor
  :straight t
  :after (vterm)
  :config
  (add-hook 'vterm-mode-hook 'with-editor-export-editor))

(defun my/configure-eshell ()
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)
  (setq eshell-history-size 10000)
  (setq eshell-hist-ingnoredups t)
  (setq eshell-buffer-maximum-lines 10000)

  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (general-define-key
   :states '(normal)
   :keymaps 'eshell-mode-map
   (kbd "C-h") 'evil-window-left
   (kbd "C-l") 'evil-window-right
   (kbd "C-k") 'evil-window-up
   (kbd "C-j") 'evil-window-down))

(use-package eshell
  :ensure nil
  :after evil-collection
  :commands (eshell)
  :init
  (my/use-doom-colors
   (epe-pipeline-delimiter-face :foreground (doom-color 'green))
   (epe-pipeline-host-face      :foreground (doom-color 'blue))
   (epe-pipeline-time-face      :foreground (doom-color 'yellow))
   (epe-pipeline-user-face      :foreground (doom-color 'red)))
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
  :init
  (my/use-doom-colors
   (aweshell-alert-buffer-face  :background (color-darken-name (doom-color 'bg) 3))
   (aweshell-alert-command-face :foreground (doom-color 'red) :weight 'bold))
  :config
  (setq eshell-prompt-regexp "^[^#\nλ]* λ[#]* ")
  (setq eshell-highlight-prompt t)
  (setq eshell-prompt-function 'epe-theme-pipeline))

(use-package eshell-info-banner
  :defer t
  :if (not my/slow-ssh)
  :straight (eshell-info-banner :type git
                                :host github
                                :repo "SqrtMinusOne/eshell-info-banner.el")
  :hook (eshell-banner-load . eshell-info-banner-update-banner)
  :config
  (setq eshell-info-banner-filter-duplicate-partitions t)
  (setq eshell-info-banner-exclude-partitions '("b/efi")))

(when (or my/slow-ssh my/remote-server)
  (general-nmap "`" 'aweshell-dedicated-toggle)
  (general-nmap "~" 'eshell))

(defun my/setup-shell ()
  (setq-local comint-use-prompt-regexp t)
  (setq-local comint-prompt-read-only t))

(add-hook 'shell-mode-hook #'my/setup-shell)

(general-define-key
 ;; "C-c c" (my/command-in-persp "Emacs.org" "conf" 1 (find-file "~/Emacs.org"))
 "C-c c" `(,(lambda () (interactive) (find-file "~/Emacs.org")) :wk "Emacs.org"))

(my-leader-def
  :infix "c"
  "" '(:which-key "configuration")
  ;; "c" (my/command-in-persp "Emacs.org" "conf" 1 (find-file "~/Emacs.org"))
  "c" `(,(lambda () (interactive) (find-file "~/Emacs.org")) :wk "Emacs.org"))

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

(general-define-key
 "C-c f" '(my/open-yadm-file :wk "yadm file"))

(my-leader-def
  "cf" '(my/open-yadm-file :wk "yadm file"))

(use-package elfeed
  :straight (:repo "SqrtMinusOne/elfeed" :host github)
  :if (not my/remote-server)
  :commands (elfeed)
  :init
  (my-leader-def "ae" (my/command-in-persp "elfeed" "elfeed" 0 (elfeed-summary)))
  :config
  (setq elfeed-db-directory "~/.elfeed")
  (setq elfeed-enclosure-default-dir (expand-file-name "~/Downloads"))
  (advice-add #'elfeed-insert-html
              :around
              (lambda (fun &rest r)
                (let ((shr-use-fonts nil))
                  (apply fun r))))
  (general-define-key
   :states '(normal)
   :keymaps 'elfeed-search-mode-map
   "o" #'my/elfeed-search-filter-source
   "c" #'elfeed-search-clear-filter
   "gl" (lambda () (interactive) (elfeed-search-set-filter "+later")))
  (general-define-key
   :states '(normal)
   :keymaps 'elfeed-show-mode-map
   "ge" #'my/elfeed-show-visit-eww))

(use-package elfeed-org
  :straight t
  :after (elfeed)
  :config
  (setq rmh-elfeed-org-files '("~/.emacs.d/private.org"))
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

(defface elfeed-videos-entry nil
  "Face for the elfeed entries with tag \"videos\"")

(defface elfeed-twitter-entry nil
  "Face for the elfeed entries with tah \"twitter\"")

(defface elfeed-emacs-entry nil
  "Face for the elfeed entries with tah \"emacs\"")

(defface elfeed-music-entry nil
  "Face for the elfeed entries with tah \"music\"")

(defface elfeed-podcasts-entry nil
  "Face for the elfeed entries with tag \"podcasts\"")

(defface elfeed-blogs-entry nil
  "Face for the elfeed entries with tag \"blogs\"")

(defface elfeed-govt-entry nil
  "Face for the elfeed entries with tag \"blogs\"")

(my/use-doom-colors
 (elfeed-search-tag-face :foreground (doom-color 'yellow))
 (elfeed-videos-entry :foreground (doom-color 'red))
 (elfeed-twitter-entry :foreground (doom-color 'blue))
 (elfeed-emacs-entry :foreground (doom-color 'magenta))
 (elfeed-music-entry :foreground (doom-color 'green))
 (elfeed-podcasts-entry :foreground (doom-color 'yellow))
 (elfeed-blogs-entry :foreground (doom-color 'orange))
 (elfeed-govt-entry :foreground (doom-color 'dark-cyan)))

(with-eval-after-load 'elfeed
  (setq elfeed-search-face-alist
        '((podcasts elfeed-podcasts-entry)
          (music elfeed-music-entry)
          (gov elfeed-govt-entry)
          (twitter elfeed-twitter-entry)
          (videos elfeed-videos-entry)
          (emacs elfeed-emacs-entry)
          (blogs elfeed-blogs-entry)
          (unread elfeed-search-unread-title-face))))

(defun my/update-my-theme-elfeed (&rest _)
  (custom-theme-set-faces
   'my-theme-1
   `(elfeed-videos-entry ((t :foreground ,(doom-color 'red))))
   `(elfeed-twitter-entry ((t :foreground ,(doom-color 'blue))))
   `(elfeed-emacs-entry ((t :foreground ,(doom-color 'magenta))))
   `(elfeed-music-entry ((t :foreground ,(doom-color 'green))))
   `(elfeed-podcasts-entry ((t :foreground ,(doom-color 'yellow))))
   `(elfeed-blogs-entry ((t :foreground ,(doom-color 'orange)))))
  (enable-theme 'my-theme-1))

(advice-add 'load-theme :after #'my/update-my-theme-elfeed)
(when (fboundp 'doom-color)
  (my/update-my-theme-elfeed))

(use-package elfeed-summary
  :commands (elfeed-summary)
  :straight t
  :config
  (setq elfeed-summary-filter-by-title t))

(use-package elfeed-sync
  :straight (:host github :repo "SqrtMinusOne/elfeed-sync")
  :after elfeed
  :config
  (elfeed-sync-mode)
  (setq elfeed-sync-tt-rss-instance "https://sqrtminusone.xyz/tt-rss")
  (setq elfeed-sync-tt-rss-login "sqrtminusone")
  (setq elfeed-sync-tt-rss-password (my/password-store-get "Selfhosted/tt-rss")))

(defun my/get-youtube-url (entry)
  (let ((watch-id (cadr
                   (assoc "watch?v"
                          (url-parse-query-string
                           (substring
                            (url-filename
                             (url-generic-parse-url (elfeed-entry-link entry)))
                            1))))))
    (when watch-id
      (concat "https://www.youtube.com/watch?v=" watch-id))))

(defun my/get-enclosures-url (entry)
  (caar (elfeed-entry-enclosures entry)))

(with-eval-after-load 'emms
  (define-emms-source elfeed (entry)
    (let ((url (or (my/get-enclosures-url entry)
                   (my/get-youtube-url entry))))
      (unless url
        (error "URL not found"))
      (let ((track (emms-track 'url url)))
        (emms-track-set track 'info-title (elfeed-entry-title entry))
        (emms-playlist-insert-track track)))))

(defun my/elfeed-add-emms ()
  (interactive)
  (emms-add-elfeed elfeed-show-entry)
  (elfeed-tag elfeed-show-entry 'watched)
  (elfeed-show-refresh))

(with-eval-after-load 'elfeed
  (general-define-key
   :states '(normal)
   :keymaps 'elfeed-show-mode-map
   "gm" #'my/elfeed-add-emms))

(defun my/rdrview-get (url callback)
  "Get the rdrview representation of URL.

Call CALLBACK with the output."
  (let* ((buffer (generate-new-buffer "rdrview"))
         (proc (start-process "rdrview" buffer "rdrview"
                              url "-T" "title,sitename,body"
                              "-H")))
    (set-process-sentinel
     proc
     (lambda (process _msg)
       (let ((status (process-status process))
             (code (process-exit-status process)))
         (cond ((and (eq status 'exit) (= code 0))
                (progn
                  (funcall callback
                           (with-current-buffer (process-buffer process)
                             (buffer-string)))
                  (kill-buffer (process-buffer process))) )
               ((or (and (eq status 'exit) (> code 0))
                    (eq status 'signal))
                (let ((err (with-current-buffer (process-buffer process)
                             (buffer-string))))
                  (kill-buffer (process-buffer process))
                  (user-error "Error in rdrview: %s" err)))))))
    proc))

(defun my/rdrview-parse (dom-string)
  (let ((dom (with-temp-buffer
               (insert dom-string)
               (libxml-parse-html-region (point-min) (point-max)))))
    (let (title sitename content (i 0))
      (dolist (child (dom-children (car (dom-by-id dom "readability-page-1"))))
        (when (listp child)
          (cond
           ((eq (car child) 'h1)
            (setq title (dom-text child)))
           ((eq (car child) 'h2)
            (setq sitename (dom-text child)))
           ((eq (car child) 'div)
            (setq content child)))))
      (while (and
              (not (dom-by-tag content 'h1))
              (dom-search
               content
               (lambda (el)
                 (when (listp el)
                   (pcase (car el)
                     ('h2 (setf (car el) 'h1))
                     ('h3 (setf (car el) 'h2))
                     ('h4 (setf (car el) 'h3))
                     ('h5 (setf (car el) 'h4))
                     ('h6 (setf (car el) 'h5))))))))
      `((title . ,title)
        (sitename . ,sitename)
        (content . ,(with-temp-buffer
                      (dom-print content)
                      (buffer-string)))))))

(defvar-local my/elfeed-show-rdrview-html nil)

(defun my/rdrview-elfeed-show ()
  (interactive)
  (unless elfeed-show-entry
    (user-error "No elfeed entry in this buffer!"))
  (my/rdrview-get
   (elfeed-entry-link elfeed-show-entry)
   (lambda (result)
     (let* ((data (my/rdrview-parse result))
            (inhibit-read-only t)
            (title (elfeed-entry-title elfeed-show-entry))
            (date (seconds-to-time (elfeed-entry-date elfeed-show-entry)))
            (authors (elfeed-meta elfeed-show-entry :authors))
            (link (elfeed-entry-link elfeed-show-entry))
            (tags (elfeed-entry-tags elfeed-show-entry))
            (tagsstr (mapconcat #'symbol-name tags ", "))
            (nicedate (format-time-string "%a, %e %b %Y %T %Z" date))
            (content (alist-get 'content data))
            (feed (elfeed-entry-feed elfeed-show-entry))
            (feed-title (elfeed-feed-title feed))
            (base (and feed (elfeed-compute-base (elfeed-feed-url feed)))))
       (erase-buffer)
       (insert (format (propertize "Title: %s\n" 'face 'message-header-name)
                       (propertize title 'face 'message-header-subject)))
       (when elfeed-show-entry-author
         (dolist (author authors)
           (let ((formatted (elfeed--show-format-author author)))
             (insert
              (format (propertize "Author: %s\n" 'face 'message-header-name)
                      (propertize formatted 'face 'message-header-to))))))
       (insert (format (propertize "Date: %s\n" 'face 'message-header-name)
                       (propertize nicedate 'face 'message-header-other)))
       (insert (format (propertize "Feed: %s\n" 'face 'message-header-name)
                       (propertize feed-title 'face 'message-header-other)))
       (when tags
         (insert (format (propertize "Tags: %s\n" 'face 'message-header-name)
                         (propertize tagsstr 'face 'message-header-other))))
       (insert (propertize "Link: " 'face 'message-header-name))
       (elfeed-insert-link link link)
       (insert "\n")
       (cl-loop for enclosure in (elfeed-entry-enclosures elfeed-show-entry)
                do (insert (propertize "Enclosure: " 'face 'message-header-name))
                do (elfeed-insert-link (car enclosure))
                do (insert "\n"))
       (insert "\n")
       (if content
           (elfeed-insert-html content base)
         (insert (propertize "(empty)\n" 'face 'italic)))
       (setq-local my/elfeed-show-rdrview-html content)
       (goto-char (point-min))))))

(with-eval-after-load 'elfeed
  (general-define-key
   :states '(normal)
   :keymaps 'elfeed-show-mode-map
   "gp" #'my/rdrview-elfeed-show))

(setq my/rdrview-template (expand-file-name
                           (concat user-emacs-directory "rdrview.tex")))

(cl-defun my/rdrview-render (content type variables callback
                                     &key file-name overwrite)
  "Render CONTENT with pandoc.

TYPE is a file extension as supported by pandoc, for instance,
html or txt.  VARIABLES is an alist that is fed into the
template.  After the rendering is complete successfully, CALLBACK
is called with the resulting PDF.

FILE-NAME is a path to the resulting PDF. If nil it's generated
randomly.

If a file with the given FILE-NAME already exists, the function will
invoke CALLBACK straight away without doing the rendering, unless
OVERWRITE is non-nil."
  (unless file-name
    (setq file-name (format "/tmp/%d.pdf" (random 100000000))))
  (let (params
        (temp-file-name (format "/tmp/%d.%s" (random 100000000) type)))
    (cl-loop for (key . value) in variables
             when value
             do (progn
                  (push "--variable" params)
                  (push (format "%s=%s" key value) params)))
    (setq params (nreverse params))
    (if (and (file-exists-p file-name) (not overwrite))
        (funcall callback file-name)
      (with-temp-file temp-file-name
        (insert content))
      (let ((proc (apply #'start-process
                         "pandoc" (get-buffer-create "*Pandoc*") "pandoc"
                         temp-file-name "-o" file-name
                         "--pdf-engine=xelatex" "--template" my/rdrview-template
                         params)))
        (set-process-sentinel
         proc
         (lambda (process _msg)
           (let ((status (process-status process))
                 (code (process-exit-status process)))
             (cond ((and (eq status 'exit) (= code 0))
                    (progn
                      (message "Done!")
                      (funcall callback file-name)))
                   ((or (and (eq status 'exit) (> code 0))
                        (eq status 'signal))
                    (user-error "Error in pandoc. Check the *Pandoc* buffer"))))))))))

(setq my/elfeed-pdf-dir (expand-file-name "~/.elfeed/pdf/"))

(defun my/elfeed-open-pdf (entry overwrite)
  "Open the current elfeed ENTRY with a pdf viewer.

If OVERWRITE is non-nil, do the rendering even if the resulting
PDF already exists."
  (interactive (list elfeed-show-entry current-prefix-arg))
  (let ((authors (mapcar (lambda (m) (plist-get m :name)) (elfeed-meta entry :authors)))
        (feed-title (elfeed-feed-title (elfeed-entry-feed entry)))
        (tags (mapconcat #'symbol-name (elfeed-entry-tags entry) ", "))
        (date (format-time-string "%a, %e %b %Y"
                                  (seconds-to-time (elfeed-entry-date entry))))
        (content (elfeed-deref (elfeed-entry-content entry)))
        (file-name (concat my/elfeed-pdf-dir
                           (elfeed-ref-id (elfeed-entry-content entry))
                           ".pdf"))
        (main-language "english")
        (other-language "russian"))
    (unless content
      (user-error "No content!"))
    (setq subtitle
          (cond
           ((seq-empty-p authors) feed-title)
           ((and (not (seq-empty-p (car authors)))
                 (string-match-p (regexp-quote (car authors)) feed-title)) feed-title)
           (t (concat (string-join authors ", ") "\\\\" feed-title))))
    (when (member 'ru (elfeed-entry-tags entry))
      (setq main-language "russian")
      (setq other-language "english"))
    (my/rdrview-render
     (if (bound-and-true-p my/elfeed-show-rdrview-html)
         my/elfeed-show-rdrview-html
       content)
     (elfeed-entry-content-type entry)
     `((title . ,(elfeed-entry-title entry))
       (subtitle . ,subtitle)
       (date . ,date)
       (tags . ,tags)
       (main-lang . ,main-language)
       (other-lang . ,other-language))
     (lambda (file-name)
       (start-process "xdg-open" nil "xdg-open" file-name))
     :file-name file-name
     :overwrite current-prefix-arg)))

(with-eval-after-load 'elfeed
  (general-define-key
   :keymaps '(elfeed-show-mode-map)
   :states '(normal)
   "gv" #'my/elfeed-open-pdf))

(defun my/get-languages (url)
  (let ((main-lang "english")
        (other-lang "russian"))
    (when (string-match-p (rx ".ru") url)
      (setq main-lang "russian"
            other-lang "english"))
    (list main-lang other-lang)))

(defun my/rdrview-open (url overwrite)
  (interactive
   (let ((url (read-from-minibuffer
               "URL: "
               (if (bound-and-true-p elfeed-show-entry)
                   (elfeed-entry-link elfeed-show-entry)))))
     (when (string-empty-p url)
       (user-error "URL is empty"))
     (list url current-prefix-arg)))
  (my/rdrview-get
   url
   (lambda (res)
     (let ((data (my/rdrview-parse res))
           (langs (my/get-languages url)))
       (my/rdrview-render
        (alist-get 'content data)
        'html
        `((title . ,(alist-get 'title data))
          (subtitle . ,(alist-get 'sitename data))
          (main-lang . ,(nth 0 langs))
          (other-lang . ,(nth 1 langs)))
        (lambda (file-name)
          (start-process "xdg-open" nil "xdg-open" file-name)))))))

(cl-defun my/youtube-subtitles-get (video-id callback &key file-name overwrite)
  "Get subtitles for VIDEO-ID in WebVTT format.

Call CALLBACK when done.

FILE-NAME is a path to the resulting WebVTT file. If nil it's
generated randomly.

If a file with the given FILE-NAME already exists, the function will
invoke CALLBACK straight away without doing the rendering, unless
OVERWRITE is non-nil."
  (unless file-name
    (setq file-name (format "/tmp/%d.vtt" (random 100000000))))
  (if (and (file-exists-p file-name) (not overwrite))
      (funcall callback file-name)
    (let* ((buffer (generate-new-buffer "youtube-transcripts"))
           (proc (start-process "youtube_transcript_api" buffer
                                "youtube_transcript_api" video-id
                                "--languages" "en" "ru" "de"
                                "--format" "webvtt")))
      (set-process-sentinel
       proc
       (lambda (process _msg)
         (let ((status (process-status process))
               (code (process-exit-status process)))
           (cond ((and (eq status 'exit) (= code 0))
                  (progn
                    (with-current-buffer (process-buffer process)
                      (setq buffer-file-name file-name)
                      (save-buffer))
                    (kill-buffer (process-buffer process))
                    (funcall callback file-name)))
                 ((or (and (eq status 'exit) (> code 0))
                      (eq status 'signal))
                  (let ((err (with-current-buffer (process-buffer process)
                               (buffer-string))))
                    (kill-buffer (process-buffer process))
                    (user-error "Error in youtube_transcript_api: %s" err)))))))
      proc)))

(setq my/elfeed-srt-dir (expand-file-name "~/.elfeed/srt/"))

(defun my/elfeed-youtube-subtitles (entry &optional arg)
  "Get subtitles for the current elfeed ENTRY.

Works only in the entry is a YouTube video.

If ARG is non-nil, re-fetch the subtitles regardless of whether
they were fetched before."
  (interactive (list elfeed-show-entry current-prefix-arg))
  (let ((video-id (cadr
                   (assoc "watch?v"
                          (url-parse-query-string
                           (substring
                            (url-filename
                             (url-generic-parse-url (elfeed-entry-link entry)))
                            1))))))
    (unless video-id
      (user-error "Can't get video ID from the entry"))
    (my/youtube-subtitles-get
     video-id
     (lambda (file-name)
       (with-current-buffer (find-file-other-window file-name)
         (setq-local elfeed-show-entry entry)
         (goto-char (point-min))))
     :file-name (concat my/elfeed-srt-dir
                        (elfeed-ref-id (elfeed-entry-content entry))
                        ".vtt")
     :overwrite arg)))

(defun my/subed-elfeed (entry)
  "Open the video file from elfeed ENTRY in MPV.

This has to be launched from inside the subtitles buffer, opened
by the `my/elfeed-youtube-subtitles' function."
  (interactive (list elfeed-show-entry))
  (unless entry
    (user-error "No entry!"))
  (unless (derived-mode-p 'subed-mode)
    (user-error "Not subed mode!"))
  (setq-local subed-mpv-arguments
              (seq-uniq
               (append subed-mpv-arguments emms-player-mpv-parameters)))
  (setq-local subed-mpv-video-file (elfeed-entry-link entry))
  (subed-mpv--play subed-mpv-video-file))

(unless (or my/is-termux my/remote-server)
  (let ((mail-file (expand-file-name "mail.el" user-emacs-directory)))
    (if (file-exists-p mail-file)
        (load-file mail-file)
      (message "Can't load mail.el"))))

(use-package emms
  :straight t
  :if (not my/remote-server)
  :commands (emms-smart-browse
             emms-browser
             emms-add-url
             emms-add-file
             emms-add-find)
  :if (not my/is-termux)
  :init
  (my-leader-def
    :infix "as"
    "" '(:which-key "emms")
    "s" (my/command-in-persp "emms" "EMMS" 0 (emms-smart-browse))
    "b" 'emms-browser
    "p" 'emms-pause
    "q" 'emms-stop
    "h" 'emms-previous
    "l" 'emms-next
    "u" 'emms-player-mpd-connect
    "ww" 'emms-lyrics
    "wb" 'emms-lyrics-toggle-display-on-minibuffer
    "wm" 'emms-lyrics-toggle-display-on-modeline)
  (setq emms-mode-line-icon-enabled-p nil)
  :config
  (require 'emms-setup)
  (require 'emms-player-mpd)
  (require 'emms-player-mpv)
  (emms-all)
  ;; MPD setup
  (setq emms-source-file-default-directory (expand-file-name "~/Music/"))
  (add-to-list 'emms-info-functions 'emms-info-mpd)
  (add-to-list 'emms-player-list 'emms-player-mpd)
  (setq emms-player-mpd-server-name "localhost")
  (setq emms-player-mpd-server-port "6600")
  (setq emms-player-mpd-music-directory "~/Music")
  (emms-player-mpd-connect)
  (add-hook 'emms-playlist-cleared-hook 'emms-player-mpd-clear)
  (emms-player-set emms-player-mpd
                   'regex
                   (rx (or (: "https://" (* nonl) (or "acast.com") (* nonl))
                           (+ (? (or "https://" "http://"))
                              (* nonl)
                              (regexp (eval (emms-player-simple-regexp
                                             "m3u" "ogg" "flac" "mp3" "wav" "mod" "au" "aiff" "m4a")))))))
  ;; MPV setup
  (add-to-list 'emms-player-list 'emms-player-mpv)
  (emms-player-set emms-player-mpv
                   'regex
                   (rx (or (: "https://" (* nonl) "youtube.com" (* nonl))
                           (+ (? (or "https://" "http://"))
                              (* nonl)
                              (regexp (eval (emms-player-simple-regexp
                              "mp4" "mov" "wmv" "webm" "flv" "avi" "mkv")))))))
  (setq my/youtube-dl-quality-list
        '("bestvideo[height<=720]+bestaudio/best[height<=720]"
          "bestvideo[height<=480]+bestaudio/best[height<=480]"
          "bestvideo[height<=1080]+bestaudio/best[height<=1080]"))
  
  (setq my/default-emms-player-mpv-parameters
        '("--quiet" "--really-quiet" "--no-audio-display"))
  
  (defun my/set-emms-mpd-youtube-quality (quality)
    (interactive "P")
    (unless quality
      (setq quality (completing-read "Quality: " my/youtube-dl-quality-list nil t)))
    (setq emms-player-mpv-parameters
          `(,@my/default-emms-player-mpv-parameters ,(format "--ytdl-format=%s" quality))))
  
  (my/set-emms-mpd-youtube-quality (car my/youtube-dl-quality-list))
  ;; evil-lion and evil-commentary shadow some gX bindings
  ;; (add-hook 'emms-browser-mode-hook
  ;; (lambda ()
  ;; (evil-lion-mode -1)
  ;; (evil-commentary-mode -1)
  ;; ))
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

(defun my/emms-cleanup-urls ()
  (interactive)
  (let ((keys-to-delete '()))
    (maphash (lambda (key value)
               (when (eq (cdr (assoc 'type value)) 'url)
                 (add-to-list 'keys-to-delete key)))
             emms-cache-db)
    (dolist (key keys-to-delete)
      (remhash key emms-cache-db)))
  (setq emms-cache-dirty t))

(my-leader-def "asc" #'my/emms-cleanup-urls)

(use-package lyrics-fetcher
  :straight t
  :after (emms)
  :init
  (my-leader-def
    "ast" #'lyrics-fetcher-show-lyrics
    "asT" #'lyrics-fetcher-show-lyrics-query)
  :config
  (setq lyrics-fetcher-genius-access-token
        (my/password-store-get "My_Online/APIs/genius.com"))
  (general-define-key
   :states '(emacs normal)
   :keymaps 'emms-browser-mode-map
   "gl" 'lyrics-fetcher-emms-browser-show-at-point
   "gC" 'lyrics-fetcher-emms-browser-fetch-covers-at-point
   "go" 'lyrics-fetcher-emms-browser-open-large-cover-at-point)

  (advice-add #'emms-lyrics-mode-line
              :override #'my/emms-lyrics-mode-line-override))

(defun my/emms-lyrics-mode-line-override ()
  (add-to-list 'global-mode-string
               '(:eval emms-lyrics-mode-line-string)))

(defun my/emms-lyrics-restore-mode-line-override ()
  "Restore the mode line."
  (setq global-mode-string
	    (remove '(:eval emms-lyrics-mode-line-string) global-mode-string))
  (force-mode-line-update))

(with-eval-after-load 'emms-lyrics
  (advice-add #'emms-lyrics-mode-line
              :override #'my/emms-lyrics-mode-line-override)
  (advice-add #'emms-lyrics-restore-mode-line
              :override #'my/emms-lyrics-restore-mode-line-override))

(with-eval-after-load 'emms-browser
  (general-define-key
   :states '(normal)
   :keymaps 'emms-browser-mode-map
   "q" 'quit-window))

(with-eval-after-load 'emms
  (general-define-key
   :states '(normal)
   :keymaps 'emms-playlist-mode-map
   "q" 'quit-window))

(use-package ytel
  :straight t
  :commands (ytel)
  :config
  (setq ytel-invidious-api-url "https://invidio.xamh.de/")
  (general-define-key
   :states '(normal)
   :keymaps 'ytel-mode-map
   "q" #'ytel-quit
   "s" #'ytel-search
   "L" #'ytel-search-next-page
   "H" #'ytel-search-previous-page
   "RET" #'my/ytel-add-emms))

(with-eval-after-load 'emms
  (define-emms-source ytel (video)
    (let ((track (emms-track
                  'url (concat "https://www.youtube.com/watch?v="
                               (ytel-video-id video)))))
      (emms-track-set track 'info-title (ytel-video-title video))
      (emms-track-set track 'info-artist (ytel-video-author video))
      (emms-playlist-insert-track track))))

(defun my/ytel-add-emms ()
  (interactive)
  (emms-add-ytel (ytel-get-current-video)))

(use-package wallabag
  :straight (:host github :repo "chenyanming/wallabag.el" :files (:defaults "default.css" "emojis.alist"))
  :commands (wallabag wallabag-add-entry)
  :config
  (setq wallabag-host "https://wallabag.sqrtminusone.xyz")
  (setq wallabag-username "sqrtminusone")
  (setq wallabag-password (my/password-store-get "Selfhosted/wallabag"))
  (setq wallabag-clientid (password-store-get-field "Selfhosted/wallabag" "client_id"))
  (setq wallabag-secret (password-store-get-field "Selfhosted/wallabag" "client_secret")))

(defun my/elfeed-wallabag (entry)
  (interactive (list elfeed-show-entry))
  (wallabag-add-entry (elfeed-entry-link entry)
                      (mapconcat #'symbol-name (elfeed-entry-tags entry) ","))
  (elfeed-recommender--rate-current 2))

(with-eval-after-load 'elfeed
  (general-define-key
   :states '(normal)
   :keymaps '(elfeed-show-mode-map)
   "gw" #'my/elfeed-wallabag))

(defun my/toggle-shr-use-fonts ()
  "Toggle the shr-use-fonts variable in buffer"
  (interactive)
  (setq-local shr-use-fonts (not shr-use-fonts)))

(my-leader-def "aw" 'eww)

(general-define-key
 :keymaps 'eww-mode-map
 "+" 'text-scale-increase
 "-" 'text-scale-decrease)

(use-package erc
  :commands (erc erc-tls)
  :straight (:type built-in)
  :config
  (setq erc-log-channels-directory "~/.erc/logs")
  (setq erc-save-buffer-on-part t)
  (add-to-list 'erc-modules 'autojoin)
  (add-to-list 'erc-modules 'notifications)
  (add-to-list 'erc-modules 'log)
  (erc-update-modules)
  (setq erc-autojoin-channels-alist
        `((,(rx "libera.chat")
           "#systemcrafters"
           "#emacs"
           "#guix")))
  (setq erc-kill-buffer-on-part t)
  (setq erc-track-shorten-start 8))

(setq erc-track-exclude-types '("NICK" "JOIN" "LEAVE" "QUIT" "PART"
                                "301"   ; away notice
                                "305"   ; return from awayness
                                "306"   ; set awayness
                                "324"   ; modes
                                "329"   ; channel creation date
                                "332"   ; topic notice
                                "333"   ; who set the topic
                                "353"   ; Names notice
                                ))

(use-package erc-hl-nicks
  :hook (erc-mode . erc-hl-nicks-mode)
  :after (erc)
  :straight t)

(use-package znc
  :straight t
  :commands (znc-erc)
  :init
  (my-leader-def "ai" (my/command-in-persp "erc" "ERC" 0 (znc-erc)))
  :config
  (setq znc-servers
        `(("sqrtminusone.xyz" 6697 t
           ((libera "sqrtminusone"
                    ,(my/password-store-get "Selfhosted/ZNC")))))))

(defun my/erc-detach-all ()
  (interactive)
  (cl-loop for buf being the buffers
           if (eq (buffer-local-value 'major-mode buf) 'erc-mode)
           do (with-current-buffer buf
                (when (erc-server-process-alive)
                  (let ((tgt (erc-default-target)))
                    (erc-server-send (format "DETACH %s" tgt) nil tgt))))))

(use-package plz
  :straight (:host github :repo "alphapapa/plz.el")
  :defer t)

(use-package ement
  :straight (:host github :repo "alphapapa/ement.el"))

(use-package google-translate
  :straight t
  :if (not my/remote-server)
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
  :infix "at"
  "" '(:which-key "google translate")
  "p" 'google-translate-at-point
  "P" 'google-translate-at-point-reverse
  "q" 'google-translate-query-translate
  "Q" 'google-translate-query-translate-reverse
  "t" 'google-translate-smooth-translate)

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
    (shell-command-to-string (format "unzip -d /tmp/tldr/ %s" tldr-saved-zip-path))
    (when (file-exists-p tldr-directory-path)
      (delete-directory tldr-directory-path 'recursive 'no-trash))
    (shell-command-to-string (format "mv %s %s" "/tmp/tldr/tldr-main" tldr-directory-path))))

(my-leader-def "hT" 'tldr)

(setq Man-width-max 180)
(my-leader-def "hM" 'man)

(general-define-key
 :states '(normal)
 :keymaps 'Info-mode-map
 (kbd "RET") 'Info-follow-nearest-node)

(defun my/man-fix-width (&rest _)
  (setq-local Man-width (- (window-width) 4)))

(advice-add #'Man-update-manpage :before #'my/man-fix-width)

(use-package devdocs
  :straight t
  :commands (devdocs-install devdocs-lookup)
  :config
  (general-define-key
   :keymaps 'devdocs-mode-map
   :states '(normal)
   "H" #'devdocs-go-back
   "L" #'devdocs-go-forward
   "o" #'devdocs-lookup
   "[" #'devdocs-previous-page
   "]" #'devdocs-next-page)
  :init
  (my-leader-def
    "he" #'devdocs-lookup
    "hE" #'devdocs-install))

(use-package sx
  :straight t
  :config
  (general-define-key
   :states '(normal)
   :keymaps '(sx-question-mode-map sx-question-list-mode-map)
   "go" #'sx-visit-externally
   "q" #'quit-window
   "s*" #'sx-tab-starred
   "sU" #'sx-tab-unanswered-my-tags
   "sa" #'sx-ask
   "sf" #'sx-tab-featured
   "sh" #'sx-tab-frontpage
   "si" #'sx-inbox
   "sm" #'sx-tab-meta-or-main
   "sn" #'sx-tab-newest
   "su" #'sx-tab-unanswered
   "sv" #'sx-tab-topvoted
   "sw" #'sx-tab-week
   "u" #'sx-upvote
   "d" #'sx-downvote
   "j" nil
   "k" nil)
  (general-define-key
   :states '(normal)
   :keymaps '(sx-question-mode-map)
   "gr" #'sx-question-mode-refresh
   "J" #'sx-question-mode-next-section
   "K" #'sx-question-mode-previous-section
   "a" #'sx-answer
   "e" #'sx-edit
   "D" #'sx-delete
   "c" #'sx-comment)
  (general-define-key
   :states '(normal)
   :keymaps '(sx-question-list-mode-map)
   "RET" 'sx-display
   "j" #'sx-question-list-next
   "k" #'sx-question-list-previous
   "S" #'sx-search
   "m" #'sx-question-list-mark-read
   "O" #'sx-question-list-order-by
   "t" #'sx-tab-switch)
  (my-leader-def
   "hs" #'sx-search
   "hS" #'sx-tab-frontpage)
  (my/use-doom-colors
   (sx-question-mode-accepted :foreground (doom-color 'green)
                              :weight 'bold)
   (sx-question-mode-content :background nil))
  (add-hook 'sx-question-mode-hook #'doom-modeline-mode)
  (add-hook 'sx-question-list-mode-hook #'doom-modeline-mode))

(use-package pass
  :straight t
  :commands (pass)
  :init
  (my-leader-def "ak" #'pass)
  :config
  (setq pass-show-keybindings nil))

(defun my/password-store-get (entry)
  (if-let ((res (password-store-get entry)))
      res
    (my/password-store-get entry)))

(use-package docker
  :straight t
  :commands (docker)
  :init
  (my-leader-def "ao" 'docker))

(use-package prodigy
  :straight t
  :commands (prodigy)
  :init
  (my-leader-def "aP" (my/command-in-persp
                       "deploy" "prodigy" nil
                       (prodigy)
                       (delete-other-windows)))
  :config
  (general-define-key
   :states '(normal)
   :keymaps 'prodigy-view-mode-map
   "C-h" 'evil-window-left
   "C-l" 'evil-window-right
   "C-k" 'evil-window-up
   "C-j" 'evil-window-down))

(defun my/get-apps-on-ports ()
  (mapcar
   (lambda (line)
     (let* ((split (split-string line (rx (| (+ " ") (+ "\t")))))
            (process (elt split 6)))
       `((netid . ,(elt split 0))
         (state . ,(elt split 1))
         (recv-q . ,(elt split 2))
         (send-q . ,(elt split 3))
         ,@(let ((data (elt split 4)))
             (save-match-data
               (string-match (rx (group-n 1 (* nonl)) ":" (group-n 2 (or (+ num) "*"))) data)
               `((local-address . ,(match-string 1 data))
                 (local-port . ,(match-string 2 data)))))
         ,@(unless (string-empty-p process)
             `((pid . ,(save-match-data
                         (string-match (rx "pid=" (+ num)) process)
                         (string-to-number (substring (match-string 0 process) 4)))))))))
   (seq-filter
    (lambda (s) (not (string-empty-p s)))
    (split-string
     (shell-command-to-string "ss -tulpnH | grep LISTEN") "\n"))))

(defun my/kill-app-on-port (port &optional signal)
  (let ((apps (my/get-apps-on-ports)))
    (dolist (app apps)
      (when (string-equal (cdr (assoc 'local-port app)) port)
        (signal-process (cdr (assoc 'pid app)) (or signal 15))
        (message "Sent %d to %d" (or signal 15) (cdr (assoc 'pid app)))))))

(use-package screenshot
  :straight (:repo "tecosaur/screenshot"
                   :host github
                   :build (:not compile))
  :if (display-graphic-p)
  :commands (screenshot)
  :init
  (my-leader-def "S" 'screenshot))

(my-leader-def "ah" 'proced)
(setq proced-auto-update-interval 1)
(add-hook 'proced-mode-hook (lambda ()
                              (visual-line-mode -1)
                              (setq-local truncate-lines t)
                              (proced-toggle-auto-update 1)))

(use-package guix
  :straight t
  :commands (guix)
  :init
  (my-leader-def "ag" 'guix))

(use-package pomm
  :straight (:host github :repo "SqrtMinusOne/pomm.el" :files (:defaults "resources"))
  ;; :straight (:local-repo "~/Code/Emacs/pomm" :files (:defaults "resources"))
  :commands (pomm)
  :init
  (my-leader-def "ap" #'pomm)
  :config
  (setq alert-default-style 'libnotify)
  (add-hook 'pomm-on-tick-hook 'pomm-update-mode-line-string)
  (add-hook 'pomm-on-status-changed-hook 'pomm-update-mode-line-string))

(use-package hledger-mode
  :straight t
  :mode (rx ".journal" eos)
  :config
  (setq hledger-jfile (concat org-directory "/ledger/ledger.journal"))
  (add-hook 'hledger-mode-hook
            (lambda ()
              (make-local-variable 'company-backends)
              (add-to-list 'company-backends 'hledger-company))))

(use-package flycheck-hledger
  :straight t
  :after (hledger-mode))

(setq calendar-date-style 'iso) ;; YYYY/mm/dd
(setq calendar-week-start-day 1)
(setq calendar-time-display-form '(24-hours ":" minutes))

(setq calendar-latitude 59.9375)
(setq calendar-longitude 30.308611)

(defun my/elcord-mask-buffer-name (name)
  (cond
   ((string-match-p (rx bos (? "CAPTURE-") (= 14 num) "-" (* not-newline) ".org" eos) name)
    "<ORG-ROAM>")
   ((string-match-p (rx bos (+ num) "-" (+ num) "-" (+ num) ".org" eos) name)
    "<ORG-JOURNAL>")
   ((string-match-p (rx bos "EXWM") name)
    "<EXWM>")
   (t name)))

(defun my/elcord-buffer-details-format-functions ()
  (format "Editing %s" (my/elcord-mask-buffer-name (buffer-name))))

(defun my/elcord-update-presence-mask-advice (r)
  (list (my/elcord-mask-buffer-name (nth 0 r)) (nth 1 r)))

(use-package elcord
  :straight t
  :if (and (or
            (string= (system-name) "indigo")
            (string= (system-name) "eminence"))
           (not my/slow-ssh)
           (not my/remote-server))
  :config
  (setq elcord-buffer-details-format-function #'my/elcord-buffer-details-format-functions)
  (advice-add 'elcord--try-update-presence :filter-args #'my/elcord-update-presence-mask-advice)
  (elcord-mode))

(use-package snow
  :straight (:repo "alphapapa/snow.el" :host github)
  :commands (snow))

(use-package power-mode
  :straight (:host github :repo "elizagamedev/power-mode.el")
  :disabled
  :commands (power-mode))

(use-package redacted
  :commands (redacted-mode)
  :straight (:host github :repo "bkaestner/redacted.el"))

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

(defun my/ytel-kill-url ()
  (interactive)
  (kill-new
   (concat
    "https://www.youtube.com/watch?v="
    (ytel-video-id (ytel-get-current-video)))))
