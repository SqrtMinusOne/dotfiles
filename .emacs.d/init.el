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
                     gcs-done)
            (setq my/emacs-started t)))

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

(use-package micromamba
  :straight t
  :if (executable-find "micromamba")
  :config
  (micromamba-activate "general"))

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

(setq auth-source-debug nil)
(setq auth-sources '("~/.authinfo.gpg"))

(let ((private-file (expand-file-name "private.el" user-emacs-directory)))
  (when (file-exists-p private-file)
    (load-file private-file)))

(use-package no-littering
  :straight t)

(defun my/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun my/quit-window-and-buffer ()
  (interactive)
  (quit-window t))

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

(defun my/dump-bindings-recursive (prefix &optional level buffer)
  (dolist (key (which-key--get-bindings (kbd prefix)))
    (with-current-buffer buffer
      (when level
        (insert (make-string level ? )))
      (insert (apply #'format "%s%s%s\n" key)))
    (when (string-match-p
           (rx bos "+" (* nonl))
           (substring-no-properties (elt key 2)))
      (my/dump-bindings-recursive
       (concat prefix " " (substring-no-properties (car key)))
       (+ 2 (or level 0))
       buffer))))

(defun my/dump-bindings (prefix)
  "Dump keybindings starting with PREFIX in a tree-like form."
  (interactive "sPrefix: ")
  (let ((buffer (get-buffer-create "bindings")))
    (with-current-buffer buffer
      (erase-buffer))
    (my/dump-bindings-recursive prefix 0 buffer)
    (with-current-buffer buffer
      (goto-char (point-min)))
    (switch-to-buffer-other-window buffer)))

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
  (when (fboundp #'undo-tree-undo)
    (evil-set-undo-system 'undo-tree))
  (when (fboundp #'general-define-key)
    (general-define-key
     :states '(motion)
     "ze" nil)))

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
   '(eww devdocs proced emms pass calendar dired ivy debug guix calc
         docker ibuffer geiser pdf info elfeed edebug bookmark company
         vterm flycheck profiler cider explain-pause-mode notmuch custom
         xref eshell helpful compile comint git-timemachine magit prodigy
         slime forge deadgrep vc-annonate telega doc-view gnus outline)))

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

(use-package ace-link
  :straight t
  :commands (ace-link-info ace-link-help ace-link-woman ace-link-eww))

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

(use-package xref
  :straight (:type built-in))

(require 'hideshow)
(general-define-key
 :keymaps '(hs-minor-mode-map outline-minor-mode-map)
 :states '(normal motion)
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
  (evil-set-undo-system 'undo-tree)
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-auto-save-history nil)

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

(setq default-input-method "russian-computer")

(defun my/toggle-input-method ()
  (interactive)
  (if (derived-mode-p 'exwm-mode)
      (my/run-in-background "xkb-switch -n")
    (if (equal (string-trim
                (shell-command-to-string "xkb-switch -p"))
               "us")
        (toggle-input-method)
      (my/run-in-background "xkb-switch -s us"))))

(general-define-key
 :keymaps 'global
 "M-\\" #'my/toggle-input-method)

(use-package smartparens
  :straight t)

(use-package visual-fill-column
  :straight t
  :commands (visual-fill-column-mode)
  :config
  ;; How did it get here?
  ;; (add-hook 'visual-fill-column-mode-hook
  ;;           (lambda () (setq visual-fill-column-center-text t)))
  )

(use-package accent
  :straight (:host github :repo "SqrtMinusOne/accent")
  :init
  (general-define-key
   :states '(normal)
   "gs" #'accent-company)
  (general-define-key
   :states '(normal insert)
   "M-n" #'accent-company)
  :commands (accent-menu)
  :config
  (general-define-key
   :keymaps 'popup-menu-keymap
   "C-j" #'popup-next
   "C-k" #'popup-previous
   "M-j" #'popup-next
   "M-k" #'popup-previous)
  (setq accent-custom '((a (ā))
                        (A (Ā)))))

(use-package binky
  :straight t
  :init
  (my-leader-def "j" #'binky-binky))

(use-package projectile
  :straight t
  :config
  (projectile-mode +1)
  (setq projectile-project-search-path '("~/Code" "~/Documents")))

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
  (require 'forge)
  (setq magit-blame-styles
        '((headings
           (heading-format . "%-20a %C %s\n"))
          (highlight
           (highlight-face . magit-blame-highlight))
          (lines
           (show-lines . t)
           (show-message . t)))))

(use-package git-gutter
  :straight t
  :if (not my/slow-ssh)
  :config
  (global-git-gutter-mode +1))

(use-package git-timemachine
  :straight t
  :commands (git-timemachine))

(use-package forge
  :after magit
  :straight t
  :config
  (add-to-list 'forge-alist '("gitlab.etu.ru"
                              "gitlab.etu.ru/api/v4"
                              "gitlab.etu.ru"
                              forge-gitlab-repository)))

(defun my/password-store-get-field (entry field)
  (if-let (field (password-store-get-field entry field))
      field
    (my/password-store-get-field entry field)))

(defun my/ghub--token (host username package &optional nocreate forge)
  (cond ((and (or (equal host "gitlab.etu.ru/api/v4")
                  (equal host "gitlab.etu.ru/api"))
              (equal username "pvkorytov"))
         (my/password-store-get-field
          "Job/Digital/Infrastructure/gitlab.etu.ru"
          (format "%s-token" package)))
        (t (error "Don't know token: %s %s %s" host username package))))

(with-eval-after-load 'ghub
  (advice-add #'ghub--token :override #'my/ghub--token))

(use-package code-review
  :straight (:host github :repo "phelrine/code-review" :branch "fix/closql-update")
  :after forge
  :config
  (setq code-review-auth-login-marker 'forge)
  (setq code-review-gitlab-base-url "gitlab.etu.ru")
  (setq code-review-gitlab-host "gitlab.etu.ru/api")
  (setq code-review-gitlab-graphql-host "gitlab.etu.ru/api")
  (general-define-key
   :states '(normal visual)
   :keymaps '(code-review-mode-map)
   "RET" #'code-review-comment-add-or-edit
   "gr" #'code-review-reload
   "r" #'code-review-transient-api
   "s" #'code-review-comment-code-suggestion
   "d" #'code-review-submit-single-diff-comment-at-point
   "TAB" #'magit-section-toggle)
  (general-define-key
   :states '(normal)
   :keymaps '(forge-topic-mode-map)
   "M-RET" #'code-review-forge-pr-at-point))

(defun my/code-review-comment-quit ()
  "Quit the comment window."
  (interactive)
  (magit-mode-quit-window t)
  (with-current-buffer (get-buffer code-review-buffer-name)
    (goto-char code-review-comment-cursor-pos)
    (code-review-comment-reset-global-vars)))

(with-eval-after-load 'code-review
  (advice-add #'code-review-comment-quit :override #'my/code-review-comment-quit))

(use-package editorconfig
  :straight t
  :config
  (unless my/slow-ssh (editorconfig-mode 1))
  (add-to-list 'editorconfig-indentation-alist
               '(emmet-mode emmet-indentation)))

(recentf-mode 1)

(save-place-mode nil)

(defun my/deadgrep-fix-buffer-advice (fun &rest args)
  (let ((buf (apply fun args)))
    (with-current-buffer buf
      (toggle-truncate-lines 1))
    buf))

(use-package deadgrep
  :straight t
  :commands (deadgrep)
  :config
  (advice-add #'deadgrep--buffer :around #'my/deadgrep-fix-buffer-advice))

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
               dired-recent-open
               org-ql-view
               my/index-nav
               org-set-effort))
  ;; Do not use prescient in find-file
  (ivy--alist-set 'ivy-sort-functions-alist #'read-file-name-internal #'ivy-sort-file-function-default))

(my-leader-def
  :infix "f"
  "" '(:which-key "various completions")'
  ;; "b" 'counsel-switch-buffer
  "b" 'persp-ivy-switch-buffer
  "e" 'micromamba-activate
  "f" 'project-find-file
  "c" 'counsel-yank-pop
  "a" 'counsel-rg
  "d" 'deadgrep
  "A" 'counsel-ag)

(general-define-key
 :states '(insert normal)
 "C-y" 'counsel-yank-pop)

(defun my/swiper-isearch ()
  (interactive)
  (if current-prefix-arg
      (swiper-all)
    (swiper-isearch)))

(my-leader-def "SPC SPC" 'ivy-resume)
(my-leader-def "s" 'my/swiper-isearch)

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
  (setq company-idle-delay 0.2)
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
  "o" 'helpful-symbol
  "i" 'info)

(general-define-key
 :keymaps 'help-map
 "f" 'helpful-function
 "k" 'helpful-key
 "v" 'helpful-variable
 "o" 'helpful-symbol)

(use-package wakatime-mode
  :straight (:host github :repo "SqrtMinusOne/wakatime-mode")
  :if (not (or my/remote-server))
  :config
  (setq wakatime-ignore-exit-codes '(0 1 102 112))
  (advice-add 'wakatime-init :after (lambda () (setq wakatime-cli-path (expand-file-name "~/bin/wakatime-cli"))))
  (when (file-exists-p "~/.wakatime.cfg")
    (setq wakatime-api-key
          (string-trim
           (shell-command-to-string "awk '/api-key/{print $NF}' ~/.wakatime.cfg"))))
  ;; (setq wakatime-cli-path (executable-find "wakatime"))
  (global-wakatime-mode))

(use-package request
  :straight t
  :defer t)

(use-package activity-watch-mode
  :straight t
  :if (not (or my/is-termux my/remote-server))
  :config
  (global-activity-watch-mode))

(unless my/is-termux
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1))

(when my/is-termux
  (menu-bar-mode -1))

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
  :if (display-graphic-p)
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
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  ;; (if my/remote-server
  ;;     (load-theme 'doom-gruvbox t)
  ;;   (load-theme 'doom-palenight t))
  (doom-themes-visual-bell-config)
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config))

(use-package modus-themes
  :straight t)

(use-package ef-themes
  :straight t
  :config
  (setq ef-duo-light-palette-overrides
        '((constant green))))

(use-package ct
  :straight t)

(defun my/doom-p ()
  (seq-find (lambda (x) (string-match-p (rx bos "doom") (symbol-name x)))
            custom-enabled-themes))

(defun my/modus-p ()
  (seq-find (lambda (x) (string-match-p (rx bos "modus") (symbol-name x)))
            custom-enabled-themes))

(defun my/ef-p ()
  (seq-find (lambda (x) (string-match-p (rx bos "ef") (symbol-name x)))
            custom-enabled-themes))

(defun my/light-p ()
  (ct-light-p (my/color-value 'bg)))

(defun my/dark-p ()
  (not (my/light-p)))

(defconst my/theme-override
  '((doom-palenight
     (red . "#f07178"))))

(defvar my/alpha-for-light 7)

(defun my/doom-color (color)
  (when (doom-color 'bg)
    (let ((override (alist-get (my/doom-p) my/theme-override))
          (color-name (symbol-name color))
          (is-light (ct-light-p (doom-color 'bg))))
      (or
       (alist-get color override)
       (cond
        ((eq 'black color)
         (if is-light (doom-color 'fg) (doom-color 'bg)))
        ((eq 'white color)
         (if is-light (doom-color 'bg) (doom-color 'fg)))
        ((eq 'border color)
         (if is-light (doom-color 'base0) (doom-color 'base8)))
        ((string-match-p (rx bos "light-") color-name)
         (ct-edit-hsl-l-inc (my/doom-color (intern (substring color-name 6)))
                            my/alpha-for-light))
        (t (doom-color color)))))))

(defun my/modus-get-base (color)
  (let ((base-value (string-to-number (substring (symbol-name color) 4 5)))
        (base-start (cadr (assoc 'bg-main (modus-themes--current-theme-palette))))
        (base-end (cadr (assoc 'fg-dim (modus-themes--current-theme-palette)))))
    (nth base-value (ct-gradient 9 base-start base-end t))))

(defun my/prot-color (color palette)
  (let ((is-light (ct-light-p (cadr (assoc 'bg-main palette)))))
    (cond
     ((member color '(black white light-black light-white))
      (let ((bg-main (cadr (assoc 'bg-main palette)))
            (fg-main (cadr (assoc 'fg-main palette))))
        (pcase color
          ('black (if is-light fg-main bg-main))
          ('white (if is-light bg-main fg-main))
          ('light-black (ct-edit-hsl-l-inc
                         (if is-light fg-main bg-main)
                         15))
          ('light-white (ct-edit-hsl-l-inc
                         (if is-light bg-main fg-main)
                         15)))))
     ((or (eq color 'bg))
      (cadr (assoc 'bg-main palette)))
     ((or (eq color 'fg))
      (cadr (assoc 'fg-main palette)))
     ((eq color 'bg-alt)
      (cadr (assoc 'bg-dim palette)))
     ((eq color 'violet)
      (cadr (assoc 'magenta-cooler palette)))
     ((string-match-p (rx bos "base" digit) (symbol-name color))
      (my/modus-get-base color))
     ((string-match-p (rx bos "dark-") (symbol-name color))
      (cadr (assoc (intern (format "%s-cooler" (substring (symbol-name color) 5)))
                   palette)))
     ((eq color 'grey)
      (my/modus-get-base 'base5))
     ((string-match-p (rx bos "light-") (symbol-name color))
      (or
       (cadr (assoc (intern (format "%s-intense" (substring (symbol-name color) 6))) palette))
       (cadr (assoc (intern (format "bg-%s-intense" (substring (symbol-name color) 6))) palette))))
     (t (cadr (assoc color palette))))))

(defun my/modus-color (color)
  (my/prot-color color (modus-themes--current-theme-palette)))

(defun my/ef-color (color)
  (my/prot-color color (ef-themes--current-theme-palette)))

(defconst my/test-colors-list
  '(black red green yellow blue magenta cyan white light-black
          light-red light-green light-yellow light-blue light-magenta
          light-cyan light-white bg fg violet grey base0 base1 base2
          base3 base4 base5 base6 base7 base8 border bg-alt))

(defun my/test-colors ()
  (interactive)
  (let ((buf (generate-new-buffer "*colors-test*")))
    (with-current-buffer buf
      (insert (format "%-20s %-10s %-10s %-10s" "Color" "Doom" "Modus" "Ef") "\n")
      (cl-loop for color in my/test-colors-list
               do (insert
                   (format "%-20s %-10s %-10s %-10s\n"
                           (prin1-to-string color)
                           (my/doom-color color)
                           (my/modus-color color)
                           (my/ef-color color))))
      (special-mode)
      (rainbow-mode))
    (switch-to-buffer buf)))

(defun my/color-value (color)
  (cond
   ((stringp color) (my/color-value (intern color)))
   ((eq color 'bg-other)
    (or (my/color-value 'bg-dim)
        (let ((color (my/color-value 'bg)))
          (if (ct-light-p color)
              (ct-edit-hsl-l-dec color 2)
            (ct-edit-hsl-l-dec color 3)))))
   ((my/doom-p) (my/doom-color color))
   ((my/modus-p) (my/modus-color color))
   ((my/ef-p) (my/ef-color color))))

(deftheme my-theme-1)

(defvar my/my-theme-update-color-params nil)

(defmacro my/use-colors (&rest data)
  `(progn
     ,@(cl-loop for i in data collect
                `(setf (alist-get ',(car i) my/my-theme-update-color-params)
                       (list ,@(cl-loop for (key value) on (cdr i) by #'cddr
                                        append `(,key ',value)))))
     (when (and (or (my/doom-p) (my/modus-p)) my/emacs-started)
       (my/update-my-theme))))

(defun my/update-my-theme (&rest _)
  (interactive)
  (cl-loop for (face . values) in my/my-theme-update-color-params
           do (custom-theme-set-faces
               'my-theme-1
               `(,face ((t ,@(cl-loop for (key value) on values by #'cddr
                                      collect key
                                      collect (eval value)))))))
  (enable-theme 'my-theme-1))

(unless my/is-termux
  (advice-add 'load-theme :after #'my/update-my-theme)
  (add-hook 'emacs-startup-hook #'my/update-my-theme))

(my/use-colors
 (tab-bar-tab :background (my/color-value 'bg)
              :foreground (my/color-value 'yellow)
              :underline (my/color-value 'yellow))
 (tab-bar :background nil :foreground nil)
 (magit-section-secondary-heading :foreground (my/color-value 'blue)
                                  :weight 'bold))

(defun my/switch-theme (theme)
  (interactive
   (list (intern (completing-read "Load custom theme: "
                                  (mapcar #'symbol-name
				                          (custom-available-themes))))))
  (cl-loop for enabled-theme in custom-enabled-themes
           if (not (or (eq enabled-theme 'my-theme-1)
                       (eq enabled-theme theme)))
           do (disable-theme enabled-theme))
  (load-theme theme t)
  (when current-prefix-arg
    (my/regenerate-desktop)))

(my/switch-theme 'ef-duo-light)

(with-eval-after-load 'transient
  (my/use-colors
   (transient-key-exit :foreground (my/color-value 'dark-red))
   (transient-key-noop :foreground (my/color-value 'grey))
   (transient-key-return :foreground (my/color-value 'yellow))
   (transient-key-stay :foreground (my/color-value 'green))))

(use-package auto-dim-other-buffers
  :straight t
  :if (display-graphic-p)
  :config
  (auto-dim-other-buffers-mode t)
  (my/use-colors
   (auto-dim-other-buffers-face
    :background (my/color-value 'bg-other))))

(with-eval-after-load 'ansi-color
  (my/use-colors
   (ansi-color-black
    :foreground (my/color-value 'base2) :background (my/color-value 'base0))
   (ansi-color-red
    :foreground (my/color-value 'red) :background (my/color-value 'red))
   (ansi-color-green
    :foreground (my/color-value 'green) :background (my/color-value 'green))
   (ansi-color-yellow
    :foreground (my/color-value 'yellow) :background (my/color-value 'yellow))
   (ansi-color-blue
    :foreground (my/color-value 'dark-blue) :background (my/color-value 'dark-blue))
   (ansi-color-magenta
    :foreground (my/color-value 'violet) :background (my/color-value 'violet))
   (ansi-color-cyan
    :foreground (my/color-value 'dark-cyan) :background (my/color-value 'dark-cyan))
   (ansi-color-white
    :foreground (my/color-value 'base8) :background (my/color-value 'base8))
   (ansi-color-bright-black
    :foreground (my/color-value 'base5) :background (my/color-value 'base5))
   (ansi-color-bright-red
    :foreground (my/color-value 'orange) :background (my/color-value 'orange))
   (ansi-color-bright-green
    :foreground (my/color-value 'teal) :background (my/color-value 'teal))
   (ansi-color-bright-yellow
    :foreground (my/color-value 'yellow) :background (my/color-value 'yellow))
   (ansi-color-bright-blue
    :foreground (my/color-value 'blue) :background (my/color-value 'blue))
   (ansi-color-bright-magenta
    :foreground (my/color-value 'magenta) :background (my/color-value 'magenta))
   (ansi-color-bright-cyan
    :foreground (my/color-value 'cyan) :background (my/color-value 'cyan))
   (ansi-color-bright-white
    :foreground (my/color-value 'fg) :background (my/color-value 'fg))))

(when (display-graphic-p)
  (if (x-list-fonts "JetBrainsMono Nerd Font")
      (let ((font "-JB  -JetBrainsMono Nerd Font-medium-normal-normal-*-17-*-*-*-m-0-iso10646-1"))
        (set-frame-font font nil t)
        (add-to-list 'default-frame-alist `(font . ,font)))
    (message "Install JetBrainsMono Nerd Font!")))

(when (display-graphic-p)
  (set-face-attribute 'variable-pitch nil :family "Cantarell" :height 1.0)
  (set-face-attribute
   'italic nil
   :family "JetBrainsMono Nerd Font"
   :weight 'regular
   :slant 'italic))

(use-package ligature
  :straight (:host github :repo "mickeynp/ligature.el")
  :if (display-graphic-p)
  :config
  (ligature-set-ligatures
   '(
     typescript-mode
     typescript-ts-mode
     js2-mode
     javascript-ts-mode
     vue-mode
     svelte-mode
     scss-mode
     php-mode
     python-mode
     python-ts-mode
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
  (setq doom-modeline-display-misc-in-all-mode-lines nil)
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

(setq my/perspective-assign-alist '())

(defvar my/perspective-assign-ignore nil
  "If non-nil, ignore `my/perspective-assign'")

(defun my/perspective-assign ()
  (when-let* ((_ (not my/perspective-assign-ignore))
              (rule (alist-get major-mode my/perspective-assign-alist)))
    (let ((workspace-index (car rule))
          (persp-name (cadr rule))
          (buffer (current-buffer)))
      (if (fboundp #'perspective-exwm-assign-window)
          (progn
            (perspective-exwm-assign-window
             :workspace-index workspace-index
             :persp-name persp-name)
            (when workspace-index
              (exwm-workspace-switch workspace-index))
            (when persp-name
              (persp-switch persp-name)))
        (with-perspective persp-name
          (persp-set-buffer buffer))
        (persp-switch-to-buffer buffer)))))

(defun my/perspective-assign-ignore-advice (fun &rest args)
  (let ((my/perspective-assign-ignore t))
    (apply fun args)))

(add-hook 'after-change-major-mode-hook #'my/perspective-assign)

(defmacro my/persp-add-rule (&rest body)
  (declare (indent 0))
  (unless (= (% (length body) 3) 0)
    (error "Malformed body in my/persp-add-rule"))
  (let (result)
    (while body
      (let ((major-mode (pop body))
            (workspace-index (pop body))
            (persp-name (pop body)))
        (push
         `(add-to-list 'my/perspective-assign-alist
                       '(,major-mode . (,workspace-index ,persp-name)))
         result)))
    `(progn
       ,@result)))

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

(defun my/lsp--progress-status ()
  "Returns the status of the progress for the current workspaces."
  (-let ((progress-status
          (s-join
           "|"
           (-keep
            (lambda (workspace)
              (let ((tokens (lsp--workspace-work-done-tokens workspace)))
                (unless (ht-empty? tokens)
                  (mapconcat
                   (-lambda ((&WorkDoneProgressBegin :message? :title :percentage?))
                     (concat (if percentage?
                                 (if (numberp percentage?)
                                     (format "%.0f%%%% " percentage?)
                                   (format "%s%%%% " percentage?))
                               "")
                             (let ((msg (url-unhex-string (or message\? title))))
                               (if (string-match-p "\\`file:///" msg)
                                   (file-name-nondirectory msg)))))
                   (ht-values tokens)
                   "|"))))
            (lsp-workspaces)))))
    (unless (s-blank? progress-status)
      (concat lsp-progress-prefix progress-status))))

(with-eval-after-load 'lsp-mode
  (advice-add 'lsp--progress-status :override #'my/lsp--progress-status))

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

(defun my/set-smartparens-indent (mode)
  (sp-local-pair mode "{" nil :post-handlers '(("|| " "SPC") ("||\n[i]" "RET")))
  (sp-local-pair mode "[" nil :post-handlers '(("|| " "SPC") ("||\n[i]" "RET")))
  (sp-local-pair mode "(" nil :post-handlers '(("|| " "SPC") ("||\n[i]" "RET"))))

(defun my/set-flycheck-eslint()
  "Override flycheck checker with eslint."
  (setq-local lsp-diagnostic-package :none)
  (setq-local flycheck-checker 'javascript-eslint))

(use-package treesit
  :straight (:type built-in)
  :if (featurep 'treesit)
  :config
  (setq treesit-language-source-alist
        (mapcar
         (lambda (item)
           (let ((lang (nth 0 item))
                 (url (nth 1 item))
                 (rev (nth 2 item))
                 (source-dir (nth 3 item)))
             `(,lang ,url ,rev ,source-dir
                     ,(executable-find "gcc") ,(executable-find "c++"))))
         '((bash "https://github.com/tree-sitter/tree-sitter-bash")
           (cmake "https://github.com/uyha/tree-sitter-cmake")
           (css "https://github.com/tree-sitter/tree-sitter-css")
           (elisp "https://github.com/Wilfred/tree-sitter-elisp")
           (go "https://github.com/tree-sitter/tree-sitter-go")
           (html "https://github.com/tree-sitter/tree-sitter-html")
           (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
           (json "https://github.com/tree-sitter/tree-sitter-json")
           (make "https://github.com/alemuller/tree-sitter-make")
           (markdown "https://github.com/ikatyang/tree-sitter-markdown")
           (python "https://github.com/tree-sitter/tree-sitter-python")
           (toml "https://github.com/tree-sitter/tree-sitter-toml")
           (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
           (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
           (yaml "https://github.com/ikatyang/tree-sitter-yaml"))))
  (setq treesit-font-lock-level 4)
  (setq major-mode-remap-alist
        '((typescript-mode . typescript-ts-mode)
          (js-mode . javascript-ts-mode)
          (python-mode . python-ts-mode)
          (json-mode . json-ts-mode)))
  (cl-loop for (old-mode . new-mode) in major-mode-remap-alist
           do (my/set-smartparens-indent new-mode)
           do (set (intern (concat (symbol-name new-mode) "-hook"))
                   (list
                    (eval `(lambda ()
                             (run-hooks
                              ',(intern (concat (symbol-name old-mode) "-hook")))))))))

(use-package dap-mode
  :straight t
  :if (not (or my/remote-server my/is-termux))
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
  :if (not (or my/remote-server my/is-termux))
  :init
  (add-hook 'prog-mode-hook #'copilot-mode)
  :config
  (setq copilot-node-executable "/home/pavel/.guix-extra-profiles/dev/dev/bin/node")
  (general-define-key
   :keymaps 'company-active-map
   "<backtab>" #'my/copilot-tab)
  (general-define-key
   :keymaps 'copilot-mode-map
   "<tab>" #'my/copilot-tab
   "M-j" #'copilot-accept-completion-by-line
   "M-l" #'copilot-accept-completion-by-word)
  (setq copilot-lispy-integration t))

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
  (add-hook 'typescript-mode-hook #'hs-minor-mode)
  :config
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
  (my/set-smartparens-indent 'web-mode)
  (setq web-mode-auto-pairs nil))

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
    (setq-local create-lockfiles nil)
    (setq-local web-mode-enable-auto-pairing nil)))

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

(use-package adoc-mode
  :straight t)

(use-package plantuml-mode
  :straight t
  :mode "(\\.\\(plantuml?\\|uml\\|puml\\)\\'"
  :config
  (setq plantuml-executable-path "/home/pavel/.guix-extra-profiles/emacs/emacs/bin/plantuml")
  (setq plantuml-default-exec-mode 'executable)
  (setq plantuml-indent-level 2)
  (setq my/plantuml-indent-regexp-return "^\s*return\s+.+$")
  (;; (add-to-list
   ;;  'plantuml-indent-regexp-end
   ;;  my/plantuml-indent-regexp-return)
   )
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  (add-to-list 'auto-mode-alist '("\\.uml\\'" . plantuml-mode))
  (add-hook 'plantuml-mode-hook #'smartparens-mode)
  (general-nmap
    :keymaps 'plantuml-mode-map
    "RET" 'plantuml-preview))

(use-package subed
  :straight (:host github :repo "rndusr/subed" :files ("subed/*.el")
                   :build (:not native-compile))
  :config
  (general-define-key
   :keymaps '(subed-mode-map subed-vtt-mode-map)
   :states '(normal)
   "gp" #'subed-mpv-toggle-pause))

(use-package lsp-ltex
  :straight t
  :after (lsp)
  :init
  (setq lsp-ltex-version "15.2.0")
  (setq lsp-ltex-check-frequency "save"))

(defun my/ltex-lang ()
  (interactive)
  (setq lsp-ltex-language (completing-read
                           "Language: "
                           '("en-US" "ru-RU" "de-DE")))
  (lsp-workspace-restart (lsp--read-workspace)))

(defun my/ltex-need-p ()
  (let ((file-name (buffer-file-name)))
    (cond
     ((null file-name) nil)
     ((string-match-p (rx "/home/pavel/" (+ alnum) ".org" eos) file-name) nil)
     ((string-match-p (rx (literal org-directory) "/" (or "roam" "inbox-notes" "literature-notes" "journal")) file-name) t)
     ((string-match-p (rx (literal org-directory)) file-name) nil)
     ((string-match-p (rx (literal (expand-file-name user-emacs-directory))) file-name) nil)
     (t t))))

(defun my/text-mode-lsp-maybe ()
  (when (my/ltex-need-p)
    (lsp)))

(add-hook 'text-mode-hook #'my/text-mode-lsp-maybe)

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

(use-package reverso
  :straight (:host github :repo "SqrtMinusOne/reverso.el")
  :init
  (my-leader-def "ar" #'reverso)
  :config
  (setq reverso-languages '(russian english german))
  (reverso-history-mode))

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

(defun advice-unadvice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

(add-hook 'inferior-emacs-lisp-mode-hook #'smartparens-mode)
(my-leader-def "bi" #'ielm)

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
  :disabled t
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
         (save-excursion
           (unless (and (fboundp #'org-src-edit-buffer-p) (org-src-edit-buffer-p))
             (py-isort-buffer))
           (python-black-buffer))))

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
  (add-hook 'json-mode #'smartparens-mode)
  (add-hook 'json-mode #'hs-minor-mode)
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

(use-package nginx-mode
  :straight t
  :config
  (my/set-smartparens-indent 'nginx-mode))

(use-package hcl-mode
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
  :straight t)

(use-package graphql-mode
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
  :disabled t
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

(use-package org
  :straight (:type built-in)
  :if (not my/remote-server)
  :defer t
  :init
  (setq org-directory (expand-file-name "~/30-39 Life/32 org-mode"))
  (unless (file-exists-p org-directory)
    (mkdir org-directory t))
  :config
  (setq org-startup-indented (not my/is-termux))
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
  (setq org-tags-exclude-from-inheritance '("crypt"))
  (setq org-crypt-key "C1EC867E478472439CC82410DE004F32AFA00205"))

(with-eval-after-load 'epg
  (setq epg-gpg-program "gpg")
  (setq epg-config--program-alist
        `((OpenPGP
           epg-gpg-program
           ;; ("gpg2" . ,epg-gpg2-minimum-version)
           ("gpg" . ((,epg-gpg-minimum-version . "2.0")
                     ,epg-gpg2-minimum-version)))
          (CMS
           epg-gpgsm-program
           ("gpgsm" . "2.0.4")))))

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
             :build t)
  :after (org)
  :if (not my/remote-server)
  :config
  (require 'ox-extra)
  (ox-extras-activate '(latex-header-blocks ignore-headlines)))

(unless (or my/remote-server my/is-termux)
  (use-package ol-notmuch
    :straight t
    :after (org notmuch)))

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

(defun my/export-rel-url (path desc format)
  (cl-case format
    (html (format "<a href=\"%s\">%s</a>" path (or desc path)))
    (latex (format "\\href{%s}{%s}" path (or desc path)))
    (otherwise path)))

(with-eval-after-load 'org
  (org-link-set-parameters "rel" :follow #'browse-url :export #'my/export-rel-url))

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
  (let* ((files
          (thread-last
            '("projects" "misc")
            (mapcar (lambda (f)
                      (directory-files (concat org-directory "/" f) t (rx ".org" eos))))
            (apply #'append)
            (mapcar (lambda (file)
                      (string-replace (concat org-directory "/") "" file)))
            (append
             '("inbox.org" "contacts.org")))))
    (find-file
     (concat org-directory "/"
             (completing-read "Org file: " files)))))

(use-package jupyter
  :straight t
  :after (org)
  :if (not (or my/remote-server my/is-termux)))

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
  :straight t
  :config
  (general-define-key
   :keymaps 'restclient-mode-map
   :states '(normal visual)
   "RET" #'restclient-http-send-current
   "M-RET" #'restclient-http-send-current-stay-in-window
   "y" nil
   "M-y" #'restclient-copy-curl-command)
  (general-define-key
   :keymaps 'restclient-response-mode-map
   :states '(normal visual)
   "q" #'quit-window))

(use-package ob-restclient
  :after (org restclient)
  :if (not my/remote-server)
  :straight t)

(with-eval-after-load-norem 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   `((emacs-lisp . t)
     (python . t)
     (sql . t)
     ;; (typescript .t)
     (hy . t)
     (shell . t)
     (plantuml . t)
     (octave . t)
     ,@(unless my/is-termux '((jupyter . t)))
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
                ;; (org-latex-preview '(16))
                ;; TODO ^somehow this stucks at running LaTeX^
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

(use-package edraw-org
  :straight (:host github :repo "misohena/el-easydraw")
  :if (and (not my/is-termux) (not my/remote-server))
  :after (org)
  :config
  (edraw-org-setup-default))

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

(defun my/update-org-agenda ()
  (interactive)
  (let ((project-files
         (mapcar
          (lambda (f) (concat
                       org-directory "/projects/"
                       f))
          (seq-filter
           (lambda (f) (not (file-directory-p f)))
           (directory-files
            (concat org-directory "/projects"))))))
    (setq org-agenda-files
          `("inbox.org"
            "misc/habit.org"
            "contacts.org"
            ,@project-files))
    (setq org-refile-targets
          `(,@(mapcar
               (lambda (f) `(,f . (:tag . "refile")))
               project-files)
            ,@(mapcar
               (lambda (f) `(,f . (:regexp . "Tasks")))
               project-files)))
    (when (file-exists-p (concat org-directory "/scripts/refile.el"))
      (load-file (concat org-directory "/scripts/refile.el"))
      (run-hooks 'my/org-refile-hooks))))

(with-eval-after-load-norem 'org
  (setq org-roam-directory (concat org-directory "/roam"))
  (my/update-org-agenda))

(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)

(setq org-extend-today-until 4)

(defun my/generate-inbox-note-name ()
  (format
   "%s/inbox-notes/%s%s.org"
   org-directory
   (format-time-string "%Y%m%d%H%M%S")
   (let ((note-name (read-string "Note name: ")))
     (if (not (string-empty-p note-name))
         (string-replace " " "-" (concat "-" (downcase note-name)))
       ""))))

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
        ("n" "note" plain (file my/generate-inbox-note-name)
         ,(concat "#+TODO: PROCESSED(p)\n"
                  "\n"
                  "* %?\n"
                  "/Entered on/ %U"))))

(use-package org-clock-agg
  :straight (:host github :repo "SqrtMinusOne/org-clock-agg")
  :commands (org-clock-agg)
  :init
  (with-eval-after-load 'org
    (my-leader-def "ol" #'org-clock-agg)))

(with-eval-after-load 'org
  (setq org-clock-persist 'clock)
  (org-clock-persistence-insinuate))

(with-eval-after-load-norem 'org
  (add-to-list
   'org-global-properties
   '("Effort_ALL" . "0 0:05 0:10 0:15 0:30 0:45 1:00 1:30 2:00 4:00 8:00")))

(setq org-log-done 'time)

(defun my/org-clock-in--fix-mode-line ()
  (when (memq 'org-mode-line-string global-mode-string)
    (let (new-global-mode-string
          appended
          (is-first t))
      (dolist (item global-mode-string)
        (cond
         ((or (equal item '(:eval (exwm-modeline-segment)))
              (equal item '(:eval (persp-mode-line))))
          (unless appended
            (when is-first
              (push "" new-global-mode-string))
            (push 'org-mode-line-string new-global-mode-string)
            (setq appended t))
          (push item new-global-mode-string))
         ((equal item 'org-mode-line-string))
         (t
          (push item new-global-mode-string)))
        (setq is-first nil))
      (unless appended
        (push 'org-mode-line-string new-global-mode-string))
      (setq global-mode-string (nreverse new-global-mode-string)))))

(add-hook 'org-clock-in-hook #'my/org-clock-in--fix-mode-line)

(defun my/org-clock-in-prompt-time (&optional select)
  (interactive "P")
  (org-clock-in
   select
   (encode-time
    (org-parse-time-string
     (org-read-date t)))))

(with-eval-after-load 'org
  (my-leader-def
    :keymaps 'org-mode-map
    :infix "SPC"
    "I" #'my/org-clock-in-prompt-time))

(defun my/org-clock-get-total-minutes-at-point ()
  "Get total clocked time for heading at point."
  (let* ((element (org-element-at-point-no-context))
         (s (buffer-substring-no-properties
             (org-element-property :begin element)
             (org-element-property :end element))))
    (with-temp-buffer
      (insert s)
      (org-clock-sum)
      org-clock-file-total-minutes)))

(defconst my/org-clock-total-prop :CLOCK_TOTAL)

(defun my/org-clock-set-total-clocked ()
  "Set total clocked time for heading at point."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (org-set-property
     (substring
      (symbol-name my/org-clock-total-prop)
      1)
     (org-duration-from-minutes
      (my/org-clock-get-total-minutes-at-point)))))

(add-hook 'org-clock-in-hook #'my/org-clock-set-total-clocked)
(add-hook 'org-clock-out-hook #'my/org-clock-set-total-clocked)
(add-hook 'org-clock-cancel-hook #'my/org-clock-set-total-clocked)

(use-package org-super-agenda
  :straight t
  :after (org)
  :config
  ;; Alphapapa doesn't like evil
  (general-define-key
   :keymaps '(org-super-agenda-header-map)
   "h" nil
   "j" nil
   "k" nil
   "l" nil)
  (org-super-agenda--def-auto-group outline-path-file "their outline paths & files"
    :key-form
    (org-super-agenda--when-with-marker-buffer (org-super-agenda--get-marker item)
      ;; org-ql depends on f and s anyway
      (s-join "/" (cons
                   (f-filename (buffer-file-name))
                   (org-get-outline-path))))))

(defun my/org-super-agenda--make-agenda-header-around (fun name)
  (remove-text-properties 0 (length name) '(line-prefix nil) name)
  (remove-text-properties 0 (length name) '(wrap-prefix nil) name)
  (funcall fun (substring-no-properties name)))

(with-eval-after-load 'org-super-agenda
  (advice-add 'org-super-agenda--make-agenda-header :around #'my/org-super-agenda--make-agenda-header-around))

(use-package org-ql
  :after (org)
  :if (not my/remote-server)
  :straight t
  :init
  ;; See https://github.com/alphapapa/org-ql/pull/237
  (setq org-ql-regexp-part-ts-time
        (rx " " (repeat 1 2 digit) ":" (repeat 2 digit)
            (optional "-" (repeat 1 2 digit) ":" (repeat 2 digit))))
  (my-leader-def "ov" #'org-ql-view)
  (my-leader-def "oq" #'org-ql-search))

(with-eval-after-load 'org-ql
  (org-ql-defpred property (property &optional value &key inherit multi)
    "Return non-nil if current entry has PROPERTY, and optionally VALUE.
If INHERIT is nil, only match entries with PROPERTY set on the
entry; if t, also match entries with inheritance.  If INHERIT is
not specified, use the Boolean value of
`org-use-property-inheritance', which see (i.e. it is only
interpreted as nil or non-nil).  If MULTI is non-nil, also check for
multi-value properties."
    :normalizers ((`(,predicate-names)
                   ;; HACK: This clause protects against the case in
                   ;; which the arguments are nil, which would cause an
                   ;; error in `rx-to-string' in other clauses.  This
                   ;; can happen with `org-ql-completing-read',
                   ;; e.g. when the input is "property:" while the user
                   ;; is typing.
                   ;; FIXME: Instead of this being moot, make this
                   ;; predicate test for whether an entry has local
                   ;; properties when no arguments are given.
                   (list 'property ""))
                  (`(,predicate-names ,property ,value . ,plist)
                   ;; Convert keyword property arguments to strings.  Non-sexp
                   ;; queries result in keyword property arguments (because to do
                   ;; otherwise would require ugly special-casing in the parsing).
                   (when (keywordp property)
                     (setf property (substring (symbol-name property) 1)))
                   (list 'property property value
                         :inherit (if (plist-member plist :inherit)
                                      (plist-get plist :inherit)
                                    org-use-property-inheritance)
                         :multi (when (plist-member plist :multi)
                                  (plist-get plist :multi)))))
    ;; MAYBE: Should case folding be disabled for properties?  What about values?
    ;; MAYBE: Support (property) without args.

    ;; NOTE: When inheritance is enabled, the preamble can't be used,
    ;; which will make the search slower.
    :preambles ((`(,predicate-names ,property ,value ,(map :multi) . ,(map :inherit))
                 ;; We do NOT return nil, because the predicate still needs to be tested,
                 ;; because the regexp could match a string not inside a property drawer.
                 (list :regexp (unless inherit
                                 (rx-to-string `(seq bol (0+ space) ":" ,property
                                                     ,@(when multi '((? "+"))) ":"
                                                     (1+ space) ,value (0+ space) eol)))
                       :query query))
                (`(,predicate-names ,property ,(map :multi) . ,(map :inherit))
                 ;; We do NOT return nil, because the predicate still needs to be tested,
                 ;; because the regexp could match a string not inside a property drawer.
                 ;; NOTE: The preamble only matches if there appears to be a value.
                 ;; A line like ":ID: " without any other text does not match.
                 (list :regexp (unless inherit
                                 (rx-to-string `(seq bol (0+ space) ":" ,property
                                                     ,@(when multi '((? "+")))
                                                     ":" (1+ space)
                                                     (minimal-match (1+ not-newline)) eol)))
                       :query query)))
    :body
    (pcase property
      ('nil (user-error "Property matcher requires a PROPERTY argument"))
      (_ (pcase value
           ('nil
            ;; Check that PROPERTY exists
            (org-ql--value-at
             (point) (lambda ()
                       (org-entry-get (point) property))))
           (_
            ;; Check that PROPERTY has VALUE.

            ;; TODO: Since --value-at doesn't account for inheritance,
            ;; we should generalize --tags-at to also work for property
            ;; inheritance and use it here, which should be much faster.
            (if multi
                (when-let (values (org-ql--value-at
                                   (point) (lambda ()
                                             ;; The default separator is space
                                             (let ((org-property-separators `((,property . "\n"))))
                                               (org-entry-get (point) property inherit)))))
                  (seq-some (lambda (v)
                              (string-equal value v))
                            (split-string values "\n")))
              (string-equal value (org-ql--value-at
                                   (point) (lambda ()
                                             (org-entry-get (point) property inherit)))))))))))

(cl-defun my/org-ql-view-recent-items
    (&key num-days (type 'ts)
          (files (org-agenda-files))
          (groups '((:auto-outline-path-file t)
                    (:auto-todo t))))
  "Show items in FILES from last NUM-DAYS days with timestamps of TYPE.
TYPE may be `ts', `ts-active', `ts-inactive', `clocked', or
`closed'."
  (interactive (list :num-days (read-number "Days: ")
                     :type (->> '(ts ts-active ts-inactive clocked closed)
                                (completing-read "Timestamp type: ")
                                intern)))
  ;; It doesn't make much sense to use other date-based selectors to
  ;; look into the past, so to prevent confusion, we won't allow them.
  (-let* ((query (pcase-exhaustive type
                   ((or 'ts 'ts-active 'ts-inactive)
                    `(,type :from ,(- num-days) :to 0))
                   ((or 'clocked 'closed)
                    `(,type :from ,(- num-days) :to 0)))))
    (org-ql-search files query
      :title "Recent items"
      :sort '(todo priority date)
      :super-groups groups)))

(defun my/org-ql-all-todo ()
  (interactive)
  ;; The hack I borrowed from notmuch to make " " a separator
  (let* ((crm-separator " ")
         (crm-local-completion-map
	      (let ((map (make-sparse-keymap)))
	        (set-keymap-parent map crm-local-completion-map)
	        (define-key map " " 'self-insert-command)
	        map))
         (ivy-prescient-sort-commands nil)
         (categories (completing-read-multiple
                      "Categories: "
                      '("TEACH" "EDU" "JOB" "LIFE" "CONFIG"))))
    (org-ql-search (org-agenda-files)
      `(and (todo)
            ,@(unless (seq-empty-p categories)
                `((category ,@categories))))
      :sort '(priority todo deadline)
      :super-groups '((:auto-outline-path-file t)))))

(setq org-ql-views
      (list
       (cons "Overview: All TODO" #'my/org-ql-all-todo)
       (cons "Review: Stale tasks"
             (list :buffers-files #'org-agenda-files
                   :query '(and (todo)
                                (not (ts :from -14)))
                   :title "Review: Stale tasks"
                   :sort '(todo priority date)
                   :super-groups '((:auto-outline-path-file t))))
       (cons "Review: Recently timestamped" #'my/org-ql-view-recent-items)
       (cons "Review: Unlinked to meetings"
             (list :buffers-files #'org-agenda-files
                   :query '(and (todo "DONE" "NO")
                                (not (property "MEETING"))
                                (ts :from -7))
                   :super-groups '((:auto-outline-path-file t))))
       (cons "Review: Meeting" #'my/org-ql-meeting-tasks)))

(defun my/org-ql-view--format-element-override (element)
  "Format ELEMENT for `org-ql-view'.

Check `org-ql-view--format-element' for the original implementation
and lots of comments which are too long for my Emacs config."
  (if (not element)
      ""
    (setf element (org-ql-view--resolve-element-properties element))
    (let* ((properties (cadr element))
           (properties (cl-loop for (key val) on properties by #'cddr
                                for symbol = (intern (cl-subseq (symbol-name key) 1))
                                unless (member symbol '(parent))
                                append (list symbol val)))
           (title (--> (org-ql-view--add-faces element)
                       (org-element-property :raw-value it)
                       (org-link-display-format it)))
           (todo-keyword (-some--> (org-element-property :todo-keyword element)
                           (org-ql-view--add-todo-face it)))
           (tag-list (if org-use-tag-inheritance
                         (if-let ((marker (or (org-element-property :org-hd-marker element)
                                              (org-element-property :org-marker element))))
                             (with-current-buffer (marker-buffer marker)
                               (org-with-wide-buffer
                                (goto-char marker)
                                (cl-loop for type in (org-ql--tags-at marker)
                                         unless (or (eq 'org-ql-nil type)
                                                    (not type))
                                         append type)))
                           (display-warning 'org-ql (format "No marker found for item: %s" title))
                           (org-element-property :tags element))
                       (org-element-property :tags element)))
           (tag-string (when tag-list
                         (--> tag-list
                              (s-join ":" it)
                              (s-wrap it ":")
                              (org-add-props it nil 'face 'org-tag))))
           ;;  (category (org-element-property :category element))
           (priority-string (-some->> (org-element-property :priority element)
                              (char-to-string)
                              (format "[#%s]")
                              (org-ql-view--add-priority-face)))
           (clock-string (let ((effort (org-element-property :EFFORT element))
                               (clocked (org-element-property my/org-clock-total-prop element)))
                           (cond
                            ((and clocked effort) (format "[%s/%s]" clocked effort))
                            ((and clocked (not effort) (format "[%s]" clocked)))
                            ((and (not clocked) effort) (format "[EST: %s]" effort)))))
           (habit-property (org-with-point-at (or (org-element-property :org-hd-marker element)
                                                  (org-element-property :org-marker element))
                             (when (org-is-habit-p)
                               (org-habit-parse-todo))))
           (due-string (pcase (org-element-property :relative-due-date element)
                         ('nil "")
                         (string (format " %s " (org-add-props string nil 'face 'org-ql-view-due-date)))))
           (string (s-join " " (-non-nil (list todo-keyword priority-string title due-string clock-string tag-string)))))
      (remove-list-of-text-properties 0 (length string) '(line-prefix) string)
      (--> string
           (concat "  " it)
           (org-add-props it properties
             'org-agenda-type 'search
             'todo-state todo-keyword
             'tags tag-list
             'org-habit-p habit-property)))))

(with-eval-after-load 'org-ql
  (advice-add #'org-ql-view--format-element :override #'my/org-ql-view--format-element-override))

(defun my/org-meeting--prompt ()
  (let* ((meetings (org-ql-query
                     :select #'element-with-markers
                     :from (org-agenda-files)
                     :where '(and (todo) (tags "mt") (ts-active :from today to 31))
                     :order-by 'scheduled))
         (data (mapcar
                (lambda (meeting)
                  (let ((raw-value (org-element-property :raw-value meeting))
                        (scheduled (org-format-timestamp
                                    (org-element-property :scheduled meeting)
                                    (cdr org-time-stamp-formats))))
                    (cons (format "%-30s %s" raw-value
                                  (propertize scheduled 'face 'org-agenda-date))
                          meeting)))
                meetings))
         (ivy-prescient-sort-commands nil))
    (cdr
     (assoc
      (completing-read "Meeting: " data nil t)
      data))))

(defun my/org-meeting--format-link (meeting)
  (format "[[file:%s::*%s][%s]]"
          (buffer-file-name
           (marker-buffer
            (org-element-property :org-marker meeting)))
          (org-element-property :raw-value meeting)
          (org-element-property :raw-value meeting)))

(defun my/org-meeting-link (&optional arg)
  (interactive "p")
  (save-excursion
    (org-back-to-heading t)
    (let* ((meeting (my/org-meeting--prompt))
           (link (my/org-meeting--format-link meeting))
           (element (org-element-at-point-no-context)))
      (if (or (not arg) (not (org-element-property :MEETING element)))
          (org-set-property "MEETING" link)
        (let ((range (org-get-property-block
                      (org-element-property :begin element)))
              (case-fold-search nil))
          (goto-char (cdr range))
          (beginning-of-line)
          (insert-and-inherit ":MEETING+: " link "\n")
          (org-indent-line))))))

(defun my/org-ql-meeting-tasks (meeting)
  (interactive (list (my/org-meeting--prompt)))
  (org-ql-search (org-agenda-files)
    `(property "MEETING" ,(my/org-meeting--format-link meeting)
               :multi t)
    :sort '(date priority todo)
    :buffer (format "*Meeting Tasks: %s*" (org-element-property :raw-value meeting))
    :super-groups '((:auto-outline-path t))))

(defun my/org-ql-meeting-tasks-agenda ()
  (interactive)
  (let ((meeting (save-window-excursion
                   (org-agenda-switch-to)
                   (org-back-to-heading)
                   (org-ql--add-markers
                    (org-element-at-point)))))
    (my/org-ql-meeting-tasks meeting)))

(with-eval-after-load 'org-agenda
  (general-define-key
   :keymaps 'org-agenda-mode-map
   :states '(normal motion)
   "gm" #'my/org-ql-meeting-tasks-agenda))

(use-package org-habit-stats
  :straight (:host github :repo "ml729/org-habit-stats")
  :after (org)
  :config
  (general-define-key
   :keymaps '(org-habit-stats-mode-map)
   :states '(normal emacs)
   "q" #'org-habit-stats-exit
   "<" #'org-habit-stats-calendar-scroll-left
   ">" #'org-habit-stats-calendar-scroll-right
   "[" #'org-habit-stats-scroll-graph-left
   "]" #'org-habit-stats-scroll-graph-right
   "{" #'org-habit-stats-scroll-graph-left-big
   "}" #'org-habit-stats-scroll-graph-right-big
   "." #'org-habit-stats-view-next-habit
   "," #'org-habit-stats-view-previous-habit)
   (add-hook 'org-after-todo-state-change-hook 'org-habit-stats-update-properties))

(defun my/org-match-at-point-p (match)
  "Return non-nil if headline at point matches MATCH.
Here MATCH is a match string of the same format used by
`org-tags-view'."
  (funcall (cdr (org-make-tags-matcher match))
           (org-get-todo-state)
           (org-get-tags-at)
           (org-reduced-level (org-current-level))))

(defun my/org-agenda-skip-without-match (match)
  "Skip current headline unless it matches MATCH.

Return nil if headline containing point matches MATCH (which
should be a match string of the same format used by
`org-tags-view').  If headline does not match, return the
position of the next headline in current buffer.

Intended for use with `org-agenda-skip-function', where this will
skip exactly those headlines that do not match."
  (save-excursion
    (unless (org-at-heading-p) (org-back-to-heading))
    (let ((next-headline (save-excursion
                           (or (outline-next-heading) (point-max)))))
      (if (my/org-match-at-point-p match) nil next-headline))))

(defun my/org-scheduled-get-time ()
  (let ((scheduled (org-get-scheduled-time (point))))
    (if scheduled
        (format-time-string "%Y-%m-%d" scheduled)
      "")))

(setq org-agenda-hide-tags-regexp (rx (or "org" "refile" "proj" "habit")))

(setq org-agenda-custom-commands
      `(("p" "My outline"
         ((agenda "" ((org-agenda-skip-function '(my/org-agenda-skip-without-match "-habit"))))
          (tags-todo "inbox"
                     ((org-agenda-overriding-header "Inbox")
                      (org-agenda-prefix-format " %i %-12:c")
                      (org-agenda-hide-tags-regexp ".")))
          (tags-todo "+waitlist+SCHEDULED<=\"<+14d>\""
                     ((org-agenda-overriding-header "Waitlist")
                      (org-agenda-hide-tags-regexp "waitlist")
                      (org-agenda-prefix-format " %i %-12:c %-12(my/org-scheduled-get-time)")))
          (tags-todo "habit+SCHEDULED<=\"<+0d>\""
                     ((org-agenda-overriding-header "Habits")
                      (org-agenda-prefix-format " %i %-12:c")
                      (org-agenda-hide-tags-regexp ".")))))))

(setq my/org-alert-notify-times '(600 60))

(setq my/org-alert--alerts (make-hash-table :test #'equal))

(defun my/org-alert--is-scheduled (label time)
  "Check if LABEL is scheduled to be shown an TIME."
  (gethash (cons label time)
           my/org-alert--alerts nil))

(defun my/org-alert--schedule (label time)
  "Schedule LABEL to be shown at TIME, unless it's already scheduled."
  (unless (my/org-alert--is-scheduled label time)
    (puthash (cons label time)
             (run-at-time time
                          nil
                          (lambda ()
                            (alert label
                                   :title "PROXIMITY ALERT")))
             my/org-alert--alerts)))

(defun my/org-alert-cleanup (&optional keys)
  "Unschedule items that do not appear in KEYS.

KEYS is a list of cons cells like (<label> . <time>)."
  (let ((existing-hash (make-hash-table :test #'equal)))
    (cl-loop for key in keys
             do (puthash key t existing-hash))
    (cl-loop for key being the hash-keys of my/org-alert--alerts
             unless (gethash key existing-hash)
             do (progn
                  (cancel-timer (gethash key my/org-alert--alerts))
                  (remhash key my/org-alert--alerts)))))

(defun my/org-alert--update-today-alerts ()
  (let ((items
         (org-ql-query
           :select 'element
           :from (org-agenda-files)
           :where `(and
                    (todo "FUTURE")
                    (ts-active :from ,(format-time-string "%Y-%m-%d %H:%M")
                               :to ,(format-time-string
                                     "%Y-%m-%d"
                                     (time-add
                                      (current-time)
                                      (* 60 60 24)))
                               :with-time t))
           :order-by 'date))
        scheduled-keys)
    (cl-loop
     for item in items
     for scheduled = (org-timestamp-to-time (org-element-property :scheduled item))
     do (cl-loop
         for before-time in my/org-alert-notify-times
         for label = (format "%s at %s [%s min. remaining]"
                             (org-element-property :raw-value item)
                             (format-time-string "%H:%M" scheduled)
                             (number-to-string (/ before-time 60)))
         for time = (time-convert
                     (+ (time-convert scheduled 'integer) (- before-time)))
         do (progn
              (my/org-alert--schedule label time)
              (push (cons label time) scheduled-keys))))
    (my/org-alert-cleanup scheduled-keys)))

(setq my/org-alert--timer nil)

(define-minor-mode my/org-alert-mode ()
  :global t
  :after-hook
  (if my/org-alert-mode
      (progn
        (my/org-alert--update-today-alerts)
        (when (timerp my/org-alert--timer)
          (cancel-timer my/org-alert--timer))
        (setq my/org-alert--timer
              (run-at-time 600 t #'my/org-alert--update-today-alerts)))
    (when (timerp my/org-alert--timer)
      (cancel-timer my/org-alert--timer))
    (my/org-alert-cleanup)))

(with-eval-after-load 'org
  (if my/emacs-started
      (my/org-alert-mode)
    (add-hook 'emacs-startup-hook #'my/org-alert-mode)))

(defun my/org-clone-subtree-with-time-shift (n &optional shift)
  (interactive "nNumber of clones to produce: ")
  (unless (wholenump n) (user-error "Invalid number of replications %s" n))
  (when (org-before-first-heading-p) (user-error "No subtree to clone"))
  (let* ((beg (save-excursion (org-back-to-heading t) (point)))
	     (end-of-tree (save-excursion (org-end-of-subtree t t) (point)))
	     (shift
	      (or shift
	          (if (and (not (equal current-prefix-arg '(4)))
		               (save-excursion
			             (goto-char beg)
			             (re-search-forward org-ts-regexp-both end-of-tree t)))
		          (read-from-minibuffer
		           "Date shift per clone (e.g. +1w, empty to copy unchanged): ")
		        "")))                   ;No time shift
	     (doshift
	      (and (org-string-nw-p shift)
	           (or (string-match "\\`[ \t]*\\([+-]?[0-9]+\\)\\([hdwmy]\\)[ \t]*\\'"
				                 shift)
		           (user-error "Invalid shift specification %s" shift)))))
    (goto-char end-of-tree)
    (unless (bolp) (insert "\n"))
    (let* ((end (point))
	       (template (buffer-substring beg end))
	       (shift-n (and doshift (string-to-number (match-string 1 shift))))
	       (shift-what (pcase (and doshift (match-string 2 shift))
			             (`nil nil)
			             ("h" 'hour)
			             ("d" 'day)
			             ("w" (setq shift-n (* 7 shift-n)) 'day)
			             ("m" 'month)
			             ("y" 'year)
			             (_ (error "Unsupported time unit"))))
	       (nmin 1)
	       (nmax n)
	       (n-no-remove -1)
	       (org-id-overriding-file-name (buffer-file-name (buffer-base-buffer)))
	       (idprop (org-entry-get beg "ID")))
      (when (and doshift
		         (string-match-p "<[^<>\n]+ [.+]?\\+[0-9]+[hdwmy][^<>\n]*>"
				                 template))
	    (delete-region beg end)
	    (setq end beg)
	    (setq nmin 0)
	    (setq nmax (1+ nmax))
	    (setq n-no-remove nmax))
      (goto-char end)
      (cl-loop for n from nmin to nmax do
	           (insert
		        ;; Prepare clone.
		        (with-temp-buffer
		          (insert template)
		          (org-mode)
		          (goto-char (point-min))
		          (org-show-subtree)
		          (and idprop (if org-clone-delete-id
				                  (org-entry-delete nil "ID")
				                (org-id-get-create t)))
		          (unless (= n 0)
		            (while (re-search-forward org-clock-line-re nil t)
		              (delete-region (line-beginning-position)
				                     (line-beginning-position 2)))
		            (goto-char (point-min))
		            (while (re-search-forward org-drawer-regexp nil t)
		              (org-remove-empty-drawer-at (point))))
		          (goto-char (point-min))

		          (when doshift
		            (while (re-search-forward org-ts-regexp-both nil t)
		              (org-timestamp-change (* n shift-n) shift-what))
                    (save-excursion
                      (goto-char (point-min))
                      (evil-numbers/inc-at-pt n (point-min)))
		            (unless (= n n-no-remove)
		              (goto-char (point-min))
		              (while (re-search-forward org-ts-regexp nil t)
			            (save-excursion
			              (goto-char (match-beginning 0))
			              (when (looking-at "<[^<>\n]+\\( +[.+]?\\+[0-9]+[hdwmy]\\)")
			                (delete-region (match-beginning 1) (match-end 1)))))))
		          (buffer-string)))))
    (goto-char beg)))

(my-leader-def
  :infix "o"
  "" '(:which-key "org-mode")
  "c" 'org-capture
  "a" 'org-agenda
  "o" #'my/org-file-open)

(with-eval-after-load 'org
  (my-leader-def
    :infix "SPC"
    :keymaps '(org-mode-map)
    "i" #'org-clock-in
    "o" #'org-clock-out
    "O" #'org-clock-cancel
    "c" #'org-clock-goto
    "p" #'org-set-property
    "e" #'org-set-effort
    "r" #'org-priority
    "m" #'my/org-meeting-link))

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
  (setq org-journal-enable-encryption t)
  (setq org-journal-time-format-post-midnight "PM: %R "))

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
      :timeout 10
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setq my/weather-value data)
                  (setq my/weather-last-time (time-convert nil 'integer))))
      :error
      (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                     (message "Got error: %S" error-thrown)))))
  my/weather-value)

(defun my/get-mood ()
  (let* ((crm-separator " ")
         (crm-local-completion-map
	      (let ((map (make-sparse-keymap)))
	        (set-keymap-parent map crm-local-completion-map)
	        (define-key map " " 'self-insert-command)
	        map))
         (ivy-prescient-sort-commands nil))
    (mapconcat
     #'identity
     (completing-read-multiple
      "How do you feel: "
      my/mood-list)
     " ")))

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
            (org-set-property "EMMS_Track" string))))))
  (when-let (mood (my/get-mood))
    (org-set-property "Mood" mood)))

(add-hook 'org-journal-after-entry-create-hook
          #'my/set-journal-header)

(use-package org-ref
  :straight (:files (:defaults "citeproc" (:exclude "*helm*")))
  :if (not my/remote-server)
  :init
  (setq bibtex-dialect 'biblatex)
  (setq bibtex-completion-bibliography '("~/30-39 Life/32 org-mode/library.bib"))
  (setq bibtex-completion-library-path '("~/30-39 Life/33 Library"))
  (setq bibtex-completion-notes-path "~/Documents/org-mode/literature-notes")
  (setq bibtex-completion-display-formats
        '((t . "${author:36} ${title:*} ${note:10} ${year:4} ${=has-pdf=:1}${=type=:7}")))
  (setq bibtex-completion-pdf-open-function
        (lambda (file)
          (start-process "dired-open" nil
                         "xdg-open" (file-truename file))))
  :after (org)
  :config
  (with-eval-after-load 'ivy-bibtex
    (require 'org-ref-ivy))
  (general-define-key
   :keymaps 'org-mode-map
   "C-c l" #'org-ref-insert-link-hydra/body)
  (general-define-key
   :keymaps 'bibtex-mode-map
   "M-RET" 'org-ref-bibtex-hydra/body))

(use-package ivy-bibtex
  :after (org-ref)
  :straight t
  :init
  (my-leader-def "fB" 'ivy-bibtex))

(add-hook 'bibtex-mode 'smartparens-mode)

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
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
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
    "T" 'org-roam-tag-remove
    "s" 'org-roam-db-autosync-mode)
  (general-define-key
   :keymap 'org-mode-map
   "C-c i" 'org-roam-node-insert))

(defface my/org-roam-count-overlay-face
  '((t :inherit tooltip))
  "Face for Org Roam count overlay.")

(defun my/org-roam--count-overlay-make (pos count)
  (let* ((overlay-value (concat
                         " "
                         (propertize
                          (format "%d" count)
                          'face 'my/org-roam-count-overlay-face)
                         " "))
         (ov (make-overlay pos pos (current-buffer) nil t)))
    (overlay-put ov 'roam-backlinks-count count)
    (overlay-put ov 'priority 1)
    (overlay-put ov 'after-string overlay-value)))

(defun my/org-roam--count-overlay-remove-all ()
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (overlay-get ov 'roam-backlinks-count)
      (delete-overlay ov))))

(defun my/org-roam--count-overlay-make-all ()
  (my/org-roam--count-overlay-remove-all)
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (elem)
      (when (string-equal (org-element-property :type elem) "id")
        (let* ((id (org-element-property :path elem))
               (count (caar
                       (org-roam-db-query
                        [:select (funcall count source)
                         :from links
                         :where (= dest $s1)
                         :and (= type "id")]
                        id))))
          (when (< 0 count)
            (my/org-roam--count-overlay-make
             (org-element-property :end elem)
             count)))))))

(define-minor-mode my/org-roam-count-overlay-mode
  "Display backlink count for org-roam links."
  :after-hook
  (if my/org-roam-count-overlay-mode
      (progn
        (my/org-roam--count-overlay-make-all)
        (add-hook 'after-save-hook #'my/org-roam--count-overlay-make-all nil t))
    (my/org-roam--count-overlay-remove-all)
    (remove-hook 'after-save-hook #'my/org-roam--count-overlay-remove-all t)))

(use-package org-roam-ui
  :straight (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :if (not my/remote-server)
  :after org-roam
  ;; :hook (org-roam . org-roam-ui-mode)
  :init
  (my-leader-def "oru" #'org-roam-ui-mode))

(use-package deft
  :straight t
  :if (not my/remote-server)
  :commands (deft)
  :after (org)
  :init
  (my-leader-def "ord" #'deft)
  :config
  (setq deft-directory org-roam-directory)
  (setq deft-recursive t)
  (setq deft-use-filter-string-for-filename t)
  (add-hook 'deft-mode-hook
            (lambda () (display-line-numbers-mode -1)))
  (general-define-key
   :keymaps 'deft-mode-map
   :states '(normal motion)
   "q" #'quit-window
   "r" #'deft-refresh
   "s" #'deft-filter
   "d" #'deft-filter-clear
   "y" #'deft-filter-yank
   "t" #'deft-toggle-incremental-search
   "o" #'deft-toggle-sort-method))

(setq deft-strip-summary-regexp
      (rx (or
           (: ":PROPERTIES:" (* anything) ":END:")
           (: "#+" (+ alnum) ":" (* nonl))
           (regexp "[\n\t]"))))

(defun my/deft-parse-summary-around (fun contents title)
  (funcall fun (org-link-display-format contents) title))

(with-eval-after-load 'deft
  (advice-add #'deft-parse-summary :around #'my/deft-parse-summary-around))

(defun my/deft-parse-title (file contents)
  (with-temp-buffer
    (insert contents)
    (goto-char (point-min))
    (if (search-forward-regexp (rx (| "#+title:" "#+TITLE:")) nil t)
        (string-trim (buffer-substring-no-properties (point) (line-end-position)))
      file)))

(defun my/deft-parse-title-around (fun file contents)
  (or (my/deft-parse-title file contents)
      (funcall fun file contents)))

(with-eval-after-load 'deft
  (advice-add #'deft-parse-title :around #'my/deft-parse-title-around))

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

(use-package org-contacts
  :straight (:type git :repo "https://repo.or.cz/org-contacts.git")
  :if (not my/remote-server)
  :after (org)
  :config
  (setq org-contacts-files (list
                            (concat org-directory "/contacts.org"))))

(defun my/calfw-setup-buffer ()
  (display-line-numbers-mode -1))

(use-package calfw
  :straight t
  :config
  (add-hook 'cfw:calendar-mode-hook #'my/calfw-setup-buffer))

(use-package calfw-org
  :after (calfw org)
  :straight t)

(defun my/org-timeblock-conf ()
  (display-line-numbers-mode -1))

(use-package org-timeblock
  :straight (:host github :repo "ichernyshovvv/org-timeblock")
  :commands (org-timeblock-mode)
  :init
  (my-leader-def "ot" #'org-timeblock)
  :config
  (add-hook 'org-timeblock-mode-hook #'my/org-timeblock-conf)
  (general-define-key
   :keymaps '(org-timeblock-mode-map)
   :states '(normal visual)
   "j" #'org-timeblock-forward-block
   "h" #'org-timeblock-backward-column
   "l" #'org-timeblock-forward-column
   "k" #'org-timeblock-backward-block
   "M-[" #'org-timeblock-day-earlier
   "M-]" #'org-timeblock-day-later
   "H" #'org-timeblock-day-earlier
   "L" #'org-timeblock-day-later
   "RET" #'org-timeblock-goto
   "t" #'org-timeblock-todo-set
   "q" #'quit-window))

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

(my/use-colors
 (org-block :background (my/color-value 'bg-other))
 (org-block-begin-line :background (my/color-value 'bg-other)
                       :foreground (my/color-value 'grey)))

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

(with-eval-after-load 'org-ref
  (setq org-ref-csl-default-locale "ru-RU")
  (setq org-ref-csl-default-style (expand-file-name
                                   (concat user-emacs-directory
                                           "gost-r-7-0-5-2008-numeric.csl"))))

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

(with-eval-after-load 'ox
  (setq org-export-dictionary
        (cl-loop for item in org-export-dictionary collect
                 (cons
                  (car item)
                  (cl-loop for entry in (cdr item)
                           if (and (equal (car entry) "ru")
                                   (plist-get (cdr entry) :utf-8))
                           collect (list "ru" :default (plist-get (cdr entry) :utf-8))
                           else collect entry)))))

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

(defun my/regenerate-desktop ()
  (interactive)
  (org-babel-tangle-file "/home/pavel/Desktop.org")
  (org-babel-tangle-file "/home/pavel/Console.org")
  (call-process "xrdb" nil nil nil "-load" "/home/pavel/.Xresources")
  (call-process "~/bin/polybar.sh")
  (call-process "pkill" nil nil nil "dunst")
  (call-process "herd" nil nil nil "restart" "xsettingsd")
  (when (fboundp #'my/exwm-set-alpha)
    (if (my/light-p)
        (my/exwm-set-alpha 100)
      (my/exwm-set-alpha 90))))

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

  (when my/is-termux
    (add-hook 'dired-mode-hook #'dired-hide-details-mode))
  (general-define-key
   :states '(normal)
   :keymaps 'dired-mode-map
   "h" #'dired-up-directory
   "l" #'dired-find-file
   "=" #'dired-narrow
   "-" #'my/dired-create-empty-file-subtree
   "~" #'eshell
   "M-r" #'wdired-change-to-wdired-mode
   "<left>" #'dired-up-directory
   "<right>" #'dired-find-file
   "M-<return>" #'dired-open-xdg)
   (dired-async-mode))

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

(use-package all-the-icons-dired
  :straight t
  :if (not (or my/slow-ssh (not (display-graphic-p))))
  :hook (dired-mode . (lambda ()
                        (unless (string-match-p "/gnu/store" default-directory)
                          (all-the-icons-dired-mode))))
  :config)

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

(defun my/dired-goto-project-root ()
  (interactive)
  (dired--find-possibly-alternative-file (projectile-project-root)))

(with-eval-after-load 'dired
  (general-define-key
   :states '(normal)
   :keymaps 'dired-mode-map
   "H" #'my/dired-goto-project-root))

(setq tramp-verbose 6)

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

(with-eval-after-load 'lsp
  (advice-add #'lsp :around #'my/tramp-void-if-tramp)
  (advice-add #'lsp-deferred :around #'my/tramp-void-if-tramp))

(with-eval-after-load 'git-gutter
  (advice-add #'git-gutter--turn-on :around #'my/tramp-void-if-tramp))

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

(defun my/get-good-buffer (buffer-major-mode prompt)
  (or
   (cl-loop
    for buf being the buffers
    if (eq (buffer-local-value 'major-mode buf) buffer-major-mode)
    collect buf into all-buffers
    if (and (eq (buffer-local-value 'major-mode buf) buffer-major-mode)
            (get-buffer-window buf t))
    collect buf into visible-buffers
    finally return (if (= (length visible-buffers) 1)
                       (car visible-buffers)
                     (if (= (length all-buffers) 1)
                         (car all-buffers)
                       (when-let ((buffers-by-name (mapcar (lambda (b)
                                                             (cons (buffer-name b) b))
                                                           all-buffers)))
                         (cdr
                          (assoc
                           (completing-read prompt buffers-by-name nil t)
                           buffers-by-name))))))
   (user-error "No buffer found!")))

(defun my/dired-attach-to-telega (files telega-buffer)
  (interactive
   (list (dired-get-marked-files nil nil #'dired-nondirectory-p)
         (my/get-good-buffer 'telega-chat-mode "Telega buffer: ")))
  (unless files
    (user-error "No (non-directory) files selected"))
  (with-current-buffer telega-buffer
    (dolist (file files)
      (telega-chatbuf-attach-file file))))

(defun my/telega-save-to-dired (msg arg)
  (interactive
   (list (telega-msg-for-interactive)
         (prefix-numeric-value current-prefix-arg)))
  (if (eq arg 4)
      (let ((default-directory
             (with-current-buffer (my/get-good-buffer 'dired-mode "Dired buffer: ")
               (dired-current-directory))))
        (telega-msg-save msg))
    (telega-msg-save msg)))

(defun my/dired-attach-to-notmuch (files notmuch-buffer)
  (interactive
   (list (dired-get-marked-files nil nil #'dired-nondirectory-p)
         (my/get-good-buffer 'notmuch-message-mode "Notmuch message buffer: ")))
  (unless files
    (user-error "No (non-directory) files selected"))
  (with-current-buffer notmuch-buffer
    (goto-char (point-max))
    (dolist (file files)
      (let ((type
             (or (mm-default-file-type file)
		         "application/octet-stream")))
        (mml-attach-file
         file
         type
         (mml-minibuffer-read-description)
         (mml-minibuffer-read-disposition type nil file))))))

(defun my/notmuch-save-to-dired (arg)
  (interactive
   (list (prefix-numeric-value current-prefix-arg)))
  (if (eq arg 4)
      (let ((default-directory
             (with-current-buffer (my/get-good-buffer 'dired-mode "Dired buffer: ")
               (dired-current-directory))))
        (notmuch-show-save-part))
    (notmuch-show-save-part)))

(defun my/dired-attach-to-ement (files ement-buffer)
  (interactive
   (list (dired-get-marked-files nil nil #'dired-nondirectory-p)
         (my/get-good-buffer 'ement-room-mode "Ement room buffer: ")))
  (unless files
    (user-error "No (non-directory) files selected"))
  (with-current-buffer ement-buffer
    (ement-with-room-and-session
      (dolist (file files)
        (ement-room-send-file
         file
         (read-from-minibuffer (format "Message body for %s: " file))
         ement-room
         ement-session)))))

(defun my/dired-attach-to-mastodon (files mastodon-buffer)
  (interactive
   (list (dired-get-marked-files nil nil #'dired-nondirectory-p)
         (or (cl-loop for buf being the buffers
                      if (eq (buffer-local-value 'mastodon-toot-mode buf) t)
                      return buf)
             (user-error "No buffer found!"))))
  (unless files
    (user-error "No (non-directory) files selected"))
  (with-current-buffer mastodon-buffer
    (dolist (file files)
      (mastodon-toot--attach-media
       file
       (read-from-minibuffer (format "Description for %s: " file))))))

(with-eval-after-load 'dired
  (general-define-key
   :states '(normal)
   :keymaps 'dired-mode-map
   "a" nil
   "at" #'my/dired-attach-to-telega
   "am" #'my/dired-attach-to-notmuch
   "ai" #'my/dired-attach-to-ement
   "an" #'my/dired-attach-to-mastodon))

(with-eval-after-load 'telega
  (general-define-key
   :keymaps 'telega-msg-button-map
   "S" #'my/telega-save-to-dired))

(with-eval-after-load 'notmuch
  (general-define-key
   :keymaps 'notmuch-show-mode-map
   :states 'normal
   ". s" #'my/notmuch-save-to-dired))

(when my/is-termux
  (straight-use-package 'vterm))

(defun my/vterm-setup ()
  (display-line-numbers-mode 0)
  (setq-local term-prompt-regexp
              (rx bol (| ">" "✕") " ")))

(use-package vterm
  :commands (vterm vterm-other-window)
  :config
  (setq vterm-kill-buffer-on-exit t)

  (setq vterm-environment '("IS_VTERM=1"))

  (add-hook 'vterm-mode-hook #'my/vterm-setup)

  ;; (advice-add 'evil-collection-vterm-insert
  ;;             :before (lambda (&rest args)
  ;;                       (ignore-errors
  ;;                         (apply #'vterm-reset-cursor-point args))))

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
  (let ((vterm-window
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

;; (unless my/slow-ssh
;;   (general-nmap "`" 'my/toggle-vterm-subteminal)
;;   (general-nmap "~" 'vterm))

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

  (general-define-key
   :states '(normal insert)
   :keymaps 'eshell-mode-map
   "<home>" #'eshell-bol
   "C-r" #'counsel-esh-history)

  (general-define-key
   :keymaps 'eshell-mode-map
   :states '(insert)
   "<tab>" 'my/eshell-complete
   "M-k" #'eshell-previous-matching-input-from-input
   "M-j" #'eshell-next-matching-input-from-input)

  (general-define-key
   :states '(normal)
   :keymaps 'eshell-mode-map
   "C-h" 'evil-window-left
   "C-l" 'evil-window-right
   "C-k" 'evil-window-up
   "C-j" 'evil-window-down)
  ;; XXX Did they forget to set it to nil?
  (setq eshell-first-time-p nil))

(use-package eshell
  :straight (:type built-in)
  :after evil-collection
  :commands (eshell)
  :init
  (setq eshell-history-size 10000)
  (setq eshell-hist-ignoredups t)
  (setq eshell-buffer-maximum-lines 10000)
  :config
  ;; XXX 90 to override `evil-collection'
  (add-hook 'eshell-first-time-mode-hook 'my/configure-eshell 90)
  (setq eshell-command-aliases-list
        '(("q" "exit")
          ("c" "clear")
          ("ll" "ls -la")
          ("e" "find-file")))
  (setq eshell-banner-message "")
  ;; (setq eshell-visual-commands
  ;;       `(,@eshell-visual-commands "jless"))
  )

(defvar-local my/eshell-last-command-start-time nil)

(defun my/get-starship-prompt ()
  (let ((cmd (format "TERM=xterm starship prompt --status=%d --cmd-duration=%d --logical-path=%s"
                     eshell-last-command-status
                     (if my/eshell-last-command-start-time
                         (let ((delta (float-time
                                       (time-subtract
                                        (current-time)
                                        my/eshell-last-command-start-time))))
                           (setq my/eshell-last-command-start-time nil)
                           (round (* delta 1000)))
                       0)
                     (shell-quote-argument default-directory))))
    (with-temp-buffer
      (call-process "bash" nil t nil "-c" cmd)
      (thread-first "\n"
                    (concat (string-trim (buffer-string)))
                    (ansi-color-apply)))))

(defun my/eshell-set-start-time (&rest _args)
  (setq-local my/eshell-last-command-start-time (current-time)))

(with-eval-after-load 'eshell
  (advice-add #'eshell-send-input :before #'my/eshell-set-start-time))

(with-eval-after-load 'eshell
  (setq eshell-prompt-regexp (rx bol (| ">" "✕") " "))
  (setq eshell-prompt-function #'my/get-starship-prompt)
  (setq eshell-highlight-prompt nil))

(my/use-colors
   (epe-pipeline-delimiter-face :foreground (my/color-value 'green))
   (epe-pipeline-host-face      :foreground (my/color-value 'blue))
   (epe-pipeline-time-face      :foreground (my/color-value 'yellow))
   (epe-pipeline-user-face      :foreground (my/color-value 'red)))

(use-package eshell-prompt-extras
  :straight t
  :after eshell
  :disabled t
  :config
  (setq eshell-prompt-function 'epe-theme-pipeline))

(use-package eshell-syntax-highlighting
  :straight t
  :after eshell
  :config
  (eshell-syntax-highlighting-global-mode))

(use-package eshell-fringe-status
  :straight t
  :after eshell
  :disabled t
  :config
  (add-hook 'eshell-mode-hook 'eshell-fringe-status-mode))

(use-package fish-completion
  :straight t
  :after eshell
  :if (executable-find "fish")
  :config
  (global-fish-completion-mode))

(defun my/eshell-get-input ()
  (save-excursion
    (beginning-of-line)
    (when (looking-at-p eshell-prompt-regexp)
      (substring-no-properties (eshell-get-old-input)))))

(defun my/shell-unquote-argument-without-process (string)
  (save-match-data
    (let ((idx 0) next inside
	      (quote-chars (rx (| "'" "`" "\"" "\\"))))
      (while (and (< idx (length string))
		          (setq next (string-match quote-chars string next)))
	    (cond ((= (aref string next) ?\\)
	           (setq string (replace-match "" nil nil string))
	           (setq next (1+ next)))
	          ((and inside (= (aref string next) inside))
	           (setq string (replace-match "" nil nil string))
	           (setq inside nil))
	          (inside
	           (setq next (1+ next)))
	          (t
	           (setq inside (aref string next))
	           (setq string (replace-match "" nil nil string)))))
      string)))

(defun my/eshell-history-is-good-suggestion (input suggestion)
  (and (string-prefix-p input suggestion)
       (if (string-prefix-p "cd " input)
           (let ((suggested-dir
                  (my/shell-unquote-argument-without-process
                   (substring suggestion 3))))
             (if (or (string-prefix-p "/" suggested-dir)
                     (string-prefix-p "~" suggested-dir))
                 (file-directory-p suggested-dir)
               (file-directory-p (concat (eshell/pwd) "/" suggested-dir))))
         t)
       (if (string-prefix-p "git" suggestion)
           ;; How is this faster than 'magit-toplevel'?
           (vc-git-root)
         t)))

(defun my/eshell-history-suggest-one (input)
  (unless (seq-empty-p input)
    (or
     (when-let (s (cl-loop for elem in (ring-elements eshell-history-ring)
                           for proc-elem = (string-trim (substring-no-properties elem))
                           when (my/eshell-history-is-good-suggestion input proc-elem)
                           return proc-elem))
       (substring s (length input)))
     (ignore-errors
       (when-let* ((pcomplete-stub input)
                   (completions (pcomplete-completions))
                   (one-completion (car (all-completions pcomplete-stub completions)))
                   (bound (car (completion-boundaries pcomplete-stub completions nil ""))))
         (unless (zerop bound)
           (setq one-completion (concat (substring pcomplete-stub 0 bound) one-completion)))
         ;; (message "%s %s" pcomplete-stub one-completion)
         (comint-quote-filename
          (substring one-completion (min
                                     (length pcomplete-stub)
                                     (length one-completion)))))))))

(defun my/eshell-overlay-get ()
  (seq-find (lambda (ov)
              (overlay-get ov 'my/eshell-completion-overlay))
            (overlays-in (point-min) (point-max))))

(defun my/eshell-overlay-update (pos value)
  (let ((overlay-value (propertize value 'face 'shadow
                                   'cursor t))
        (overlay (my/eshell-overlay-get)))
    (if overlay
        (move-overlay overlay pos pos)
      (setq overlay (make-overlay pos pos (current-buffer) nil t))
      (overlay-put overlay 'my/eshell-completion-overlay t))
    (overlay-put overlay 'after-string overlay-value)))

(defun my/eshell-overlay-remove (&rest _)
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (overlay-get ov 'my/eshell-completion-overlay)
      (delete-overlay ov))))

(defun my/eshell-overlay-suggest (&rest _args)
  (if-let* ((input (my/eshell-get-input))
            (suggestion (my/eshell-history-suggest-one input))
            (_ (not company-prefix)))
      (my/eshell-overlay-update (line-end-position) suggestion)
    (my/eshell-overlay-remove)))

(define-minor-mode my/eshell-overlay-suggest-mode
  "Fish-like suggestions for eshell."
  :after-hook
  (if my/eshell-overlay-suggest-mode
      (progn
        (add-hook 'after-change-functions #'my/eshell-overlay-suggest nil t)
        (add-hook 'company-completion-started-hook #'my/eshell-overlay-suggest nil t)
        (add-hook 'company-after-completion-hook #'my/eshell-overlay-suggest nil t))
    (remove-hook 'after-change-functions #'my/eshell-overlay-suggest t)
    (add-hook 'company-completion-started-hook #'my/eshell-overlay-suggest t)
    (add-hook 'company-after-completion-hook #'my/eshell-overlay-suggest t)
    (my/eshell-overlay-remove)))

;; (add-hook 'eshell-mode-hook #'my/eshell-overlay-suggest-mode)

(defun my/eshell-complete ()
  (interactive)
  (if (and (= (point) (line-end-position)))
      (if-let ((overlay (my/eshell-overlay-get)))
          (progn
            (delete-overlay overlay)
            (insert (overlay-get overlay 'after-string)))
        (company-complete))
    (company-complete)))

(add-to-list 'display-buffer-alist
             '("eshell-dedicated.*"
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (side . bottom)
               (reusable-frames . visible)
               (window-height . 0.33)))

(defun my/eshell-dedicated ()
  (interactive)
  ;; XXX the byte-compiler freaks out if eshell is required within the
  ;; `let*' block because it binds `dedicated-buffer'... dynamically?
  ;; How?
  (require 'eshell)
  (let* ((eshell-buffer-name "eshell-dedicated")
         (dedicated-buffer (get-buffer eshell-buffer-name)))
    (if (not dedicated-buffer)
        (eshell)
      (let ((window (get-buffer-window dedicated-buffer)))
        (if (eq (selected-window) window)
            (kill-buffer-and-window)
          (select-window window))))))

(defun eshell/prt ()
  (if-let ((root (projectile-project-root)))
      (eshell/cd root)
    (message "Not in a project")))

(defun my/eshell-maybe-configure-for-tramp ()
  (when (file-remote-p default-directory)
    (setq-local company-idle-delay nil)))

(add-hook 'eshell-mode-hook #'my/eshell-maybe-configure-for-tramp)

(general-define-key
 :states '(normal)
 "`" #'my/eshell-dedicated
 "~" #'eshell)

(use-package eat
  :straight (:files ("*.el" ("term" "term/*.el") "*.texi"
               "*.ti" ("terminfo/e" "terminfo/e/*")
               ("terminfo/65" "terminfo/65/*")
               ("integration" "integration/*")
               (:exclude ".dir-locals.el" "*-tests.el"))))

(add-hook 'eshell-load-hook #'eat-eshell-mode)

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
  :if (not (or my/is-termux my/remote-server))
  :commands (elfeed)
  :init
  (my-leader-def "ae" #'elfeed-summary)
  (my/persp-add-rule
    elfeed-summary-mode 0 "elfeed"
    elfeed-search-mode 0 "elfeed"
    elfeed-show-mode 0 "elfeed")
  (setq shr-max-image-proportion 0.5)
  :config
  (setq elfeed-db-directory "~/.elfeed")
  (setq elfeed-enclosure-default-dir (expand-file-name "~/Downloads"))
  ;; (advice-add #'elfeed-insert-html
  ;;             :around
  ;;             (lambda (fun &rest r)
  ;;               (let ((shr-use-fonts nil))
  ;;                 (apply fun r))))
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

(my/use-colors
 (elfeed-search-tag-face :foreground (my/color-value 'yellow))
 (elfeed-videos-entry :foreground (my/color-value 'red))
 (elfeed-twitter-entry :foreground (my/color-value 'blue))
 (elfeed-emacs-entry :foreground (my/color-value 'magenta))
 (elfeed-music-entry :foreground (my/color-value 'green))
 (elfeed-podcasts-entry :foreground (my/color-value 'yellow))
 (elfeed-blogs-entry :foreground (my/color-value 'orange))
 (elfeed-govt-entry :foreground (my/color-value 'dark-cyan)))

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

(use-package elfeed-summary
  :commands (elfeed-summary)
  :straight t
  :config
  (setq elfeed-summary-filter-by-title t)
  (setq elfeed-summary-skip-sync-tag 'skip))

(use-package elfeed-sync
  :straight (:host github :repo "SqrtMinusOne/elfeed-sync")
  :if (not my/remote-server)
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

(use-package elfeed-tube
  :straight t
  :after elfeed
  :config
  (setq elfeed-tube-auto-fetch-p nil)
  (elfeed-tube-setup)
  (general-define-key
   :states 'normal
   :keymaps '(elfeed-search-mode-map elfeed-show-mode-map)
   "gf" #'elfeed-tube-fetch))

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
  (interactive (list (read-string "Video ID: ")
                     (lambda (file-name)
                       (find-file file-name))
                     :file-name nil
                     :overwrite t))
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

(defun my/invoke-whisper--direct (input output-dir remove-wav)
  "Extract subtitles from a WAV audio file.

INPUT is the absolute path to audio file, OUTPUT-DIR is the path to
the directory with resulting files."
  (let* ((default-directory output-dir)
         (buffer (generate-new-buffer "whisper"))
         (proc (start-process
                "whisper" buffer
                "whisper-cpp" "--model" "/home/pavel/.whisper/ggml-tiny.en.bin"
                "-otxt" "-ovtt" "-osrt" input)))
    (set-process-sentinel
     proc
     (lambda (process _msg)
       (let ((status (process-status process))
             (code (process-exit-status process)))
         (cond ((and (eq status 'exit) (= code 0))
                (notifications-notify :body "Audio conversion completed"
                                      :title "Whisper")
                (when remove-wav
                  (delete-file input))
                (dolist (extension '(".txt" ".vtt" ".srt"))
                  (rename-file (concat input extension)
                               (concat (file-name-sans-extension input) extension)))
                (kill-buffer (process-buffer process)))
               ((or (and (eq status 'exit) (> code 0))
                    (eq status 'signal))
                (let ((err (with-current-buffer (process-buffer process)
                             (buffer-string))))
                  (user-error "Error in Whisper: %s" err)))))))))

(defun my/invoke-whisper (input output-dir)
  "Extract subtitles from the audio file.

INPUT is the absolute path to the audio file, OUTPUT-DIR is the path
to the directory with resulting files.

Run ffmpeg if the file is not WAV."
  (interactive
   (list
    (read-file-name "Input file: " nil nil t)
    (read-directory-name "Output directory: ")))
  (if (string-match-p (rx ".wav" eos) input)
      (my/invoke-whisper--direct input output-dir)
    (let* ((ffmpeg-proc
            (start-process
             "ffmpef" nil "ffmpeg" "-i" input "-ar" "16000" "-ac" "1" "-c:a"
             "pcm_s16le" (concat (file-name-sans-extension input) ".wav"))))
      (set-process-sentinel
       ffmpeg-proc
       (lambda (process _msg)
         (let ((status (process-status process))
               (code (process-exit-status process)))
           (cond ((and (eq status 'exit) (= code 0))
                  (my/invoke-whisper--direct
                   (concat (file-name-sans-extension input) ".wav") output-dir t))
                 ((or (and (eq status 'exit) (> code 0))
                      (eq status 'signal))
                  (let ((err (with-current-buffer (process-buffer process)
                               (buffer-string))))
                    (user-error "Error in running ffmpeg: %s" err))))))))))

(with-eval-after-load 'elfeed
  (defvar my/elfeed-whisper-podcast-files-directory
    (concat elfeed-db-directory "/podcast-files/")))

(defun my/elfeed-whisper-get-transcript-new (entry)
  (interactive (list elfeed-show-entry))
  (let* ((url (caar (elfeed-entry-enclosures entry)))
         (file-name (concat
                     (elfeed-ref-id (elfeed-entry-content entry))
                     "."
                     (file-name-extension url)))
         (file-path (expand-file-name
                     (concat
                      my/elfeed-whisper-podcast-files-directory
                      file-name))))
    (message "Download started")
    (unless (file-exists-p my/elfeed-whisper-podcast-files-directory)
      (mkdir my/elfeed-whisper-podcast-files-directory))
    (request url
      :type "GET"
      :encoding 'binary
      :complete
      (cl-function
       (lambda (&key data &allow-other-keys)
         (let ((coding-system-for-write 'binary)
               (write-region-annotate-functions nil)
               (write-region-post-annotation-function nil))
           (write-region data nil file-path nil :silent))
         (message "Conversion started")
         (my/invoke-whisper file-path my/elfeed-srt-dir)))
      :error
      (cl-function
       (lambda (&key error-thrown &allow-other-keys)
         (message "Error!: %S" error-thrown))))))

(defun my/elfeed-show-related-files (entry)
  (interactive (list elfeed-show-entry))
  (let* ((files
          (mapcar
           (lambda (file) (cons (file-name-extension file) file))
           (seq-filter
            (lambda (file)
              (string-match-p
               (rx bos (literal (elfeed-ref-id (elfeed-entry-content entry))) ".")
               file))
            (directory-files my/elfeed-srt-dir))))
         (buffer
          (find-file-other-window
           (concat
            my/elfeed-srt-dir
            (alist-get
             (completing-read "File: " files)
             files nil nil #'equal)))))
    (with-current-buffer buffer
      (setq-local elfeed-show-entry entry))))

(defun my/elfeed-whisper-get-transcript (entry)
  "Retrieve transcript for the enclosure of the current elfeed ENTRY."
  (interactive (list elfeed-show-entry))
  (let ((enclosure (caar (elfeed-entry-enclosures entry))))
    (unless enclosure
      (user-error "No enclosure found!"))
    (let ((srt-path (concat my/elfeed-srt-dir
                            (elfeed-ref-id (elfeed-entry-content entry))
                            ".srt")))
      (if (file-exists-p srt-path)
          (let ((buffer (find-file-other-window srt-path)))
            (with-current-buffer buffer
              (setq-local elfeed-show-entry entry)))
        (my/elfeed-whisper-get-transcript-new entry)))))

(defun my/elfeed-whisper-subed (entry)
  "Run MPV for the current Whisper-generated subtitles file.

ENTRY is an instance of `elfeed-entry'."
  (interactive (list elfeed-show-entry))
  (unless entry
    (user-error "No entry!"))
  (unless (derived-mode-p 'subed-mode)
    (user-error "Not subed mode!"))
  (setq-local subed-mpv-video-file
              (expand-file-name
               (concat my/elfeed-whisper-podcast-files-directory
                       (my/get-file-name-from-url
                        (caar (elfeed-entry-enclosures entry))))))
  (subed-mpv--play subed-mpv-video-file))

(defun my/whisper-url (url file-name output-dir)
  (interactive
   (list (read-from-minibuffer "URL: ")
         (read-from-minibuffer "File name: ")
         (read-directory-name "Output directory: ")))
  (let ((file-path
         (concat output-dir file-name "." (file-name-extension url))))
    (message "Download started")
    (request url
      :type "GET"
      :encoding 'binary
      :complete
      (cl-function
       (lambda (&key data &allow-other-keys)
         (let ((coding-system-for-write 'binary)
               (write-region-annotate-functions nil)
               (write-region-post-annotation-function nil))
           (write-region data nil file-path nil :silent))
         (message "Conversion started")
         (my/invoke-whisper file-path output-dir)))
      :error
      (cl-function
       (lambda (&key error-thrown &allow-other-keys)
         (message "Error!: %S" error-thrown))))))

(unless (or my/is-termux my/remote-server)
  (let ((mail-file (expand-file-name "mail.el" user-emacs-directory)))
    (if (file-exists-p mail-file)
        (load-file mail-file)
      (message "Can't load mail.el"))))

(use-package gnus
  :straight t
  :init
  (my-leader-def "au" #'gnus)
  :config
  (my/persp-add-rule
    gnus-summary-mode 0 "gnus"
    ;; gnus-article-edit-mode 0 "gnus"
    gnus-browse-mode 0 "gnus"
    gnus-server-mode 0 "gnus"
    gnus-article-mode 0 "gnus"
    gnus-group-mode 0 "gnus"
    gnus-category-mode 0 "gnus")
  (let ((gnus-directory (concat user-emacs-directory "gnus")))
    (unless (file-directory-p gnus-directory)
      (make-directory gnus-directory))
    (setq gnus-dribble-directory (concat gnus-directory "/dribble"))
    (setq gnus-init-file (concat gnus-directory "/gnus.el"))
    (setq gnus-startup-file (concat gnus-directory "/newsrc")))
  ;; Sources
  (setq gnus-select-method '(nntp "news.gwene.org"))
  ;; Dribble
  (setq gnus-always-read-dribble-file t)
  ;; Agent
  (setq gnus-agent-article-alist-save-format 1)
  (setq gnus-agent-cache t))

(defun my/gnus-topic-toggle-topic ()
  (interactive "" gnus-topic-mode)
  (when (gnus-group-topic-p)
    (let ((topic (gnus-topic-find-topology (gnus-current-topic))))
      (if (eq (cadadr topic) 'visible)
          (progn
            (gnus-topic-goto-topic (gnus-current-topic))
            (gnus-topic-remove-topic nil nil))
        (gnus-topic-remove-topic t nil)))))

(with-eval-after-load 'gnus-group
  ;; Group
  (add-hook 'gnus-group-mode-hook #'gnus-topic-mode)
  (general-define-key
   :states '(normal)
   :keymaps '(gnus-group-mode-map)
   "a" #'gnus-group-toggle-subscription-at-point)
  (general-define-key
   :states '(normal)
   :keymaps '(gnus-topic-mode-map)
   "TAB" #'my/gnus-topic-toggle-topic
   "r" #'gnus-topic-catchup-articles))

(with-eval-after-load 'gnus-summary
  (setq gnus-summary-line-format "%U%R%z%I%(%[%4L: %-23,23f%]%) %s\n")
  (setq gnus-sum-thread-tree-false-root "> ")
  (setq gnus-sum-thread-tree-indent "  ")
  (setq gnus-sum-thread-tree-single-indent " ")
  (setq gnus-sum-thread-tree-leaf-with-other "+-> ")
  (setq gnus-sum-thread-tree-root "> ")
  (setq gnus-sum-thread-tree-single-leaf "\\-> ")
  (setq gnus-sum-thread-tree-vertical "| "))

(use-package emms
  :straight t
  :if (not (or my/remote-server my/is-termux))
  :commands (emms-smart-browse
             emms-browser
             emms-add-url
             emms-add-file
             emms-add-find)
  :init
  (my-leader-def
    :infix "as"
    "" '(:which-key "emms")
    "s" (my/command-in-persp "EMMS" "EMMS" 0 (emms-smart-browse))
    "b" #'emms-browser
    "p" #'emms-pause
    "q" #'emms-stop
    ;; "h" #'emms-previous
    ;; "l" #'emms-next
    "u" #'emms-player-mpd-connect
    "ww" #'emms-lyrics
    "wb" #'emms-lyrics-toggle-display-on-minibuffer
    "wm" #'emms-lyrics-toggle-display-on-modeline
    "k" #'emms-volume-raise
    "j" #'emms-volume-lower)
  (my/persp-add-rule
    emms-browser-mode 0 "EMMS"
    emms-playlist-mode 0 "EMMS")
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
  (add-to-list 'emms-player-list 'emms-player-mpv t)
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
  ;; <I've just read the line below as "I hate everything">
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
   "gr" #'emms-browse-by-artist
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

(defun my/set-volume (value)
  (start-process "ponymix" nil "ponymix"
                 (if (< 0 value) "increase" "decrease")
                 (number-to-string (abs value))
                 "--max-volume" "150"))

(setq emms-volume-change-function #'my/set-volume)
(setq emms-volume-change-amount 5)

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

(setq my/invidious-instances-url
      "https://api.invidious.io/instances.json?pretty=1&sort_by=health")

(defun my/ytel-instances-fetch-json ()
  "Fetch list of invidious instances as json, sorted by health."
  (let
      ((url-request-method "GET")
       (url-request-extra-headers
        '(("Accept" . "application/json"))))
    (with-current-buffer
        (url-retrieve-synchronously my/invidious-instances-url)
      (goto-char (point-min))
      (re-search-forward "^$")
      (let* ((json-object-type 'alist)
             (json-array-type 'list)
             (json-key-type 'string))
        (json-read)))))

(defun my/ytel-instances-alist-from-json ()
  "Make the json of invidious instances into an alist."
  (let ((jsonlist (my/ytel-instances-fetch-json))
        (inst ()))
    (while jsonlist
      (push (concat "https://" (caar jsonlist)) inst)
      (setq jsonlist (cdr jsonlist)))
    (nreverse inst)))

(defun my/ytel-choose-instance ()
  "Prompt user to choose an invidious instance to use."
  (interactive)
  (setq ytel-invidious-api-url
        (or (condition-case nil
                (completing-read "Using instance: "
                                 (cl-subseq (my/ytel-instances-alist-from-json) 0 11) nil "confirm" "https://")
              (error nil))
            "https://invidious.synopyta.org")))

(defun my/ytel-draw--buffer-nil-videos-fix ()
  (let ((inhibit-read-only t)
	    (current-line      (line-number-at-pos)))
    (erase-buffer)
    (setf header-line-format
          (concat "Search results for "
				  (propertize ytel-search-term 'face 'ytel-video-published-face)
				  ", page "
				  (number-to-string ytel-current-page)))
    (seq-do
     (lambda (v)
	   (ytel--insert-video v)
	   (insert "\n"))
     (seq-filter
      (lambda (v)
        (ytel-video-title v))
      ytel-videos))
    (goto-char (point-min))))

(with-eval-after-load 'ytel
  (advice-add #'ytel--draw-buffer :override #'my/ytel-draw--buffer-nil-videos-fix))

(defun my/ytel--format-unknown-fix (fun &rest args)
  (if (car args)
      (apply fun args)
    "unknown   "))

(with-eval-after-load 'ytel
  (advice-add #'ytel--format-video-length :around #'my/ytel--format-unknown-fix)
  (advice-add #'ytel--format-video-published :around #'my/ytel--format-unknown-fix)
  (advice-add #'ytel--format-video-views :around #'my/ytel--format-unknown-fix))

(defun my/ytel-kill-url ()
  (interactive)
  (kill-new
   (concat
    "https://www.youtube.com/watch?v="
    (ytel-video-id (ytel-get-current-video)))))

(defun my/toggle-shr-use-fonts ()
  "Toggle the shr-use-fonts variable in buffer"
  (interactive)
  (setq-local shr-use-fonts (not shr-use-fonts)))

(defface my/shr-face
  `((t :inherit variable-pitch))
  "Default face for shr rendering.")

(my/use-colors
  (my/shr-face :foreground (my/color-value 'blue)))

(defun my/shr-insert-around (fun &rest args)
  (let ((shr-current-font (or shr-current-font 'my/shr-face)))
    (apply fun args)))

(defun my/shr-urlify-around (fun start url &optional title)
  (funcall fun start url title)
  (let ((faces (get-text-property start 'face)))
    (put-text-property
     start (point)
     'face
     (mapcar
      (lambda (face)
        (if (eq face 'my/shr-face)
            'link
          face))
      (if (sequencep faces) faces (list faces))))))

(with-eval-after-load 'shr
  (advice-add #'shr-insert :around #'my/shr-insert-around)
  (advice-add #'shr-urlify :around #'my/shr-urlify-around))

(my-leader-def "aw" 'eww)
(my/persp-add-rule
  eww-mode 2 "browser")

(with-eval-after-load 'eww
  (general-define-key
   :keymaps '(eww-mode-map)
   :states '(normal emacs)
   "f" #'ace-link-eww
   "+" 'text-scale-increase
   "-" 'text-scale-decrease))

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
           "#systemcrafters" "#systemcrafters-emacs")))
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
  ;; (my-leader-def "ai" #'znc-erc)
  (my/persp-add-rule
    erc-mode 3 "ERC")
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

(use-package mastodon
  :straight t
  :commands (my/mastodon)
  :init
  (my-leader-def "an" #'my/mastodon)
  :config
  (setq mastodon-instance-url "https://emacs.ch")
  (setq mastodon-active-user "sqrtminusone")
  (my/persp-add-rule mastodon-mode 0 "mastodon")
  ;; Hide spoilers by default
  (setq-default mastodon-toot--content-warning t)
  (setq mastodon-media--avatar-height 40)
  (setq mastodon-tl--show-avatars t)
  ;; The default emojis take two characters for me
  (setq mastodon-tl--symbols
        '((reply "" . "R")
          (boost "" . "B")
          (favourite "" . "F")
          (bookmark "" . "K")
          (media "" . "[media]")
          (verified "" . "V")
          (locked "" . "[locked]")
          (private "" . "[followers]")
          (direct "" . "[direct]")
          (edited "" . "[edited]"))))

(use-package mastodon-alt
  :straight (:host github :repo "rougier/mastodon-alt")
  :after (mastodon)
  :config
  (mastodon-alt-tl-activate))

(use-package transient
  :straight t
  :defer t)

(defun my/mastodon-configure ()
  (display-line-numbers-mode -1))

(add-hook 'mastodon-mode-hook #'my/mastodon-configure)

(defun my/mastodon-reset ()
  (interactive)
  (cl-loop for process in (process-list)
           if (string-match-p "emacs.ch" (process-name process))
           do (delete-process process)))

(with-eval-after-load 'mastodon
  (general-define-key
   :states '(normal motion)
   :keymaps '(mastodon-mode-map)
   "J" #'mastodon-tl--goto-next-item
   "K" #'mastodon-tl--goto-prev-item
   "M-j" #'mastodon-tl--next-tab-item
   "M-k" #'mastodon-tl--prev-tab-item
   "<tab>" #'mastodon-tl--next-tab-item
   "<backtab>" #'mastodon-tl--previous-tab-item
   "o" #'my/mastodon-toot
   "r" 'mastodon-tl--update
   "c" #'mastodon-tl--toggle-spoiler-text-in-toot
   "q" #'kill-current-buffer))

(defvar my/mastodon-mode-string "")

(defvar my/mastodon-mode-line-unread-ids nil)

(defvar my/mastodon-mode-line-saved-ids nil)

(defvar my/mastodon-mode-line-timer nil)

(defvar my/mastodon-mode-line-file
  (concat no-littering-var-directory "mastodon/notif-ids"))

(defun my/mastodon-mode-line-load-meta ()
  (when (file-exists-p my/mastodon-mode-line-file)
    (ignore-errors
      (with-temp-buffer
        (insert-file-contents my/mastodon-mode-line-file)
        (setq my/mastodon-mode-line-saved-ids
              (read (current-buffer)))))))

(defun my/mastodon-mode-line-persist-meta ()
  (mkdir (file-name-directory my/mastodon-mode-line-file) t)
  (let ((coding-system-for-write 'utf-8))
    (ignore-errors
      (with-temp-file my/mastodon-mode-line-file
        (let ((standard-output (current-buffer))
              (print-level nil)
              (print-length nil)
              (print-circle nil))
          (princ ";;; Mastodon Saved Notifications\n\n")
          (prin1 my/mastodon-mode-line-saved-ids))))))

(defun my/mastodon-mode-line-update ()
  (if my/mastodon-mode-line-unread-ids
      (setq my/mastodon-mode-string
            (concat "["
                    (propertize (number-to-string
                                 (length my/mastodon-mode-line-unread-ids))
                                'face 'success)
                    "]"))
    (setq my/mastodon-mode-string "")))

(defun my/mastodon-mode-line-update-fetch ()
  (mastodon-http--get-json-async
   (mastodon-http--api "notifications") nil
   (lambda (data)
     (let ((fetched-ids
            (cl-loop for datum in data collect (alist-get 'id datum))))
       (setq my/mastodon-mode-line-unread-ids
             (seq-difference fetched-ids my/mastodon-mode-line-saved-ids))
       (setq my/mastodon-mode-line-saved-ids
             (seq-intersection my/mastodon-mode-line-saved-ids fetched-ids)))
     (my/mastodon-mode-line-update))))

(defun my/mastodon-notifications--timeline-before (toots)
  (let* ((all-ids (seq-uniq
                   (append
                    my/mastodon-mode-line-saved-ids
                    (cl-loop for datum in toots
                             collect (alist-get 'id datum))))))
    (setq my/mastodon-mode-line-unread-ids
          (seq-difference my/mastodon-mode-line-unread-ids all-ids))
    (setq my/mastodon-mode-line-saved-ids all-ids))
  (my/mastodon-mode-line-update))

(with-eval-after-load 'mastodon
  (define-minor-mode my/mastodon-mode-line
    "Display mastodon notification count in mode line."
    :require 'mastodon
    :global t
    :group 'mastodon
    :after-hook
    (progn
      (when (timerp my/mastodon-mode-line-timer)
        (cancel-timer my/mastodon-mode-line-timer))
      (if my/mastodon-mode-line
          (progn
            (add-to-list 'mode-line-misc-info '(:eval my/mastodon-mode-string) t)
            (my/mastodon-mode-line-load-meta)
            (setq my/mastodon-mode-line-timer
                  (run-with-timer 0 150 #'my/mastodon-mode-line-update-fetch))
            (advice-add #'mastodon-notifications--timeline :before
                        #'my/mastodon-notifications--timeline-before)
            (add-hook 'kill-emacs-hook #'my/mastodon-mode-line-persist-meta))
        (setq mode-line-misc-info (delete '(:eval my/mastodon-mode-string)
                                          mode-line-misc-info))
        (advice-remove #'mastodon-notifications--timeline
                       #'my/mastodon-notifications--timeline-before)
        (remove-hook 'kill-emacs-hook #'my/mastodon-mode-line-persist-meta)
        (my/mastodon-mode-line-persist-meta)))))

(defun my/mastodon-get-update-funciton (hide-replies hide-boosts)
  (lambda (toots)
    (let* ((is-profile (eq (mastodon-tl--get-buffer-type) 'profile-statuses))
           (hide-replies (and (not is-profile) hide-replies))
           (hide-boosts (and (not is-profile) hide-boosts))
           (toots (seq-filter
                   (lambda (toot)
                     (and
                      (or (not hide-replies)
                          ;; Why is the original function inverted??
                          (mastodon-tl--is-reply toot))
                      (or (not hide-boosts)
                          (not (alist-get 'reblog toot)))))
                   toots)))
      (mapc #'mastodon-tl--toot toots))))

(defun my/mastodon-tl--get-home (hide-replies hide-boosts)
  (mastodon-tl--init
   "home"
   "timelines/home"
   (my/mastodon-get-update-funciton hide-replies hide-boosts)
   nil
   `(("limit" . ,mastodon-tl--timeline-posts-count))
   nil))

(with-eval-after-load 'mastodon
  (require 'transient)
  (transient-define-prefix my/mastodon-tl ()
    ["Home timeline params"
     ("-r" "--hide-replies" "--hide-replies" :init-value
      (lambda (obj) (oset obj value "--hide-replies")))
     ("-b" "--hide-boosts" "--hide-boosts" :init-value
      (lambda (obj) (oset obj value "--hide-boosts")))]
    ["Timelines"
     :class transient-row
     ("t" "Home" (lambda (args)
                   (interactive (list (transient-args transient-current-command)))
                   (my/mastodon-tl--get-home
                    (seq-contains-p args "--hide-replies")
                    (seq-contains-p args "--hide-boosts"))))
     ("l" "Local" mastodon-tl--get-local-timeline)
     ("f" "Federated" mastodon-tl--get-federated-timeline)
     ("g" "One tag" mastodon-tl--get-tag-timeline)
     ("a" "Followed tags" mastodon-tl--followed-tags-timeline)
     ("s" "Some followed tags" mastodon-tl--some-followed-tags-timeline)]
    ["Misc"
     :class transient-row
     ("q" "Quit" transient-quit-one)]))

(with-eval-after-load 'mastodon
  (require 'transient)
  (transient-define-prefix my/mastodon ()
    "Mastodon."
    ["Various views"
     :class transient-row
     ("m" "Mastodon" mastodon)
     ("t" "Timelines" my/mastodon-tl)
     ("n" "Notifications" mastodon-notifications-get)
     ("s" "Search query" mastodon-search--query)]
    ["Tags"
     :class transient-row
     ("aa" "Followed tags" mastodon-tl--list-followed-tags)
     ("af" "Follow tag" mastodon-tl--follow-tag)
     ("aF" "Unfollow tag" mastodon-tl--unfollow-tag)]
    ["Own profile"
     :class transient-row
     ("c" "Toot" mastodon-toot)
     ("o" "My profile" mastodon-profile--my-profile)
     ("u" "Update profile note" mastodon-profile--update-user-profile-note)
     ("f" "Favourites" mastodon-profile--view-favourites)
     ("b" "Bookmarks" mastodon-profile--view-bookmarks)]
    ["Minor views"
     :class transient-row
     ("F" "Follow requests" mastodon-views--view-follow-requests)
     ("S" "Scheduled toots" mastodon-views--view-scheduled-toots)
     ("I" "Filters" mastodon-views--view-filters)
     ("G" "Follow suggestions" mastodon-views--view-follow-suggestions)
     ("L" "Lists" mastodon-views--view-lists)]
    ["Misc"
     :class transient-row
     ("/" "Switch to buffer" mastodon-switch-to-buffer)
     ("Q" "Kill all buffers" mastodon-kill-all-buffers)
     ("q" "Quit" transient-quit-one)]))

(defmacro my/def-confirmer (func text)
  `(defun ,(intern (concat "my/" (symbol-name func) "-confirm")) ()
     (interactive)
     (when (y-or-n-p ,text)
       (call-interactively #',func))))

(defun my/mastodon-toot--browse ()
  "Copy URL of toot at point.
If the toot is a fave/boost notification, copy the URLof the
base toot."
  (interactive)
  (let* ((toot (or (mastodon-tl--property 'base-toot)
                   (mastodon-tl--property 'toot-json)))
         (url (if (mastodon-tl--field 'reblog toot)
                  (alist-get 'url (alist-get 'reblog toot))
                (alist-get 'url toot))))
    (browse-url url)))

(with-eval-after-load 'mastodon
  (my/def-confirmer mastodon-toot--toggle-boost "Toggle boost for this post? ")
  (my/def-confirmer mastodon-toot--toggle-favourite "Toggle favourite this post? ")
  (my/def-confirmer mastodon-toot--toggle-bookmark "Toggle bookmark this post? ")
  (my/def-confirmer mastodon-tl--follow-user "Follow this user? ")
  (my/def-confirmer mastodon-tl--unfollow-user "Unfollow this user? ")
  (my/def-confirmer mastodon-tl--block-user "Block this user? ")
  (my/def-confirmer mastodon-tl--unblock-user "Unblock this user? ")
  (my/def-confirmer mastodon-tl--mute-user "Mute this user? ")
  (my/def-confirmer mastodon-tl--unmute-user "Unmute this user? ")
  (my/def-confirmer mastodon-tl--unmute-user "Unmute this user? ")

  (transient-define-prefix my/mastodon-toot ()
    "Mastodon toot actions."
    ["View"
     :class transient-row
     ("o" "Thread" mastodon-tl--thread)
     ("w" "Browser" my/mastodon-toot--browse)
     ("le" "List edits" mastodon-toot--view-toot-edits)
     ("lf" "List favouriters" mastodon-toot--list-toot-favouriters)
     ("lb" "List boosters" mastodon-toot--list-toot-boosters)]
    ["Toot Actions"
     :class transient-row
     ("r" "Reply" mastodon-toot--reply)
     ("v" "Vote" mastodon-tl--poll-vote)
     ("b" "Boost" my/mastodon-toot--toggle-boost-confirm)
     ("f" "Favourite" my/mastodon-toot--toggle-favourite-confirm)
     ("k" "Bookmark" my/mastodon-toot--toggle-bookmark-confirm)]
    ["My Toot Actions"
     :class transient-row
     ("md" "Delete" mastodon-toot--delete-toot)
     ("mD" "Delete and redraft" mastodon-toot--delete-and-redraft-toot)
     ("mp" "Pin" mastodon-toot--pin-toot-toggle)
     ("me" "Edit" mastodon-toot--edit-toot-at-point)]
    ["Profile Actions"
     :class transient-row
     ("pp" "Profile" mastodon-profile--show-user)
     ("pf" "List followers" mastodon-profile--open-followers)
     ("pF" "List following" mastodon-profile--open-following)
     ("ps" "List statues (no reblogs)" mastodon-profile--open-statuses-no-reblogs)]
    ["User Actions"
     :class transient-row
     ("uf" "Follow user" my/mastodon-tl--follow-user-confirm)
     ("uF" "Unfollow user" my/mastodon-tl--unfollow-user-confirm)
     ("ub" "Block user" my/mastodon-tl--block-user-confirm)
     ("uB" "Unblock user" my/mastodon-tl--unblock-user-confirm)
     ("um" "Mute user" my/mastodon-tl--mute-user-confirm)
     ("uB" "Unmute user" my/mastodon-tl--unmute-user-confirm)]
    ["Misc"
     :class transient-row
     ("q" "Quit" transient-quit-one)]))

(use-package plz
  :straight (:host github :repo "alphapapa/plz.el")
  :defer t)

(defun my/ement ()
  (interactive)
  (ement-connect
   :user-id "@sqrtminusone:matrix.org"
   :password (my/password-store-get "My_Online/Accounts/matrix")))

(use-package ement
  :straight (:host github :repo "alphapapa/ement.el")
  :commands (ement-connect)
  :init
  (my-leader-def "ai" #'my/ement)
  :config
  (setq ement-room-list-auto-update t)
  (setq ement-room-mark-rooms-read 'send)
  (my/persp-add-rule
    ement-room-mode 3 "ement"
    ement-describe-room-mode 3 "ement"
    ement-room-occur-mode 3 "ement"
    ement-room-list-mode 3 "ement")
  ;; Room UI
  (setq ement-room-message-format-spec "%S> %W%B%r%R[%t]")
  (setq ement-room-left-margin-width 0)
  (setq ement-room-right-margin-width 10)
  (setq ement-room-sender-in-left-margin nil)
  (setq ement-room-sender-headers nil)
  (setq ement-room-sender-in-headers nil)
  (setq ement-room-wrap-prefix "-> ")
  ;; Changing some default faces
  (set-face-attribute 'ement-room-reactions nil :height 'unspecified)
  (set-face-attribute 'ement-room-reactions-key nil :height 'unspecified)
  (set-face-attribute 'ement-room-timestamp nil :inherit 'font-lock-function-name-face)
  (set-face-attribute 'ement-room-membership nil :height 0.9
                      :inherit 'font-lock-warning-face)
  (set-face-attribute 'ement-room-wrap-prefix nil :inherit 'unspecified)
  (set-face-attribute 'ement-room-timestamp-header nil :height 'unspecified)
  (set-face-attribute 'ement-room-wrap-prefix nil :inherit 'unspecified)
  ;; Notify only on mentions
  (setq ement-notify-notification-predicates
        '(ement-notify--event-mentions-session-user-p
          ement-notify--event-mentions-room-p
          ement-notify--room-unread-p))
  ;; Fix the anti-synergy with major mode re-activation in `ement-room-list-revert'
  (advice-add #'ement-room-list-revert
              :around #'my/perspective-assign-ignore-advice))

(with-eval-after-load 'ement-room-list
  (general-define-key
   :states '(normal visual)
   :keymaps '(ement-room-list-mode-map)
   "<tab>" #'magit-section-toggle
   "C-j" #'magit-section-forward
   "C-k" #'magit-section-backward
   "q" #'quit-window
   "gr" #'revert-buffer
   "RET" #'ement-room-list-RET))

(with-eval-after-load 'ement-tabulated-room-list
  (general-define-key
   :states '(normal visual)
   :keymaps '(ement-tabulated-room-list-mode-map)
   "q" #'quit-window))

(defun my/ement-room-send-reaction (key position)
  (interactive (list
                (completing-read "Add reaction: " (append telega-emoji-reaction-list '("👋")))
                (point)))
  (ement-room-send-reaction key position))

(defun my/ement-room-compose-quit ()
  (interactive)
  (when (or (string-empty-p (buffer-string))
            (y-or-n-p "Quit compose? "))
    (quit-window t)))

(defun my/ement-room-compose-setup ()
  (ement-room-compose-org)
  (setq company-backends '(telega-company-emoji company-capf))
  (general-define-key
   :states '(normal visual)
   :keymaps 'local
   "Q" #'my/ement-room-compose-quit
   "C-c C-k" (lambda () (interactive) (quit-window t))
   "C-c C-c" #'ement-room-compose-send))

(add-hook 'ement-room-compose-hook #'my/ement-room-compose-setup)

(with-eval-after-load 'ement
  (general-define-key
   :states '(normal visual)
   :keymaps '(ement-room-mode-map)
   "q" #'quit-window
   "?" #'ement-room-transient
   "C-u" #'ement-room-scroll-down-command
   "C-d" #'ement-room-scroll-up-mark-read
   "r" #'ement-room-write-reply
   "a" #'ement-room-send-message
   "i" #'ement-room-send-message
   "e" #'ement-room-edit-message
   "M-<RET>" #'ement-room-compose-message
   "<RET>" #'ement-room-send-message
   "K" #'ement-room-goto-prev
   "J" #'ement-room-goto-next
   "gr" #'ement-room-sync
   "g?" #'ement-describe-room
   "R?" #'ement-describe-room
   "Rm" #'ement-list-members
   "Rn" #'ement-room-set-notification-state
   "Rt" #'ement-room-set-topic
   "!" #'my/ement-room-send-reaction
   "m?" #'ement-room-view-event
   "Zf" #'ement-room-send-file
   "ui" #'ement-invite-user)
  (general-define-key
   :states '(normal visual)
   :keymaps '(ement-describe-room-mode-map)
   "q" #'quit-window)
  (general-define-key
   :states '(motion)
   :keymaps '(ement-room-mode-map)
   "C-u" #'ement-room-scroll-down-command
   "C-d" #'ement-room-scroll-up-mark-read))

(defun my/ement-about-me-p (event)
  (let ((me (ement-user-id (ement-session-user ement-session))))
    (or
     (equal (ement-user-id (ement-event-sender event)) me)
     (when-let ((formatted-body
                 (alist-get
                  'formatted_body
                  (ement-event-content event))))
       (string-match-p me formatted-body)))))

(defun my/ement-scroll-to-previous-about-me ()
  (interactive)
  (let ((scrolled 0))
    (when (< (line-number-at-pos) 20)
      (forward-line 20))
    (if ement-room-retro-loading
        (run-with-timer 0.5 nil #'my/ement-scroll-to-previous-about-me)
      (while (let ((event (ewoc-data (ewoc-locate ement-ewoc))))
               (and
                (not ement-room-retro-loading)
                (or
                 (not (ement-event-p event))
                 (not (my/ement-about-me-p event)))))
        (condition-case _err
            (scroll-down 1)
          (beginning-of-buffer
           (call-interactively #'ement-room-retro)
           (run-with-timer 0.5 nil #'my/ement-scroll-to-previous-about-me)))
        (cl-incf scrolled)
        (message "Scrolled %s" scrolled)))))

(use-package telega
  :straight t
  :if (not (or my/remote-server my/is-termux))
  :commands (telega)
  :init
  (my-leader-def "a l" (my/command-in-persp "telega" "telega" 3 (telega)))
  (my/use-colors
   (telega-button-active :foreground (my/color-value 'base0)
                         :background (my/color-value 'cyan))
   (telega-webpage-chat-link :foreground (my/color-value 'base0)
                             :background (my/color-value 'fg)))
  :config
  (setq telega-emoji-use-images nil)
  (setq telega-chat-fill-column 80)
  (general-define-key
   :keymaps '(telega-root-mode-map telega-chat-mode-map)
   :states '(normal)
   "gp" telega-prefix-map)
  (general-define-key
   :keymaps '(telega-msg-button-map)
   "<SPC>" nil)
  (general-define-key
   :keymaps '(telega-chat-mode-map)
   "C-<return>" #'newline)
  (my/persp-add-rule
    telega-root-mode 3 "telega"
    telega-chat-mode 3 "telega"
    telega-image-mode 3 "telega"
    telega-webpage-mode 3 "telega"))

(defun my/telega-server-build ()
  (interactive)
  (setq telega-server-libs-prefix
        (string-trim
         (shell-command-to-string "guix build tdlib")))
  (telega-server-build "CC=gcc"))

(add-hook 'telega-load-hook #'telega-mode-line-mode)
(setq telega-mode-line-string-format
      '("["
        (:eval
         (telega-mode-line-online-status))
        (:eval
         (when telega-use-tracking-for
           (telega-mode-line-tracking)))
        (:eval
         (telega-mode-line-unread-unmuted))
        (:eval
         (telega-mode-line-mentions 'messages))
        "]"))

(defun my/telega-chat-setup ()
  (set (make-local-variable 'company-backends)
       (append (list 'telega-company-emoji
                     'telega-company-username
                     'telega-company-hashtag
                     'telega-company-markdown-precode)
               (when (telega-chat-bot-p telega-chatbuf--chat)
                 '(telega-company-botcmd))))
  (company-mode 1)
  (setopt visual-fill-column-width
          (+ telega-chat-fill-column 4))
  (setq-local split-width-threshold 1))
(add-hook 'telega-chat-mode-hook #'my/telega-chat-setup)

(defun my/telega-online-status ()
  (derived-mode-p 'telega-root-mode 'telega-chat-mode
                  'telega-image-mode 'telega-webpage-mode))

(setq telega-online-status-function #'my/telega-online-status)

(defun my/telega-switch-to-topic ()
  (interactive)
  (let* ((topics-data (gethash
                       (plist-get telega-chatbuf--chat :id)
                       telega--chat-topics))
         (topics-string
          (mapcar
           (lambda (topic)
             (let* ((name (plist-get (plist-get topic :info) :name))
                    (unread-count (plist-get topic :unread_count))
                    (name-string (with-temp-buffer
                                   (telega-ins--topic-title topic 'with-icon)
                                   (buffer-string))))
               (if (zerop unread-count)
                   name-string
                 (format "%-40s (%s)"
                         name-string
                         (propertize (format "%d" unread-count)
                                     'face 'telega-unread-unmuted-modeline)))))
           topics-data))
         (topics-collection (cl-loop for datum in topics-data
                                     for string in topics-string
                                     collect (cons string datum)))
         (topic (completing-read "Topic: " topics-collection nil t)))
    (telega-chat--goto-thread
     telega-chatbuf--chat
     (plist-get
      (plist-get
       (alist-get topic topics-collection nil nil #'equal)
       :info)
      :message_thread_id))))

(with-eval-after-load 'telega
  (general-define-key
   :states '(normal)
   :keymaps 'telega-chat-mode-map
   "T" #'my/telega-switch-to-topic))

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

(use-package biome
  :straight t
  :commands (biome)
  :init
  (my-leader-def "ab" #'biome)
  :config
  (add-to-list 'biome-query-coords
               '("Saint-Petersburg, Russia" 59.93863 30.31413))
  (add-to-list 'biome-query-coords
               '("Tyumen, Russia" 57.15222 65.52722)))

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
(my-leader-def "hM" 'woman)
(setq woman-fill-column 90)

(general-define-key
 :states '(normal)
 :keymaps 'Info-mode-map
 (kbd "RET") #'Info-follow-nearest-node
 "H" #'Info-history-back
 "L" #'Info-history-forward
 "n" #'Info-search-next
 "b" #'Info-search-backward
 "f" #'ace-link-info)

(defun my/man-fix-width (&rest _)
  (setq-local Man-width (- (window-width) 4)))

(advice-add #'Man-update-manpage :before #'my/man-fix-width)

(use-package devdocs-browser
  :straight t
  :init
  (my-leader-def
    :infix "hd"
    "" '(:wk "devdocs")
    "d" #'devdocs-browser-open
    "o" #'devdocs-browser-open-in
    "i" #'devdocs-browser-install-doc
    "n" #'devdocs-browser-uninstall-doc
    "o" #'devdocs-browser-download-offline-data
    "O" #'devdocs-browser-remove-offline-data
    "u" #'devdocs-browser-upgrade-all-docs
    "r" #'devdocs-browser-update-docs))

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
  (my/use-colors
   (sx-question-mode-accepted :foreground (my/color-value 'green)
                              :weight 'bold)
   (sx-question-mode-content :background nil))
  (add-hook 'sx-question-mode-hook #'doom-modeline-mode)
  (add-hook 'sx-question-list-mode-hook #'doom-modeline-mode))

(use-package ini
  :straight (:host github :repo "daniel-ness/ini.el"))

(defvar my/index-root (concat (getenv "HOME") "/"))

(with-eval-after-load 'org
  (defvar my/index-file
    (concat org-directory "/misc/index.org")))

(defun my/index--tree-get-recursive (heading &optional path)
  "Read the index tree recursively from HEADING.

HEADING is an org-element of type `headline'.

If PATH is provided, it is the path to the current node. If not
provided, it is assumed to be the root of the index.

The return value is an alist; see `my/index--tree-get' for details."
  (when (eq (org-element-type heading) 'headline)
    (let (val
          (new-path (concat
                     (or path my/index-root)
                     (org-element-property :raw-value heading)
                     "/")))
      (when-let* ((children (thread-last
                              (org-element-contents heading)
                              (mapcar (lambda (e)
                                        (my/index--tree-get-recursive
                                         e new-path)))
                              (seq-filter #'identity))))
        (setf (alist-get :children val) children))
      (when-let ((machine (org-element-property :MACHINE heading)))
        (setf (alist-get :machine val) (split-string machine)))
      (when-let ((symlink (org-element-property :SYMLINK heading)))
        (setf (alist-get :symlink val) symlink))
      (when (org-element-property :PROJECT heading)
        (setf (alist-get :project val) t))
      (when-let* ((kind-str (org-element-property :KIND heading))
                  (kind (intern kind-str)))
        (setf (alist-get :kind val) kind)
        (when (equal kind 'git)
          (let ((remote (org-element-property :REMOTE heading)))
            (unless remote
              (user-error "No remote for %s" (alist-get :name val)))
            (setf (alist-get :remote val) remote))))
      (setf (alist-get :name val) (org-element-property :raw-value heading)
            (alist-get :path val) new-path)
      val)))

(defun my/index--tree-get ()
  "Read the index tree from the current org buffer.

The return value is a list of alists, each representing a
folder/node.  Alists can have the following keys:
- `:name'
- `:path'
- `:children' - child nodes
- `:machine' - list of machines on which the node is active
- `:symlink' - a symlink to create
- `:kind' - one of \"git\", \"mega\", or \"dummy\"
- `:remote' - the remote to use for git nodes"
  (let* ((tree
          (thread-last
            (org-element-map (org-element-parse-buffer) 'headline #'identity)
            (seq-filter (lambda (el)
                          (and
                           (= (org-element-property :level el) 1)
                           (seq-contains-p
                            (mapcar #'substring-no-properties (org-element-property :tags el))
                            "folder"))))
            (mapcar #'my/index--tree-get-recursive))))
    tree))

(defun my/index--extact-number (name)
  "Extract the number from the index NAME.

NAME is a string.  The number is the first sequence of digits, e.g.:
- 10-19
- 10.01
- 10.01.Y22.01"
  (save-match-data
    (string-match (rx bos (+ (| num alpha "." "-"))) name)
    (match-string 0 name)))

(defun my/tree--verfify-recursive (elem &optional current)
  "Verify that ELEM is a valid tree element.

CURRENT is the current number or name of the parent element."
  (let* ((name (alist-get :name elem))
         (number (my/index--extact-number name)))
    (unless number
      (user-error "Can't find number: %s" name))
    (cond
     ((and (listp current) (not (null current)))
      (unless (seq-some (lambda (cand) (string-prefix-p cand name)) current)
        (user-error "Name: %s doesn't match: %s" name current)))
     ((stringp current)
      (unless (string-prefix-p current name)
        (user-error "Name: %s doesn't match: %s" name current))))
    (let ((recur-value
           (if (string-match-p (rx (+ num) "-" (+ num)) number)
               (let* ((borders (split-string number "-"))
                      (start (string-to-number (nth 0 borders)))
                      (end (string-to-number (nth 1 borders))))
                 (cl-loop for i from start to (1- end) collect (number-to-string i)))
             number)))
      (mapcar (lambda (e) (my/tree--verfify-recursive e recur-value))
              (alist-get :children elem))))
  t)

(defun my/index--tree-verify (tree)
  "Verify that TREE is a valid tree.

Return t if it is valid, otherwise raise an error.

See `my/index--tree-get' for the format of TREE."
  (mapcar #'my/tree--verfify-recursive tree))

(defun my/index--tree-narrow-recursive (elem machine)
  "Remove all children of ELEM that are not active on MACHINE."
  (unless (when-let ((elem-machines (alist-get :machine elem)))
            (not (seq-some (lambda (elem-machine)
                             (string-equal elem-machine machine))
                           elem-machines)))
    (setf (alist-get :children elem)
          (seq-filter
           #'identity
           (mapcar (lambda (e)
                     (my/index--tree-narrow-recursive e machine))
                   (alist-get :children elem))))
    elem))

(defun my/index--tree-narrow (tree)
  "Remove all elements of TREE that are not active on machine."
  (seq-filter
   #'identity
   (mapcar
    (lambda (elem) (my/index--tree-narrow-recursive elem (system-name)))
    (copy-tree tree))))

(defun my/index--filesystem-tree-mapping (full-tree tree &optional active-paths)
  "Return a \"sync state\" between the filesystem and the tree.

FULL-TREE and TREE are forms as defined by `my/index--tree-get'.  TREE
is the narrowed FULL-TREE (returned by `my/index--tree-narrow').

ACTIVE-PATHS is a list of paths that are currently active.  If not
provided, it is computed from TREE.

The return value is a list of alists with the following keys:
- path - the path of the folder
- exists - whether the folder exists on the filesystem
- has-to-exist - whether the folder exists in the tree
- extra - if the folder exists in the filesystem but not in the tree.
- children - a list of alists with the same keys for the children of
  the folder."
  (let ((active-paths (or active-paths (my/index--tree-get-paths tree))))
    (cl-loop for elem in full-tree
             for path = (alist-get :path elem)
             for extra-folders = (when (and (alist-get :children elem)
                                            (file-directory-p path))
                                   (seq-difference
                                    (mapcar (lambda (d) (if (file-directory-p d)
                                                            (concat d "/")
                                                          d))
                                            (directory-files path t (rx (not ".") eos)))
                                    (cl-loop for child in (alist-get :children elem)
                                             collect (alist-get :path child))))
             for folder-exists = (file-directory-p path)
             for folder-has-to-exist = (seq-contains-p active-paths path)
             collect `((path . ,path)
                       (exists . ,folder-exists)
                       (has-to-exist . ,folder-has-to-exist)
                       (children . ,(append
                                     (cl-loop for f in extra-folders
                                              collect `((path . ,f)
                                                        (exists . t)
                                                        (has-to-exist . nil)
                                                        (extra . t)))
                                     (my/index--filesystem-tree-mapping
                                      (alist-get :children elem) tree active-paths)))))))

(defun my/index--filesystem-commands (mapping)
  "Get commands to sync filesystem with the tree.

MAPPING is a form generated by `my/index--filesystem-tree-mapping'
that describes the \"sync state\" between the filesystem and the
tree.

The return value is a list of commands as defined by
`my/index--commands-display'."
  (cl-loop for elem in mapping
           for path = (alist-get 'path elem)
           for exists = (alist-get 'exists elem)
           for has-to-exist = (alist-get 'has-to-exist elem)
           for extra = (alist-get 'extra elem)
           when (and (not exists) has-to-exist)
           collect (list (format "mkdir \"%s\"" path) "Make directories" 1)
           when (and exists (not has-to-exist))
           collect (list (format "rm -rf \"%s\"" path)
                         (if extra "Remove extra files" "Remove directories")
                         (if extra 20 10))
           append (my/index--filesystem-commands (alist-get 'children elem))))

(defun my/parse-table-str (string)
  "Convert a table-like STRING into alist.

The input format is as follows:
HEADER1 HEADER2 HEADER3
value1  value2  3
value4  value5  6

Which creates the following output:
\(((HEADER1. \"value1\") (HEADER2 . \"value2\") (HEADER3 . \"3\"))
 ((HEADER1. \"value4\") (HEADER2 . \"value5\") (HEADER3 . \"6\")))

The functions also skips lines in [square brackets] and ones that
start with more than 3 spaces."
  (when-let* ((lines (seq-filter
                 (lambda (s) (not (or (string-empty-p s)
                                      (string-match-p (rx bos "[" (* nonl) "]") s)
                                      (string-match-p (rx bos (>= 3 " ")) s))))
                 (split-string string "\n")))
         (first-line (car lines))
         (headers (split-string first-line))
         (header-indices (mapcar
                          (lambda (header)
                            (cl-search header first-line))
                          headers)))
    (cl-loop for line in (cdr lines)
             collect (cl-loop for header in headers
                              for start in header-indices
                              for end in (append (cdr header-indices)
                                                 (list (length line)))
                              collect (cons
                                       (intern header)
                                       (string-trim
                                        (substring line start end)))))))

(defun my/index--mega-data-from-sync ()
  "Get the current MEGA sync status.

The return value is a list of alists with the following keys:
- path - path to file or directory
- enabled - whether the file or directory is enabled for sync"
  (let ((mega-result (my/parse-table-str
                      (shell-command-to-string "mega-sync --path-display-size=10000"))))
    (cl-loop for value in mega-result
             for localpath = (alist-get 'LOCALPATH value)
             collect `((path . ,(if (file-directory-p localpath)
                                    (concat localpath "/")
                                  localpath))
                       (enabled . ,(string-equal (alist-get 'ACTIVE value)
                                                 "Enabled"))))))

(defun my/index--tree-get-paths (tree &optional kind)
  "Get paths from TREE.

TREE is a form a defined by `my/index--tree-get'.  KIND is either a
filter by the kind attribute or nil, in which case all paths are
returned.

The return value is a list of strings."
  (cl-loop for elem in tree
           when (or (null kind) (eq (alist-get :kind elem) kind))
           collect (alist-get :path elem)
           append (my/index--tree-get-paths
                   (alist-get :children elem) kind)))

(defun my/index--mega-local-path (path)
  "Get path in the MEGA cloud by the local path PATH."
  (string-replace my/index-root "/" path))

(defun my/index--mega-commands (full-tree tree)
  "Get commands to sync the mega-sync state with TREE.

FULL-TREE and TREE are forms as defined by `my/index--tree-get'.  TREE
is the narrowed FULL-TREE (returned by `my/index--tree-narrow').

The return value is a list of commands as defined by
`my/index--commands-display'."
  (let* ((paths-all (my/index--tree-get-paths full-tree))
         (mega-paths-to-enable (my/index--tree-get-paths tree 'mega))
         (mega-info (my/index--mega-data-from-sync))
         (mega-paths-enabled (seq-map
                              (lambda (e) (alist-get 'path e))
                              (seq-filter (lambda (e) (alist-get 'enabled e))
                                          mega-info)))
         (mega-paths-disabled (seq-map
                               (lambda (e) (alist-get 'path e))
                               (seq-filter (lambda (e) (not (alist-get 'enabled e)))
                                           mega-info))))
    (append
     (cl-loop for path in (seq-difference mega-paths-to-enable mega-paths-enabled)
              if (seq-contains-p mega-paths-disabled path)
              collect (list (format "mega-sync -e \"%s\"" path) "Mega enable sync" 5)
              else append (list
                           (list (format "mega-mkdir -p \"%s\""
                                         (my/index--mega-local-path path))
                                 "Mega mkdirs" 4)
                           (list (format "mega-sync \"%s\" \"%s\""
                                         path (my/index--mega-local-path path))
                                 "Mega add sync" 5)))
     (cl-loop for path in (seq-difference
                           (seq-intersection mega-paths-enabled paths-all)
                           mega-paths-to-enable)
              collect (list
                       (format "mega-sync -d \"%s\""
                               (substring path 0 (1- (length path))))
                       "Mega remove sync" 4)))))

(defun my/index--git-commands (tree)
  "Get commands to clone the yet uncloned git repos in TREE.

TREE is a form a defined by `my/index--tree-get'.  This is supposed to
be the tree narrowed to the current machine (`my/index--tree-narrow').

The return value is a list of commands as defined by
`my/index--commands-display'."
  (cl-loop for elem in tree
           for path = (alist-get :path elem)
           when (and (eq (alist-get :kind elem) 'git)
                     (or (not (file-directory-p path))
                         (directory-empty-p path)))
           collect (list (format "git clone \"%s\" \"%s\""
                                 (alist-get :remote elem)
                                 path)
                         "Init git repos" 2)
           append (my/index--git-commands (alist-get :children elem))))

(defun my/index--bare-project-name (name)
  "Remove the alphanumeric prefix from NAME.

E.g. 10.03.R.01 Project Name -> Project Name."
  (replace-regexp-in-string
   (rx bos (+ (| num alpha "." "-")) space) "" name))

(defun my/index--wakatime-escape (string)
  "Escape STRING for use in a WakaTime config file."
  (thread-last
    string
    (replace-regexp-in-string (rx "'") "\\\\'")
    (replace-regexp-in-string (rx "(") "\\\\(")
    (replace-regexp-in-string (rx ")") "\\\\)")))

(defun my/index--wakatime-get-map-tree (tree)
  "Get a list of (folder-name . bare-project-name) pairs from TREE.

TREE is a form as defined by `my/index--tree-get'.
\"bare-project-name\" is project name without the alphanumeric
prefix."
  (cl-loop for elem in tree
           for name = (alist-get :name elem)
           if (eq (alist-get :kind elem) 'git)
           collect (cons (my/index--wakatime-escape name)
                         (my/index--wakatime-escape
                          (my/index--bare-project-name name)))
           if (and (eq (alist-get :kind elem) 'git)
                   (alist-get :symlink elem))
           collect (cons (my/index--wakatime-escape
                          ;; lmao
                          ;; /a/b/c/ -> c
                          ;; /a/b/c -> b
                          (file-name-nondirectory
                           (directory-file-name
                            (file-name-directory (alist-get :symlink elem)))))
                         (my/index--wakatime-escape
                          (my/index--bare-project-name name)))
           append (my/index--wakatime-get-map-tree (alist-get :children elem))))

(defun my/index--wakatime-commands (tree)
  "Get commands to update WakaTime config from TREE.

TREE is a form a defined by `my/index--tree-get'. The return value is
a list of commands as defined by `my/index--commands-display'."
  (let* ((map-tree (my/index--wakatime-get-map-tree tree))
         (map-tree-encoding (ini-encode `(("projectmap" . ,map-tree))))
         (map-tree-saved (with-temp-buffer
                           (insert-file-contents (expand-file-name "~/.wakatime.cfg"))
                           (string-match-p (regexp-quote map-tree-encoding)
                                           (buffer-string)))))
    (unless map-tree-saved
      (let ((insert-command (list (format "echo \"\n\n%s\" >> ~/.wakatime.cfg"
                                          map-tree-encoding)
                                  "Update WakaTime config" 9)))
        (list (list (format "sed -i -z 's/\\[projectmap\\]\\n[^[]*//g' ~/.wakatime.cfg")
                    "Update WakaTime config" 9)
              insert-command)))))

(defun my/index-get-symlink-commands (tree)
  "Get commands to create symlinks from TREE.

TREE is a form a defined by `my/index--tree-get'. The return value is
a list of commands as defined by `my/index--commands-display'."
  (cl-loop for elem in tree
           for path = (alist-get :path elem)
           for symlink = (alist-get :symlink elem)
           when (and symlink (not (string-match-p (rx "/" eos) symlink)))
           do (user-error "Wrong symlink: %s (should be a directory)" symlink)
           when (and path symlink
                     (or (file-exists-p symlink)
                         (file-exists-p (substring symlink 0 -1)))
                     (not (file-symlink-p (substring symlink 0 -1))))
           collect (list (format "rm -rf %s" (substring symlink 0 -1))
                         "Remove files to make symlinks" 6)
           when (and path symlink
                     (not (file-symlink-p (substring symlink 0 -1))))
           collect (list (format "ln -s '%s' '%s'" path
                                 (substring symlink 0 -1))
                         "Make symlinks" 7)
           append (my/index-get-symlink-commands (alist-get :children elem))))

(defvar my/index-commands-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-c C-c") #'my/index-commands-exec)
    (define-key keymap (kbd "q") #'my/quit-window-and-buffer)
    (when (fboundp 'evil-define-key*)
      (evil-define-key* 'normal keymap
        "q" #'my/quit-window-and-buffer))
    keymap)
  "Keymap for `biome-api-error-mode'.")

(define-derived-mode my/index-commands-mode sh-mode "Index Commands"
  "A mode to display index commands.")

(defvar-local my/index-commands nil
  "Commands to be executed by `my/index-commands-exec'")

(defun my/index--commands-display (commands)
  "Display COMMANDS in a buffer.

COMMANDS is a list of commands as defined by `my/index--commands-display'."
  (unless commands
    (user-error "No commands to display"))
  (let ((buffer (get-buffer-create "*index commands*"))
        (groups (seq-sort-by
                 (lambda (g) (nth 2 (nth 1 g)))
                 #'<
                 (seq-group-by (lambda (c) (nth 1 c))
                               commands))))
    (with-current-buffer buffer
      (my/index-commands-mode)
      (let ((inhibit-read-only t)
            commands-sequence)
        (erase-buffer)
        (setq-local my/index-commands nil)
        (cl-loop for g in groups
                 for group-name = (car g)
                 for elems = (cdr g)
                 do (insert "# " group-name "\n")
                 do (cl-loop for elem in elems
                             do (push (nth 0 elem) my/index-commands)
                             do (insert (nth 0 elem) "\n")))
        (setq-local buffer-read-only t)))
    (switch-to-buffer buffer)))

(defun my/index-commands-exec ()
  (interactive)
  (unless (eq major-mode 'my/index-commands-mode)
    (user-error "Not shell mode"))
  (let ((filename (make-temp-file "index-commands-")))
    (write-region (point-min) (point-max) filename)
    (compile (concat "bash -x " filename))))

(defvar my/index--tree nil
  "The last version of the index tree.")

(defun my/index--tree-retrive ()
  "Retrive the last version of the index tree.

This function returns the last saved version of the index tree if it
is still valid. Otherwise, it re-parses the index file."
  (setq
   my/index--tree
   (cond ((string-equal (buffer-file-name) my/index-file)
          (my/index--tree-get))
         ((or (null my/index--tree)
              (file-has-changed-p my/index-file 'index))
          (with-temp-buffer
            (insert-file-contents my/index-file)
            (let ((buffer-file-name my/index-file))
              (my/index--tree-get))))
         (t my/index--tree))))

(defun my/index-commands-sync ()
  "Sync the filesystem with the index."
  (interactive)
  (let* ((full-tree (my/index--tree-retrive)))
    (my/index--tree-verify full-tree)
    (let* ((tree (my/index--tree-narrow full-tree))
           (mega-commands (my/index--mega-commands full-tree tree))
           (mapping (my/index--filesystem-tree-mapping full-tree tree))
           (folder-commands (my/index--filesystem-commands mapping))
           (git-commands (my/index--git-commands tree))
           (waka-commands (my/index--wakatime-commands tree))
           (symlink-commands (my/index-get-symlink-commands tree)))
      (my/index--commands-display (append mega-commands folder-commands git-commands
                                          waka-commands symlink-commands)))))

(defun my/index--nav-extend (name path)
  "Find all index-related files in PATH.

NAME is the name of the root index entry, e.g. \"10.01
Something\".  If PATH containts folders like \"10.01.01
Something\", \"10.01.02 ...\", they will be returned.

The return value is a form as defined by `my/index--nav-get'."
  (when (file-directory-p path)
    (let* ((number (my/index--extact-number name))
           (files (mapcar
                   (lambda (f) (cons f (concat path f)))
                   (seq-filter (lambda (f) (not (string-prefix-p "." f)))
                               (directory-files path))))
           (matching-files
            (seq-filter
             (lambda (f) (and (file-directory-p (cdr f))
                              (string-prefix-p number (car f))))
             files)))
      (when (and (length> matching-files 0)
                 (length< matching-files (length files)))
        (user-error "Extraneuous files in %s" path))
      (cl-loop for (name-1 . path-1) in matching-files
               append (if-let ((child-files (my/index--nav-extend name-1 (concat path-1 "/"))))
                          (mapcar
                           (lambda (child-datum)
                             (push name-1 (alist-get :names child-datum))
                             child-datum)
                           child-files)
                        `(((:names . (,name-1))
                           (:path . ,(concat path-1 "/")))))))))

(defun my/index--nav-get (tree &optional names)
  "Get the navigation structure from TREE.

TREE is a form as defined by `my/index--tree-get'.  NAMES is a
list of names of the parent entries, e.g. (\"10.01 Something\"), used
for recursive calls.

The result is a list of alists with the following keys:
- `:names` - list of names, e.g.
  (\"10.01 Something\" \"10.01.01 Something\")
- `:path` - path to the folder, e.g.
  \"/path/10 stuff/10.01 Something/10.01.01 Something/\"
- `:child-navs` - list of child navigation structures (optional)"
  (seq-sort-by
   (lambda (item) (alist-get :path item))
   #'string-lessp
   (cl-reduce
    (lambda (acc elem)
      (let* ((name (alist-get :name elem))
             (path (alist-get :path elem)))
        (cond ((alist-get :project elem)
               (let ((current-nav `((:names . (,@names ,name))
                                    (:path . ,path))))
                 (when-let (child-navs
                            (and (alist-get :children elem)
                                 (my/index--nav-get (alist-get :children elem))))
                   (setf (alist-get :child-navs current-nav) child-navs))
                 (push current-nav acc)))
              ((alist-get :children elem)
               (when-let (child-navs (my/index--nav-get
                                      (alist-get :children elem)
                                      `(,@names ,name)))
                 (cl-loop for child-nav in child-navs
                          do (push child-nav acc))))
              (t (if-let ((extended-nav (my/index--nav-extend name path)))
                     (cl-loop for child-nav in extended-nav
                              do (setf (alist-get :names child-nav)
                                       (append names (list name)
                                               (alist-get :names child-nav)))
                              do (push child-nav acc))
                   (push `((:names . (,@names ,name))
                           (:path . ,path))
                         acc))))
        acc))
    tree
    :initial-value nil)))

(defvar my/index--nav nil
  "Navigation stucture for the index.")

(defun my/index--nav-retrive ()
  "Retrive the navigation structure from the index file.

The return value is a form as defined by `my/index--nav-get'."
  (if (or (null my/index--nav)
          (file-has-changed-p my/index-file 'nav))
      (let ((tree (my/index--tree-retrive)))
        (setq my/index--nav (my/index--nav-get
                             (my/index--tree-narrow tree))))
    my/index--nav))

(defun my/index--nav-prompt (nav)
  "Prompt the user for the navigation item to select.

NAV is a structure as defined by `my/index--nav-get'."
  (let* ((collection
          (mapcar (lambda (item)
                    (cons (car (last (alist-get :names item)))
                          (alist-get :path item)))
                  nav))
         (ivy-prescient-sort-commands nil))
    (cdr
     (assoc
      (completing-read "Index: " collection nil t)
      collection))))

(defun my/index--nav-find-path (nav path)
  "Find the navigation item in NAV with the given PATH.

NAV is a structure as defined by `my/index--nav-get'."
  (seq-find
   (lambda (item)
     (string-prefix-p (alist-get :path item) path))
   nav))

(defun my/index-nav (arg &optional func)
  "Navigate the filesystem index.

If ARG is nil, navigate all levels sequentially from the top one.

If ARG is '(4), select another directory from the same level.

FUNC is the function to call with the selected path.  It defaults
to `dired' if used interactively."
  (interactive (list current-prefix-arg #'dired))
  (let* ((nav (my/index--nav-retrive))
         (current-nav (my/index--nav-find-path
                       nav (expand-file-name default-directory)))
         (current-child-navs (alist-get :child-navs current-nav)))
    (cond ((null arg)
           (let ((selected (my/index--nav-find-path
                            nav
                            (my/index--nav-prompt nav))))
             (if-let (child-navs (alist-get :child-navs selected))
                 (funcall func (my/index--nav-prompt child-navs))
               (funcall func (alist-get :path selected)))))
          ((and (equal arg '(4)) current-child-navs)
           (funcall func (my/index--nav-prompt current-child-navs)))
          ((and (equal arg '(4)) (null current-child-navs))
           (funcall func (my/index--nav-prompt nav))))))

(defun my/index-nav-with-select-file (arg)
  (interactive (list current-prefix-arg))
  (my/index-nav
   arg
   (lambda (dir)
     (let ((default-directory dir))
       (projectile-find-file)))))

(defun my/index-open-file ()
  (interactive)
  (find-file my/index-file))

(my-leader-def
  :infix "i"
  "" '(:wk "index")
  "i" #'my/index-nav
  "s" #'my/index-commands-sync
  "p" #'my/index-nav-with-select-file
  "f" #'my/index-open-file)

(defun my/index-export (file)
  (interactive (list (read-file-name "File: " "~/logs-sync/data/index.json")))
  (let ((full-tree (my/index--tree-retrive)))
    (unless (file-exists-p (file-name-directory file))
      (make-directory (file-name-directory file) t))
    (with-temp-file file
      (insert (json-encode full-tree))
      (json-pretty-print-buffer))))

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
  :if (not (or my/remote-server my/is-termux))
  :commands (docker)
  :init
  (my-leader-def "ao" 'docker))

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
  :if (not (or my/remote-server my/is-termux))
  :commands (guix)
  :init
  (my-leader-def "ag" 'guix)
  (defun geiser-company--setup (&rest args)
    "A dummy function.")
  (defvar geiser-repl-company-p nil
    "A dummy variable."))

(use-package atomic-chrome
  :if (not (or my/remote-server my/is-termux))
  :commands (atomic-chrome-start-server)
  :straight t)

(use-package pinentry
  :straight t
  :if my/is-termux
  :config
  (setenv "GPG_AGENT_INFO" nil) ;; use emacs pinentry
  (setq auth-source-debug t)

  (setq epg-gpg-program "gpg") ;; not necessary
  (require 'epa-file)
  (epa-file-enable)
  (setq epa-pinentry-mode 'loopback)
  (setq epg-pinentry-mode 'loopback)
  (pinentry-start))

(use-package pomm
  :straight t
  ;; :straight (:local-repo "~/Code/Emacs/pomm" :files (:defaults "resources"))
  :commands (pomm pomm-third-time)
  :init
  (my-leader-def "ap" #'pomm-third-time)
  (setq alert-default-style 'libnotify)
  :config
  (pomm-mode-line-mode))

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

(use-package chess
  :straight t)

(setq my/chess-python "/home/pavel/.guix-extra-profiles/dev/dev/bin/python3")

(defun org-babel-execute:pgn (body params)
  (let ((out-file (or (alist-get :file params)
                      (org-babel-temp-file "pgn-" ".png"))))
    (org-babel-eval
     (format "%s %s '%s' '%s'" my/chess-python
             "~/bin/python-scripts/render_pgn.py"
             body out-file)
     "")
    nil))

(defvar org-babel-default-header-args:pgn
  '((:results . "file") (:exports . "results"))
  "Default arguments for evaluating a pgn source block.")

(defun org-babel-execute:fen (body params)
  (let ((out-file (or (alist-get :file params)
                      (org-babel-temp-file "fen-" ".png"))))
    (org-babel-eval
     (format "%s %s '%s' '%s' true" my/chess-python
             "~/bin/python-scripts/render_pgn.py"
             body out-file)
     "")
    nil))

(defvar org-babel-default-header-args:fen
  '((:results . "file") (:exports . "results"))
  "Default arguments for evaluating a pgn source block.")

(defun my/elcord-mask-buffer-name (name)
  (cond
   ((string-match-p (rx bos (? "CAPTURE-") (= 14 num) "-" (* not-newline) ".org" eos) name)
    "<ORG-ROAM>")
   ((string-match-p (rx bos (+ num) "-" (+ num) "-" (+ num) ".org" eos) name)
    "<ORG-JOURNAL>")
   ((string-match-p (rx bos "EXWM") name)
    "<EXWM>")
   ((string-match-p (rx bos "*Org-Habit") name)
    "<ORG>")
   ((with-current-buffer (get-buffer name)
      (derived-mode-p 'telega-root-mode 'telega-chat-mode))
    "<TELEGA>")
   (t name)))

(defun my/elcord-buffer-details-format-functions ()
  (format "Editing %s" (my/elcord-mask-buffer-name (buffer-name))))

(defun my/elcord-update-presence-mask-advice (r)
  (list (my/elcord-mask-buffer-name (nth 0 r)) (nth 1 r)))

(defun my/elcord-symlink ()
  (shell-command-to-string "bash -c 'ln -sf {app/com.discordapp.Discord,$XDG_RUNTIME_DIR}/discord-ipc-0 &'"))

(use-package elcord
  :straight t
  :if (and (or
            (string= (system-name) "violet")
            (string= (system-name) "eminence")
            (string= (system-name) "iris"))
           (not my/slow-ssh)
           (not my/remote-server))
  :config
  (setq elcord-buffer-details-format-function #'my/elcord-buffer-details-format-functions)
  (advice-add 'elcord--try-update-presence :filter-args #'my/elcord-update-presence-mask-advice)
  (add-to-list 'elcord-mode-text-alist '(telega-chat-mode . "Telega Chat"))
  (add-to-list 'elcord-mode-text-alist '(telega-root-mode . "Telega Root"))
  (elcord-mode)
  (my/elcord-symlink))

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

(defun my/gravatar-retrieve-sync (email file-name)
  "Get gravatar for EMAIL and save it to FILE-NAME."
  (let ((gravatar-default-image "identicon")
        (gravatar-size nil)
        (coding-system-for-write 'binary)
        (write-region-annotate-functions nil)
        (write-region-post-annotation-function nil))
    (write-region
     (image-property (gravatar-retrieve-synchronously email) :data)
     nil file-name nil :silent)))

(setq my/gravatar-folder "/home/pavel/.cache/gravatars/")

(defun my/gravatar-save (email author)
  "Download gravatar for EMAIL.

AUTHOR is the username."
  (let ((file-name (concat my/gravatar-folder author ".png")))
    (mkdir my/gravatar-folder t)
    (unless (file-exists-p file-name)
      (message "Fetching gravatar for %s (%s)" author email)
      (my/gravatar-retrieve-sync email file-name))))

(defun my/git-get-authors (repo &optional authors-init)
  "Extract and merge all combinations of authors & emails from REPO.

REPO is the path to a git repository.

AUTHORS-INIT is the previous output of `my/git-get-authors'.  It can
be used to extract that information from multiple repositories.

The output is a list of alists with following keys:
- emails: list of (<email> . <count>)
- authors: list of (<username> . <count>)
- email: the most popular email
- author: the most popular username
I.e. one alist is all emails and usernames of one author."
  (let* ((default-directory repo)
         (data (shell-command-to-string
                "git log --pretty=format:\"%ae|%an\" | sort | uniq -c | sed \"s/^[ \t]*//;s/ /|/\""))
         (authors
          (cl-loop for string in (split-string data "\n")
                   if (= (length (split-string string "|")) 3)
                   collect (let ((datum (split-string string "|")))
                             `((count . ,(string-to-number (nth 0 datum)))
                               (email . ,(downcase (nth 1 datum)))
                               (author . ,(nth 2 datum)))))))
    (mapcar
     (lambda (datum)
       (setf (alist-get 'author datum)
             (car (cl-reduce
                   (lambda (acc author)
                     (if (> (cdr author) (cdr acc))
                         author
                       acc))
                   (alist-get 'authors datum)
                   :initial-value '(nil . -1))))
       (setf (alist-get 'email datum)
             (car (cl-reduce
                   (lambda (acc email)
                     (if (> (cdr email) (cdr acc))
                         email
                       acc))
                   (alist-get 'emails datum)
                   :initial-value '(nil . -1))))
       datum)
     (cl-reduce
      (lambda (acc val)
        (let* ((author (alist-get 'author val))
               (email (alist-get 'email val))
               (count (alist-get 'count val))
               (saved-value
                (seq-find
                 (lambda (cand)
                   (or (alist-get email (alist-get 'emails cand)
                                  nil nil #'string-equal)
                       (alist-get author (alist-get 'authors cand)
                                  nil nil #'string-equal)
                       (alist-get email (alist-get 'authors cand)
                                  nil nil #'string-equal)
                       (alist-get author (alist-get 'emails cand)
                                  nil nil #'string-equal)))
                 acc)))
          (if saved-value
              (progn
                (if (alist-get email (alist-get 'emails saved-value)
                               nil nil #'string-equal)
                    (cl-incf (alist-get email (alist-get 'emails saved-value)
                                        nil nil #'string-equal)
                             count)
                  (push (cons email count) (alist-get 'emails saved-value)))
                (if (alist-get author (alist-get 'authors saved-value)
                               nil nil #'string-equal)
                    (cl-incf (alist-get author (alist-get 'authors saved-value)
                                        nil nil #'string-equal)
                             count)
                  (push (cons author count) (alist-get 'authors saved-value))))
            (setq saved-value
                  (push `((emails . ((,email . ,count)))
                          (authors . ((,author . ,count))))
                        acc)))
          acc))
      authors
      :initial-value authors-init))))

(defun my/gource-prepare-log (repo authors)
  "Create gource log string for REPO.

AUTHORS is the output of `my/git-get-authors'."
  (let ((log (shell-command-to-string
              (concat
               "gource --output-custom-log - "
               repo)))
        (authors-mapping (make-hash-table :test #'equal))
        (prefix (file-name-base repo)))
    (cl-loop for author-datum in authors
             for author = (alist-get 'author author-datum)
             do (my/gravatar-save (alist-get 'email author-datum) author)
             do (cl-loop for other-author in (alist-get 'authors author-datum)
                         unless (string-equal (car other-author) author)
                         do (puthash (car other-author) author
                                     authors-mapping)))
    (cl-loop for line in (split-string log "\n")
             concat (let ((fragments (split-string line "|")))
                      (when (> (length fragments) 3)
                        (when-let (mapped-author (gethash (nth 1 fragments)
                                                          authors-mapping))
                          (setf (nth 1 fragments) mapped-author))
                        (setf (nth 3 fragments)
                              (concat "/" prefix (nth 3 fragments))))
                      (string-join fragments "|"))
             concat "\n")))

(defun my/gource-dired-create-logs (repos log-name)
  "Create combined gource log for REPOS.

REPOS is a list of strings, where a string is a path to a git repo.
LOG-NAME is the path to the resulting log file.

This function is meant to be invoked from `dired', where the required
repositories are marked."
  (interactive (list (or (dired-get-marked-files nil nil #'file-directory-p)
                         (user-error "Select at least one directory"))
                     (read-file-name "Log file name: " nil "combined.log")))
  (let ((authors
         (cl-reduce
          (lambda (acc repo)
            (my/git-get-authors repo acc))
          repos
          :initial-value nil)))
    (with-temp-file log-name
      (insert
       (string-join
        (seq-filter
         (lambda (line)
           (not (string-empty-p line)))
         (seq-sort-by
          (lambda (line)
            (if-let (time (car (split-string line "|")))
                (string-to-number time)
              0))
          #'<
          (split-string
           (mapconcat
            (lambda (repo)
              (my/gource-prepare-log repo authors))
            repos "\n")
           "\n")))
        "\n")))))

(use-package imgur
  :straight (:host github :repo "larsmagne/imgur.el")
  :defer t)

(use-package meme
  :straight (:host github :repo "larsmagne/meme" :files (:defaults "images"))
  :commands (meme))

(use-package ed-mode
  :straight (:host github :repo "ryanprior/ed-mode")
  :commands (ed))
