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

(setq my/remote-server
      (or (string= (getenv "IS_REMOTE") "true")
          (string= (system-name) "dev-digital")
          (string= (system-name) "viridian")))

(setq my/is-termux (string-match-p (rx (* nonl) "com.termux" (* nonl)) (getenv "HOME")))

(defun my/system-name ()
  (or (getenv "ANDROID_NAME")
      (system-name)))

(setq my/nested-emacs (and (getenv "IS_EMACS") t))
(setenv "IS_EMACS" "true")

(setq my/emacs-started nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)
            (setq my/emacs-started t)))

(setq use-package-verbose nil)

(setq use-package-compute-statistics t)

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

(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message "Hello there <3\n\n")

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
      (goto-char (point-min))
      (setq-local buffer-read-only t))
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
     :states '(motion))))

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
   '(eww devdocs proced emms pass calendar dired debug guix calc
         docker ibuffer geiser pdf info elfeed edebug bookmark company
         vterm flycheck profiler cider explain-pause-mode notmuch custom
         xref eshell helpful compile comint git-timemachine magit prodigy
         slime forge deadgrep vc-annonate telega doc-view gnus outline)))

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

(defun my/lisp-interaction-buffer ()
  (interactive)
  (let ((buf (get-buffer-create "*lisp-interaction*")))
    (with-current-buffer buf
      (lisp-interaction-mode))
    (switch-to-buffer buf)))

(my-leader-def
  :infix "b"
  "" '(:which-key "buffers")
  "s" '(my/lisp-interaction-buffer
        :which-key "*lisp-interaction*")
  "m" '((lambda () (interactive) (persp-switch-to-buffer "*Messages*"))
        :which-key "*Messages*")
  "l" 'next-buffer
  "h" 'previous-buffer
  "k" 'kill-buffer
  ;; "b" 'persp-ivy-switch-buffer
  "b" #'persp-switch-to-buffer*
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
 :keymaps '(hs-minor-mode-map outline-minor-mode-map outline-mode-map)
 :states '(normal motion)
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

(when (and my/is-termux (not (equal (my/system-name) "snow")))
  (define-key key-translation-map (kbd "`") (kbd "<escape>"))
  (define-key key-translation-map (kbd "<escape>") (kbd "`")))

(when my/is-termux
  (setq split-width-threshold 90))

(unless (or my/remote-server my/nested-emacs)
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
    (if (or
         (not (executable-find "xkb-switch"))
         (equal (string-trim
                 (shell-command-to-string "xkb-switch -p"))
                "us"))
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

(defvar my/default-accents
  '((a . ä)
    (o . ö)
    (u . ü)
    (s . ß)
    (A . Ä)
    (O . Ö)
    (U . Ü)
    (S . ẞ)))

(defun my/accent (arg)
  (interactive "P")
  (require 'accent)
  (message "%s" arg)
  (let* ((after? (eq accent-position 'after))
         (char (if after? (char-after) (char-before)))
         (curr (intern (string char)))
         (default-diac (cdr (assoc curr my/default-accents))))
    (if (and default-diac (not arg))
        (progn
          (delete-char (if after? 1 -1))
          (insert (format "%c" default-diac)))
      (call-interactively #'accent-company))))

(use-package accent
  :straight (:host github :repo "eliascotto/accent")
  :init
  (general-define-key
   :states '(normal)
   "gs" #'accent-company)
  (general-define-key
   :states '(normal insert)
   "M-n" #'my/accent)
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

(defun my/round-number-at-point (word signs)
  (interactive
   (list (or (when (region-active-p)
               (buffer-substring-no-properties
                (region-beginning)
                (region-end)))
             (thing-at-point 'number 'no-properties))
         (read-number "Decimal signs: " 2)))
  (when (stringp word)
    (setq word (string-to-number word)))
  (let ((number (/ (float (round (* (expt 10 signs) word)))
                   (expt 10 signs))))
    (save-excursion
      (replace-string-in-region
       (number-to-string word)
       (number-to-string number)
       (line-beginning-position)
       (line-end-position)))))

(use-package projectile
  :straight t
  :config
  (projectile-mode +1)
  (setq projectile-project-search-path '("~/Code" "~/Documents"))
  (general-define-key
    :keymaps 'projectile-command-map
    "b" #'consult-project-buffer))

(my-leader-def
  "p" '(:keymap projectile-command-map :which-key "projectile"))

(general-nmap "C-p" #'projectile-find-file)

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
  :config
  (global-git-gutter-mode +1))

(use-package git-timemachine
  :straight t
  :commands (git-timemachine))

(use-package difftastic
  :straight t
  :commands (difftastic-magit-diff
             difftastic-magit-show
             difftastic-files
             difftastic-buffers)
  :init
  (with-eval-after-load 'magit-diff
    (transient-append-suffix 'magit-diff '(-1 -1)
      [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
       ("S" "Difftastic show" difftastic-magit-show)])
    (general-define-key
     :keymaps 'magit-blame-read-only-mode-map
     :states 'normal
     "D" #'difftastic-magit-show
     "S" #'difftastic-magit-show))
  :config
  (setq difftastic-executable (executable-find "difft"))
  (general-define-key
   :keymaps 'difftastic-mode-map
   :states '(normal)
   "gr" #'difftastic-rerun
   "q" #'kill-buffer-and-window))

(defun my/difftastic-pop-at-bottom (buffer-or-name _requested-width)
  (let ((window (split-window-below)))
    (select-window window)
    (evil-move-window 'below))
  (set-window-buffer (selected-window) buffer-or-name))

(setq difftastic-display-buffer-function #'my/difftastic-pop-at-bottom)

(setq difftastic-requested-window-width-function
      (lambda () (- (frame-width) 4)))

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
  (add-to-list 'editorconfig-indentation-alist
               '(emmet-mode emmet-indentation))
  (editorconfig-mode))

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

(defun my/register-clear (register)
  (interactive (list (register-read-with-preview "Clear register: ")))
  (setq register-alist (delq (assoc register register-alist) register-alist)))

(setq register-preview-delay which-key-idle-delay)

(my-leader-def
  :infix "g"
  "" '(:wk "registers & marks")
  "y" #'copy-to-register
  "p" #'insert-register
  "o" #'point-to-register
  "c" #'my/register-clear
  "r" #'jump-to-register
  "R" #'consult-register
  "w" #'window-configuration-to-register)

(defun my/push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
   Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(defun my/mark-ring-clear ()
  (interactive)
  (setq mark-ring nil))

(my-leader-def
  :infix "g"
  "G" #'consult-global-mark
  "g" #'consult-mark
  "C" #'my/mark-ring-clear
  "m" #'my/push-mark-no-activate)

(general-define-key
 :keymaps 'global
 "C-SPC" #'my/push-mark-no-activate)

(use-package avy
  :straight t
  :config
  (setq avy-timeout-seconds 0.5)
  (setq avy-ignored-modes
        '(image-mode doc-view-mode pdf-view-mode exwm-mode))
  (general-define-key
   :states '(normal motion)
   "-" #'avy-goto-char-timer))

(defun avy-action-embark (pt)
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

(with-eval-after-load 'avy
  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark))

(use-package ace-link
  :straight t
  :commands (ace-link-info ace-link-help ace-link-woman ace-link-eww))

(use-package vertico
  :straight t
  :config
  (setq enable-recursive-minibuffers t)
  (general-define-key
   :keymaps '(vertico-map)
   "M-j" #'vertico-next
   "M-k" #'vertico-previous
   "TAB" #'minibuffer-complete)
  (vertico-mode))

(defun crm-indicator (args)
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator)
                (car args))
        (cdr args)))
(with-eval-after-load 'crm
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator))

(use-package savehist
  :init
  (savehist-mode))

(use-package vertico-directory
  :after (vertico)
  :config
  (general-define-key
   :keymaps '(vertico-map)
   "RET" #'vertico-directory-enter
   "DEL" #'vertico-directory-delete-char)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))

(use-package vertico-grid
  :after (vertico))

(defun my/sort-directories-first (files)
  (setq files (vertico-sort-alpha files))
  (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
         (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))

(use-package vertico-multiform
  :after vertico
  :config
  (vertico-multiform-mode)
  (general-define-key
   :keymap 'vertico-multiform-map
   "M-b" #'vertico-multiform-buffer
   "M-g" #'vertico-multiform-grid)
  (setq vertico-multiform-categories
        '((file (vertico-sort-function . my/sort-directories-first))
          (password-store-pass grid)))
  (setq vertico-multiform-commands
        '((eshell-atuin-history (vertico-sort-function . nil))
          (my/index-nav (vertico-sort-function . nil))
          (org-ql-view (vertico-sort-function . nil))
          (my/consult-line (vertico-sort-function . nil))
          (telega-msg-add-reaction grid))))

(use-package vertico-quick
  :after vertico
  :config
  (general-define-key
   :keymaps '(vertico-map)
   "M-q" #'vertico-quick-insert
   "C-q" #'vertico-quick-exit))

(use-package orderless
  :straight t
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides
        '((file (styles partial-completion))))
  (setq orderless-matching-styles
        '(orderless-literal orderless-initialism orderless-regexp)))

(defun company-completion-styles (capf-fn &rest args)
  (let ((completion-styles '(basic partial-completion)))
    (apply capf-fn args)))

(with-eval-after-load 'company
  (advice-add 'company-capf :around #'company-completion-styles))

(use-package consult
  :straight t
  :config
  (setq consult-preview-excluded-files
        `("\\`/[^/|:]+:"
          ,(rx "html" eos))))

(use-package marginalia
  :straight t
  :config
  (marginalia-mode)
  (push '(projectile-find-file . file)
        marginalia-command-categories))

(use-package embark
  :straight t
  :commands (embark-act embark-dwim embark-bindings)
  :init
  (general-define-key
   "M-e" #'embark-act))

(use-package embark-consult
  :straight t
  :after (embark)
  :config
  (add-hook 'embark-collect-mode #'consult-preview-at-point-mode))

(defun embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" "")))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymapp) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil nil t (lambda (binding)
                   (not (string-suffix-p "-argument" (cdr binding))))))))

(defun embark-hide-which-key-indicator (fn &rest args)
  "Hide the which-key indicator immediately when using the completing-read prompter."
  (which-key--hide-popup-ignore-command)
  (let ((embark-indicators
         (remq #'embark-which-key-indicator embark-indicators)))
    (apply fn args)))

(with-eval-after-load 'embark
  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator)
  (setq embark-indicators (delq #'embark-mixed-indicator embark-indicators))
  (push #'embark-which-key-indicator embark-indicators))

(my-leader-def
  :infix "f"
  "" '(:which-key "various completions")'
  "b" #'persp-switch-to-buffer*
  "e" 'micromamba-activate
  "f" 'project-find-file
  "c" 'consult-yank-pop
  "a" 'consult-ripgrep
  "d" 'deadgrep)

(general-define-key
 :states '(insert normal)
 "C-y" 'consult-yank-pop)

(defun my/consult-line ()
  (interactive)
  (if current-prefix-arg
      (call-interactively #'consult-line-multi)
    (consult-line nil t)))

;; (my-leader-def "SPC SPC" 'ivy-resume)
(my-leader-def "s" 'my/consult-line)

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
  (advice-add 'wakatime-init :after
              (lambda ()
                (setq wakatime-cli-path (or
                                         (executable-find "wakatime-cli")
                                         (expand-file-name "~/bin/wakatime-cli")))))
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
  :commands (olivetti-mode)
  :config
  (setq-default olivetti-body-width 86))

(use-package keycast
  :straight t
  :init
  (define-minor-mode keycast-mode
    "Keycast mode"
    :global t
    (if keycast-mode
        (progn
          (add-to-list 'global-mode-string '("" keycast-mode-line " "))
          (add-hook 'pre-command-hook 'keycast--update t) )
      (remove-hook 'pre-command-hook 'keycast--update)
      (setq global-mode-string (delete '("" keycast-mode-line " ") global-mode-string))))
  :commands (keycast--update))

(use-package doom-themes
  :straight t
  ;; Not deferring becuase I want `doom-themes-visual-bell-config'
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
        ((string-match-p (rx bos "dark-") color-name)
         (or (doom-color color)
             (ct-edit-hsl-l-dec (my/doom-color (intern (substring color-name 5)))
                                my/alpha-for-light)))
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
          dark-red dark-green dark-yellow dark-blue dark-magenta dark-cyan
          light-red light-green light-yellow light-blue light-magenta
          light-cyan light-white bg bg-alt fg fg-alt violet grey base0 base1
          base2 base3 base4 base5 base6 base7 base8 border))

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
   ((eq color 'modeline)
    (or
     (my/color-value 'bg-mode-line-active)
     (my/color-value 'bg-mode-line)
     (if (my/light-p)
         (ct-edit-hsl-l-dec (my/color-value 'bg-alt) 10)
       (ct-edit-hsl-l-inc (my/color-value 'bg-alt) 15))))
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
 (tab-bar :background 'unspecified :foreground 'unspecified)
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

(if my/is-termux
    (progn
      (my/switch-theme 'modus-operandi-tinted))
  (my/switch-theme 'ef-duo-light))

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

(use-package nerd-icons
  :straight t)

(use-package indent-bars
  :straight (:host github :repo "jdtsmith/indent-bars")
  :if (not (or my/remote-server))
  :hook ((prog-mode . indent-bars-mode)
         (LaTeX-mode . indent-bars-mode))
  :config
  (require 'indent-bars-ts)
  (setopt indent-bars-no-descend-lists t
          indent-bars-treesit-support t
          indent-bars-width-frac 0.3))

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
  (when my/is-termux
    (setopt doom-modeline-icon nil))
  :config
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-irc nil)
  (setq doom-modeline-buffer-state-icon nil)
  (doom-modeline-mode 1))

(defun my/tab-bar-mode-line--format ()
  (unless (derived-mode-p 'company-box-mode)
    (cl-letf (((symbol-function 'window-pixel-width)
               'frame-pixel-width)
              ((symbol-function 'window-margins)
               (lambda (&rest _)
                 (list nil))))
      (let ((doom-modeline-window-width-limit nil)
            (doom-modeline--limited-width-p nil))
        (format-mode-line
         '("%e"
           (:eval
            (doom-modeline-format--main))))))))

(defun my/hide-mode-line-if-only-window ()
  (let* ((windows (window-list))
         (hide-mode-line-p (length= windows 1)))
    (dolist (win windows)
      (with-current-buffer (window-buffer win)
        (unless (eq hide-mode-line-p hide-mode-line-mode)
          (hide-mode-line-mode
           (if hide-mode-line-p +1 -1)))))))

(define-minor-mode my/tab-bar-mode-line-mode
  "Use tab-bar as mode line mode."
  :global t
  (if my/tab-bar-mode-line-mode
      (progn
        (tab-bar-mode +1)
        (setq tab-bar-format '(my/tab-bar-mode-line--format))
        (set-face-attribute 'tab-bar nil :inherit 'mode-line)
        (add-hook 'window-configuration-change-hook #'my/hide-mode-line-if-only-window)

        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (doom-modeline-set-modeline 'minimal)))
        (doom-modeline-set-modeline 'minimal 'default)

        (dolist (frame (frame-list))
          (with-selected-frame frame
            (my/hide-mode-line-if-only-window))
          (when-let (cb-frame (company-box--get-frame frame))
            (set-frame-parameter cb-frame 'tab-bar-lines 0)))
        (setenv "POLYBAR_BOTTOM" "false")
        (when (fboundp #'my/exwm-run-polybar)
          (my/exwm-run-polybar)))
    (tab-bar-mode -1)
    (setq tab-bar-format
          '(tab-bar-format-history tab-bar-format-tabs tab-bar-separator tab-bar-format-add-tab))
    (set-face-attribute 'tab-bar nil :inherit 'default)
    (remove-hook 'window-configuration-change-hook #'my/hide-mode-line-if-only-window)
    (global-hide-mode-line-mode -1)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (doom-modeline-set-modeline 'main)))
    (doom-modeline-set-modeline 'main 'default)
    (setenv "POLYBAR_BOTTOM" "true")
    (when (fboundp #'my/exwm-run-polybar)
      (my/exwm-run-polybar))))

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
   "b" 'persp-switch-to-buffer
   "x" 'persp-switch-to-buffer*
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
  :if (not (or my/is-termux my/remote-server))
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
  (setq lsp-volar-take-over-mode nil)
  (add-to-list 'lsp-language-id-configuration '(svelte-mode . "svelte")))

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-delay 2)
  (setq lsp-ui-sideline-show-hover nil))

(use-package all-the-icons
  :straight t)

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

(setq my/lsp--vue-diagnostics-last-update (make-hash-table :test #'equal))

(defun my/lsp--on-diagnostics (fn workspace params)
  (if (equal (gethash 'vue-semantic-server lsp-clients)
             (lsp--workspace-client workspace))
      (progn
        (let* ((is-empty (seq-empty-p (gethash "diagnostics" params)))
               (uri (gethash "uri" params))
               (last-update (gethash uri my/lsp--vue-diagnostics-last-update))
               (current-update (time-convert nil #'integer)))
          (unless is-empty
            (puthash uri current-update my/lsp--vue-diagnostics-last-update))
          (when (or (not is-empty)
                    (not last-update)
                    (> (- current-update (or last-update 0)) 5))
            (funcall fn workspace params))))
    (funcall fn workspace params)))

(with-eval-after-load 'lsp
  (advice-add #'lsp--on-diagnostics :around #'my/lsp--on-diagnostics))

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

(use-package treesit-fold
  :straight (treesit-fold :type git :host github :repo "emacs-tree-sitter/treesit-fold")
  :commands (treesit-fold-mode))

(defun my/treesit-fold--get-nodes-to-fold ()
  (when-let*
      ((node (ignore-errors (treesit-buffer-root-node)))
       (patterns (seq-mapcat (lambda (fold-range) `((,(car fold-range)) @name))
                             (alist-get major-mode treesit-fold-range-alist)))
       (query (ignore-errors
                (treesit-query-compile (treesit-node-language node)
                                       patterns)))
       (nodes-to-fold (treesit-query-capture node query))
       (mode-ranges (alist-get major-mode treesit-fold-range-alist))
       (nodes-to-fold
        (cl-remove-if (lambda (node)
                        (treesit-fold--non-foldable-node-p (cdr node) mode-ranges))
                      nodes-to-fold)))
    nodes-to-fold))

(defun my/treesit-fold-hide-children ()
  (interactive)
  (let* ((current-node (treesit-fold--foldable-node-at-pos))
         (all-nodes-to-fold (my/treesit-fold--get-nodes-to-fold))
         ;; Find foldable children of `current-node'
         (target-nodes-to-fold
          (seq-filter
           (lambda (n)
             (cl-block tree-iter
               (while n
                 (setq n (treesit-node-parent n))
                 (when (equal n current-node)
                   (cl-return-from tree-iter t)))))
           (mapcar #'cdr all-nodes-to-fold))))
    (dolist (node target-nodes-to-fold)
      (treesit-fold-close node))))

(defun my/evil-fold-hide-level ()
  (interactive)
  (cond
   (hs-minor-mode (hs-hide-level))
   (treesit-fold-mode (my/treesit-fold-hide-children))))

(with-eval-after-load 'treesit-fold
  (general-define-key
   :states '(normal)
   "ze" #'my/evil-fold-hide-level)
  (keymap-unset evil-motion-state-map "z e" t))

(use-package combobulate
  :straight (:host github :repo "mickeynp/combobulate")
  :commands (combobulate))

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
                 (or hs-minor-mode treesit-fold-mode outline-minor-mode))
        (evil-toggle-fold)
        t)
      (indent-for-tab-command)))

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el")
  :commands (copilot-mode)
  :disabled t
  :init
  (add-hook 'emacs-startup-hook
            (lambda ()
              (add-hook 'prog-mode-hook #'copilot-mode)))
  :config
  (push '(copilot) warning-suppress-types)
  (setq copilot-node-executable "/home/pavel/.guix-extra-profiles/dev/dev/bin/node")
  (general-define-key
   :keymaps 'company-active-map
   "<backtab>" #'my/copilot-tab)
  (general-define-key
   :keymaps 'copilot-mode-map
   "<tab>" #'my/copilot-tab
   "M-j" #'copilot-accept-completion-by-line
   "M-l" #'copilot-accept-completion-by-word))

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
  (with-eval-after-load 'editorconfig
    (push
     'standard-indent
     (alist-get 'web-mode editorconfig-indentation-alist)))
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
  :mode (rx (| ".asciidoc") eos)
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
  ;; (cons (rx (| "srt" "vtt" "ass") eos) #'subed-mode)
  :mode ("\\(?:ass\\|\\(?:sr\\|vt\\)t\\)\\'" . subed-mode)
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
                           '("en-GB" "ru-RU" "de-DE")))
  (lsp-workspace-restart (lsp--read-workspace)))

(defun my/ltex-need-p ()
  (let ((file-name (buffer-file-name)))
    (cond
     (my/is-termux nil)
     ((null file-name) nil)
     ((string-match-p (rx "/home/pavel/" (+ alnum) ".org" eos) file-name) nil)
     ((string-match-p (rx (literal org-directory) "/" (or "roam" "inbox-notes" "literature-notes" "journal")) file-name) t)
     ((string-match-p (rx (literal org-directory)) file-name) nil)
     ((string-match-p (rx (literal (expand-file-name user-emacs-directory))) file-name) nil)
     (t t))))

(defun my/text-mode-lsp-maybe ()
  (when (my/ltex-need-p)
    (lsp)))

;; (add-hook 'text-mode-hook #'my/text-mode-lsp-maybe)

(use-package langtool
  :straight t
  :commands (langtool-check)
  :config
  (setq langtool-language-tool-server-jar "/home/pavel/bin/LanguageTool-6.4/languagetool-server.jar")
  (setq langtool-mother-tongue "ru")
  (setq langtool-default-language "ru-RU"))

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
  :commands (reverso)
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
  :defer t
  :init
  (defun my/flycheck-package-setup ()
    (require 'flycheck-package)
    (flycheck-package-setup)
    (remove-hook 'emacs-lisp-mode-hook #'my/flycheck-package-setup))
  (add-hook 'emacs-lisp-mode-hook #'my/flycheck-package-setup))

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

(setq org-directory (expand-file-name "~/30-39 Life/32 org-mode"))

(use-package org
  :straight (:type built-in)
  :if (not my/remote-server)
  :defer t
  :init
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

(with-eval-after-load 'org
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

(with-eval-after-load 'epa
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

(defun my/outline-prev-or-up-heading ()
  (interactive)
  (if (outline-on-heading-p)
      (outline-up-heading 1)
    (outline-previous-visible-heading 1)))

(with-eval-after-load 'org
  (general-define-key
   :keymaps 'org-mode-map
   "C-c d" #'org-decrypt-entry
   "C-c e" #'org-encrypt-entry
   "M-p" #'org-latex-preview
   "M-o" #'org-redisplay-inline-images)

  (general-define-key
   :keymaps 'org-mode-map
   :states '(normal emacs)
   "L" #'org-shiftright
   "H" #'org-shiftleft
   "S-<next>" #'org-next-visible-heading
   "S-<prior>" #'org-previous-visible-heading
   "M-0" #'org-next-visible-heading
   "M-9" #'org-previous-visible-heading
   "C-0" #'org-forward-heading-same-level
   "C-9" #'org-backward-heading-same-level
   "(" #'my/outline-prev-or-up-heading
   "M-]" #'org-babel-next-src-block
   "M-[" #'org-babel-previous-src-block)

  (general-define-key
   :keymaps 'org-agenda-mode-map
   "M-]" #'org-agenda-later
   "M-[" #'org-agenda-earlier)

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

(with-eval-after-load 'org
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
            '("projects" "misc" "learning")
            (mapcar (lambda (f)
                      (directory-files (concat org-directory "/" f) t (rx ".org" eos))))
            (apply #'append)
            (mapcar (lambda (file)
                      (string-replace (concat org-directory "/") "" file)))
            (append
             '("inbox.org" "contacts.org" "recurring.org")))))
    (find-file
     (concat org-directory "/"
             (completing-read "Org file: " files)))))

(defun my/enable-org-latex ()
  (interactive)
  (customize-set-variable 'org-highlight-latex-and-related '(native))
  (add-hook 'org-mode-hook (lambda () (yas-activate-extra-mode 'LaTeX-mode)))
  (sp-local-pair 'org-mode "$" "$")
  (sp--remove-local-pair "'"))

(with-eval-after-load 'org
  (setq my/org-latex-scale 1.75)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale my/org-latex-scale)))

(with-eval-after-load 'org
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

(unless (display-graphic-p)
  (add-hook 'org-mode-hook #'org-indent-mode))

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

(use-package org-appear
  :after (org)
  :straight t)

(use-package org-fragtog
  :after (org)
  :straight t)

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

(defun my/org-load-jupyter ()
  (interactive)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((jupyter . t)))
  (my/jupyter-refesh-langs))

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
  :mode ("\\.http\\'" . restclient-mode)
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

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   `((emacs-lisp . t)
     (python . t)
     (sql . t)
     (sqlite . t)
     ;; (typescript .t)
     (hy . t)
     (shell . t)
     (plantuml . t)
     (octave . t)
     (sparql . t)
     (gnuplot . t)))

  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images))

(with-eval-after-load 'ob-jupyter
  (org-babel-jupyter-override-src-block "python")
  (org-babel-jupyter-override-src-block "hy"))

(add-hook 'org-src-mode-hook
          (lambda ()
            ;; (hs-minor-mode -1)
            ;; (electric-indent-local-mode -1)
            ;; (rainbow-delimiters-mode -1)
            ;; (highlight-indent-guides-mode -1)
            ))

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

(defun my/org-babel-execute-marked (&optional arg)
  (interactive "P")
  (let (markers)
    (org-element-map (org-element-parse-buffer) 'src-block
      (lambda (elem)
        (let ((params (org-element-property :parameters elem)))
          (when (and params
                     (string-match-p (rx "startup t") params))
            (let ((m (make-marker)))
              (set-marker m (org-element-property :begin elem))
              (set-marker-insertion-type m t)
              (push m markers))))))
    (setq markers (nreverse markers))
    (when arg
      (setq markers
            (seq-filter
             (lambda (m) (> (marker-position m) (point)))
             markers)))
    (dolist (m markers)
      (goto-char m)
      (ignore-errors
        (org-babel-execute-src-block)))))

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
  :commands (hide-mode-line-mode))

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
  :straight t
  :config
  (setq org-attach-screenshot-auto-refresh 'never))

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

(use-package phscroll
  :straight (:host github :repo "misohena/phscroll")
  :commands (org-phscroll-mode)
  :config
  (with-eval-after-load 'org
    (require 'org-phscroll)
    (org-phscroll-deactivate)))

(defun my/update-org-agenda ()
  (interactive)
  (let ((project-files
         (when (file-directory-p (concat org-directory "/projects"))
           (thread-last "/projects"
                        (concat org-directory)
                        (directory-files)
                        (mapcar (lambda (f)
                                  (concat
                                   org-directory "/projects/" f)))
                        (seq-filter (lambda (f)
                                      (not (file-directory-p f))))))))
    (setq org-agenda-files
          (seq-filter #'file-exists-p
                      (append
                       project-files
                       (mapcar (lambda (f)
                                 (concat org-directory "/" f))
                               '("inbox.org"
                                 "misc/habit.org"
                                 "contacts.org")))))
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

(setq org-roam-directory (concat org-directory "/roam"))
(with-eval-after-load 'org
  (require 'seq)
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
    (my-leader-def "ol" #'org-clock-agg))
  :config
  (setq org-clock-agg-node-format
    "%-%(+ title-width)t %20c %8z %s/%S")
  (setq org-clock-agg-node-title-width-delta 47)
  (push
   (cons "Agenda+Archive"
         (append
          (org-agenda-files)
          (thread-last "/projects/archive"
                       (concat org-directory)
                       (directory-files)
                       (mapcar (lambda (f)
                                 (concat
                                  org-directory "/projects/archive/" f)))
                       (seq-filter (lambda (f)
                                     (not (file-directory-p f)))))))
   org-clock-agg-files-preset))

(with-eval-after-load 'org
  (setq org-clock-persist 'clock)
  (org-clock-persistence-insinuate))

(with-eval-after-load 'org
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

(defun my/org-clock-recent ()
  (interactive)
  (let* ((entries (org-ql-query
                    :select #'element-with-markers
                    :from (org-agenda-files)
                    :where '(clocked :from -1)))
         (entries-data (mapcar (lambda (e)
                                 (cons (org-element-property :raw-value e) e))
                               entries)))
    (unless entries
      (user-error "No recently clocked entries!"))
    entries-data
    (let* ((entry (alist-get (completing-read "Entry: " entries-data)
                             entries-data nil nil #'equal))
           (marker (org-element-property :org-marker entry)))
      (pop-to-buffer-same-window (marker-buffer marker))
      (goto-char marker))))

(with-eval-after-load 'org
  (my-leader-def
    :keymaps 'org-mode-map
    :infix "SPC"
    "C" #'my/org-clock-recent))

(defun my/org-fix-task-kind ()
  (interactive)
  (let ((entries (org-ql-query
                   :select #'element-with-markers
                   :from (current-buffer)
                   :where '(and (olp "Tasks")
                                (not (property "TASK_KIND"))
                                (clocked)))))
    (org-fold-show-all)
    (dolist (entry entries)
      (let ((marker (org-element-property :org-marker entry)))
        (org-with-point-at marker
          (let ((value (org-read-property-value "TASK_KIND")))
            (org-set-property "TASK_KIND" value)))))))

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
  :config
  (setq org-ql-ask-unsafe-queries nil)
  :init
  ;; See https://github.com/alphapapa/org-ql/pull/237
  (setq org-ql-regexp-part-ts-time
        (rx " " (repeat 1 2 digit) ":" (repeat 2 digit)
            (optional "-" (repeat 1 2 digit) ":" (repeat 2 digit))))
  (my-leader-def
    :infix "o"
    "v" #'org-ql-view
    "q" #'org-ql-search))

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
         (vertico-sort-function nil)
         (categories (completing-read-multiple
                      "Categories: "
                      '("TEACH" "EDU" "JOB" "LIFE" "COMP"))))
    (org-ql-search (org-agenda-files)
      `(and (todo)
            ,@(unless (seq-empty-p categories)
                `((category ,@categories))))
      :sort '(priority todo deadline)
      :super-groups '((:auto-outline-path-file t)))))

(defun my/org-ql-clocked-today ()
  (interactive)
  (let ((today (format-time-string
                "%Y-%m-%d"
                (days-to-time
                 (- (org-today) (time-to-days 0))))))
    (org-ql-search (org-agenda-files) `(clocked :from ,today)
      :title "Clocked today"
      :sort '(todo priority date)
      :super-groups '((:auto-outline-path-file t)
                      (:auto-todo t)))))

(defun my/org-ql-closed-today ()
  (interactive)
  (let ((today (format-time-string
                "%Y-%m-%d"
                (days-to-time
                 (- (org-today) (time-to-days 0))))))
    (org-ql-search (org-agenda-files) `(closed :from ,today)
      :title "Closed today"
      :sort '(todo priority date)
      :super-groups '((:auto-outline-path-file t)
                      (:auto-todo t)))))

(setq org-ql-views
      (list
       (cons "Overview: All TODO" #'my/org-ql-all-todo)
       (cons "Review: Stale tasks"
             (list :buffers-files #'org-agenda-files
                   :query '(and (todo)
                                (not (tags "nots"))
                                (not (ts :from -14)))
                   :title "Review: Stale tasks"
                   :sort '(todo priority date)
                   :super-groups '((:auto-outline-path-file t))))
       (cons "Review: Unclocked tasks"
             (list :buffers-files #'org-agenda-files
                   :query '(and (done)
                                (ts :from -14)
                                (not (clocked))
                                (not (tags "nots")))
                   :title "Review: Unclocked tasks"
                   :sort '(todo priority date)
                   :super-groups '((:auto-outline-path-file t))))
       (cons "Review: Recently timestamped" #'my/org-ql-view-recent-items)
       (cons "Review: Clocked today" #'my/org-ql-clocked-today)
       (cons "Review: Closed today" #'my/org-ql-closed-today)
       (cons "Fix: tasks without TASK_KIND"
             (lambda ()
               (interactive)
               (org-ql-search (current-buffer)
                 '(and (olp "Tasks")
                       (not (property "TASK_KIND"))
                       (clocked))
                 :super-groups '((:auto-outline-path-file t)))))))

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

(use-package org-yaap
  :straight (org-yaap :type git :host gitlab :repo "SqrtMinusOne/org-yaap")
  :after (org)
  :if (not my/nested-emacs)
  :disabled t
  :config
  (org-yaap-mode 1)
  (setq org-yaap-alert-before '(10 1))
  (setq org-yaap-alert-title "PROXIMITY ALERT")
  (setq org-yaap-todo-keywords-only '("FUTURE")))

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
  (when-let* ((files (org-agenda-files))
              (items
               (org-ql-query
                 :select 'element
                 :from files
                 :where `(and
                          (todo "FUTURE")
                          (ts-active :from ,(format-time-string "%Y-%m-%d %H:%M")
                                     :to ,(format-time-string
                                           "%Y-%m-%d"
                                           (time-add
                                            (current-time)
                                            (* 60 60 24)))
                                     :with-time t))
                 :order-by 'date)))
    (let (scheduled-keys)
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
      (my/org-alert-cleanup scheduled-keys))))

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

(defun my/org--headings-in-outline ()
  (org-ql-query
    :select (lambda () (propertize
                        (substring-no-properties (org-get-heading t t t))
                        'marker (copy-marker (point))))
    :from (append
           (list (buffer-file-name))
           (let ((archive
                  (concat (file-name-directory (buffer-file-name))
                          "archive/"
                          (file-name-nondirectory (buffer-file-name)))))
             (when (file-exists-p archive)
               (list archive))))
    :where `(and (outline-path ,@(org-get-outline-path))
                 (level ,(org-current-level)))))

(defun my/org--heading-strip (heading)
  (thread-last
    heading
    (substring-no-properties)
    (replace-regexp-in-string (rx (| "(" "[") (+ nonl) (| "]" ")")) "")
    (replace-regexp-in-string (rx " " (+ (or digit "."))) " ")
    (replace-regexp-in-string (rx (+ " ")) " ")
    (string-trim)))

(defun my/org--headings-group-seq (headings)
  (thread-last
    headings
    (seq-group-by #'my/org--heading-strip)
    (seq-sort-by #'car #'string-lessp)
    (mapcar (lambda (group)
              (cons (car group)
                    (seq-sort-by
                     (lambda (heading)
                       (save-match-data
                         (or
                          (and (string-match (rx (group (+ digit)))
                                             heading)
                               (string-to-number (match-string 1 heading)))
                          -1)))
                     #'<
                     (cdr group)))))))

(defun my/org-headings-seq ()
  (interactive)
  (let* ((headings (my/org--headings-in-outline))
         (headings-seq (my/org--headings-group-seq headings))
         (buffer (generate-new-buffer "*Sequential Headings in Outline*")))
    (with-current-buffer buffer
      (outline-mode)
      (setq-local widget-push-button-prefix "")
      (setq-local widget-push-button-suffix "")
      (dolist (group headings-seq)
        (insert (format "* %s\n" (car group)))
        (dolist (heading (cdr group))
          (widget-create 'push-button
                         :marker (get-text-property 0 'marker heading)
                         :notify (lambda (widget &rest ignore)
                                   (let ((marker (widget-get widget :marker)))
                                     (pop-to-buffer (marker-buffer marker))
                                     (goto-char marker)))
                         (concat "** " (substring-no-properties heading)))
          (insert "\n")))
      (widget-setup)
      (setq buffer-read-only t)
      (goto-char (point-min)))
    (pop-to-buffer buffer)))

(defun my/org-heading-seq-insert ()
  (interactive)
  (let* ((headings (my/org--headings-in-outline))
         (headings-seq (my/org--headings-group-seq headings))
         (heading (completing-read "Headings: " headings-seq))
         (last-number
          (thread-last headings-seq
                       (assoc heading)
                       (cdr)
                       (mapcar (lambda (x)
                                 (save-match-data
                                   (or
                                    (when (string-match (rx (group (+ digit)))
                                                        x)
                                      (string-to-number (match-string 1 x)))
                                    1))))
                       (seq-max)
                       (1+))))
    (org-insert-heading '(4))
    (insert (format "FUTURE %s %s" heading last-number))))

(defun my/org-archive--get-file ()
  "Get an archive version of the file."
  (let ((archive-file
         (concat
          (file-name-directory (buffer-file-name))
          "archive/" (file-name-nondirectory (buffer-file-name)))))
    (unless (file-exists-p archive-file)
      (make-empty-file archive-file))
    archive-file))

(defun my/org-refile--assert-path-exists (refile-path)
  (cl-assert (equal org-refile-use-outline-path 'file))
  (let* ((parts (string-split refile-path "/"))
         (tbl (mapcar
               (lambda (x)
                 (cons (concat (car x) "/") (cdr x)))
               org-refile-target-table)))
    (cl-loop for i from 1
             for part in (cdr parts)
             for target = (org-refile--get-location
                           (string-join (seq-take parts (1+ i)) "/")
                           tbl)
             unless target
             do (let ((parent-target
                       (org-refile--get-location
                        (string-join (seq-take parts i) "/")
                        tbl)))
                  (push (org-refile-new-child parent-target part) tbl)))))

(defun my/org-archive-refile ()
  (interactive)
  (let* ((org-refile-targets `((,(my/org-archive--get-file) . (:maxlevel . 6))))
         (org-refile-target-table (org-refile-get-targets))
         (org-refile-history nil)
         (org-refile-use-outline-path 'file)
         (org-refile-allow-creating-parent-nodes t)
         (org-outline-path-complete-in-steps nil)
         (refile-path (string-join
                       (append
                        (list (file-name-nondirectory
                               (buffer-file-name)))
                        (org-get-outline-path nil t))
                       "/")))
    ;; The path is already known
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) refile-path)))
      (my/org-refile--assert-path-exists refile-path)
      (org-refile))))

(defun my/org-archive-refile-all (days)
  (interactive (list (read-number "Days: " 60)))
  (let ((records (org-ql-query
                   :select #'element-with-markers
                   :from (current-buffer)
                   :where `(and (ts :to ,(- days)) (done)))))
    (when (y-or-n-p (format "Archive %d records? " (length records)))
      (dolist (record records)
        (let ((marker (org-element-property :org-marker record)))
          (org-with-point-at marker
            (my/org-archive-refile)))))))

(my-leader-def
  :infix "o"
  "" '(:which-key "org-mode")
  "c" 'org-capture
  "a" 'org-agenda
  "o" #'my/org-file-open
  "v" #'org-ql-view
  "q" #'org-ql-search)

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
         (vertico-sort-function nil))
    (mapconcat
     #'identity
     (completing-read-multiple
      "How do you feel: "
      my/mood-list)
     " ")))

(defun my/set-journal-header ()
  (org-set-property "Emacs" emacs-version)
  (org-set-property "Hostname" (my/system-name))
  (org-journal-tags-prop-apply-delta :add (list (format "host.%s" (my/system-name))))
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

(defun my/org-journal-decrypt ()
  "Decrypt the current org journal file."
  (interactive)
  (org-journal-tags--ensure-decrypted))

(use-package citar
  :straight t
  :init
  (my-leader-def "fB" #'citar-open)
  :commands (citar-open citar-insert-citation)
  :config
  (setq
   org-cite-global-bibliography '("~/30-39 Life/32 org-mode/library.bib")
   org-cite-insert-processor 'citar
   org-cite-follow-processor 'citar
   org-cite-activate-processor 'citar
   citar-bibliography org-cite-global-bibliography)
  (setq org-cite-export-processors
        '((latex bibtex "numeric")))
  (setq citar-library-paths
        '("~/30-39 Life/33 Library/33.01 Documents/"))
  (add-hook 'latex-mode #'citar-capf-setup)
  (add-hook 'org-mode #'citar-capf-setup))

(use-package citar-embark
  :after (citar embark)
  :straight t
  :config
  (citar-embark-mode))

(use-package org-ref
  :straight (:files (:defaults "citeproc" (:exclude "*helm*")))
  :if (not my/remote-server)
  :commands (org-ref-insert-link-hydra/body
             org-ref-bibtex-hydra/body)
  :init
  (setq bibtex-dialect 'biblatex)
  (add-hook 'bibtex-mode 'smartparens-mode)
  :after (org)
  :config
  (general-define-key
   :keymaps 'org-mode-map
   "C-c l" #'org-ref-insert-link-hydra/body)
  (general-define-key
   :keymaps 'bibtex-mode-map
   "M-RET" 'org-ref-bibtex-hydra/body)
  (setq org-ref-insert-cite-function
        (lambda ()
          (call-interactively #'citar-insert-citation))))

(use-package emacsql-sqlite
  :defer t
  :if (not my/remote-server)
  :straight (:type built-in))

(use-package org-roam
  :straight (:host github :repo "org-roam/org-roam"
                   :files (:defaults "extensions/*.el"))
  :if (and
       (not my/remote-server)
       (file-directory-p org-roam-directory))
  :after org
  :init
  (setq org-roam-file-extensions '("org"))
  (setq org-roam-v2-ack t)
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  :config
  (org-roam-setup)
  (require 'org-roam-protocol))

(setq org-roam-capture-templates
      `(("d" "default" plain "%?"
         :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
         :unnarrowed t)
        ("f" "fleeting" plain "%?"
         :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: :fleeting:\n")
         :unnarrowed t)
        ("e" "encrypted" plain "%?"
         :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org.gpg" "#+title: ${title}\n")
         :unnarrowed t)))

(use-package org-roam-ql
  :straight t
  :after (org-roam)
  :config
  (general-define-key
   :states '(normal visual)
   :keymaps '(org-roam-ql-mode-map)
   "s" #'org-roam-ql-buffer-dispatch))

(defun my/org-roam-node-find-permanent (&optional other-window)
  (interactive current-prefix-arg)
  (org-roam-node-find
   other-window
   nil
   (lambda (node)
     (not
      (seq-contains-p
       "fleeting"
       (org-roam-node-tags node))))))

(defun my/org-roam-node-insert-permanent ()
  (interactive)
  (org-roam-node-insert
   (lambda (node)
     (not
      (seq-contains-p
       (org-roam-node-tags node)
       "fleeting")))))

(defun my/org-roam-ql-fleeting ()
  (interactive)
  (org-roam-ql-search
   '(tags "fleeting")
   "Fleeting notes"))

(with-eval-after-load 'org-roam
  (my-leader-def
    :infix "or"
    "" '(:which-key "org-roam")
    "i" #'my/org-roam-node-insert-permanent
    "r" #'my/org-roam-node-find-permanent
    "g" #'org-roam-graph
    "c" #'org-roam-capture
    "b" #'org-roam-buffer-toggle
    "q" #'org-roam-ql-search
    "f" #'my/org-roam-ql-fleeting)
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
    "t" #'org-roam-tag-add
    "T" #'org-roam-tag-remove
    "s" #'org-roam-db-autosync-mode
    "a" #'org-roam-alias-add)
  (general-define-key
   :keymap 'org-mode-map
   "C-c i" #'my/org-roam-node-insert-permanent
   "C-c I" #'org-roam-node-insert))

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

(defun my/org-roam-extract-links ()
  (interactive)
  (let ((buffer (generate-new-buffer "*roam-links*"))
        elems)
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (elem)
        (when (string-equal (org-element-property :type elem) "id")
          (push elem elems))))
    (with-current-buffer buffer
      (cl-loop for elem in elems
               for file-name =
               (file-name-nondirectory
                (caar
                 (org-roam-db-query
                  [:select [file]
                           :from nodes
                           :where (= id $s1)]
                  (org-element-property :path elem))))
               do (insert file-name "\n")))
    (switch-to-buffer buffer)))

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

(defun my/org-roam-node-setup ()
  (setq-local org-hide-emphasis-markers t)
  (org-appear-mode 1)
  (when (display-graphic-p)
    (org-fragtog-mode 1)
    (org-latex-preview '(16))))

(with-eval-after-load 'org
  (add-hook 'org-roam-find-file-hook 'my/org-roam-node-setup))

(setq my/git-diff-status
      '(("A" . added)
        ("C" . copied)
        ("D" . deleted)
        ("M" . modified)
        ("R" . renamed)
        ("R100" . moved)
        ("T" . type-changed)
        ("U" . unmerged)))

(defun my/get-files-status (rev)
  (let ((files (shell-command-to-string (concat "git diff --name-status " rev))))
    (mapcar
     (lambda (file)
       (let ((elems (split-string file "\t")))
         (cons
          (cdr (assoc (car elems) my/git-diff-status))
          (car (last elems)))))
     (split-string files "\n" t))))

(defun my/org-changed-files-since-date (date)
  (let ((default-directory org-directory))
    (my/get-files-status (format "@{%s}" date))))

(defun my/org-review--org-roam-get-changes (date)
  (let ((changes (my/org-changed-files-since-date date))
        (nodes (org-roam-node-list))
        (nodes-by-file (make-hash-table :test #'equal)))
    (cl-loop for node in nodes
             for file = (org-roam-node-file node)
             do (puthash file node nodes-by-file))
    (let* ((changed-nodes
            (thread-last
              changes
              (mapcar (lambda (c)
                        (cons (car c)
                              (gethash
                               (concat org-directory "/" (cdr c))
                               nodes-by-file))))
              (seq-filter #'cdr)))
           (changed-inbox
            (thread-last
              changes
              (seq-filter
               (lambda (file) (string-match-p (rx bos "inbox-notes") (cdr file))))))
           (changed-fleeting
            (thread-last
              changed-nodes
              (seq-filter (lambda (c)
                            (seq-contains-p (org-roam-node-tags (cdr c))
                                            "fleeting")))
              (seq-sort-by (lambda (c) (concat (symbol-name (car c))
                                               (org-roam-node-title (cdr c))))
                           #'string-lessp)))
           (changed-permanent
            (thread-last
              changed-nodes
              (seq-filter (lambda (c)
                            (not (seq-contains-p (org-roam-node-tags (cdr c))
                                                 "fleeting"))))
              (seq-sort-by (lambda (c) (concat (symbol-name (car c))
                                               (org-roam-node-title (cdr c))))
                           #'string-lessp))))
      (list
       changed-inbox
       changed-fleeting
       changed-permanent))))

(defun my/org-review-org-roam-format (date)
  (let* ((data (my/org-review--org-roam-get-changes date))
         (changed-inbox (nth 0 data))
         (changed-fleeting (nth 1 data))
         (changed-permanent (nth 2 data)))
    (concat
     "Changes in inbox:\n"
     (thread-last
       changed-inbox
       (mapcar (lambda (change)
                 (format "- %s :: %s\n"
                         (cond
                          ((or (member (car change) '(deleted moved))
                               (string-match-p "figured-out" (cdr change)))
                           "Processed")
                          (t (capitalize (symbol-name (car change)))))
                         (cdr change))))
       (apply #'concat))
     "\nChanges in fleeting notes:\n"
     (thread-last
       changed-fleeting
       (mapcar (lambda (c)
                 (format "- %s :: [[id:%s][%s]]\n"
                         (capitalize (symbol-name (car c)))
                         (org-roam-node-id (cdr c))
                         (org-roam-node-title (cdr c)))))
       (apply #'concat))
     "\nChanges in permanent notes:\n"
     (thread-last
       changed-permanent
       (mapcar (lambda (c)
                 (format "- %s :: [[id:%s][%s]]\n"
                         (capitalize (symbol-name (car c)))
                         (org-roam-node-id (cdr c))
                         (org-roam-node-title (cdr c)))))
       (apply #'concat)))))

(defun my/org-review-get-last-review-date (kind)
  (let* ((start-of-day (- (time-convert nil #'integer)
                          (% (time-convert nil #'integer)
                             (* 24 60 60))))
         (query-res (org-journal-tags-query
                     :tag-names (list (format "review.%s" kind))
                     :start-date (pcase kind
                                   ('weekly
                                    (- start-of-day
                                       (* 21 24 60 60)))
                                   ('zk
                                    (- start-of-day
                                       (* 45 24 60 60)))
                                   (_ (error "Unsupported kind: %s" kind)))
                     :location 'section
                     :order 'descending)))
    (if query-res
        (org-journal-tag-reference-date (car query-res))
      (pcase kind
        ('weekly (- start-of-day (* 7 24 60 60)))
        ('zk (- start-of-day (* 45 24 60 60)))))))

(defun my/org-review-set-weekly-record ()
  (save-excursion
    (let ((last-review-date (my/org-review-get-last-review-date 'weekly)))
      (org-journal-tags-prop-apply-delta :add '("review.weekly"))
      (insert "Weekly Review")
      (goto-char (point-max))

      (insert "Last review date: "
              (format-time-string
               "[%Y-%m-%d]"
               (seconds-to-time last-review-date)))
      (insert "

Review checklist (/delete this/):
- [ ] Clear email inbox
- [ ] Reconcile ledger
- [ ] Clear [[file:~/Downloads][downloads]] and [[file:~/00-Scratch][scratch]] folders
- [ ] Process [[file:~/30-39 Life/35 Photos/35.00 Inbox/][photo inbox]]
- [ ] Process [[file:../inbox.org][inbox]]
- [ ] Create [[file:../recurring.org][recurring tasks]] for next week
- [ ] Check agenda (-1 / +2 weeks): priorities, deadlines
- [ ] Check TODOs: priorities, deadlines
  - [[org-ql-search:todo%3A?buffers-files=%22org-agenda-files%22&super-groups=%28%28%3Aauto-outline-path-file%20t%29%29&sort=%28priority%20todo%20deadline%29][org-ql-search: All TODOs]]
  - [[org-ql-search:(and (todo) (not (tags \"nots\")) (not (ts :from -14)) (not (todo \"MAYBE\")))?buffers-files=%22org-agenda-files%22&super-groups=%28%28%3Aauto-outline-path-file%20t%29%29&sort=%28priority%20todo%20deadline%29][org-ql-search: Stale tasks]]
  - [[org-ql-search:todo%3AWAIT?buffers-files=%22org-agenda-files%22&super-groups=%28%28%3Aauto-outline-path-file%20t%29%29&sort=%28priority%20todo%20deadline%29][org-ql-search: WAIT]]
  - [[org-ql-search:todo%3AMAYBE?buffers-files=%22org-agenda-files%22&super-groups=%28%28%3Aauto-outline-path-file%20t%29%29&sort=%28priority%20todo%20deadline%29][org-ql-search: MAYBE]]
- [ ] Run auto-archiving
- [ ] Review journal records
")

      (insert "
*** Summary
TODO Write something, maybe? "))))

(defun my/org-review-weekly ()
  (interactive)
  (let ((org-journal-after-entry-create-hook
         `(,@org-journal-after-entry-create-hook
           my/org-review-set-weekly-record)))
    (org-journal-new-entry nil)
    (org-fold-show-subtree)))

(with-eval-after-load 'org-journal
  (my-leader-def "ojw" #'my/org-review-weekly))

(defun my/kill-messengers ()
  (interactive)
  (when (get-buffer telega-root-buffer-name)
    (telega-kill t))
  (call-process-shell-command "pkill -f rocketchat-desktop")
  (call-process-shell-command "pkill -f 'bwrap --args 36 element'")
  (call-process-shell-command "pkill -f element-desktop"))

(defun my/org-review-set-daily-record ()
  (let* ((today (format-time-string
                 "%Y-%m-%d"
                 (days-to-time
                  (- (org-today) (time-to-days 0)))))
         (roam-changes (my/org-review--org-roam-get-changes today)))
    (save-excursion
      (org-journal-tags-prop-apply-delta :add '("review.daily"))
      (insert "Daily Review")
      (goto-char (point-max))

      (insert "
Maintenance checklist (/delete this/):
- [ ] [[elisp:(my/kill-messengers)][Close all messengers]]
- [ ] Process [[file:../inbox.org][inbox]]
- [ ] Check if clocked tasks are properly annotated
  - [[elisp:(my/org-ql-clocked-today)][Tasks clocked today]]
  - [[elisp:(my/org-ql-closed-today)][Tasks closed today]]
- [ ] Check agenda for the current week

/Remember, all of the following headers are optional./

*** Happened today
Happened to me:
- /Anything interesting?/
Happened to the world:
- /Anything important?/

*** New ideas
/Write them down in org-roam with the \"fleeting\" tag; leave links here. Perhaps note what sparked that idea?/
"
              (thread-last
                (nth 1 roam-changes)
                (seq-filter (lambda (c) (eq 'added (car c))))
                (mapcar (lambda (c)
                          (format "- [[id:%s][%s]]\n"
                                  (org-roam-node-id (cdr c))
                                  (org-roam-node-title (cdr c)))))
                (apply #'concat))
              "
*** Interactions today
/Any meaninginful interactions, conflicts or tensions?/

*** Emotions today
/How did I feel?/
"))))

(defun my/org-review-daily ()
  (interactive)
  (let ((org-journal-after-entry-create-hook
         `(,@org-journal-after-entry-create-hook
           my/org-review-set-daily-record)))
    (org-journal-new-entry nil)
    (org-fold-show-subtree)))

(with-eval-after-load 'org-journal
  (my-leader-def "ojd" #'my/org-review-daily))

(defun my/org-review-org-roam-format-zk-before (date)
  (let* ((data (my/org-review--org-roam-get-changes date))
         (changed-inbox (nth 0 data))
         (changed-fleeting (nth 1 data))
         (changed-permanent (nth 2 data)))
    (concat
     (when changed-inbox
       (concat
        "Process these changes in inbox:\n"
        (thread-last
          changed-inbox
          (mapcar (lambda (change)
                    (format "- [ ] %s :: %s\n"
                            (cond
                             ((or (member (car change) '(deleted moved))
                                  (string-match-p "figured-out" (cdr change)))
                              "Processed")
                             (t (capitalize (symbol-name (car change)))))
                            (cdr change))))
          (apply #'concat))
        "\n"))
     (when changed-fleeting
       (concat
        "Process these fleeting notes:\n"
        (thread-last
          changed-fleeting
          (mapcar (lambda (c)
                    (format "- [ ] %s :: [[id:%s][%s]]\n"
                            (capitalize (symbol-name (car c)))
                            (org-roam-node-id (cdr c))
                            (org-roam-node-title (cdr c)))))
          (apply #'concat))
        "\n"))
     (when changed-permanent
       (concat
        "Check these changes in permanent notes:\n"
        (thread-last
          changed-permanent
          (mapcar (lambda (c)
                    (format "- [ ] %s :: [[id:%s][%s]]\n"
                            (capitalize (symbol-name (car c)))
                            (org-roam-node-id (cdr c))
                            (org-roam-node-title (cdr c)))))
          (apply #'concat)))))))

(defun my/org-review-org-roam-finish (date)
  (org-roam-db-sync)
  (save-excursion
    (org-back-to-heading)
    (replace-regexp
     (rx
      ":BEGIN_REVIEW:" (* anything) ":END_REVIEW:")
     (string-trim
      (my/org-review-org-roam-format date)))))

(defun my/org-review-set-zk-record ()
  (save-excursion
    (let ((last-review-date (my/org-review-get-last-review-date 'zk)))
      (org-journal-tags-prop-apply-delta :add '("review.zk"))
      (insert "Zettelkasten Review")
      (goto-char (point-max))

      (insert "Last review date: "
              (format-time-string
               "[%Y-%m-%d]"
               (seconds-to-time last-review-date)))

      (insert "\n\n:BEGIN_REVIEW:\n"
              "Process all the required categories in this block, then execute \"Finish review\".\n\n"
              (string-trim
               (my/org-review-org-roam-format-zk-before last-review-date))
              "\n\n[[elisp:(my/org-review-org-roam-finish \""
              (format-time-string "%Y-%m-%d" last-review-date)
              "\")][Finish review]]"
              "\n:END_REVIEW:\n"))))

(defun my/org-review-zk ()
  (interactive)
  (let ((org-journal-after-entry-create-hook
         `(,@org-journal-after-entry-create-hook
           my/org-review-set-zk-record)))
    (org-journal-new-entry nil)
    (org-fold-show-subtree)))

(with-eval-after-load 'org-journal
  (my-leader-def "ojz" #'my/org-review-zk))

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
  :defer t
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

(use-package org-drill
  :straight t
  :commands (org-drill)
  :after (org))

(use-package ox-hugo
  :straight t
  :if (not my/remote-server)
  :after ox)

(use-package ox-ipynb
  :straight (:host github :repo "jkitchin/ox-ipynb")
  :if (not my/remote-server)
  :disabled t
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
(with-eval-after-load 'ox-latex
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
      (mapcar
       #'expand-file-name
       '("~/Emacs.org"
         "~/Desktop.org"
         "~/Console.org"
         "~/Guix.org"
         "~/Mail.org")))

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
   "M-<return>" #'my/dired-open-xdg))

(defun my/dired-home ()
  "Open dired at $HOME"
  (interactive)
  (dired (expand-file-name "~")))

(my-leader-def
  "ad" #'dired
  "aD" #'my/dired-bookmark-open)

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
  :config
  (dired-recent-mode)
  (general-define-key
   :keymaps 'dired-recent-mode-map
   "C-x C-d" nil))

(use-package all-the-icons-dired
  :straight t
  :disabled t
  :after (dired)
  :if (display-graphic-p)
  :hook (dired-mode . (lambda ()
                        (unless (string-match-p "/gnu/store" default-directory)
                          (all-the-icons-dired-mode)))))

(use-package nerd-icons-dired
  :straight t
  :after (dired)
  :hook (dired-mode . (lambda ()
                        (unless (or (file-remote-p default-directory)
                                    (string-match-p "/gnu/store" default-directory))
                          (nerd-icons-dired-mode))))
  :config
  (advice-add #'dired-create-empty-file :around #'nerd-icons-dired--refresh-advice))

(use-package dired-open
  :straight t
  :after (dired)
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

(defun my/dired-rsync--refresh ()
  (cl-loop for window being the windows
           do (with-current-buffer (window-buffer window)
                (when (derived-mode-p 'dired-mode)
                  (revert-buffer)))))

(use-package dired-rsync
  :straight t
  :after (dired)
  :config
  (add-to-list 'global-mode-string '(:eval dired-rsync-modeline-status))
  (add-hook 'dired-rsync-success-hook #'my/dired-rsync--refresh)
  (general-define-key
   :states '(normal)
   :keymaps '(dired-mode-map)
   "C" #'dired-rsync
   "gC" #'dired-rsync-transient
   "gd" #'dired-do-copy))

(use-package dired-rsync-transient
  :straight t
  :after (dired))

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

(defun my/dired-open-xdg ()
  "Try to run `xdg-open' to open the file under point."
  (interactive)
  (when (executable-find "xdg-open")
    (let ((file (ignore-errors (dired-get-file-for-visit))))
      (start-process "dired-open" nil
                     "xdg-open" (file-truename file)))))

(defun my/dired-do-async-shell-command (command &optional arg file-list)
  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg nil nil t)))
     (list
      ;; Want to give feedback whether this file or marked files are used:
      (dired-read-shell-command "& on %s: " current-prefix-arg files)
      current-prefix-arg
      files)))
  (start-process-shell-command
   "*Dired Command*" nil
   (dired-shell-stuff-it command file-list arg)))

(with-eval-after-load 'dired
  (general-define-key
   :states '(normal insert)
   :keymaps '(dired-mode-map)
   "&" #'my/dired-do-async-shell-command))

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
      (progn
        (setq telega-msg-save-dir
              (with-current-buffer (my/get-good-buffer 'dired-mode "Dired buffer: ")
                (dired-current-directory)))
        (telega-msg-save msg))
    (setq default-directory (expand-file-name "~"))
    (setq telega-msg-save-dir nil)
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
      (mastodon-toot-attach-media
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

(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

(with-eval-after-load 'tramp
  (setq tramp-remote-path
        (append tramp-remote-path
                '(tramp-own-remote-path))))

(when (or my/remote-server)
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
   "<home>" #'eshell-bol)

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
      (when my/is-termux
        (let ((inhibit-message t))
          (replace-string "\\[" "" nil (point-min) (point-max))
          (replace-string "\\]" "" nil (point-min) (point-max))))
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

(use-package eshell-atuin
  :straight (:host github :repo "SqrtMinusOne/eshell-atuin")
  :after eshell
  :config
  (eshell-atuin-mode)
  (setq eshell-atuin-search-fields '(time duration command))
  (setq eshell-atuin-history-format "%-160c %t + %d")
  (general-define-key
   :states '(normal insert)
   :keymaps 'eshell-mode-map
   "C-r" #'eshell-atuin-history))

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

(general-define-key
 :states '(normal)
 "`" #'my/eshell-dedicated
 "~" #'eshell)

(use-package eat
  :straight (:files ("*.el" ("term" "term/*.el") "*.texi"
                     "*.ti" ("terminfo/e" "terminfo/e/*")
                     ("terminfo/65" "terminfo/65/*")
                     ("integration" "integration/*")
                     (:exclude ".dir-locals.el" "*-tests.el")))
  :commands (eat eat-shell-mode)
  :config
  (setq eat-shell "/bin/bash"))

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

(unless (or my/remote-server)
  (let ((mail-file (expand-file-name "mail.el" user-emacs-directory)))
    (if (file-exists-p mail-file)
        (load-file mail-file)
      (message "Can't load mail.el"))))

(use-package gnus
  :straight t
  :init
  (my-leader-def "au" #'gnus)
  :commands (gnus)
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
  (setq mastodon-instance-url "https://mastodon.bsd.cafe")
  (setq mastodon-active-user "sqrtminusone")
  (my/persp-add-rule mastodon-mode 0 "mastodon")
  ;; Hide spoilers by default
  ;; (setq-default mastodon-toot--content-warning nil)
  (setq mastodon-media--avatar-height 40)
  (setq mastodon-tl--timeline-posts-count "40")
  (setq mastodon-tl--show-avatars t)
  (setq mastodon-tl--horiz-bar
        (make-string shr-max-width
                     (if (char-displayable-p ?―) ?― ?-)))
  ;; The default emojis take two characters for me
  (mapcar (lambda (item)
            (setf (alist-get (car item) mastodon-tl--symbols)
                  (cdr item)))
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
  :disabled t
  :after (mastodon)
  :config
  (mastodon-alt-tl-activate))

(use-package transient
  :straight t
  :defer t)

(unless (display-graphic-p)
  (defun image-transforms-p () nil)
  (setq image-types '(svg png gif tiff jpeg xpm xbm pbm)))

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
   "J" #'mastodon-tl-goto-next-item
   "K" #'mastodon-tl-goto-prev-item
   "M-j" #'mastodon-tl-next-tab-item
   "M-k" #'mastodon-tl-prev-tab-item
   "<tab>" #'mastodon-tl-next-tab-item
   "<backtab>" #'mastodon-tl-previous-tab-item
   "o" #'my/mastodon-toot
   "r" 'mastodon-tl-update
   "c" #'mastodon-tl-toggle-spoiler-text-in-toot
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
                          (not (mastodon-tl--is-reply toot)))
                      (or (not hide-boosts)
                          (not (alist-get 'reblog toot)))))
                   toots))
           (start-pos (point)))
      (mapc #'mastodon-tl--toot toots)
      (when mastodon-tl--display-media-p
        (save-excursion
          (mastodon-media--inline-images start-pos (point)))))))

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
     ("l" "Local" mastodon-tl-get-local-timeline)
     ("f" "Federated" mastodon-tl-get-federated-timeline)
     ("g" "One tag" mastodon-tl-get-tag-timeline)
     ("a" "Followed tags" mastodon-tl-followed-tags-timeline)
     ("s" "Some followed tags" mastodon-tl-some-followed-tags-timeline)]
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
     ("s" "Search query" mastodon-search-query)]
    ["Tags"
     :class transient-row
     ("aa" "Followed tags" mastodon-tl-list-followed-tags)
     ("af" "Follow tag" mastodon-tl-follow-tag)
     ("aF" "Unfollow tag" mastodon-tl-unfollow-tag)]
    ["Own profile"
     :class transient-row
     ("c" "Toot" mastodon-toot)
     ("o" "My profile" mastodon-profile-my-profile)
     ("u" "Update profile note" mastodon-profile-update-user-profile-note)
     ("f" "Favourites" mastodon-profile-view-favourites)
     ("b" "Bookmarks" mastodon-profile-view-bookmarks)]
    ["Minor views"
     :class transient-row
     ("F" "Follow requests" mastodon-views-view-follow-requests)
     ("S" "Scheduled toots" mastodon-views-view-scheduled-toots)
     ("I" "Filters" mastodon-views-view-filters)
     ("G" "Follow suggestions" mastodon-views-view-follow-suggestions)
     ("L" "Lists" mastodon-views-view-lists)]
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
  (my/def-confirmer mastodon-toot-toggle-boost "Toggle boost for this post? ")
  (my/def-confirmer mastodon-toot-toggle-favourite "Toggle favourite this post? ")
  (my/def-confirmer mastodon-toot-toggle-bookmark "Toggle bookmark this post? ")
  (my/def-confirmer mastodon-tl-follow-user "Follow this user? ")
  (my/def-confirmer mastodon-tl-unfollow-user "Unfollow this user? ")
  (my/def-confirmer mastodon-tl-block-user "Block this user? ")
  (my/def-confirmer mastodon-tl-unblock-user "Unblock this user? ")
  (my/def-confirmer mastodon-tl-mute-user "Mute this user? ")
  (my/def-confirmer mastodon-tl-unmute-user "Unmute this user? ")
  (my/def-confirmer mastodon-tl-unmute-user "Unmute this user? ")

  (transient-define-prefix my/mastodon-toot ()
    "Mastodon toot actions."
    ["View"
     :class transient-row
     ("o" "Thread" mastodon-tl-thread)
     ("w" "Browser" my/mastodon-toot--browse)
     ("le" "List edits" mastodon-toot-view-toot-edits)
     ("lf" "List favouriters" mastodon-toot-list-favouriters)
     ("lb" "List boosters" mastodon-toot-list-boosters)]
    ["Toot Actions"
     :class transient-row
     ("r" "Reply" mastodon-toot-reply)
     ("v" "Vote" mastodon-tl-poll-vote)
     ("b" "Boost" my/mastodon-toot--toggle-boost-confirm)
     ("f" "Favourite" my/mastodon-toot--toggle-favourite-confirm)
     ("k" "Bookmark" my/mastodon-toot--toggle-bookmark-confirm)]
    ["My Toot Actions"
     :class transient-row
     ("md" "Delete" mastodon-toot-delete-toot)
     ("mD" "Delete and redraft" mastodon-toot-delete-and-redraft-toot)
     ("mp" "Pin" mastodon-toot-pin-toot-toggle)
     ("me" "Edit" mastodon-toot-edit-toot-at-point)]
    ["Profile Actions"
     :class transient-row
     ("pp" "Profile" mastodon-profile-show-user)
     ("pf" "List followers" mastodon-profile-open-followers)
     ("pF" "List following" mastodon-profile-open-following)
     ("ps" "List statues (no reblogs)" mastodon-profile-open-statuses-no-reblogs)]
    ["User Actions"
     :class transient-row
     ("uf" "Follow user" my/mastodon-tl-follow-user-confirm)
     ("uF" "Unfollow user" my/mastodon-tl-unfollow-user-confirm)
     ("ub" "Block user" my/mastodon-tl-block-user-confirm)
     ("uB" "Unblock user" my/mastodon-tl-unblock-user-confirm)
     ("um" "Mute user" my/mastodon-tl-mute-user-confirm)
     ("uB" "Unmute user" my/mastodon-tl-unmute-user-confirm)]
    ["Misc"
     :class transient-row
     ("q" "Quit" transient-quit-one)]))

(use-package wallabag
  :straight (:host github :repo "chenyanming/wallabag.el" :files (:defaults "default.css" "emojis.alist"))
  :init
  (my-leader-def "aE" #'wallabag)
  :commands (wallabag wallabag-add-entry)
  :config
  (setq wallabag-host "https://wallabag.sqrtminusone.xyz")
  (setq wallabag-username "sqrtminusone")
  (setq wallabag-password (my/password-store-get "Selfhosted/wallabag"))
  (setq wallabag-clientid (password-store-get-field "Selfhosted/wallabag" "client_id"))
  (setq wallabag-secret (password-store-get-field "Selfhosted/wallabag" "client_secret")))

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
  (my-leader-def "ax" #'my/ement)
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
  ;; :straight (:type built-in)
  ;; For now emacs-telega-server is compatible with the latest telega.el
  :straight t
  :if (not (or my/remote-server))
  :commands (telega)
  :init
  (my-leader-def "a l" (my/command-in-persp "telega" "telega" 3 (telega)))
  (my/use-colors
   (telega-button-active :foreground (my/color-value 'base0)
                         :background (my/color-value 'cyan))
   (telega-webpage-chat-link :foreground (my/color-value 'base0)
                             :background (my/color-value 'fg))
   (telega-entity-type-spoiler :background (my/color-value 'base8)))
  :config
  (when (file-exists-p "~/.guix-extra-profiles/emacs/emacs/bin/telega-server")
    (setq telega-server-command
          (expand-file-name
           "~/.guix-extra-profiles/emacs/emacs/bin/telega-server")))
  (setq telega-emoji-use-images nil)
  (setq telega-chat-fill-column 80)
  (setq telega-completing-read-function #'completing-read)
  (setq telega-sticker-size '(12 . 24))
  (add-to-list 'savehist-additional-variables 'telega-msg-add-reaction)
  (remove-hook 'telega-chat-mode-hook #'telega-chat-auto-fill-mode)
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
        (if (executable-find "guix")
            (string-trim
             (shell-command-to-string "guix build tdlib"))
          (expand-file-name "~/bin/td/build/res/usr/local")))
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
  (interactive)
  (set (make-local-variable 'company-backends)
       (append (list 'telega-company-emoji
                     'telega-company-username
                     'telega-company-hashtag
                     'telega-company-markdown-precode)
               (when (telega-chat-bot-p telega-chatbuf--chat)
                 '(telega-company-botcmd))))
  (company-mode 1)
  (setopt visual-fill-column-width
          (+ telega-chat-fill-column
             (if (display-graphic-p) 5 6)))
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
  :commands (google-translate-at-point
             google-translate-at-point-reverse
             google-translate-query-translate
             google-translate-query-translate-reverse
             google-translate-smooth-translate)
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
          ("ru" . "en")
          ("de" . "en")
          ("en" . "de"))))

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
  (when my/is-termux
    (setq biome-query-tab-key "<TAB>")
    (setq biome-api-try-parse-error-as-response t))
  :config
  (add-to-list 'biome-query-coords
               '("Saint-Petersburg, Russia" 59.942651 30.229930))
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
  :commands (devdocs-browser-open
             devdocs-browser-open-in
             devdocs-browser-install-doc
             devdocs-browser-uninstall-doc
             devdocs-browser-download-offline-data
             devdocs-browser-remove-offline-data
             devdocs-browser-upgrade-all-docs
             devdocs-browser-update-docs)
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
  :commands (sx-search sx-tab-frontpage)
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

(use-package gptel
  :straight t
  :if (not my/is-termux)
  :init
  (my-leader-def
    :infix "ai"
    "" '(:wk "AI")
    "i" #'gptel)
  :commands (gptel gptel-send gptel-menu)
  :config
  (setq gptel-mode "llama3:latest")
  (setq gptel-track-media t)
  (setq gptel-backend
        (gptel-make-ollama "Ollama"
          :host "localhost:11434"
          :stream t
          :models '("llama3.1:8b" "deepseek-r1:32b"
                    "qwen2.5:32b" "qwen2.5-coder:32b"
                    "eva-qwen2.5-q4_k_l-32b:latest"
                    "t-pro-1.0-q4_k_m:latest"
                    "qwq:32b"
                    (gemma3:32b
                     :capabilities (media)
                     :mime-types ("image/jpeg" "image/png")))))
  (gptel-make-openai "OpenRouter"
    :host "openrouter.ai/api"
    :key (lambda () (my/password-store-get-field
                     "My_Online/Accounts/openrouter" "api-key"))
    :stream t
    :models '("anthropic/claude-3.5-haiku"))
  (setq gptel--known-backends
        (seq-filter
         (lambda (cell)
           (not (equal (car cell) "ChatGPT")))
         gptel--known-backends))
  (setq gptel-response-prefix-alist
        '((markdown-mode . "[Response] ")
          (org-mode . "*** Response: ")
          (text-mode . "[Response]")))

  (general-define-key
   :keymaps '(gptel-mode-map)
   :states '(insert normal)
   "C-<return>" 'gptel-send
   "M-o" #'gptel-menu))

(use-package ellama
  :straight t
  :init
  (setq ellama-language "English")
  :defer t
  :config
  (require 'llm-ollama)
  ;; I've looked for this option for 1.5 hours
  (setq ellama-long-lines-length 100000)

  (setq ellama-provider (make-llm-ollama
                         :chat-model "qwen2.5:32b"
                         :embedding-model "qwen2.5:32b"))
  (setq ellama-coding-provider (make-llm-ollama
                                :chat-model "qwen2.5-coder:32b"
                                :embedding-model "qwen2.5-coder:32b"))
  (setq ellama-providers
        `(("llama3.1:8b" . ,(make-llm-ollama
                             :chat-model "llama3.1:latest"
                             :embedding-model "llama3.1:latest"))
          ("phi4:latest" . ,(make-llm-ollama
                             :chat-model "phi4:latest"
                             :embedding-model "phi4:latest"))
          ("qwen2.5:32b" . ,(make-llm-ollama
                             :chat-model "qwen2.5:32b"
                             :embedding-model "qwen2.5:32b"))
          ("qwen2.5-coder:32b" . ,(make-llm-ollama
                                   :chat-model "qwen2.5-coder:32b"
                                   :embedding-model "qwen2.5-coder:32b")))))

(with-eval-after-load 'ellama
  (transient-define-prefix my/ellama-transient ()
    "Ellama actions."
    ["General"
     :class transient-row
     ("a" "Chat" ellama-chat)]
    ["Code"
     :class transient-row
     ("ca" "Add" ellama-code-add)
     ("cc" "Complete" ellama-code-complete)
     ("ce" "Edit" ellama-code-edit)
     ("cr" "Review" ellama-code-review)
     ("ci" "Improve" ellama-code-improve)]
    ["Natural Language"
     :class transient-row
     ("np" "Proof-read" my/ellama-proof-read)]
    ["Formatting"
     :class transient-row
     ("ff" "Format" ellama-make-format)
     ("fm" "List" ellama-make-list)
     ("ft" "Table" ellama-make-table)]
    ["Explain & Summarize"
     :class transient-row
     ("es" "Summarize" ellama-summarize)
     ("ea" "Ask about" ellama-ask-about)
     ("es" "Send to chat" ellama-ask-selection)
     ("ew" "Word definition" ellama-define-word)]
    ["Context"
     :class transient-row
     ("xb" "Add buffer" ellama-context-add-buffer)
     ("xf" "Add file" ellama-context-add-file)
     ("xi" "Add info" ellama-context-add-info-node)
     ("xs" "Add selection" ellama-context-add-selection)]
    ["Settings & Sessions"
     :class transient-row
     ("sp" "Provider" ellama-provider-select)
     ("ss" "Session" ellama-session-switch)
     ("sr" "Rename ression" ellama-session-rename)
     ("sd" "Delete session" ellama-session-remove)]))

(defun my/ellama ()
  (interactive)
  (require 'ellama)
  (call-interactively #'my/ellama-transient))

(my-leader-def "aie" #'my/ellama)

(defun my/diff-strings (str1 str2)
  (let ((file1 (make-temp-file "diff1"))
        (file2 (make-temp-file "diff2")))
    (unwind-protect
        (progn
          (with-temp-file file1
            (insert str1))
          (with-temp-file file2
            (insert str2))
          (with-temp-buffer
            (diff-mode)
            (diff-no-select file1 file2 (diff-switches) t (current-buffer))
            (font-lock-fontify-buffer)
            (buffer-string)))
      (delete-file file1)
      (delete-file file2))))

(defun my/ellama-proof-read--display (text is-org-mode prompt)
  (llm-chat-async
   ellama-provider
   (llm-make-chat-prompt
    (format prompt text))
   (lambda (response)
     (let* ((parts (split-string response "-FIXED TEXT ENDS-"))
            (changed-text (nth 0 parts))
            (comments (nth 1 parts))
            (buffer (generate-new-buffer "*ellama-diff*")))
       (when is-org-mode
         (setq changed-text (ellama--translate-markdown-to-org-filter changed-text)))
       (with-current-buffer buffer
         (text-mode)
         (insert
          (propertize "Changed text:\n" 'face 'transient-heading)
          (string-trim changed-text)
          "\n\n"
          (propertize "Comments:\n" 'face 'transient-heading)
          (string-trim comments)
          "\n\n"
          (propertize "Diff:\n" 'face 'transient-heading)
          (my/diff-strings text changed-text)))
       (display-buffer buffer)))
   (lambda (&rest err)
     (message "Error: %s" err))))

(setq my/ellama-proof-read-prompt
      "Proof-read the following text. Follow these rules:
- Fix all grammar errors
- Keep the original style and punctuation, including linebreaks.
- Use British spelling
- Do not replace ' with ’, and do not touch other such symbols

Output the following and nothing else:
- The fixed text
- The string -FIXED TEXT ENDS-
- List of found errors
- List of style suggestions
%s")

(defun my/ellama--text ()
  (if (region-active-p)
	  (buffer-substring-no-properties (region-beginning) (region-end))
	(buffer-substring-no-properties (point-min) (point-max))))

(defun my/ellama-proof-read (text is-org-mode)
  (interactive (list (my/ellama--text) (derived-mode-p 'org-mode)))
  (require 'ellama)
  (my/ellama-proof-read--display text is-org-mode my/ellama-proof-read-prompt))

(defun my/whisper--format-vtt-seconds (seconds)
  (if (numberp seconds)
      (let* ((hours (/ (floor seconds) (* 60 60)))
             (minutes (/ (- (floor seconds) (* hours 60 60)) 60))
             (sec (% (floor seconds) 60))
             (ms (floor (* 1000 (- seconds (floor seconds))))))
        (format "%.2d:%.2d:%.2d.%.3d" hours minutes sec ms))
    ""))

(defun my/whisper--save-chucks-vtt (path data)
  (with-temp-file path
    (insert "WEBVTT\n\n")
    (cl-loop for chunk across (alist-get 'chunks data)
             for start = (my/whisper--format-vtt-seconds
                          (aref (alist-get 'timestamp chunk) 0))
             for end = (my/whisper--format-vtt-seconds
                        (aref (alist-get 'timestamp chunk) 1))
             do (insert (format "%s --> %s" start end) "\n")
             do (insert (string-trim (alist-get 'text chunk)) "\n\n"))))

(defun my/whisper--save-speakers-vtt (path data)
  (with-temp-file path
    (insert "WEBVTT\n\n")
    (cl-loop for chunk across (alist-get 'speakers data)
             for start = (my/whisper--format-vtt-seconds
                          (aref (alist-get 'timestamp chunk) 0))
             for end = (my/whisper--format-vtt-seconds
                        (aref (alist-get 'timestamp chunk) 1))
             do (insert (format "%s --> %s" start end) "\n")
             do (insert
                 (format "<v %s>" (alist-get 'speaker chunk))
                 (string-trim (alist-get 'text chunk)) "\n\n"))))

(defun my/whisper--save-speakers-txt (path data)
  (with-temp-file path
    (cl-loop with prev-speaker
             for chunk across (alist-get 'speakers data)
             for speaker = (alist-get 'speaker chunk)
             if (not (equal speaker prev-speaker))
             do (progn
                  (when prev-speaker
                    (fill-region
                     (line-beginning-position)
                     (line-end-position))
                    (insert "\n\n"))
                  (insert (format "[%s]" speaker) "\n")
                  (setq prev-speaker speaker))
             do (insert (string-trim (alist-get 'text chunk)) " "))
    (fill-region
     (line-beginning-position)
     (line-end-position))))

(defun my/whisper--process-output (transcript-path)
  (let ((data (json-read-file transcript-path)))
    (when (alist-get 'text data)
      (with-temp-file (concat
                       (file-name-sans-extension transcript-path)
                       ".txt")
        (insert (string-trim (alist-get 'text data)))
        (do-auto-fill)))
    (unless (seq-empty-p (alist-get 'speakers data))
      (my/whisper--save-speakers-vtt
       (concat (file-name-sans-extension transcript-path) "-spk.vtt")
       data)
      (my/whisper--save-speakers-txt
       (concat (file-name-sans-extension transcript-path) "-spk.txt")
       data))
    (my/whisper--save-chucks-vtt
     (concat (file-name-sans-extension transcript-path) ".vtt")
     data)))

(defvar my/whisper-path
  "/home/pavel/micromamba/envs/insanely-fast-whisper/bin/insanely-fast-whisper")

(defun my/invoke-whisper (input output-dir &optional language num-speakers)
  (interactive
   (list
    (read-file-name "Input file:" nil nil t)
    (read-directory-name "Output-directory: ")
    (let ((lang (read-string "Language (optional): ")))
      (if (string-empty-p lang) nil lang))
    (let ((num (read-number "Number of speakers (optional): " 0)))
      (when (> num 0)
        (number-to-string num)))))
  (let* ((transcript-path (concat
                           (expand-file-name (file-name-as-directory output-dir))
                           (file-name-base input)
                           ".json"))
         (args
          `("--file-name" ,(expand-file-name input)
            "--transcript-path" ,transcript-path
            "--hf-token" ,(my/password-store-get-field "My_Online/Accounts/huggingface.co" "token")
            ,@(when language
                `("--language" ,language))
            ,@(when num-speakers
                `("--num-speakers" ,num-speakers))))
         (buffer (generate-new-buffer "*whisper*"))
         (proc (apply #'start-process "whisper" buffer my/whisper-path args)))
    (set-process-sentinel
     proc
     (lambda (process _msg)
       (let ((status (process-status process))
             (code (process-exit-status process)))
         (cond ((and (eq status 'exit) (= code 0))
                (my/whisper--process-output transcript-path)
                (notifications-notify :body "Audio conversion completed"
                                      :title "Whisper")
                (kill-buffer (process-buffer process)))
               ((or (and (eq status 'exit) (> code 0))
                    (eq status 'signal))
                (let ((err (with-current-buffer (process-buffer process)
                             (buffer-string))))
                  (user-error "Error in Whisper: %s" err)))))))))

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

(defun my/whisper-url (url file-name output-dir &optional language num-speakers)
  (interactive
   (list (read-from-minibuffer "URL: ")
         (read-from-minibuffer "File name: ")
         (read-directory-name "Output directory: ")
         (let ((lang (read-string "Language (optional): ")))
           (if (string-empty-p lang) nil lang))
         (let ((num (read-number "Number of speakers (optional): " 0)))
           (when (> num 0)
             (number-to-string num)))))
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
         (my/invoke-whisper file-path output-dir language num-speakers)))
      :error
      (cl-function
       (lambda (&key error-thrown &allow-other-keys)
         (message "Error!: %S" error-thrown))))))

(use-package ini
  :mode "\\.ini\\'"
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
    (lambda (elem) (my/index--tree-narrow-recursive elem (my/system-name)))
    (copy-tree tree))))

(defvar my/index-keep-files
  '(".dtrash"))

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
                                              collect
                                              `((path . ,f)
                                                (exists . t)
                                                (has-to-exist
                                                 . ,(member
                                                     (file-name-nondirectory
                                                      (directory-file-name f))
                                                     my/index-keep-files))
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
                       (enabled . ,(seq-contains-p
                                    '("Pending" "Loading" "Running")
                                    (alist-get 'RUN_STATE value)))))))

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
              collect (list (format "mega-sync -r \"%s\"" path) "Mega enable sync" 5)
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
  (require 'ini)
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
         (vertico-sort-function nil))
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
  (my-leader-def "ap" #'pomm)
  ;; (my-leader-def "ap" #'pomm-third-time)
  (setq alert-default-style 'libnotify)
  (setq pomm-audio-enabled t)
  (setq pomm-audio-player-executable (executable-find "mpv"))
  :config
  (pomm-mode-line-mode)
  (add-hook 'pomm-on-status-changed-hook #'pomm--sync-org-clock)
  (add-hook 'pomm-third-time-on-status-changed-hook
            #'pomm-third-time--sync-org-clock))

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

(use-package casual
  :straight (:host github :repo "kickingvegas/Casual")
  :after calc
  :config
  (general-define-key
   :states '(normal)
   :keymaps 'calc-mode-map
   "M-o" #'casual-main-menu))

(use-package chess
  :commands (chess-pgn-mode)
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
   ((string-match-p (rx "german" (* not-newline) ".org" eos) name)
    "<LEARNING>")
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
           (not my/remote-server)
           (not my/nested-emacs))
  :config
  (setq elcord-buffer-details-format-function #'my/elcord-buffer-details-format-functions)
  (advice-add 'elcord--try-update-presence :filter-args #'my/elcord-update-presence-mask-advice)
  (add-to-list 'elcord-mode-text-alist '(telega-chat-mode . "Telega Chat"))
  (add-to-list 'elcord-mode-text-alist '(telega-root-mode . "Telega Root"))
  ;; (elcord-mode)
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
  :commands (zone)
  :config
  (setq original-zone-programs (copy-sequence zone-programs)))

(defun my/zone-with-select ()
  (interactive)
  (let ((zone-programs
         (vector
          (intern
           (completing-read
            "Zone programs"
            (cl-mapcar 'symbol-name original-zone-programs))))))
    (zone)))

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
