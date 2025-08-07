;;; -*- lexical-binding: t -*-
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

(provide 'sqrt-keybindings)
