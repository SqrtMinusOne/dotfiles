;;; -*- lexical-binding: t -*-
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

(defun my/set-smartparens-indent (mode)
  (sp-local-pair mode "{" nil :post-handlers '(("|| " "SPC") ("||\n[i]" "RET")))
  (sp-local-pair mode "[" nil :post-handlers '(("|| " "SPC") ("||\n[i]" "RET")))
  (sp-local-pair mode "(" nil :post-handlers '(("|| " "SPC") ("||\n[i]" "RET"))))

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

(defvar my/edit-elisp-string--window-config nil)

(defun my/edit-elisp-string ()
  (interactive)
  (if (org-src-edit-buffer-p)
      (org-edit-src-exit)
    (let* ((bounds (bounds-of-thing-at-point 'string))
           (orig-buf (current-buffer))
           (orig-str (when bounds
                       (buffer-substring-no-properties (car bounds) (cdr bounds)))))
      (unless bounds
        (user-error "No string under cursor"))
      ;; Not sure if there's a better way
      (let* ((mode (intern
                    (completing-read
                     "Major mode: " obarray
                     (lambda (sym)
                       (and (commandp sym)
                            (string-suffix-p "-mode" (symbol-name sym))))
                     t)))
             (edit-buf (generate-new-buffer "*string-edit*")))
        (setq edit-elisp-string--window-config (current-window-configuration))
        (with-current-buffer edit-buf
          (insert (string-replace
                   "\\\"" "\"" (substring orig-str 1 -1)))
          (funcall mode)
          (use-local-map (copy-keymap (current-local-map)))
          ;; Confirm edit
          (local-set-key
           (kbd "C-c '")
           (lambda ()
             (interactive)
             (let ((new-str (buffer-substring-no-properties
                             (point-min) (point-max))))
               (with-current-buffer orig-buf
                 (delete-region (car bounds) (cdr bounds))
                 (goto-char (car bounds))
                 (insert (prin1-to-string new-str))))
             (kill-buffer edit-buf)
             (when edit-elisp-string--window-config
               (set-window-configuration edit-elisp-string--window-config))))
          ;; Cancel edit
          (local-set-key
           (kbd "C-c C-k")
           (lambda ()
             (interactive)
             (kill-buffer edit-buf)
             (when edit-elisp-string--window-config
               (set-window-configuration edit-elisp-string--window-config)))))
        (pop-to-buffer edit-buf)))))

(general-define-key
 :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
 "C-c '" #'my/edit-elisp-string)

(use-package projectile
  :straight t
  :config
  (projectile-mode +1)
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
  ;; (require 'forge)
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

(provide 'sqrt-general-config)
