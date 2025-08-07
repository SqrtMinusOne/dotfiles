;;; -*- lexical-binding: t -*-
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

(provide 'sqrt-terms)
