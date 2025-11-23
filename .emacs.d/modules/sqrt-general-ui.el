;;; -*- lexical-binding: t -*-
(unless my/is-termux
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1))

(when my/is-termux
  (menu-bar-mode -1))

;; (set-frame-parameter (selected-frame) 'alpha '(90 . 90))
;; (add-to-list 'default-frame-alist '(alpha . (90 . 90)))

(setq confirm-kill-emacs 'y-or-n-p)

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
  (doom-themes-visual-bell-config)
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config))

;; (use-package modus-themes
;;   :straight (:build (:not native-compile)))

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
  (let* ((base-value (string-to-number (substring (symbol-name color) 4 5)))
         (palette (modus-themes-get-theme-palette
                   (or (my/modus-p) (my/ef-p))
                   :with-overrides :with-user-palette))
         (base-start (cadr (assoc 'bg-main palette)))
         (base-end (cadr (assoc 'fg-dim palette))))
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
  (my/prot-color color (modus-themes-get-theme-palette
                        (or (my/modus-p) (my/ef-p))
                        :with-overrides :with-user-palette)))

(defalias 'my/ef-color 'my/modus-color)

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

(defun my/advice-my-theme ()
  (advice-add 'load-theme :after #'my/update-my-theme))

(unless my/is-termux
  (add-hook 'emacs-startup-hook #'my/update-my-theme)
 ;; (add-hook 'emacs-startup-hook #'my/advice-my-theme)
)

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
  (my/update-my-theme)
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
  :if (display-graphic-p)
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

(provide 'sqrt-general-ui)
