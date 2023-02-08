(defun my/exwm-run-polybar ()
  (call-process "~/bin/polybar.sh"))

(defun my/exwm-set-wallpaper ()
  (call-process-shell-command "feh --bg-fill ~/Pictures/wallpaper.jpg"))

(defun my/exwm-run-shepherd ()
  (when (string-empty-p (shell-command-to-string "pgrep -u pavel shepherd"))
    (call-process "shepherd")))

(use-package pinentry
  :straight t
  :after (exwm)
  :config
  (setenv "GPG_AGENT_INFO" nil) ;; use emacs pinentry
  (setq auth-source-debug t)

  (setq epg-gpg-program "gpg2") ;; not necessary
  (require 'epa-file)
  (epa-file-enable)
  (setq epa-pinentry-mode 'loopback)
  (setq epg-pinentry-mode 'loopback)
  (pinentry-start))

(use-package exwm-modeline
  :straight (:host github :repo "SqrtMinusOne/exwm-modeline")
  :config
  (add-hook 'exwm-init-hook #'exwm-modeline-mode)
  (my/use-doom-colors
   (exwm-modeline-current-workspace
    :foreground (doom-color 'yellow)
    :weight 'bold)))

(defun my/exwm-direction-exists-p (dir)
  "Check if there is space in the direction DIR.

Does not take the minibuffer into account."
  (cl-some (lambda (dir)
             (let ((win (windmove-find-other-window dir)))
               (and win (not (window-minibuffer-p win)))))
           (pcase dir
             ('width '(left right))
             ('height '(up down)))))

(defun my/exwm-move-window (dir)
  "Move the current window in the direction DIR."
  (let ((other-window (windmove-find-other-window dir))
        (other-direction (my/exwm-direction-exists-p
                          (pcase dir
                            ('up 'width)
                            ('down 'width)
                            ('left 'height)
                            ('right 'height)))))
    (cond
     ((and other-window (not (window-minibuffer-p other-window)))
      (window-swap-states (selected-window) other-window))
     (other-direction
      (evil-move-window dir)))))

(setq my/exwm-resize-value 5)

(defun my/exwm-resize-window (dir kind &optional value)
  "Resize the current window in the direction DIR.

DIR is either 'height or 'width, KIND is either 'shrink or
 'grow.  VALUE is `my/exwm-resize-value' by default.

If the window is an EXWM floating window, execute the
corresponding command from the exwm-layout group, execute the
command from the evil-window group."
  (unless value
    (setq value my/exwm-resize-value))
  (let* ((is-exwm-floating
          (and (derived-mode-p 'exwm-mode)
               exwm--floating-frame))
         (func (if is-exwm-floating
                   (intern
                    (concat
                     "exwm-layout-"
                     (pcase kind ('shrink "shrink") ('grow "enlarge"))
                     "-window"
                     (pcase dir ('height "") ('width "-horizontally"))))
                 (intern
                  (concat
                   "evil-window"
                   (pcase kind ('shrink "-decrease-") ('grow "-increase-"))
                   (symbol-name dir))))))
    (when is-exwm-floating
      (setq value (* 5 value)))
    (funcall func value)))

(defhydra my/exwm-resize-hydra (:color pink :hint nil :foreign-keys run)
  "
^Resize^
_l_: Increase width   _h_: Decrease width   _j_: Increase height   _k_: Decrease height

_=_: Balance          "
  ("h" (lambda () (interactive) (my/exwm-resize-window 'width 'shrink)))
  ("j" (lambda () (interactive) (my/exwm-resize-window 'height 'grow)))
  ("k" (lambda () (interactive) (my/exwm-resize-window 'height 'shrink)))
  ("l" (lambda () (interactive) (my/exwm-resize-window 'width 'grow)))
  ("=" balance-windows)
  ("q" nil "quit" :color blue))

(defun my/exwm-fill-other-window (&rest _)
  "Open the most recently used buffer in the next window."
  (interactive)
  (when (and (eq major-mode 'exwm-mode) (not (eq (next-window) (get-buffer-window))))
    (let ((other-exwm-buffer
           (cl-loop with other-buffer = (persp-other-buffer)
                    for buf in (sort (persp-current-buffers) (lambda (a _) (eq a other-buffer)))
                    with current-buffer = (current-buffer)
                    when (and (not (eq current-buffer buf))
                              (buffer-live-p buf)
                              (not (string-match-p (persp--make-ignore-buffer-rx) (buffer-name buf)))
                              (not (get-buffer-window buf)))
                    return buf)))
      (when other-exwm-buffer
        (with-selected-window (next-window)
          (switch-to-buffer other-exwm-buffer))))))

(advice-add 'evil-window-split :after #'my/exwm-fill-other-window)
(advice-add 'evil-window-vsplit :after #'my/exwm-fill-other-window)

(use-package perspective-exwm
  :straight t
  :config
  (setq perspective-exwm-override-initial-name
        '((0 . "misc")
          (1 . "core")
          (2 . "browser")
          (3 . "comms")
          (4 . "dev")))
  (general-define-key
   :keymaps 'perspective-map
   "e" #'perspective-exwm-move-to-workspace
   "E" #'perspective-exwm-copy-to-workspace))

(defun my/exwm-configure-window ()
  (interactive)
  (pcase exwm-class-name
    ((or "Firefox" "Nightly")
     (perspective-exwm-assign-window
      :workspace-index 2
      :persp-name "browser"))
    ("Nyxt"
     (perspective-exwm-assign-window
      :workspace-index 2
      :persp-name "browser"))
    ("Alacritty"
     (perspective-exwm-assign-window
      :persp-name "term"))
    ((or "VK" "Slack" "discord" "TelegramDesktop")
     (perspective-exwm-assign-window
      :workspace-index 3
      :persp-name "comms"))
    ((or "Chromium-browser" "jetbrains-datagrip")
     (perspective-exwm-assign-window
      :workspace-index 4
      :persp-name "dev"))))

(add-hook 'exwm-manage-finish-hook #'my/exwm-configure-window)

(setq my/exwm-last-workspaces '(1))

(defun my/exwm-store-last-workspace ()
  "Save the last workspace to `my/exwm-last-workspaces'."
  (setq my/exwm-last-workspaces
        (seq-uniq (cons exwm-workspace-current-index
                        my/exwm-last-workspaces))))

(add-hook 'exwm-workspace-switch-hook
          #'my/exwm-store-last-workspace)

(defun my/exwm-last-workspaces-clear ()
  "Clean `my/exwm-last-workspaces' from deleted workspaces."
  (setq my/exwm-last-workspaces
        (seq-filter
         (lambda (i) (nth i exwm-workspace--list))
         my/exwm-last-workspaces)))

(setq my/exwm-monitor-list
      (pcase (system-name)
        ("indigo" '(nil "DVI-D-0"))
        (_ '(nil))))

(defun my/exwm-get-current-monitor ()
  "Return the current monitor name or nil."
  (plist-get exwm-randr-workspace-output-plist
             (cl-position (selected-frame)
                          exwm-workspace--list)))

(defun my/exwm-get-other-monitor (dir)
  "Cycle the monitor list in the direction DIR.

DIR is either 'left or 'right."
  (nth
   (% (+ (cl-position
          (my/exwm-get-current-monitor)
          my/exwm-monitor-list
          :test #'string-equal)
         (length my/exwm-monitor-list)
         (pcase dir
           ('right 1)
           ('left -1)))
      (length my/exwm-monitor-list))
   my/exwm-monitor-list))

(defun my/exwm-switch-to-other-monitor (&optional dir)
  "Switch to another monitor."
  (interactive)
  (my/exwm-last-workspaces-clear)
  (let ((mouse-autoselect-window nil))
    (exwm-workspace-switch
     (cl-loop with other-monitor = (my/exwm-get-other-monitor (or dir 'right))
              for i in (append my/exwm-last-workspaces
                               (cl-loop for i from 0
                                        for _ in exwm-workspace--list
                                        collect i))
              if (if other-monitor
                     (string-equal (plist-get exwm-randr-workspace-output-plist i)
                                   other-monitor)
                   (not (plist-get exwm-randr-workspace-output-plist i)))
              return i))))

(defun my/exwm-workspace-switch-monitor ()
  "Move the current workspace to another monitor."
  (interactive)
  (let ((new-monitor (my/exwm-get-other-monitor 'right))
        (current-monitor (my/exwm-get-current-monitor)))
    (when (and current-monitor
               (>= 1
                   (cl-loop for (key value) on exwm-randr-workspace-monitor-plist
                            by 'cddr
                            if (string-equal value current-monitor) sum 1)))
      (error "Can't remove the last workspace on the monitor!"))
    (setq exwm-randr-workspace-monitor-plist
          (map-delete exwm-randr-workspace-monitor-plist exwm-workspace-current-index))
    (when new-monitor
      (setq exwm-randr-workspace-monitor-plist
            (plist-put exwm-randr-workspace-monitor-plist
                       exwm-workspace-current-index
                       new-monitor))))
  (exwm-randr-refresh))

(defun my/exwm-windmove (dir)
  "Move to window or monitor in the direction DIR."
  (if (or (eq dir 'down) (eq dir 'up))
      (windmove-do-window-select dir)
    (let ((other-window (windmove-find-other-window dir))
          (other-monitor (my/exwm-get-other-monitor dir))
          (opposite-dir (pcase dir
                          ('left 'right)
                          ('right 'left))))
      (if other-window
          (windmove-do-window-select dir)
        (let ((mouse-autoselect-window nil))
          (my/exwm-switch-to-other-monitor dir))
        (cl-loop while (windmove-find-other-window opposite-dir)
                 do (windmove-do-window-select opposite-dir))))))

(use-package ivy-posframe
  :straight t
  :disabled
  :config
  (setq ivy-posframe-parameters '((left-fringe . 10)
                                  (right-fringe . 10)
                                  (parent-frame . nil)
                                  (max-width . 80)))
  (setq ivy-posframe-height-alist '((t . 20)))
  (setq ivy-posframe-width 180)
  (setq ivy-posframe-min-height 5)
  (setq ivy-posframe-display-functions-alist
        '((swiper . ivy-display-function-fallback)
          (swiper-isearch . ivy-display-function-fallback)
          (t . ivy-posframe-display)))
  (ivy-posframe-mode 1))

(defun my/advise-fn-suspend-follow-mouse (fn &rest args)
  (let ((focus-follows-mouse nil)
        (mouse-autoselect-window nil)
        (pos (x-mouse-absolute-pixel-position)))
    (unwind-protect
        (apply fn args)
      (x-set-mouse-absolute-pixel-position (car pos)
                                           (cdr pos)))))
(with-eval-after-load 'ivy-posframe
  (advice-add #'ivy-posframe--read :around #'my/advise-fn-suspend-follow-mouse))

(defun my/setup-posframe (&rest args)
  (mapc
   (lambda (var)
     (kill-local-variable var)
     (setf (symbol-value var) nil))
   '(exwm-workspace-warp-cursor
     mouse-autoselect-window
     focus-follows-mouse)))

(defun my/restore-posframe (&rest args)
  (run-with-timer
   0.25
   (lambda ()
     (mapc
      (lambda (var)
        (kill-local-variable var)
        (setf (symbol-value var) t))
      '(exwm-workspace-warp-cursor
        mouse-autoselect-window
        focus-follows-mouse)))))

(with-eval-after-load 'ivy-posframe
  (advice-add #'posframe--create-posframe :after #'my/setup-posframe)
  (advice-add #'ivy-posframe-cleanup :after #'my/restore-posframe))

(defun my/counsel-linux-app-format-function (name comment _exec)
  (format "% -45s%s"
          (propertize
           (ivy--truncate-string name 45)
           'face 'counsel-application-name)
          (if comment
              (concat ": " (ivy--truncate-string comment 100))
            "")))

(setq counsel-linux-app-format-function #'my/counsel-linux-app-format-function)

(use-package password-store-ivy
  :straight (:host github :repo "SqrtMinusOne/password-store-ivy")
  :after (exwm))

(use-package emojify
  :straight t)

(defun my/emojify-type ()
  "Type an emoji."
  (interactive)
  (let ((emoji (emojify-completing-read "Type emoji: ")))
    (kill-new emoji)
    (password-store-ivy--async-commands
     (list
      (password-store-ivy--get-wait-command 10)
      "xdotool key Shift+Insert"))))

(defun my/exwm-quit ()
  (interactive)
  (when (or (not (eq (selected-window) (next-window)))
            (y-or-n-p "This is the last window. Are you sure?"))
    (evil-quit)))

(defun my/exwm-update-global-keys ()
  (interactive)
  (setq exwm-input--global-keys nil)
  (dolist (i exwm-input-global-keys)
    (exwm-input--set-key (car i) (cdr i)))
  (when exwm--connection
    (exwm-input--update-global-prefix-keys)))

(defun my/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defhydra my/exwm-apps-hydra (:color blue :hint nil)
  "
^Apps^
_t_: Terminal (Alacritty)
_b_: Browser (Firefox)
_s_: Slack
_e_: Telegram
_d_: Discord
"
  ("t" (lambda () (interactive) (my/run-in-background "alacritty")))
  ("b" (lambda () (interactive) (my/run-in-background "firefox")))
  ("s" (lambda () (interactive) (my/run-in-background "slack-wrapper")))
  ("e" (lambda () (interactive) (my/run-in-background "telegram-desktop")))
  ("d" (lambda () (interactive) (my/run-in-background "flatpak run com.discordapp.Discord"))))

(defun my/exwm-lock ()
  (interactive)
  (my/run-in-background "i3lock -f -i /home/pavel/Pictures/lock-wallpaper.png"))

(defun my/fix-exwm-floating-windows ()
  (setq-local exwm-workspace-warp-cursor nil)
  (setq-local mouse-autoselect-window nil)
  (setq-local focus-follows-mouse nil))

(add-hook 'exwm-floating-setup-hook #'my/fix-exwm-floating-windows)

(setq exwm-manage-configurations
   '(((member exwm-class-name '("Nyxt"))
	   char-mode t)))

(defun my/exwm-init ()
  (exwm-workspace-switch 1)

  (my/exwm-run-polybar)
  (my/exwm-set-wallpaper)
  (my/exwm-run-shepherd)
  (my/run-in-background "gpgconf --reload gpg-agent"))

(defun my/exwm-update-class ()
  (exwm-workspace-rename-buffer (format "EXWM :: %s" exwm-class-name)))

(use-package exwm
  :straight t
  :config
  (setq exwm-workspace-number 5)
  (add-hook 'exwm-init-hook #'my/exwm-init)
  (add-hook 'exwm-update-class-hook #'my/exwm-update-class)

  (require 'exwm-randr)
  (exwm-randr-enable)
  (start-process-shell-command "xrandr" nil "~/bin/scripts/screen-layout")
  (when (string= (system-name) "indigo")
    (setq my/exwm-another-monitor "DVI-D-0")
    (setq exwm-randr-workspace-monitor-plist `(2 ,my/exwm-another-monitor 3 ,my/exwm-another-monitor)))

  (setq exwm-workspace-warp-cursor t)
  (setq mouse-autoselect-window t)
  (setq focus-follows-mouse t)

  
  (setq exwm-input-prefix-keys
        `(?\C-x
          ?\C-w
          ?\M-x
          ?\M-u))
  (defmacro my/app-command (command)
    `(lambda () (interactive) (my/run-in-background ,command)))
  
  (general-define-key
   :keymaps '(exwm-mode-map)
   "C-q" #'exwm-input-send-next-key
   "<print>" (my/app-command "flameshot gui")
   "<mode-line> s-<mouse-4>" #'perspective-exwm-cycle-exwm-buffers-backward
   "<mode-line> s-<mouse-5>" #'perspective-exwm-cycle-exwm-buffers-forward
   "M-x" #'counsel-M-x
   "M-SPC" (general-key "SPC"))
  (setq exwm-input-simulation-keys `((,(kbd "M-w") . ,(kbd "C-w"))
                                     (,(kbd "M-c") . ,(kbd "C-c"))))
  (setq exwm-input-global-keys
        `(
          ;; Reset to line-mode
          (,(kbd "s-R") . exwm-reset)
  
          ;; Switch windows
          (,(kbd "s-<left>") . (lambda () (interactive) (my/exwm-windmove 'left)))
          (,(kbd "s-<right>") . (lambda () (interactive) (my/exwm-windmove 'right)))
          (,(kbd "s-<up>") . (lambda () (interactive) (my/exwm-windmove 'up)))
          (,(kbd "s-<down>") . (lambda () (interactive) (my/exwm-windmove 'down)))
  
          (,(kbd "s-h"). (lambda () (interactive) (my/exwm-windmove 'left)))
          (,(kbd "s-l") . (lambda () (interactive) (my/exwm-windmove 'right)))
          (,(kbd "s-k") . (lambda () (interactive) (my/exwm-windmove 'up)))
          (,(kbd "s-j") . (lambda () (interactive) (my/exwm-windmove 'down)))
  
          ;; Moving windows
          (,(kbd "s-H") . (lambda () (interactive) (my/exwm-move-window 'left)))
          (,(kbd "s-L") . (lambda () (interactive) (my/exwm-move-window 'right)))
          (,(kbd "s-K") . (lambda () (interactive) (my/exwm-move-window 'up)))
          (,(kbd "s-J") . (lambda () (interactive) (my/exwm-move-window 'down)))
  
          ;; Fullscreen
          (,(kbd "s-f") . exwm-layout-toggle-fullscreen)
          (,(kbd "s-F") . exwm-floating-toggle-floating)
  
          ;; Quit
          (,(kbd "s-Q") . my/exwm-quit)
  
          ;; Split windows
          (,(kbd "s-s") . evil-window-vsplit)
          (,(kbd "s-v") . evil-window-hsplit)
  
          ;; Switch perspectives
          (,(kbd "s-,") . persp-prev)
          (,(kbd "s-.") . persp-next)
  
          ;; Switch buffers
          (,(kbd "s-e") . persp-ivy-switch-buffer)
          (,(kbd "s-E") . my/persp-ivy-switch-buffer-other-window)
  
          ;; Resize windows
          (,(kbd "s-r") . my/exwm-resize-hydra/body)
  
          ;; Apps & stuff
          (,(kbd "s-p") . counsel-linux-app)
          (,(kbd "s-P") . async-shell-command)
          (,(kbd "s-;") . my/exwm-apps-hydra/body)
          (,(kbd "s--") . password-store-ivy)
          (,(kbd "s-=") . my/emojify-type)
          (,(kbd "s-i") . ,(my/app-command "copyq menu"))
  
          ;; Basic controls
          (,(kbd "<XF86AudioRaiseVolume>") . ,(my/app-command "ponymix increase 5 --max-volume 150"))
          (,(kbd "<XF86AudioLowerVolume>") . ,(my/app-command "ponymix decrease 5 --max-volume 150"))
          (,(kbd "<XF86MonBrightnessUp>") . ,(my/app-command "light -A 5"))
          (,(kbd "<XF86MonBrightnessDown>") . ,(my/app-command "light -U 5"))
          (,(kbd "<XF86AudioMute>") . ,(my/app-command "ponymix toggle"))
  
          (,(kbd "<XF86AudioPlay>") . ,(my/app-command "mpc toggle"))
          (,(kbd "<XF86AudioPause>") . ,(my/app-command "mpc pause"))
          (,(kbd "<print>") . ,(my/app-command "flameshot gui"))
  
          ;; Switch workspace
          (,(kbd "s-q") . my/exwm-switch-to-other-monitor)
          (,(kbd "s-w") . exwm-workspace-switch)
          (,(kbd "s-W") . exwm-workspace-move-window)
          (,(kbd "s-<tab>") . my/exwm-workspace-switch-monitor)
  
          ;; Perspectives
          (,(kbd "s-[") . perspective-exwm-cycle-exwm-buffers-backward)
          (,(kbd "s-]") . perspective-exwm-cycle-exwm-buffers-forward)
          (,(kbd "s-<mouse-4>") . perspective-exwm-cycle-exwm-buffers-backward)
          (,(kbd "s-<mouse-5>") . perspective-exwm-cycle-exwm-buffers-forward)
          (,(kbd "s-`") . perspective-exwm-switch-perspective)
          (,(kbd "s-o") . ,(my/app-command "rofi -show window"))
  
          ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (when (or (< ,i (exwm-workspace--count))
                                    (y-or-n-p (format "Create workspace %d" ,i)))
                            (exwm-workspace-switch-create ,i) ))))
                    (number-sequence 0 9))))
  
  (defun exwm-input--fake-last-command ()
    "Fool some packages into thinking there is a change in the buffer."
    (setq last-command #'exwm-input--noop)
    (condition-case hook-error
        (progn
          (run-hooks 'pre-command-hook)
          (run-hooks 'post-command-hook))
      ((error)
       (exwm--log "Error occurred while running command hooks: %s\n\nBacktrace:\n\n%s"
                  hook-error
                  (with-temp-buffer
                    (setq-local standard-output (current-buffer))
                    (backtrace)
                    (buffer-string))))))

  (set-frame-parameter (selected-frame) 'alpha '(90 . 90))
  (add-to-list 'default-frame-alist '(alpha . (90 . 90)))

  (perspective-exwm-mode)
  (exwm-enable))
