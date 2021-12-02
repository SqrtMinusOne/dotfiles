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
  (pinentry-start)
  (my/run-in-background "gpgconf --reload gpg-agent"))

(defun my/exwm-direction-exists-p (dir)
  (cl-some (lambda (dir)
          (let ((win (windmove-find-other-window dir)))
            (and win (not (window-minibuffer-p win)))))
        (pcase dir
          ('width '(left right))
          ('height '(up down)))))

(defun my/exwm-move-window (dir)
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

(use-package perspective-exwm
  :straight (:host github :repo "SqrtMinusOne/perspective-exwm.el")
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

(defun my/exwm-workspace-switch-monitor ()
  (interactive)
  (if (plist-get exwm-randr-workspace-monitor-plist exwm-workspace-current-index)
      (setq exwm-randr-workspace-monitor-plist
            (map-delete exwm-randr-workspace-monitor-plist exwm-workspace-current-index))
    (setq exwm-randr-workspace-monitor-plist
          (plist-put exwm-randr-workspace-monitor-plist
                     exwm-workspace-current-index
                     my/exwm-another-monitor)))
  (exwm-randr-refresh))

(use-package transient
  :straight t)

(defun my/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(transient-define-prefix my/exwm-apps ()
  ["Apps"
   ("t" "Termnial (Alacritty)" (lambda () (interactive) (my/run-in-background "alacritty")))
   ("b" "Browser (Firefox)" (lambda () (interactive) (my/run-in-background "firefox")))
   ("v" "VK" (lambda () (interactive) (my/run-in-background "vk")))
   ("s" "Slack" (lambda () (interactive) (my/run-in-background "slack-wrapper")))
   ("d" "Discord" (lambda () (interactive) (my/run-in-background "flatpak run com.discordapp.Discord")))
   ("q" "Quit" transient-quit-one)])

(defun my/exwm-lock ()
  (interactive)
  (my/run-in-background "i3lock -f -i /home/pavel/Pictures/lock-wallpaper.png"))

(defun my/exwm-update-global-keys ()
  (interactive)
  (setq exwm-input--global-keys nil)
  (dolist (i exwm-input-global-keys)
    (exwm-input--set-key (car i) (cdr i)))
  (when exwm--connection
    (exwm-input--update-global-prefix-keys)))

(defun my/exwm-init ()
  (exwm-workspace-switch 1)

  (my/exwm-run-polybar)
  (my/exwm-set-wallpaper)
  (my/exwm-run-shepherd)
  ;; (with-eval-after-load 'perspective
  ;;   (my/exwm-setup-perspectives))
  )

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

  (setq my/exwm-monitor-workspace '())
  
  (defun my/exwm-get-current-monitor ()
    (if (plist-get exwm-randr-workspace-monitor-plist exwm-workspace-current-index)
        1 0))
  
  (defun my/exwm-update-current-monitor ()
    (setf (alist-get (my/exwm-get-current-monitor) my/exwm-monitor-workspace)
          exwm-workspace-current-index))
  
  (add-hook 'exwm-workspace-switch-hook
            #'my/exwm-update-current-monitor)
  (defun my/exwm-switch-to-other-monitor ()
    (interactive)
    (let* ((current (my/exwm-get-current-monitor))
           (other (seq-some
                   (lambda (m)
                     (and (not (= (car m) current)) (cdr m)))
                   my/exwm-monitor-workspace))
           (focus-follows-mouse nil)
           (mouse-autoselect-window nil))
      (exwm-workspace-switch other)))
  (setq exwm-input-prefix-keys
        `(?\C-x
          ?\C-w
          ?\M-x
          ?\M-u))
  (defmacro my/app-command (command)
    `(lambda () (interactive) (my/run-in-background ,command)))
  
  (general-define-key
   :keymaps '(exwm-mode-map)
   "C-q" 'exwm-input-send-next-key
   "<print>" (my/app-command "flameshot gui")
   "<mode-line> s-<mouse-4>" 'perspective-exwm-cycle-exwm-buffers-backward
   "<mode-line> s-<mouse-5>" 'perspective-exwm-cycle-exwm-buffers-forward
   "M-x" 'counsel-M-x
   "M-SPC" (general-key "SPC"))
  (setq exwm-input-simulation-keys `((,(kbd "M-w") . ,(kbd "C-w"))
                                     (,(kbd "M-c") . ,(kbd "C-c"))))
  (setq exwm-input-global-keys
        `(
          ;; Reset to line-mode
          (,(kbd "s-R") . exwm-reset)
  
          ;; Switch windows
          (,(kbd "s-<left>"). windmove-left)
          (,(kbd "s-<right>") . windmove-right)
          (,(kbd "s-<up>") . windmove-up)
          (,(kbd "s-<down>") . windmove-down)
  
          (,(kbd "s-h"). windmove-left)
          (,(kbd "s-l") . windmove-right)
          (,(kbd "s-k") . windmove-up)
          (,(kbd "s-j") . windmove-down)
  
          ;; Moving windows
          (,(kbd "s-H") . (lambda () (interactive) (my/exwm-move-window 'left)))
          (,(kbd "s-L") . (lambda () (interactive) (my/exwm-move-window 'right)))
          (,(kbd "s-K") . (lambda () (interactive) (my/exwm-move-window 'up)))
          (,(kbd "s-J") . (lambda () (interactive) (my/exwm-move-window 'down)))
  
          ;; Fullscreen
          (,(kbd "s-f") . exwm-layout-toggle-fullscreen)
          (,(kbd "s-F") . exwm-floating-toggle-floating)
  
          ;; Quit
          (,(kbd "s-Q") . evil-quit)
  
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
          (,(kbd "s-p") . ,(my/app-command "rofi -modi drun,run -show drun"))
          (,(kbd "s-;") . my/exwm-apps)
          (,(kbd "s--") . ,(my/app-command "rofi-pass"))
          (,(kbd "s-=") . ,(my/app-command "rofimoji"))
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
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))
  (defvar my/exwm-mode-line-info "")
  
  (add-to-list 'mode-line-misc-info
               '(:eval my/exwm-mode-line-info))
  
  (defun my/exwm-mode-line-info-update ()
    (setq my/exwm-mode-line-info
          (concat
           "["
           (propertize (funcall exwm-workspace-index-map exwm-workspace-current-index)
                       'face
                       `(foreground-color . ,(doom-color 'yellow)))
           "]"))
    (setq my/exwm-mode-line-info-no-props (funcall exwm-workspace-index-map exwm-workspace-current-index))
    (force-mode-line-update))
  
  (add-hook 'exwm-workspace-switch-hook #'my/exwm-mode-line-info-update)
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
