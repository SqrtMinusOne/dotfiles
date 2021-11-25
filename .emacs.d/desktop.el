(defun my/exwm-run-polybar ()
  (call-process "~/bin/polybar.sh"))

(defun my/exwm-set-wallpaper ()
  (call-process-shell-command "feh --bg-fill ~/Pictures/wallpaper.jpg"))

(defun my/exwm-run-shepherd ()
  (when (string-empty-p (shell-command-to-string "pgrep -u pavel shepherd"))
    (call-process "shepherd")))

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

(use-package transient
  :straight t)

(setq my/exwm-resize-value 5)

(defun my/exwm-resize-window (dir kind &optional value)
  (unless value
    (setq value my/exwm-resize-value))
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
        (evil-window-increase-height value))))))

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

(defun my/cycle-persp-exwm-buffers (dir)
  (let* ((current (current-buffer))
         (ignore-rx (persp--make-ignore-buffer-rx))
         (visible-buffers '())
         (exwm-data
          (cl-loop for buf in (persp-current-buffers)
                   for is-another = (and (get-buffer-window buf) (not (eq current buf)))
                   if (and (buffer-live-p buf)
                           (eq 'exwm-mode (buffer-local-value 'major-mode buf))
                           (not (string-match-p ignore-rx (buffer-name buf))))
                   collect buf into all-buffers
                   and if (not is-another) collect buf into cycle-buffers
                   finally (return (list all-buffers cycle-buffers))))
         (all-buffers (nth 0 exwm-data))
         (cycle-buffers (nth 1 exwm-data))
         (current-pos (or (cl-position current cycle-buffers) -1)))
    (if (seq-empty-p cycle-buffers)
        (message "No EXWM buffers to cycle!")
      (let* ((next-pos (% (+ current-pos (length cycle-buffers)
                             (if (eq dir 'forward) 1 -1))
                          (length cycle-buffers)))
             (next-buffer (nth next-pos cycle-buffers)))
        (switch-to-buffer next-buffer)
        (message
         "%s"
         (mapconcat
          (lambda (buf)
            (let ((name (string-replace "EXWM :: " "" (buffer-name buf))))
              (cond
               ((eq (current-buffer) buf)
                (concat
                 "["
                 (propertize name 'face `(foreground-color . ,(doom-color 'yellow)))
                 "]"))
               ((not (member buf cycle-buffers))
                (concat
                 "["
                 (propertize name 'face `(foreground-color . ,(doom-color 'blue)))
                 "]"))
               (t (format " %s " name)))))
          all-buffers
          " "))))))

(defun my/add-exwm-buffers-to-current-perspective ()
  (interactive)
  (let ((ignore-rx (persp--make-ignore-buffer-rx)))
    (cl-loop for buf in (buffer-list)
             if (and (buffer-live-p buf)
                     (eq 'exwm-mode (buffer-local-value 'major-mode buf))
                     (not (string-match-p ignore-rx (buffer-name buf))))
             do (persp-add-buffer (buffer-name buf)))))

(defun my/exwm-revive-perspectives ()
  "Make perspectives in the current frame not killed."
  (interactive)
  (let ((to-switch nil))
    (maphash
     (lambda (_ v)
       (setf (persp-killed v) nil)
       (unless to-switch
         (setq to-switch v)))
     (frame-parameter nil 'persp--hash))
    (when to-switch
      (persp-switch (persp-name to-switch)))))

(defun my/exwm-lock ()
  (interactive)
  (my/run-in-background "i3lock -f -i /home/pavel/Pictures/lock-wallpaper.png"))

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
  
          ;; Resize windows
          (,(kbd "s-r") . my/exwm-resize-hydra/body)
  
          ;; Apps & stuff
          (,(kbd "s-p") . ,(my/app-command "rofi -modi drun,run -show drun"))
          (,(kbd "s-;") . my/exwm-apps)
          (,(kbd "s--") . ,(my/app-command "rofi-pass"))
          (,(kbd "s-=") . ,(my/app-command "rofimoji"))
  
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
  
          ;; Cycle EXWM windows in the current perspective
          (,(kbd "s-[") . (lambda () (interactive) (my/cycle-persp-exwm-buffers 'backward)))
          (,(kbd "s-]") . (lambda () (interactive) (my/cycle-persp-exwm-buffers 'forward)))
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

  (set-frame-parameter (selected-frame) 'alpha '(90 . 90))
  (add-to-list 'default-frame-alist '(alpha . (90 . 90)))

  (exwm-enable))
