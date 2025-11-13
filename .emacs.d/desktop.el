(defun my/exwm-run-polybar ()
  (interactive)
  (call-process "~/bin/polybar.sh"))

(defun my/exwm-set-wallpaper ()
  (call-process-shell-command "feh --bg-fill ~/Pictures/wallpaper.jpg"))

(defun my/exwm-run-shepherd ()
  (when (and (string-empty-p (shell-command-to-string "pgrep -u pavel shepherd"))
             (executable-find "shepherd"))
    (call-process "shepherd")))

(defun my/exwm-run-systemd ()
  (call-process "systemctl" nil nil nil "--user" "start" "wm-session.target"))

(use-package pinentry
  :straight t
  :after (exwm)
  :config
  (setenv "GPG_AGENT_INFO" nil) ;; use emacs pinentry
  (setq auth-source-debug t)

  (setq epg-gpg-program "gpg") ;; not necessary
  (require 'epa-file)
  (epa-file-enable)
  (setq epa-pinentry-mode 'loopback)
  (setq epg-pinentry-mode 'loopback)
  (pinentry-start))

(use-package exwm-modeline
  :straight t
  :config
  (add-hook 'exwm-init-hook #'exwm-modeline-mode)
  (my/use-colors
   (exwm-modeline-current-workspace
    :foreground (my/color-value 'yellow)
    :weight 'bold)))

(defun my/is-arch ()
  (file-exists-p "/etc/arch-release"))

(require 'windmove)

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
  (setq perspective-exwm-cycle-max-message-length 180)
  (general-define-key
   :keymaps 'perspective-map
   "e" #'perspective-exwm-move-to-workspace
   "E" #'perspective-exwm-copy-to-workspace))

(defun my/exwm-configure-window ()
  (interactive)
  (unless exwm--floating-frame
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
      ((or "VK" "Slack" "discord" "TelegramDesktop" "Rocket.Chat")
       (perspective-exwm-assign-window
        :workspace-index 3
        :persp-name "comms"))
      ((or "Chromium-browser" "jetbrains-datagrip")
       (perspective-exwm-assign-window
        :workspace-index 4
        :persp-name "dev")))))

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

(defun my/exwm-xrandr-monitor-list ()
  (split-string
   (string-trim
    (shell-command-to-string "xrandr --listmonitors | grep '+' | awk {'print $4'}
"))))

(setq my/exwm-monitor-list
      (pcase (system-name)
        ("indigo" '(nil "DVI-D-0"))
        ("violet" (my/exwm-xrandr-monitor-list))
        (_ '(nil))))

(defun my/exwm-get-current-monitor ()
  "Return the current monitor name or nil."
  (or
   (plist-get exwm-randr-workspace-monitor-plist
              (cl-position (selected-frame)
                           exwm-workspace--list))
   (car my/exwm-monitor-list)))

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
     (or
      (cl-loop with other-monitor = (my/exwm-get-other-monitor (or dir 'right))
               for i in (append my/exwm-last-workspaces
                                (cl-loop for i from 0
                                         for _ in exwm-workspace--list
                                         collect i))
               if (if other-monitor
                      (string-equal (plist-get exwm-randr-workspace-monitor-plist i)
                                    other-monitor)
                    (not (plist-get exwm-randr-workspace-monitor-plist i)))
               return i)))))

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

(defun my/exwm-refresh-monitors (&optional refresh)
  (interactive (list t))
  (setq my/exwm-monitor-list (my/exwm-xrandr-monitor-list))
  (cl-loop for i from 0 to (1- exwm-workspace-number)
           for monitor = (plist-get exwm-randr-workspace-monitor-plist
                                    i)
           if (not (member monitor my/exwm-monitor-list))
           do
           (setf (plist-get exwm-randr-workspace-monitor-plist i)
                 (car my/exwm-monitor-list)))
  (when refresh
    (exwm-randr-refresh)))

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

(use-package app-launcher
  :straight '(app-launcher :host github :repo "SebastienWae/app-launcher"))

(use-package password-store-completion
  :straight (:host github :repo "SqrtMinusOne/password-store-completion")
  :after (exwm)
  :config
  (add-to-list 'savehist-additional-variables 'password-store-completion)
  (require 'password-store-embark)
  (password-store-embark-mode))

(use-package emojify
  :straight t)

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

(defhydra my/exwm-apps-hydra (:color blue :hint nil)
  "
^Apps^
_t_: Terminal (Alacritty)
_b_: Browser (Firefox)
_s_: Rocket.Chat
_e_: Element
_d_: Discord
"
  ("t" (lambda () (interactive) (my/run-in-background "alacritty")))
  ("b" (lambda () (interactive) (my/run-in-background "firefox")))
  ("s" (lambda () (interactive) (my/run-in-background "flatpak run chat.rocket.RocketChat")))
  ("e" (lambda () (interactive) (my/run-in-background "flatpak run im.riot.Riot")))
  ("d" (lambda () (interactive) (my/run-in-background "flatpak run com.discordapp.Discord"))))

(defun my/exwm-lock ()
  (interactive)
  (my/run-in-background "i3lock -f -i /home/pavel/Pictures/lock-wallpaper.png"))

(defun my/fix-exwm-floating-windows ()
  (setq-local exwm-workspace-warp-cursor nil)
  (setq-local mouse-autoselect-window nil)
  (setq-local focus-follows-mouse nil))

(add-hook 'exwm-floating-setup-hook #'my/fix-exwm-floating-windows)

(defun exwm--on-ClientMessage-old (raw-data _synthetic)
  "Handle ClientMessage event."
  (let ((obj (make-instance 'xcb:ClientMessage))
        type id data)
    (xcb:unmarshal obj raw-data)
    (setq type (slot-value obj 'type)
          id (slot-value obj 'window)
          data (slot-value (slot-value obj 'data) 'data32))
    (exwm--log "atom=%s(%s)" (x-get-atom-name type exwm-workspace--current)
               type)
    (cond
     ;; _NET_NUMBER_OF_DESKTOPS.
     ((= type xcb:Atom:_NET_NUMBER_OF_DESKTOPS)
      (let ((current (exwm-workspace--count))
            (requested (elt data 0)))
        ;; Only allow increasing/decreasing the workspace number by 1.
        (cond
         ((< current requested)
          (make-frame))
         ((and (> current requested)
               (> current 1))
          (let ((frame (car (last exwm-workspace--list))))
            (exwm-workspace--get-remove-frame-next-workspace frame)
            (delete-frame frame))))))
     ;; _NET_CURRENT_DESKTOP.
     ((= type xcb:Atom:_NET_CURRENT_DESKTOP)
      (exwm-workspace-switch (elt data 0)))
     ;; _NET_ACTIVE_WINDOW.
     ((= type xcb:Atom:_NET_ACTIVE_WINDOW)
      (let ((buffer (exwm--id->buffer id))
            iconic window)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when (eq exwm--frame exwm-workspace--current)
              (if exwm--floating-frame
                  (select-frame exwm--floating-frame)
                (setq iconic (exwm-layout--iconic-state-p))
                (when iconic
                  ;; State change: iconic => normal.
                  (set-window-buffer (frame-selected-window exwm--frame)
                                     (current-buffer)))
                ;; Focus transfer.
                (setq window (get-buffer-window nil t))
                (when (or iconic
                          (not (eq window (selected-window))))
                  (select-window window))))))))
     ;; _NET_CLOSE_WINDOW.
     ((= type xcb:Atom:_NET_CLOSE_WINDOW)
      (let ((buffer (exwm--id->buffer id)))
        (when (buffer-live-p buffer)
          (exwm--defer 0 #'kill-buffer buffer))))
     ;; _NET_WM_MOVERESIZE
     ((= type xcb:Atom:_NET_WM_MOVERESIZE)
      (let ((direction (elt data 2))
            (buffer (exwm--id->buffer id)))
        (unless (and buffer
                     (not (buffer-local-value 'exwm--floating-frame buffer)))
          (cond ((= direction
                    xcb:ewmh:_NET_WM_MOVERESIZE_SIZE_KEYBOARD)
                 ;; FIXME
                 )
                ((= direction
                    xcb:ewmh:_NET_WM_MOVERESIZE_MOVE_KEYBOARD)
                 ;; FIXME
                 )
                ((= direction xcb:ewmh:_NET_WM_MOVERESIZE_CANCEL)
                 (exwm-floating--stop-moveresize))
                ;; In case it's a workspace frame.
                ((and (not buffer)
                      (catch 'break
                        (dolist (f exwm-workspace--list)
                          (when (or (eq id (frame-parameter f 'exwm-outer-id))
                                    (eq id (frame-parameter f 'exwm-id)))
                            (throw 'break t)))
                        nil)))
                (t
                 ;; In case it's a floating frame,
                 ;; move the corresponding X window instead.
                 (unless buffer
                   (catch 'break
                     (dolist (pair exwm--id-buffer-alist)
                       (with-current-buffer (cdr pair)
                         (when
                             (and exwm--floating-frame
                                  (or (eq id
                                          (frame-parameter exwm--floating-frame
                                                           'exwm-outer-id))
                                      (eq id
                                          (frame-parameter exwm--floating-frame
                                                           'exwm-id))))
                           (setq id exwm--id)
                           (throw 'break nil))))))
                 ;; Start to move it.
                 (exwm-floating--start-moveresize id direction))))))
     ;; _NET_REQUEST_FRAME_EXTENTS
     ((= type xcb:Atom:_NET_REQUEST_FRAME_EXTENTS)
      (let ((buffer (exwm--id->buffer id))
            top btm)
        (if (or (not buffer)
                (not (buffer-local-value 'exwm--floating-frame buffer)))
            (setq top 0
                  btm 0)
          (setq top (window-header-line-height)
                btm (window-mode-line-height)))
        (xcb:+request exwm--connection
            (make-instance 'xcb:ewmh:set-_NET_FRAME_EXTENTS
                           :window id
                           :left 0
                           :right 0
                           :top top
                           :bottom btm)))
      (xcb:flush exwm--connection))
     ;; _NET_WM_DESKTOP.
     ((= type xcb:Atom:_NET_WM_DESKTOP)
      (let ((buffer (exwm--id->buffer id)))
        (when (buffer-live-p buffer)
          (exwm-workspace-move-window (elt data 0) id))))
     ;; _NET_WM_STATE
     ((= type xcb:Atom:_NET_WM_STATE)
      (let ((action (elt data 0))
            (props (list (elt data 1) (elt data 2)))
            (buffer (exwm--id->buffer id))
            props-new)
        ;; only support _NET_WM_STATE_FULLSCREEN / _NET_WM_STATE_ADD for frames
        (when (and (not buffer)
                   (memq xcb:Atom:_NET_WM_STATE_FULLSCREEN props)
                   (= action xcb:ewmh:_NET_WM_STATE_ADD))
          (xcb:+request
              exwm--connection
              (make-instance 'xcb:ewmh:set-_NET_WM_STATE
                             :window id
                             :data (vector xcb:Atom:_NET_WM_STATE_FULLSCREEN)))
          (xcb:flush exwm--connection))
        (when buffer                    ;ensure it's managed
          (with-current-buffer buffer
            ;; _NET_WM_STATE_FULLSCREEN
            (when (or (memq xcb:Atom:_NET_WM_STATE_FULLSCREEN props)
                      (memq xcb:Atom:_NET_WM_STATE_ABOVE props))
              (cond ((= action xcb:ewmh:_NET_WM_STATE_ADD)
                     (unless (exwm-layout--fullscreen-p)
                       (exwm-layout-set-fullscreen id))
                     (push xcb:Atom:_NET_WM_STATE_FULLSCREEN props-new))
                    ((= action xcb:ewmh:_NET_WM_STATE_REMOVE)
                     (when (exwm-layout--fullscreen-p)
                       (exwm-layout-unset-fullscreen id)))
                    ((= action xcb:ewmh:_NET_WM_STATE_TOGGLE)
                     (if (exwm-layout--fullscreen-p)
                         (exwm-layout-unset-fullscreen id)
                       (exwm-layout-set-fullscreen id)
                       (push xcb:Atom:_NET_WM_STATE_FULLSCREEN props-new)))))
            ;; _NET_WM_STATE_DEMANDS_ATTENTION
            ;; FIXME: check (may require other properties set)
            (when (memq xcb:Atom:_NET_WM_STATE_DEMANDS_ATTENTION props)
              (when (= action xcb:ewmh:_NET_WM_STATE_ADD)
                (unless (eq exwm--frame exwm-workspace--current)
                  (set-frame-parameter exwm--frame 'exwm-urgency t)
                  (setq exwm-workspace--switch-history-outdated t)))
              ;; xcb:ewmh:_NET_WM_STATE_REMOVE?
              ;; xcb:ewmh:_NET_WM_STATE_TOGGLE?
              )
            (xcb:+request exwm--connection
                (make-instance 'xcb:ewmh:set-_NET_WM_STATE
                               :window id :data (vconcat props-new)))
            (xcb:flush exwm--connection)))))
     ((= type xcb:Atom:WM_PROTOCOLS)
      (let ((type (elt data 0)))
        (cond ((= type xcb:Atom:_NET_WM_PING)
               (setq exwm-manage--ping-lock nil))
              (t (exwm--log "Unhandled WM_PROTOCOLS of type: %d" type)))))
     ((= type xcb:Atom:WM_CHANGE_STATE)
      (let ((buffer (exwm--id->buffer id)))
        (when (and (buffer-live-p buffer)
                   (= (elt data 0) xcb:icccm:WM_STATE:IconicState))
          (with-current-buffer buffer
            (if exwm--floating-frame
                (call-interactively #'exwm-floating-hide)
              (bury-buffer))))))
     (t
      (exwm--log "Unhandled: %s(%d)"
                 (x-get-atom-name type exwm-workspace--current) type)))))

(with-eval-after-load 'exwm
  (advice-add 'exwm--on-ClientMessage :override #'exwm--on-ClientMessage-old))

(setq exwm-manage-configurations
   '(((member exwm-class-name '("Nyxt"))
	   char-mode t)))

(defun my/exwm-init ()
  (exwm-workspace-switch 1)

  (my/exwm-set-wallpaper)
  (my/exwm-run-shepherd)
  (my/exwm-run-systemd)
  (my/run-in-background "gpgconf --reload gpg-agent")
  (my/exwm-run-polybar)
  (setenv "DBUS_SESSION_BUS_ADDRESS" "unix:path=/run/user/1000/bus")
  (when (my/is-arch)
    (my/run-in-background "set_layout")))

(defun my/exwm-update-class ()
  (exwm-workspace-rename-buffer (format "EXWM :: %s" exwm-class-name)))

(defun my/exwm-set-alpha (alpha)
  (setf (alist-get 'alpha default-frame-alist)
        `(,alpha . ,alpha))
  (cl-loop for frame being the frames
           do (set-frame-parameter frame 'alpha `(,alpha . ,alpha))))

(use-package exwm
  :straight t
  :config
  (setq exwm-workspace-number 5)
  (add-hook 'exwm-init-hook #'my/exwm-init)
  (add-hook 'exwm-update-class-hook #'my/exwm-update-class)

  (require 'exwm-randr)
  (exwm-randr-mode 1)
  (start-process-shell-command "xrandr" nil "~/bin/scripts/screen-layout")
  (when (string= (system-name) "violet")
    (setq my/exwm-another-monitor "DP-1")
    (setq exwm-randr-workspace-monitor-plist `(2 ,my/exwm-another-monitor 3 ,my/exwm-another-monitor))
    (my/exwm-refresh-monitors))

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
   "<mode-line> s-<mouse-4>" #'perspective-exwm-cycle-all-buffers-backward
   "<mode-line> s-<mouse-5>" #'perspective-exwm-cycle-all-buffers-forward
   "M-x" #'execute-extended-command
   "M-SPC" (general-key "SPC"))
  (setopt exwm-input-simulation-keys `(
                                     ;; (,(kbd "M-w") . ,(kbd "C-w"))
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
          (,(kbd "s-e") . persp-switch-to-buffer*)
          ;; (,(kbd "s-E") . my/persp-ivy-switch-buffer-other-window)
  
          ;; Resize windows
          (,(kbd "s-r") . my/exwm-resize-hydra/body)
  
          ;; Apps & stuff
          (,(kbd "s-p") . app-launcher-run-app)
          (,(kbd "s-P") . async-shell-command)
          (,(kbd "s-;") . my/exwm-apps-hydra/body)
          (,(kbd "s--") . password-store-completion)
          (,(kbd "s-=") . emoji-insert)
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
  
          ;; Input method
          (,(kbd "M-\\") . my/toggle-input-method)
  
          ;; Switch workspace
          (,(kbd "s-q") . my/exwm-switch-to-other-monitor)
          (,(kbd "s-w") . exwm-workspace-switch)
          (,(kbd "s-W") . exwm-workspace-move-window)
          (,(kbd "s-<tab>") . my/exwm-workspace-switch-monitor)
  
          ;; Perspectives
          (,(kbd "s-{") . perspective-exwm-cycle-all-buffers-backward)
          (,(kbd "s-}") . perspective-exwm-cycle-all-buffers-forward)
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

  (if (my/light-p)
      (my/exwm-set-alpha 100)
    (my/exwm-set-alpha 90))

  (perspective-exwm-mode)
  (exwm-enable))
