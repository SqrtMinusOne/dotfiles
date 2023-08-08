(define mpd
  (make <service>
    #:provides '(mpd)
    #:respawn? #t
    #:start (make-forkexec-constructor '("mpd" "--no-daemon"))
    #:stop (make-kill-destructor)))

(define sqrt-data-agent-mpd
  (make <service>
    #:provides '(sqrt-data-agent-mpd)
    #:respawn? #t
    #:start (make-forkexec-constructor '("sqrt_data_agent_mpd"))
    #:stop (make-kill-destructor)
    #:requires '(mpd)))

(define mcron
  (make <service>
    #:provides '(mcron)
    #:respawn? #t
    #:start (make-forkexec-constructor '("mcron"))
    #:stop (make-kill-destructor)))

(define aw-server
  (make <service>
    #:provides '(aw-server)
    #:respawn? #t
    #:start (make-forkexec-constructor '("aw-server"))
    #:stop (make-kill-destructor)))

(define aw-watcher-afk
  (make <service>
    #:provides '(aw-watcher-afk)
    #:requires '(aw-server)
    #:respawn? #t
    #:start (make-forkexec-constructor '("/home/pavel/bin/scripts/aw-watcher-afk-wrapper"))
    #:stop (make-kill-destructor)))

(define aw-watcher-window
  (make <service>
    #:provides '(aw-watcher-window)
    #:requires '(aw-server)
    #:respawn? #t
    #:start (make-forkexec-constructor '("aw-watcher-window"))
    #:stop (make-kill-destructor)))

(define pulseeffects
  (make <service>
    #:provides '(pulseeffects)
    #:respawn? #t
    #:start (make-forkexec-constructor '("flatpak" "run" "com.github.wwmm.pulseeffects" "--gapplication-service"))
    #:stop (make-kill-destructor)))

(define xsettingsd
  (make <service>
    #:provides '(xsettingsd)
    #:respawn? #t
    #:start (make-forkexec-constructor '("xsettingsd"))
    #:stop (make-kill-destructor)))

(define nm-applet
  (make <service>
    #:provides '(nm-applet)
    #:respawn? #t
    #:start (make-forkexec-constructor '("nm-applet"))
    #:stop (make-kill-destructor)))

(define discord-rich-presence
  (make <service>
    #:provides '(discord-rich-presence)
    #:one-shot? #t
    #:start (make-system-constructor "ln -sf {app/com.discordapp.Discord,$XDG_RUNTIME_DIR}/discord-ipc-0")))

(define polkit-gnome
  (make <service>
    #:provides '(polkit-gnome)
    #:respawn? #t
    #:start (make-forkexec-constructor '("/home/pavel/.guix-extra-profiles/desktop-misc/desktop-misc/libexec/polkit-gnome-authentication-agent-1"))
    #:stop (make-kill-destructor)))

(define xmodmap
  (make <service>
    #:provides '(xmodmap)
    #:one-shot? #t
    #:start (make-system-constructor "xmodmap /home/pavel/.Xmodmap")))

(define vpn
  (make <service>
    #:provides '(vpn)
    #:respawn? #t
    #:start (make-forkexec-constructor '("/home/pavel/bin/scripts/vpn-start"))
    #:stop (make-kill-destructor)))

(define davmail
  (make <service>
    #:provides '(davmail)
    #:respawn? #t
    #:start (make-forkexec-constructor '("/home/pavel/bin/davmail"))
    #:stop (make-kill-destructor)))

(define vnstatd
  (make <service>
    #:provides '(vnstatd)
    #:respawn? #t
    #:start (make-forkexec-constructor '("vnstatd" "-n"))
    #:stop (make-kill-destructor)))

(register-services
 mpd
 sqrt-data-agent-mpd
 mcron
 aw-server
 aw-watcher-afk
 aw-watcher-window
 pulseeffects
 xsettingsd
 ;; discord-rich-presence
 polkit-gnome
 vpn
 davmail
 ;; xmodmap
 nm-applet
 vnstatd)

(action 'shepherd 'daemonize)

(for-each start '(mpd
                  sqrt-data-agent-mpd
                  mcron
                  aw-server
                  aw-watcher-afk
                  aw-watcher-window
                  pulseeffects
                  xsettingsd
                  ; discord-rich-presence
                  polkit-gnome
                  davmail
                  ; xmodmap
                  nm-applet
                  vnstatd))
