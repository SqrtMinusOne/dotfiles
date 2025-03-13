(use-modules (shepherd service timer))

(define mpd
  (service '(mpd)
    #:respawn? #t
    #:start (make-forkexec-constructor '("mpd" "--no-daemon"))
    #:stop (make-kill-destructor)))

(define sqrt-data-agent-mpd
  (service '(sqrt-data-agent-mpd)
    #:respawn? #t
    #:start (make-forkexec-constructor '("sqrt_data_agent_mpd"))
    #:stop (make-kill-destructor)
    #:requirement '(mpd)))

(define deterred-mpd
  (service '(deterred-mpd)
    #:respawn? #t
    #:start (make-forkexec-constructor
             '("python" "/home/pavel/10-19 Code/13 Other Projects/13.02 sqrt-data/13.02.R Repos/13.02.R.05 deterred/watchers/deterred-mpd.py"
               "--db" "/home/pavel/.deterred/database.db"))
    #:stop (make-kill-destructor)
    #:requirement '(mpd)))

(define mcron
  (service '(mcron)
    #:respawn? #t
    #:start (make-forkexec-constructor '("mcron"))
    #:stop (make-kill-destructor)))

(define aw-server
  (service '(aw-server)
    #:respawn? #t
    #:start (make-forkexec-constructor '("/home/pavel/bin/scripts/aw-run" "aw-server"))
    #:stop (make-kill-destructor)))

(define aw-watcher-afk
  (service '(aw-watcher-afk)
    #:requirement '(aw-server)
    #:respawn? #t
    #:start (make-forkexec-constructor '("/home/pavel/bin/scripts/aw-run" "aw-watcher-afk"))
    #:stop (make-kill-destructor)))

(define aw-watcher-window
  (service '(aw-watcher-window)
    #:requirement '(aw-server)
    #:respawn? #t
    #:start (make-forkexec-constructor '("/home/pavel/bin/scripts/aw-run" "aw-watcher-window"))
    #:stop (make-kill-destructor)))

(define pulseeffects
  (service '(pulseeffects)
    #:respawn? #t
    #:start (make-forkexec-constructor '("flatpak" "run" "com.github.wwmm.pulseeffects" "--gapplication-service"))
    #:stop (make-kill-destructor)))

(define xsettingsd
  (service '(xsettingsd)
    #:respawn? #t
    #:start (make-forkexec-constructor '("xsettingsd"))
    #:stop (make-kill-destructor)))

(define nm-applet
  (service '(nm-applet)
    #:respawn? #t
    #:start (make-forkexec-constructor '("nm-applet"))
    #:stop (make-kill-destructor)))

(define discord-rich-presence
  (service '(discord-rich-presence)
    #:one-shot? #t
    #:start (make-system-constructor "ln -sf {app/com.discordapp.Discord,$XDG_RUNTIME_DIR}/discord-ipc-0")))

(define polkit-gnome
  (service '(polkit-gnome)
    #:respawn? #t
    #:start (make-forkexec-constructor
             (if (file-exists? "/home/pavel/.guix-extra-profiles/")
                 '("/home/pavel/.guix-extra-profiles/desktop-misc/desktop-misc/libexec/polkit-gnome-authentication-agent-1")
                 '("/usr/libexec/polkit-agent-helper-1")))
    #:stop (make-kill-destructor)))

(define xmodmap
  (service '(xmodmap)
    #:one-shot? #t
    #:start (make-system-constructor "xmodmap /home/pavel/.Xmodmap")))

(define vpn
  (service '(vpn)
    #:respawn? #t
    #:start (make-forkexec-constructor '("/home/pavel/bin/scripts/vpn-start"))
    #:stop (make-kill-destructor)))

(define davmail
  (service '(davmail)
    #:respawn? #t
    #:start (make-forkexec-constructor '("/home/pavel/bin/davmail"))
    #:stop (make-kill-destructor)))

(define vnstatd
  (service '(vnstatd)
    #:respawn? #t
    #:start (make-forkexec-constructor '("vnstatd" "-n"))
    #:stop (make-kill-destructor)))

(define opensnitchd
  (service '(opensnitchd)
    #:respawn? #t
    #:start (make-forkexec-constructor '("sudo" "opensnitchd"))
    #:stop (make-kill-destructor)))

(define opensnitch-ui
  (service '(opensnitch-ui)
    #:respawn? #t
    #:start (make-forkexec-constructor '("sudo" "opensnitch-ui"))
    #:stop (make-kill-destructor)))

(define ollama
  (service '(ollama)
    #:respawn? #t
    #:start (make-forkexec-constructor '("ollama" "serve"))
    #:stop (make-kill-destructor)))

(register-services
 mpd
 sqrt-data-agent-mpd
 deterred-mpd
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
 vnstatd
 ;; opensnitchd
 ;; opensnitch-ui
 ollama)

(perform-service-action root-service 'daemonize)

(for-each start-service
          (list
           mpd
           sqrt-data-agent-mpd
           deterred-mpd
           mcron
           aw-server
           aw-watcher-afk
           aw-watcher-window
           pulseeffects
           xsettingsd
           ;; discord-rich-presence
           ;; polkit-gnome
           davmail
           ;; ; xmodmap
           ;; nm-applet
           vnstatd
           ;; opensnitchd
           ;; opensnitch-ui
           ))
