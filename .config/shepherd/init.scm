(define mpd
  (make <service>
    #:provides '(mpd)
    #:respawn? #t
    #:start (make-forkexec-constructor '("mpd" "--no-daemon"))
    #:stop (make-kill-destructor)))

(define mpd-watcher
  (make <service>
    #:provides '(mpd-watcher)
    #:respawn? #t
    #:start (make-forkexec-constructor '("mpd_watcher"))
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
    #:start (make-forkexec-constructor '("aw-watcher-afk"))
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

(define discord-rich-presence
  (make <service>
    #:provides '(discord-rich-presence)
    #:one-shot? #t
    #:start (make-system-constructor "ln -sf {app/com.discordapp.Discord,$XDG_RUNTIME_DIR}/discord-ipc-0")))

(register-services
 mpd
 mpd-watcher
 mcron
 aw-server
 aw-watcher-afk
 aw-watcher-window
 pulseeffects
 xsettingsd
 discord-rich-presence)

(action 'shepherd 'daemonize)

(for-each start '(mpd mpd-watcher mcron aw-server aw-watcher-afk aw-watcher-window pulseeffects xsettingsd discord-rich-presence))
