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

(register-services
 mpd
 mpd-watcher
 mcron
 aw-server
 aw-watcher-afk
 aw-watcher-window)

(action 'shepherd 'daemonize)

(for-each start '(mpd mpd-watcher mcron aw-server aw-watcher-afk aw-watcher-window))
