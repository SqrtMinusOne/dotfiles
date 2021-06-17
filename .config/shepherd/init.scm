(define mpd
  (make <service>
    #:provides '(mpd)
    #:respawn? #t
    #:start (make-system-constructor "mpd")
    #:stop (make-kill-destructor)))

(define mpd-watcher
  (make <service>
    #:provides '(mpd-watcher)
    #:respawn? #t
    #:start (make-forkexec-constructor '("mpd_watcher"))
    #:stop (make-kill-destructor)
    #:requires '(mpd)))

(register-services mpd mpd-watcher)

(action 'shepherd 'daemonize)

(for-each start '(mpd mpd-watcher))
