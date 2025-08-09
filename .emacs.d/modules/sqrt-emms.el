;;; -*- lexical-binding: t -*-
(use-package emms
  :straight t
  :commands (emms-smart-browse
             emms-browser
             emms-add-url
             emms-add-file
             emms-add-find)
  :init
  (my-leader-def
    :infix "as"
    "" '(:which-key "emms")
    "s" (my/command-in-persp "EMMS" "EMMS" 0 (emms-smart-browse))
    "b" #'emms-browser
    "p" #'emms-pause
    "q" #'emms-stop
    ;; "h" #'emms-previous
    ;; "l" #'emms-next
    "u" #'emms-player-mpd-connect
    "ww" #'emms-lyrics
    "wb" #'emms-lyrics-toggle-display-on-minibuffer
    "wm" #'emms-lyrics-toggle-display-on-modeline
    "k" #'emms-volume-raise
    "j" #'emms-volume-lower)
  (my/persp-add-rule
    emms-browser-mode 0 "EMMS"
    emms-playlist-mode 0 "EMMS")
  (setq emms-mode-line-icon-enabled-p nil)
  :config
  (require 'emms-setup)
  (require 'emms-player-mpd)
  (require 'emms-player-mpv)
  (emms-all)
  ;; MPD setup
  (setq emms-source-file-default-directory (expand-file-name "~/Music/"))
  (add-to-list 'emms-info-functions 'emms-info-mpd)
  (add-to-list 'emms-player-list 'emms-player-mpd)
  (setq emms-player-mpd-server-name "localhost")
  (setq emms-player-mpd-server-port "6600")
  (setq emms-player-mpd-music-directory "~/Music")
  (emms-player-mpd-connect)
  (add-hook 'emms-playlist-cleared-hook 'emms-player-mpd-clear)
  (emms-player-set emms-player-mpd
                   'regex
                   (rx (or (: "https://" (* nonl) (or "acast.com") (* nonl))
                           (+ (? (or "https://" "http://"))
                              (* nonl)
                              (regexp (eval (emms-player-simple-regexp
                                             "m3u" "ogg" "flac" "mp3" "wav" "mod" "au" "aiff" "m4a")))))))
  ;; MPV setup
  (add-to-list 'emms-player-list 'emms-player-mpv t)
  (emms-player-set emms-player-mpv
                   'regex
                   (rx (or (: "https://" (* nonl) "youtube.com" (* nonl))
                           (+ (? (or "https://" "http://"))
                              (* nonl)
                              (regexp (eval (emms-player-simple-regexp
                                             "mp4" "mov" "wmv" "webm" "flv" "avi" "mkv")))))))
  (setq my/youtube-dl-quality-list
        '("bestvideo[height<=720]+bestaudio/best[height<=720]"
          "bestvideo[height<=480]+bestaudio/best[height<=480]"
          "bestvideo[height<=1080]+bestaudio/best[height<=1080]"))

  (setq my/default-emms-player-mpv-parameters
        '("--quiet" "--really-quiet" "--no-audio-display"))

  (defun my/set-emms-mpd-youtube-quality (quality)
    (interactive "P")
    (unless quality
      (setq quality (completing-read "Quality: " my/youtube-dl-quality-list nil t)))
    (setq emms-player-mpv-parameters
          `(,@my/default-emms-player-mpv-parameters ,(format "--ytdl-format=%s" quality))))

  (my/set-emms-mpd-youtube-quality (car my/youtube-dl-quality-list))
  ;; evil-lion and evil-commentary shadow some gX bindings
  ;; (add-hook 'emms-browser-mode-hook
  ;; (lambda ()
  ;; (evil-lion-mode -1)
  ;; (evil-commentary-mode -1)
  ;; ))
  ;; <I've just read the line below as "I hate everything">
  ;; I have everything I need in polybar
  (emms-mode-line-mode -1)
  (emms-playing-time-display-mode -1)
  (defun emms-info-mpd-process (track info)
    (dolist (data info)
      (let ((name (car data))
            (value (cdr data)))
        (setq name (cond ((string= name "artist") 'info-artist)
                         ((string= name "albumartist") 'info-albumartist)
                         ((string= name "composer") 'info-composer)
                         ((string= name "performer") 'info-performer)
                         ((string= name "title") 'info-title)
                         ((string= name "album") 'info-album)
                         ((string= name "track") 'info-tracknumber)
                         ((string= name "disc") 'info-discnumber)
                         ((string= name "date") 'info-year)
                         ((string= name "genre") 'info-genre)
                         ((string= name "time")
                          (setq value (string-to-number value))
                          'info-playing-time)
                         (t nil)))
        (when name
          (emms-track-set track name value)))))
  (defun emms-player-mpd-get-alists (info)
    "Turn the given parsed INFO from MusicPD into an list of alists.

  The list will be in reverse order."
    (when (and info
               (null (car info))          ; no error has occurred
               (cdr info))                ; data exists
      (let ((alists nil)
            (alist nil)
            cell)
        (dolist (line (cdr info))
          (when (setq cell (emms-player-mpd-parse-line line))
            (if (member (car cell) '("file" "directory" "playlist"))
                (setq alists (cons alist alists)
                      alist (list cell))
              (setq alist (cons cell alist)))))
        (when alist
          (setq alists (cons alist alists)))
        alists))))

(defun my/emms-cleanup-urls ()
  (interactive)
  (let ((keys-to-delete '()))
    (maphash (lambda (key value)
               (when (eq (cdr (assoc 'type value)) 'url)
                 (add-to-list 'keys-to-delete key)))
             emms-cache-db)
    (dolist (key keys-to-delete)
      (remhash key emms-cache-db)))
  (setq emms-cache-dirty t))

(my-leader-def "asc" #'my/emms-cleanup-urls)

(use-package lyrics-fetcher
  :straight t
  :after (emms)
  :init
  (my-leader-def
    "ast" #'lyrics-fetcher-show-lyrics
    "asT" #'lyrics-fetcher-show-lyrics-query)
  :config
  (setq lyrics-fetcher-genius-access-token
        (my/password-store-get "My_Online/APIs/genius.com"))
  (general-define-key
   :states '(emacs normal)
   :keymaps 'emms-browser-mode-map
   "gr" #'emms-browse-by-artist
   "gl" 'lyrics-fetcher-emms-browser-show-at-point
   "gC" 'lyrics-fetcher-emms-browser-fetch-covers-at-point
   "go" 'lyrics-fetcher-emms-browser-open-large-cover-at-point)

  (advice-add #'emms-lyrics-mode-line
              :override #'my/emms-lyrics-mode-line-override))

(defun my/emms-lyrics-mode-line-override ()
  (add-to-list 'global-mode-string
               '(:eval emms-lyrics-mode-line-string)))

(defun my/emms-lyrics-restore-mode-line-override ()
  "Restore the mode line."
  (setq global-mode-string
        (remove '(:eval emms-lyrics-mode-line-string) global-mode-string))
  (force-mode-line-update))

(with-eval-after-load 'emms-lyrics
  (advice-add #'emms-lyrics-mode-line
              :override #'my/emms-lyrics-mode-line-override)
  (advice-add #'emms-lyrics-restore-mode-line
              :override #'my/emms-lyrics-restore-mode-line-override))

(with-eval-after-load 'emms-browser
  (general-define-key
   :states '(normal)
   :keymaps 'emms-browser-mode-map
   "q" 'quit-window))

(with-eval-after-load 'emms
  (general-define-key
   :states '(normal)
   :keymaps 'emms-playlist-mode-map
   "q" 'quit-window))

(defun my/set-volume (value)
  (start-process "ponymix" nil "ponymix"
                 (if (< 0 value) "increase" "decrease")
                 (number-to-string (abs value))
                 "--max-volume" "150"))

(setq emms-volume-change-function #'my/set-volume)
(setq emms-volume-change-amount 5)

(provide 'sqrt-emms)
