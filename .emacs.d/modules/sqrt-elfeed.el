;;; -*- lexical-binding: t -*-
(use-package elfeed
  :straight (:repo "SqrtMinusOne/elfeed" :host github)
  :if (not (or my/is-termux my/remote-server))
  :commands (elfeed)
  :init
  (my-leader-def "ae" #'elfeed-summary)
  (my/persp-add-rule
    elfeed-summary-mode 0 "elfeed"
    elfeed-search-mode 0 "elfeed"
    elfeed-show-mode 0 "elfeed")
  (setq shr-max-image-proportion 0.5)
  :config
  (setq elfeed-db-directory "~/.elfeed")
  (setq elfeed-enclosure-default-dir (expand-file-name "~/Downloads"))
  ;; (advice-add #'elfeed-insert-html
  ;;             :around
  ;;             (lambda (fun &rest r)
  ;;               (let ((shr-use-fonts nil))
  ;;                 (apply fun r))))
  (general-define-key
   :states '(normal)
   :keymaps 'elfeed-search-mode-map
   "o" #'my/elfeed-search-filter-source
   "c" #'elfeed-search-clear-filter
   "gl" (lambda () (interactive) (elfeed-search-set-filter "+later")))
  (general-define-key
   :states '(normal)
   :keymaps 'elfeed-show-mode-map
   "ge" #'my/elfeed-show-visit-eww))

(use-package elfeed-org
  :straight t
  :after (elfeed)
  :config
  (setq rmh-elfeed-org-files '("~/.emacs.d/private.org"))
  (elfeed-org))

(defun my/elfeed-search-filter-source (entry)
  "Filter elfeed search buffer by the feed under cursor."
  (interactive (list (elfeed-search-selected :ignore-region)))
  (when (elfeed-entry-p entry)
    (elfeed-search-set-filter
     (concat
      "@6-months-ago "
      "+unread "
      "="
      (replace-regexp-in-string
       (rx "?" (* not-newline) eos)
       ""
       (elfeed-feed-url (elfeed-entry-feed entry)))))))

(defun my/elfeed-show-visit-eww ()
  "Visit the current entry in eww"
  (interactive)
  (let ((link (elfeed-entry-link elfeed-show-entry)))
    (when link
      (eww link))))

(defface elfeed-videos-entry nil
  "Face for the elfeed entries with tag \"videos\"")

(defface elfeed-twitter-entry nil
  "Face for the elfeed entries with tah \"twitter\"")

(defface elfeed-emacs-entry nil
  "Face for the elfeed entries with tah \"emacs\"")

(defface elfeed-music-entry nil
  "Face for the elfeed entries with tah \"music\"")

(defface elfeed-podcasts-entry nil
  "Face for the elfeed entries with tag \"podcasts\"")

(defface elfeed-blogs-entry nil
  "Face for the elfeed entries with tag \"blogs\"")

(defface elfeed-govt-entry nil
  "Face for the elfeed entries with tag \"blogs\"")

(my/use-colors
 (elfeed-search-tag-face :foreground (my/color-value 'yellow))
 (elfeed-videos-entry :foreground (my/color-value 'red))
 (elfeed-twitter-entry :foreground (my/color-value 'blue))
 (elfeed-emacs-entry :foreground (my/color-value 'magenta))
 (elfeed-music-entry :foreground (my/color-value 'green))
 (elfeed-podcasts-entry :foreground (my/color-value 'yellow))
 (elfeed-blogs-entry :foreground (my/color-value 'orange))
 (elfeed-govt-entry :foreground (my/color-value 'dark-cyan)))

(with-eval-after-load 'elfeed
  (setq elfeed-search-face-alist
        '((podcasts elfeed-podcasts-entry)
          (music elfeed-music-entry)
          (gov elfeed-govt-entry)
          (twitter elfeed-twitter-entry)
          (videos elfeed-videos-entry)
          (emacs elfeed-emacs-entry)
          (blogs elfeed-blogs-entry)
          (unread elfeed-search-unread-title-face))))

(use-package elfeed-summary
  :commands (elfeed-summary)
  :straight t
  :config
  (setq elfeed-summary-filter-by-title t)
  (setq elfeed-summary-skip-sync-tag 'skip))

(use-package elfeed-sync
  :straight (:host github :repo "SqrtMinusOne/elfeed-sync")
  :if (not my/remote-server)
  :after elfeed
  :config
  (elfeed-sync-mode)
  (setq elfeed-sync-tt-rss-instance "https://sqrtminusone.xyz/tt-rss")
  (setq elfeed-sync-tt-rss-login "sqrtminusone")
  (setq elfeed-sync-tt-rss-password (my/password-store-get "Selfhosted/tt-rss")))

(defun my/get-youtube-url (entry)
  (let ((watch-id (cadr
                   (assoc "watch?v"
                          (url-parse-query-string
                           (substring
                            (url-filename
                             (url-generic-parse-url (elfeed-entry-link entry)))
                            1))))))
    (when watch-id
      (concat "https://www.youtube.com/watch?v=" watch-id))))

(defun my/get-enclosures-url (entry)
  (caar (elfeed-entry-enclosures entry)))

(use-package elfeed-tube
  :straight t
  :after elfeed
  :config
  (setq elfeed-tube-auto-fetch-p nil)
  (elfeed-tube-setup)
  (general-define-key
   :states 'normal
   :keymaps '(elfeed-search-mode-map elfeed-show-mode-map)
   "gf" #'elfeed-tube-fetch))

(with-eval-after-load 'emms
  (define-emms-source elfeed (entry)
    (let ((url (or (my/get-enclosures-url entry)
                   (my/get-youtube-url entry))))
      (unless url
        (error "URL not found"))
      (let ((track (emms-track 'url url)))
        (emms-track-set track 'info-title (elfeed-entry-title entry))
        (emms-playlist-insert-track track)))))

(defun my/elfeed-add-emms ()
  (interactive)
  (emms-add-elfeed elfeed-show-entry)
  (elfeed-tag elfeed-show-entry 'watched)
  (elfeed-show-refresh))

(with-eval-after-load 'elfeed
  (general-define-key
   :states '(normal)
   :keymaps 'elfeed-show-mode-map
   "gm" #'my/elfeed-add-emms))

(defun my/rdrview-get (url callback)
  "Get the rdrview representation of URL.

Call CALLBACK with the output."
  (let* ((buffer (generate-new-buffer "rdrview"))
         (proc (start-process "rdrview" buffer "rdrview"
                              url "-T" "title,sitename,body"
                              "-H")))
    (set-process-sentinel
     proc
     (lambda (process _msg)
       (let ((status (process-status process))
             (code (process-exit-status process)))
         (cond ((and (eq status 'exit) (= code 0))
                (progn
                  (funcall callback
                           (with-current-buffer (process-buffer process)
                             (buffer-string)))
                  (kill-buffer (process-buffer process))) )
               ((or (and (eq status 'exit) (> code 0))
                    (eq status 'signal))
                (let ((err (with-current-buffer (process-buffer process)
                             (buffer-string))))
                  (kill-buffer (process-buffer process))
                  (user-error "Error in rdrview: %s" err)))))))
    proc))

(defun my/rdrview-parse (dom-string)
  (let ((dom (with-temp-buffer
               (insert dom-string)
               (libxml-parse-html-region (point-min) (point-max)))))
    (let (title sitename content (i 0))
      (dolist (child (dom-children (car (dom-by-id dom "readability-page-1"))))
        (when (listp child)
          (cond
           ((eq (car child) 'h1)
            (setq title (dom-text child)))
           ((eq (car child) 'h2)
            (setq sitename (dom-text child)))
           ((eq (car child) 'div)
            (setq content child)))))
      (while (and
              (not (dom-by-tag content 'h1))
              (dom-search
               content
               (lambda (el)
                 (when (listp el)
                   (pcase (car el)
                     ('h2 (setf (car el) 'h1))
                     ('h3 (setf (car el) 'h2))
                     ('h4 (setf (car el) 'h3))
                     ('h5 (setf (car el) 'h4))
                     ('h6 (setf (car el) 'h5))))))))
      `((title . ,title)
        (sitename . ,sitename)
        (content . ,(with-temp-buffer
                      (dom-print content)
                      (buffer-string)))))))

(defvar-local my/elfeed-show-rdrview-html nil)

(defun my/rdrview-elfeed-show ()
  (interactive)
  (unless elfeed-show-entry
    (user-error "No elfeed entry in this buffer!"))
  (my/rdrview-get
   (elfeed-entry-link elfeed-show-entry)
   (lambda (result)
     (let* ((data (my/rdrview-parse result))
            (inhibit-read-only t)
            (title (elfeed-entry-title elfeed-show-entry))
            (date (seconds-to-time (elfeed-entry-date elfeed-show-entry)))
            (authors (elfeed-meta elfeed-show-entry :authors))
            (link (elfeed-entry-link elfeed-show-entry))
            (tags (elfeed-entry-tags elfeed-show-entry))
            (tagsstr (mapconcat #'symbol-name tags ", "))
            (nicedate (format-time-string "%a, %e %b %Y %T %Z" date))
            (content (alist-get 'content data))
            (feed (elfeed-entry-feed elfeed-show-entry))
            (feed-title (elfeed-feed-title feed))
            (base (and feed (elfeed-compute-base (elfeed-feed-url feed)))))
       (erase-buffer)
       (insert (format (propertize "Title: %s\n" 'face 'message-header-name)
                       (propertize title 'face 'message-header-subject)))
       (when elfeed-show-entry-author
         (dolist (author authors)
           (let ((formatted (elfeed--show-format-author author)))
             (insert
              (format (propertize "Author: %s\n" 'face 'message-header-name)
                      (propertize formatted 'face 'message-header-to))))))
       (insert (format (propertize "Date: %s\n" 'face 'message-header-name)
                       (propertize nicedate 'face 'message-header-other)))
       (insert (format (propertize "Feed: %s\n" 'face 'message-header-name)
                       (propertize feed-title 'face 'message-header-other)))
       (when tags
         (insert (format (propertize "Tags: %s\n" 'face 'message-header-name)
                         (propertize tagsstr 'face 'message-header-other))))
       (insert (propertize "Link: " 'face 'message-header-name))
       (elfeed-insert-link link link)
       (insert "\n")
       (cl-loop for enclosure in (elfeed-entry-enclosures elfeed-show-entry)
                do (insert (propertize "Enclosure: " 'face 'message-header-name))
                do (elfeed-insert-link (car enclosure))
                do (insert "\n"))
       (insert "\n")
       (if content
           (elfeed-insert-html content base)
         (insert (propertize "(empty)\n" 'face 'italic)))
       (setq-local my/elfeed-show-rdrview-html content)
       (goto-char (point-min))))))

(with-eval-after-load 'elfeed
  (general-define-key
   :states '(normal)
   :keymaps 'elfeed-show-mode-map
   "gp" #'my/rdrview-elfeed-show))

(setq my/rdrview-template (expand-file-name
                           (concat user-emacs-directory "rdrview.tex")))

(cl-defun my/rdrview-render (content type variables callback
                                     &key file-name overwrite)
  "Render CONTENT with pandoc.

TYPE is a file extension as supported by pandoc, for instance,
html or txt.  VARIABLES is an alist that is fed into the
template.  After the rendering is complete successfully, CALLBACK
is called with the resulting PDF.

FILE-NAME is a path to the resulting PDF. If nil it's generated
randomly.

If a file with the given FILE-NAME already exists, the function will
invoke CALLBACK straight away without doing the rendering, unless
OVERWRITE is non-nil."
  (unless file-name
    (setq file-name (format "/tmp/%d.pdf" (random 100000000))))
  (let (params
        (temp-file-name (format "/tmp/%d.%s" (random 100000000) type)))
    (cl-loop for (key . value) in variables
             when value
             do (progn
                  (push "--variable" params)
                  (push (format "%s=%s" key value) params)))
    (setq params (nreverse params))
    (if (and (file-exists-p file-name) (not overwrite))
        (funcall callback file-name)
      (with-temp-file temp-file-name
        (insert content))
      (let ((proc (apply #'start-process
                         "pandoc" (get-buffer-create "*Pandoc*") "pandoc"
                         temp-file-name "-o" file-name
                         "--pdf-engine=xelatex" "--template" my/rdrview-template
                         params)))
        (set-process-sentinel
         proc
         (lambda (process _msg)
           (let ((status (process-status process))
                 (code (process-exit-status process)))
             (cond ((and (eq status 'exit) (= code 0))
                    (progn
                      (message "Done!")
                      (funcall callback file-name)))
                   ((or (and (eq status 'exit) (> code 0))
                        (eq status 'signal))
                    (user-error "Error in pandoc. Check the *Pandoc* buffer"))))))))))

(setq my/elfeed-pdf-dir (expand-file-name "~/.elfeed/pdf/"))

(defun my/elfeed-open-pdf (entry overwrite)
  "Open the current elfeed ENTRY with a pdf viewer.

If OVERWRITE is non-nil, do the rendering even if the resulting
PDF already exists."
  (interactive (list elfeed-show-entry current-prefix-arg))
  (let ((authors (mapcar (lambda (m) (plist-get m :name)) (elfeed-meta entry :authors)))
        (feed-title (elfeed-feed-title (elfeed-entry-feed entry)))
        (tags (mapconcat #'symbol-name (elfeed-entry-tags entry) ", "))
        (date (format-time-string "%a, %e %b %Y"
                                  (seconds-to-time (elfeed-entry-date entry))))
        (content (elfeed-deref (elfeed-entry-content entry)))
        (file-name (concat my/elfeed-pdf-dir
                           (elfeed-ref-id (elfeed-entry-content entry))
                           ".pdf"))
        (main-language "english")
        (other-language "russian"))
    (unless content
      (user-error "No content!"))
    (setq subtitle
          (cond
           ((seq-empty-p authors) feed-title)
           ((and (not (seq-empty-p (car authors)))
                 (string-match-p (regexp-quote (car authors)) feed-title)) feed-title)
           (t (concat (string-join authors ", ") "\\\\" feed-title))))
    (when (member 'ru (elfeed-entry-tags entry))
      (setq main-language "russian")
      (setq other-language "english"))
    (my/rdrview-render
     (if (bound-and-true-p my/elfeed-show-rdrview-html)
         my/elfeed-show-rdrview-html
       content)
     (elfeed-entry-content-type entry)
     `((title . ,(elfeed-entry-title entry))
       (subtitle . ,subtitle)
       (date . ,date)
       (tags . ,tags)
       (main-lang . ,main-language)
       (other-lang . ,other-language))
     (lambda (file-name)
       (start-process "xdg-open" nil "xdg-open" file-name))
     :file-name file-name
     :overwrite current-prefix-arg)))

(with-eval-after-load 'elfeed
  (general-define-key
   :keymaps '(elfeed-show-mode-map)
   :states '(normal)
   "gv" #'my/elfeed-open-pdf))

(defun my/get-languages (url)
  (let ((main-lang "english")
        (other-lang "russian"))
    (when (string-match-p (rx ".ru") url)
      (setq main-lang "russian"
            other-lang "english"))
    (list main-lang other-lang)))

(defun my/rdrview-open (url overwrite)
  (interactive
   (let ((url (read-from-minibuffer
               "URL: "
               (if (bound-and-true-p elfeed-show-entry)
                   (elfeed-entry-link elfeed-show-entry)))))
     (when (string-empty-p url)
       (user-error "URL is empty"))
     (list url current-prefix-arg)))
  (my/rdrview-get
   url
   (lambda (res)
     (let ((data (my/rdrview-parse res))
           (langs (my/get-languages url)))
       (my/rdrview-render
        (alist-get 'content data)
        'html
        `((title . ,(alist-get 'title data))
          (subtitle . ,(alist-get 'sitename data))
          (main-lang . ,(nth 0 langs))
          (other-lang . ,(nth 1 langs)))
        (lambda (file-name)
          (start-process "xdg-open" nil "xdg-open" file-name)))))))

(cl-defun my/youtube-subtitles-get (video-id callback &key file-name overwrite)
  "Get subtitles for VIDEO-ID in WebVTT format.

Call CALLBACK when done.

FILE-NAME is a path to the resulting WebVTT file. If nil it's
generated randomly.

If a file with the given FILE-NAME already exists, the function will
invoke CALLBACK straight away without doing the rendering, unless
OVERWRITE is non-nil."
  (interactive (list (read-string "Video ID: ")
                     (lambda (file-name)
                       (find-file file-name))
                     :file-name nil
                     :overwrite t))
  (unless file-name
    (setq file-name (format "/tmp/%d.vtt" (random 100000000))))
  (if (and (file-exists-p file-name) (not overwrite))
      (funcall callback file-name)
    (let* ((buffer (generate-new-buffer "youtube-transcripts"))
           (proc (start-process "youtube_transcript_api" buffer
                                "youtube_transcript_api" video-id
                                "--languages" "en" "ru" "de"
                                "--format" "webvtt")))
      (set-process-sentinel
       proc
       (lambda (process _msg)
         (let ((status (process-status process))
               (code (process-exit-status process)))
           (cond ((and (eq status 'exit) (= code 0))
                  (progn
                    (with-current-buffer (process-buffer process)
                      (setq buffer-file-name file-name)
                      (save-buffer))
                    (kill-buffer (process-buffer process))
                    (funcall callback file-name)))
                 ((or (and (eq status 'exit) (> code 0))
                      (eq status 'signal))
                  (let ((err (with-current-buffer (process-buffer process)
                               (buffer-string))))
                    (kill-buffer (process-buffer process))
                    (user-error "Error in youtube_transcript_api: %s" err)))))))
      proc)))

(setq my/elfeed-srt-dir (expand-file-name "~/.elfeed/srt/"))

(defun my/elfeed-youtube-subtitles (entry &optional arg)
  "Get subtitles for the current elfeed ENTRY.

Works only in the entry is a YouTube video.

If ARG is non-nil, re-fetch the subtitles regardless of whether
they were fetched before."
  (interactive (list elfeed-show-entry current-prefix-arg))
  (let ((video-id (cadr
                   (assoc "watch?v"
                          (url-parse-query-string
                           (substring
                            (url-filename
                             (url-generic-parse-url (elfeed-entry-link entry)))
                            1))))))
    (unless video-id
      (user-error "Can't get video ID from the entry"))
    (my/youtube-subtitles-get
     video-id
     (lambda (file-name)
       (with-current-buffer (find-file-other-window file-name)
         (setq-local elfeed-show-entry entry)
         (goto-char (point-min))))
     :file-name (concat my/elfeed-srt-dir
                        (elfeed-ref-id (elfeed-entry-content entry))
                        ".vtt")
     :overwrite arg)))

(defun my/subed-elfeed (entry)
  "Open the video file from elfeed ENTRY in MPV.

This has to be launched from inside the subtitles buffer, opened
by the `my/elfeed-youtube-subtitles' function."
  (interactive (list elfeed-show-entry))
  (unless entry
    (user-error "No entry!"))
  (unless (derived-mode-p 'subed-mode)
    (user-error "Not subed mode!"))
  (setq-local subed-mpv-arguments
              (seq-uniq
               (append subed-mpv-arguments emms-player-mpv-parameters)))
  (setq-local subed-mpv-video-file (elfeed-entry-link entry))
  (subed-mpv--play subed-mpv-video-file))

(provide 'sqrt-elfeed)
