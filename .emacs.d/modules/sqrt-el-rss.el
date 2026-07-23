;;; -*- lexical-binding: t -*-
(use-package el-rss
  :if (file-exists-p "/home/pavel/10-19 Code/12 My Emacs Packages/12.23 el-rss/")
  :straight (:local-repo "/home/pavel/10-19 Code/12 My Emacs Packages/12.23 el-rss/"
                         :files ("*.el" ("migrations" "migrations/*.sql")))
  :commands (el-rss)
  :init
  (my-leader-def "ae" #'el-rss)
  :config
  (setq el-rss-tt-rss-url "https://sqrtminusone.xyz/tt-rss/")
  (setq el-rss-tt-rss-username "sqrtminusone")
  (setq el-rss-tt-rss-password-function
        (lambda ()
          (my/password-store-get
           "Selfhosting/Accounts/sqrtminusone.xyz-tt-rss")))
  (setq el-rss-directory (expand-file-name "./el-rss/" no-littering-etc-directory))
  (setq el-rss-readeck-url "https://readeck.sqrtminusone.xyz/")
  (setq el-rss-readeck-token-function
        (lambda ()
          (my/password-store-get-field
           "Selfhosting/Accounts/readeck.sqrtminusone.xyz"
           "elrss_token")))
  (setq el-rss-tree-width 50)
  (setq el-rss-tt-rss-sync-plugin 'required)
  (setq el-rss-tt-rss-proxy-media t)
  (setq el-rss-tt-rss-download-media-during-sync nil
        el-rss-tt-rss-fetch-media-on-open t)
  (my/persp-add-rule
    el-rss-list-mode 0 "el-rss"
    el-rss-tree-mode 0 "el-rss"
    el-rss-show-mode 0 "el-rss"))

(setq el-rss-list-color-rules
      '(("category:AI" . font-lock-builtin-face)
        ("category:Entertainment/Music" . font-lock-comment-face)
        ("category:Entertainment" . font-lock-constant-face)
        ("category:\"Science & Mind\"" . font-lock-property-name-face)
        ("category:Technology" . warning)
        ("category:\"Politcs & Warfare\"" . error)
        ("category:Rationality" . font-lock-doc-markup-face)))

(defvar my/pandoc-pdf-template
  (expand-file-name "rdrview.tex" user-emacs-directory))

(require 'cl-lib)

(defun my/pandoc--cleanup (render-file work-directory)
  "Remove RENDER-FILE and the disposable WORK-DIRECTORY."
  (when (and render-file (file-exists-p render-file))
    (delete-file render-file))
  (when (file-directory-p work-directory)
    (delete-directory work-directory t)))

(defun my/pandoc--sentinel (process _event)
  "Install PROCESS output and clean its temporary files."
  (when (memq (process-status process) '(exit signal))
    (let ((render-file (process-get process 'my/pandoc-render-file))
          (file-name (process-get process 'my/pandoc-file-name))
          (work-directory
           (process-get process 'my/pandoc-work-directory))
          (output-buffer (process-buffer process))
          (callback (process-get process 'my/pandoc-callback)))
      (unwind-protect
          (if (and (eq (process-status process) 'exit)
                   (= (process-exit-status process) 0))
              (condition-case install-error
                  (progn
                    (rename-file render-file file-name t)
                    (when (buffer-live-p output-buffer)
                      (kill-buffer output-buffer))
                    (message "Rendered %s" file-name)
                    (funcall callback file-name))
                (error
                 (when (buffer-live-p output-buffer)
                   (display-buffer output-buffer))
                 (message "Could not install Pandoc output: %s"
                          (error-message-string install-error))))
            (when (buffer-live-p output-buffer)
              (display-buffer output-buffer))
            (message "Pandoc failed (exit %s); see %s"
                     (process-exit-status process)
                     (buffer-name output-buffer)))
        (my/pandoc--cleanup render-file work-directory)))))

(cl-defun my/pandoc-render-pdf
    (content input-format metadata template-variables callback
             &key file-name overwrite temporary-directory)
  "Render CONTENT to FILE-NAME asynchronously with Pandoc.

INPUT-FORMAT is a Pandoc input format such as \"html\".  METADATA
and TEMPLATE-VARIABLES are alists.  Invoke CALLBACK with the
finished PDF path.

Unless OVERWRITE is non-nil, reuse an existing FILE-NAME.
TEMPORARY-DIRECTORY, when non-nil, must be disposable: this
function takes ownership of it and removes it after Pandoc exits."
  (unless (and (stringp file-name) (file-name-absolute-p file-name))
    (user-error "FILE-NAME must be an absolute path"))
  (let* ((file-name (expand-file-name file-name))
         (work-directory
          (file-name-as-directory
           (expand-file-name
            (or temporary-directory
                (make-temp-file "pandoc-pdf-" t)))))
         input-file render-file output-buffer)
    (condition-case err
        (if (and (file-exists-p file-name) (not overwrite))
            (unwind-protect
                (funcall callback file-name)
              (my/pandoc--cleanup render-file work-directory))
            (unless (file-exists-p my/pandoc-pdf-template)
              (user-error "Pandoc template does not exist: %s"
                          my/pandoc-pdf-template))
            (unless (executable-find "pandoc")
              (user-error "The pandoc executable is not available"))
            (make-directory work-directory t)
            (make-directory (file-name-directory file-name) t)
            (setq input-file
                  (make-temp-file
                   (expand-file-name "article-" work-directory)
                   nil (format ".%s" input-format))
                  render-file
                  (make-temp-file (concat file-name ".pandoc-") nil ".pdf")
                  output-buffer
                  (generate-new-buffer "*Pandoc*"))
            (let ((coding-system-for-write 'utf-8-unix))
              (with-temp-file input-file
                (insert content)))
            (let*
                ((arguments
                  (append
                   (list input-file
                         "--from" (format "%s" input-format)
                         "--output" render-file
                         "--pdf-engine=xelatex"
                         "--template" my/pandoc-pdf-template)
                   (cl-loop for (key . value) in metadata
                            when value
                            append
                            (list "--metadata"
                                  (format "%s=%s" key value)))
                   (cl-loop for (key . value) in template-variables
                            when value
                            append
                            (list "--variable"
                                  (format "%s=%s" key value)))))
                 (process
                  (make-process
                   :name "pandoc"
                   :buffer output-buffer
                   :command (cons "pandoc" arguments)
                   :noquery t)))
              (process-put process 'my/pandoc-render-file render-file)
              (process-put process 'my/pandoc-file-name file-name)
              (process-put process 'my/pandoc-work-directory work-directory)
              (process-put process 'my/pandoc-callback callback)
              (set-process-sentinel process #'my/pandoc--sentinel)
              process))
      (error
       (my/pandoc--cleanup render-file work-directory)
       (when (buffer-live-p output-buffer)
         (kill-buffer output-buffer))
       (signal (car err) (cdr err))))))

(defvar my/el-rss-pdf-directory
  (expand-file-name "el-rss/pdf/" no-littering-var-directory))

(defun my/el-rss-open-pdf (&optional overwrite)
  "Render and open the current el-rss article as a PDF.

With a prefix argument OVERWRITE, refresh an existing cached PDF."
  (interactive "P")
  (let* ((article (el-rss-ui-current-article))
         (article-id (plist-get article :id))
         (system-id (plist-get article :system-id))
         (content (plist-get article :content))
         (published-at (or (plist-get article :published-at)
                           (plist-get article :received-at))))
    (unless (and (stringp content)
                 (not (string-empty-p (string-trim content))))
      (user-error "The current article has no cached content"))
    (make-directory my/el-rss-pdf-directory t)
    (let* ((content-hash (substring (secure-hash 'sha256 content) 0 16))
           (file-name
            (expand-file-name
             (format "%s-%s.pdf" article-id content-hash)
             my/el-rss-pdf-directory))
           (open-pdf
            (lambda (path)
              (start-process "xdg-open-el-rss-pdf" nil
                             "xdg-open" path))))
      (if (and (file-exists-p file-name) (not overwrite))
          (funcall open-pdf file-name)
        (require 'el-rss-export)
        (let ((temporary-directory
               (make-temp-file "el-rss-pandoc-" t))
              ownership-transferred)
          (unwind-protect
              (let* ((html
                      (el-rss-export-materialize-article-html
                       article-id temporary-directory))
                     (russian-p
                      (el-rss-db-match-article-query
                       (list article-id) system-id "category:Russian"))
                     (languages
                      (if russian-p
                          '(("main-lang" . "russian")
                            ("other-lang" . "english"))
                        '(("main-lang" . "english")
                          ("other-lang" . "russian"))))
                     (process
                      (my/pandoc-render-pdf
                       html
                       (plist-get article :content-type)
                       `(("title" . ,(plist-get article :title))
                         ("subtitle" . ,(plist-get article :source-title))
                         ("author" . ,(plist-get article :author))
                         ("date" .
                          ,(when published-at
                             (format-time-string
                              "%a, %e %b %Y"
                              (seconds-to-time published-at)))))
                       languages open-pdf
                       :file-name file-name
                       :overwrite overwrite
                       :temporary-directory temporary-directory)))
                (setq ownership-transferred (processp process))
                process)
            (unless ownership-transferred
              (when (file-directory-p temporary-directory)
                (delete-directory temporary-directory t)))))))))

(with-eval-after-load 'el-rss-ui
  (general-define-key
   :keymaps '(el-rss-list-mode-map el-rss-show-mode-map)
   :states '(normal)
   "gv" #'my/el-rss-open-pdf))

(provide 'sqrt-el-rss)
