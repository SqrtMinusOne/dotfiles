;;; -*- lexical-binding: t -*-
(defun my/gravatar-retrieve-sync (email file-name)
  "Get gravatar for EMAIL and save it to FILE-NAME."
  (let ((gravatar-default-image "identicon")
        (gravatar-size nil)
        (coding-system-for-write 'binary)
        (write-region-annotate-functions nil)
        (write-region-post-annotation-function nil))
    (write-region
     (image-property (gravatar-retrieve-synchronously email) :data)
     nil file-name nil :silent)))

(setq my/gravatar-folder "/home/pavel/.cache/gravatars/")

(defun my/gravatar-save (email author)
  "Download gravatar for EMAIL.

AUTHOR is the username."
  (let ((file-name (concat my/gravatar-folder author ".png")))
    (mkdir my/gravatar-folder t)
    (unless (file-exists-p file-name)
      (message "Fetching gravatar for %s (%s)" author email)
      (my/gravatar-retrieve-sync email file-name))))

(defun my/git-get-authors (repo &optional authors-init)
  "Extract and merge all combinations of authors & emails from REPO.

REPO is the path to a git repository.

AUTHORS-INIT is the previous output of `my/git-get-authors'.  It can
be used to extract that information from multiple repositories.

The output is a list of alists with following keys:
- emails: list of (<email> . <count>)
- authors: list of (<username> . <count>)
- email: the most popular email
- author: the most popular username
I.e. one alist is all emails and usernames of one author."
  (let* ((default-directory repo)
         (data (shell-command-to-string
                "git log --pretty=format:\"%ae|%an\" | sort | uniq -c | sed \"s/^[ \t]*//;s/ /|/\""))
         (authors
          (cl-loop for string in (split-string data "\n")
                   if (= (length (split-string string "|")) 3)
                   collect (let ((datum (split-string string "|")))
                             `((count . ,(string-to-number (nth 0 datum)))
                               (email . ,(downcase (nth 1 datum)))
                               (author . ,(nth 2 datum)))))))
    (mapcar
     (lambda (datum)
       (setf (alist-get 'author datum)
             (car (cl-reduce
                   (lambda (acc author)
                     (if (> (cdr author) (cdr acc))
                         author
                       acc))
                   (alist-get 'authors datum)
                   :initial-value '(nil . -1))))
       (setf (alist-get 'email datum)
             (car (cl-reduce
                   (lambda (acc email)
                     (if (> (cdr email) (cdr acc))
                         email
                       acc))
                   (alist-get 'emails datum)
                   :initial-value '(nil . -1))))
       datum)
     (cl-reduce
      (lambda (acc val)
        (let* ((author (alist-get 'author val))
               (email (alist-get 'email val))
               (count (alist-get 'count val))
               (saved-value
                (seq-find
                 (lambda (cand)
                   (or (alist-get email (alist-get 'emails cand)
                                  nil nil #'string-equal)
                       (alist-get author (alist-get 'authors cand)
                                  nil nil #'string-equal)
                       (alist-get email (alist-get 'authors cand)
                                  nil nil #'string-equal)
                       (alist-get author (alist-get 'emails cand)
                                  nil nil #'string-equal)))
                 acc)))
          (if saved-value
              (progn
                (if (alist-get email (alist-get 'emails saved-value)
                               nil nil #'string-equal)
                    (cl-incf (alist-get email (alist-get 'emails saved-value)
                                        nil nil #'string-equal)
                             count)
                  (push (cons email count) (alist-get 'emails saved-value)))
                (if (alist-get author (alist-get 'authors saved-value)
                               nil nil #'string-equal)
                    (cl-incf (alist-get author (alist-get 'authors saved-value)
                                        nil nil #'string-equal)
                             count)
                  (push (cons author count) (alist-get 'authors saved-value))))
            (setq saved-value
                  (push `((emails . ((,email . ,count)))
                          (authors . ((,author . ,count))))
                        acc)))
          acc))
      authors
      :initial-value authors-init))))

(defun my/gource-prepare-log (repo authors)
  "Create gource log string for REPO.

AUTHORS is the output of `my/git-get-authors'."
  (let ((log (shell-command-to-string
              (concat
               "gource --output-custom-log - "
               repo)))
        (authors-mapping (make-hash-table :test #'equal))
        (prefix (file-name-base repo)))
    (cl-loop for author-datum in authors
             for author = (alist-get 'author author-datum)
             do (my/gravatar-save (alist-get 'email author-datum) author)
             do (cl-loop for other-author in (alist-get 'authors author-datum)
                         unless (string-equal (car other-author) author)
                         do (puthash (car other-author) author
                                     authors-mapping)))
    (cl-loop for line in (split-string log "\n")
             concat (let ((fragments (split-string line "|")))
                      (when (> (length fragments) 3)
                        (when-let (mapped-author (gethash (nth 1 fragments)
                                                          authors-mapping))
                          (setf (nth 1 fragments) mapped-author))
                        (setf (nth 3 fragments)
                              (concat "/" prefix (nth 3 fragments))))
                      (string-join fragments "|"))
             concat "\n")))

(defun my/gource-dired-create-logs (repos log-name)
  "Create combined gource log for REPOS.

REPOS is a list of strings, where a string is a path to a git repo.
LOG-NAME is the path to the resulting log file.

This function is meant to be invoked from `dired', where the required
repositories are marked."
  (interactive (list (or (dired-get-marked-files nil nil #'file-directory-p)
                         (user-error "Select at least one directory"))
                     (read-file-name "Log file name: " nil "combined.log")))
  (let ((authors
         (cl-reduce
          (lambda (acc repo)
            (my/git-get-authors repo acc))
          repos
          :initial-value nil)))
    (with-temp-file log-name
      (insert
       (string-join
        (seq-filter
         (lambda (line)
           (not (string-empty-p line)))
         (seq-sort-by
          (lambda (line)
            (if-let (time (car (split-string line "|")))
                (string-to-number time)
              0))
          #'<
          (split-string
           (mapconcat
            (lambda (repo)
              (my/gource-prepare-log repo authors))
            repos "\n")
           "\n")))
        "\n")))))

(provide 'sqrt-gource)
