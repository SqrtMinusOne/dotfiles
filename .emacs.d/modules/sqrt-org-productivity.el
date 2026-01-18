;;; -*- lexical-binding: t -*-
(defun my/update-org-agenda ()
  (interactive)
  (let ((project-files
         (when (file-directory-p (concat org-directory "/projects"))
           (thread-last "/projects"
                        (concat org-directory)
                        (directory-files)
                        (mapcar (lambda (f)
                                  (concat
                                   org-directory "/projects/" f)))
                        (seq-filter (lambda (f)
                                      (not (file-directory-p f))))))))
    (setq org-agenda-files
          (seq-filter #'file-exists-p
                      (append
                       project-files
                       (mapcar (lambda (f)
                                 (concat org-directory "/" f))
                               '("inbox.org"
                                 "misc/habit.org"
                                 "contacts.org")))))
    (setq org-refile-targets
          `(,@(mapcar
               (lambda (f) `(,f . (:tag . "refile")))
               project-files)
            ,@(mapcar
               (lambda (f) `(,f . (:regexp . "Tasks")))
               project-files)))
    (when (file-exists-p (concat org-directory "/scripts/refile.el"))
      (load-file (concat org-directory "/scripts/refile.el"))
      (run-hooks 'my/org-refile-hooks))))

(setq org-roam-directory (concat org-directory "/roam"))
(with-eval-after-load 'org
  (require 'seq)
  (my/update-org-agenda))

(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)

(setq org-extend-today-until 4)

(defun my/generate-inbox-note-name ()
  (format
   "%s/inbox-notes/%s%s.org"
   org-directory
   (format-time-string "%Y%m%d%H%M%S")
   (let ((note-name (read-string "Note name: ")))
     (if (not (string-empty-p note-name))
         (string-replace " " "-" (concat "-" (downcase note-name)))
       ""))))

(setq org-capture-templates
      `(("i" "Inbox" entry  (file "inbox.org")
         ,(concat "* TODO %?\n"
                  "/Entered on/ %U"))
        ("e" "email" entry (file "inbox.org")
         ,(concat "* TODO %:from %:subject \n"
                  "/Entered on/ %U\n"
                  "/Received on/ %:date-timestamp-inactive\n"
                  "%a\n"))
        ("f" "elfeed" entry (file "inbox.org")
         ,(concat "* TODO %:elfeed-entry-title\n"
                  "/Entered on/ %U\n"
                  "%a\n"))
        ("n" "note" plain (file my/generate-inbox-note-name)
         ,(concat "#+TODO: PROCESSED(p)\n"
                  "\n"
                  "* %?\n"
                  "/Entered on/ %U"))))

(use-package org-clock-agg
  :straight (:host github :repo "SqrtMinusOne/org-clock-agg")
  :commands (org-clock-agg)
  :init
  (with-eval-after-load 'org
    (my-leader-def "ol" #'org-clock-agg))
  :config
  (setq org-clock-agg-node-format
    "%-%(+ title-width)t %20c %8z %s/%S")
  (setq org-clock-agg-node-title-width-delta 47)
  (push
   (cons "Agenda+Archive"
         (my/org-agenda-and-archive))
   org-clock-agg-files-preset))

(with-eval-after-load 'org
  (setq org-clock-persist 'clock)
  (org-clock-persistence-insinuate))

(setq org-log-done 'time)

(defun my/org-clock-in--fix-mode-line ()
  (when (memq 'org-mode-line-string global-mode-string)
    (let (new-global-mode-string
          appended
          (is-first t))
      (dolist (item global-mode-string)
        (cond
         ((or (equal item '(:eval (exwm-modeline-segment)))
              (equal item '(:eval (persp-mode-line))))
          (unless appended
            (when is-first
              (push "" new-global-mode-string))
            (push 'org-mode-line-string new-global-mode-string)
            (setq appended t))
          (push item new-global-mode-string))
         ((equal item 'org-mode-line-string))
         (t
          (push item new-global-mode-string)))
        (setq is-first nil))
      (unless appended
        (push 'org-mode-line-string new-global-mode-string))
      (setq global-mode-string (nreverse new-global-mode-string)))))

(add-hook 'org-clock-in-hook #'my/org-clock-in--fix-mode-line)

(defun my/org-clock-in-prompt-time (&optional select)
  (interactive "P")
  (org-clock-in
   select
   (encode-time
    (org-parse-time-string
     (org-read-date t)))))

(with-eval-after-load 'org
  (my-leader-def
    :keymaps 'org-mode-map
    :infix "SPC"
    "I" #'my/org-clock-in-prompt-time))

(defun my/org-clock-get-total-minutes-at-point ()
  "Get total clocked time for heading at point."
  (let* ((element (org-element-at-point-no-context))
         (s (buffer-substring-no-properties
             (org-element-property :begin element)
             (org-element-property :end element))))
    (with-temp-buffer
      (insert s)
      (let (org-mode-hook)
        (org-mode))
      (org-clock-sum)
      org-clock-file-total-minutes)))

(defconst my/org-clock-total-prop :CLOCK_TOTAL)

(defun my/org-clock-set-total-clocked ()
  "Set total clocked time for heading at point."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (org-set-property
     (substring
      (symbol-name my/org-clock-total-prop)
      1)
     (org-duration-from-minutes
      (my/org-clock-get-total-minutes-at-point)))))

(add-hook 'org-clock-in-hook #'my/org-clock-set-total-clocked)
(add-hook 'org-clock-out-hook #'my/org-clock-set-total-clocked)
(add-hook 'org-clock-cancel-hook #'my/org-clock-set-total-clocked)

(defun my/org-clock-recent ()
  (interactive)
  (let* ((entries (org-ql-query
                    :select #'element-with-markers
                    :from (org-agenda-files)
                    :where '(clocked :from -1)))
         (entries-data (mapcar (lambda (e)
                                 (cons (org-element-property :raw-value e) e))
                               entries)))
    (unless entries
      (user-error "No recently clocked entries!"))
    entries-data
    (let* ((entry (alist-get (completing-read "Entry: " entries-data)
                             entries-data nil nil #'equal))
           (marker (org-element-property :org-marker entry)))
      (pop-to-buffer-same-window (marker-buffer marker))
      (goto-char marker))))

(with-eval-after-load 'org
  (my-leader-def
    :keymaps 'org-mode-map
    :infix "SPC"
    "C" #'my/org-clock-recent))

(defun my/org-fix-task-kind ()
  (interactive)
  (let ((entries (org-ql-query
                   :select #'element-with-markers
                   :from (current-buffer)
                   :where '(and (olp "Tasks")
                                (not (property "TASK_KIND"))
                                (clocked)))))
    (org-fold-show-all)
    (dolist (entry entries)
      (let ((marker (org-element-property :org-marker entry)))
        (org-with-point-at marker
          (let ((value (org-read-property-value "TASK_KIND")))
            (org-set-property "TASK_KIND" value)))))))

(setq org-priority-highest ?A)
(setq org-priority-lowest (+ ?A 9))

(with-eval-after-load 'org
  (add-to-list
   'org-global-properties
   '("Effort_ALL" . "0 0:05 0:10 0:15 0:30 0:45 1:00 1:30 2:00 4:00 8:00")))

(use-package org-super-agenda
  :straight t
  :after (org)
  :config
  ;; Alphapapa doesn't like evil
  (general-define-key
   :keymaps '(org-super-agenda-header-map)
   "h" nil
   "j" nil
   "k" nil
   "l" nil)

  (org-super-agenda--def-auto-group outline-path-file "their outline paths & files"
    :key-form
    (org-super-agenda--when-with-marker-buffer (org-super-agenda--get-marker item)
      ;; org-ql depends on f and s anyway
      (s-join "/" (cons
                   (f-filename (buffer-file-name))
                   (org-get-outline-path)))))

  (org-super-agenda--def-auto-group priority-outline-path-file
    "priorities, files and outline paths"
    :key-form
    (org-super-agenda--when-with-marker-buffer (org-super-agenda--get-marker item)
      ;; org-ql depends on f and s anyway
      (concat
       (format "[%s] " (org-super-agenda--get-priority-cookie item))
       (s-join "/" (cons
                    (f-filename (buffer-file-name))
                    (org-get-outline-path)))))))

(defun my/org-super-agenda--make-agenda-header-around (fun name)
  (remove-text-properties 0 (length name) '(line-prefix nil) name)
  (remove-text-properties 0 (length name) '(wrap-prefix nil) name)
  (funcall fun (substring-no-properties name)))

(with-eval-after-load 'org-super-agenda
  (advice-add 'org-super-agenda--make-agenda-header :around #'my/org-super-agenda--make-agenda-header-around))

(use-package org-ql
  :after (org)
  :straight t
  :config
  (setq org-ql-ask-unsafe-queries nil)
  :init
  ;; See https://github.com/alphapapa/org-ql/pull/237
  (setq org-ql-regexp-part-ts-time
        (rx " " (repeat 1 2 digit) ":" (repeat 2 digit)
            (optional "-" (repeat 1 2 digit) ":" (repeat 2 digit))))
  (my-leader-def
    :infix "o"
    "v" #'org-ql-view
    "q" #'org-ql-search))

(cl-defun my/org-ql-view-recent-items
    (&key num-days (type 'ts)
          (files (org-agenda-files))
          (groups '((:auto-outline-path-file t)
                    (:auto-todo t))))
  "Show items in FILES from last NUM-DAYS days with timestamps of TYPE.
TYPE may be `ts', `ts-active', `ts-inactive', `clocked', or
`closed'."
  (interactive (list :num-days (read-number "Days: ")
                     :type (->> '(ts ts-active ts-inactive clocked closed)
                                (completing-read "Timestamp type: ")
                                intern)))
  ;; It doesn't make much sense to use other date-based selectors to
  ;; look into the past, so to prevent confusion, we won't allow them.
  (-let* ((query (pcase-exhaustive type
                   ((or 'ts 'ts-active 'ts-inactive)
                    `(,type :from ,(- num-days) :to 0))
                   ((or 'clocked 'closed)
                    `(,type :from ,(- num-days) :to 0)))))
    (org-ql-search files query
      :title "Recent items"
      :sort '(todo priority date)
      :super-groups groups)))

(defun my/org-ql-all-todo ()
  (interactive)
  ;; The hack I borrowed from notmuch to make " " a separator
  (let* ((crm-separator " ")
         (crm-local-completion-map
          (let ((map (make-sparse-keymap)))
            (set-keymap-parent map crm-local-completion-map)
            (define-key map " " 'self-insert-command)
            map))
         (vertico-sort-function nil)
         (categories (completing-read-multiple
                      "Categories: "
                      '("TEACH" "EDU" "JOB" "LIFE" "COMP"))))
    (org-ql-search (org-agenda-files)
      `(and (todo)
            ,@(unless (seq-empty-p categories)
                `((category ,@categories))))
      :sort '(priority todo deadline)
      :super-groups '((:auto-outline-path-file t)))))

(defun my/backlog ()
  (interactive)
  (org-ql-search (org-agenda-files)
    `(and (todo)
          (not (todo "WAIT"))
          (not (deadline))
          (not (scheduled))
          (not (tags "nb"))
          (category "JOB" "TEACH" "EDU"))
    :title "Backlog"
    :sort '(todo priority)
    :super-groups '((:auto-priority-outline-path-file t))))

(defun my/org-ql-clocked-today ()
  (interactive)
  (let ((today (format-time-string
                "%Y-%m-%d"
                (days-to-time
                 (- (org-today) (time-to-days 0))))))
    (org-ql-search (org-agenda-files) `(clocked :from ,today)
      :title "Clocked today"
      :sort '(todo priority date)
      :super-groups '((:auto-outline-path-file t)
                      (:auto-todo t)))))

(defun my/org-ql-closed-today ()
  (interactive)
  (let ((today (format-time-string
                "%Y-%m-%d"
                (days-to-time
                 (- (org-today) (time-to-days 0))))))
    (org-ql-search (org-agenda-files) `(closed :from ,today)
      :title "Closed today"
      :sort '(todo priority date)
      :super-groups '((:auto-outline-path-file t)
                      (:auto-todo t)))))

(setq org-ql-views
      (list
       (cons "Overview: All TODO" #'my/org-ql-all-todo)
       (cons "Overview: Backlog" #'my/backlog)
       (cons "Review: Stale tasks"
             (list :buffers-files #'org-agenda-files
                   :query '(and (todo)
                                (clocked)
                                (not (tags "nots"))
                                (not (ts :from -14)))
                   :title "Review: Stale tasks"
                   :sort '(todo priority date)
                   :super-groups '((:auto-outline-path-file t))))
       (cons "Review: Unclocked tasks"
             (list :buffers-files #'org-agenda-files
                   :query '(and (done)
                                (ts :from -14)
                                (not (clocked))
                                (not (tags "nots")))
                   :title "Review: Unclocked tasks"
                   :sort '(todo priority date)
                   :super-groups '((:auto-outline-path-file t))))
       (cons "Review: Recently timestamped" #'my/org-ql-view-recent-items)
       (cons "Review: Clocked today" #'my/org-ql-clocked-today)
       (cons "Review: Closed today" #'my/org-ql-closed-today)
       (cons "Fix: tasks without TASK_KIND"
             (lambda ()
               (interactive)
               (org-ql-search (current-buffer)
                 '(and (olp "Tasks")
                       (not (property "TASK_KIND"))
                       (clocked))
                 :super-groups '((:auto-outline-path-file t)))))))

(defun my/org-ql-view--format-element-override (element)
  "Format ELEMENT for `org-ql-view'.

Check `org-ql-view--format-element' for the original implementation
and lots of comments which are too long for my Emacs config."
  (if (not element)
      ""
    (setf element (org-ql-view--resolve-element-properties element))
    (let* ((properties (cadr element))
           (properties (cl-loop for (key val) on properties by #'cddr
                                for symbol = (intern (cl-subseq (symbol-name key) 1))
                                unless (member symbol '(parent))
                                append (list symbol val)))
           (title (--> (org-ql-view--add-faces element)
                       (org-element-property :raw-value it)
                       (org-link-display-format it)))
           (todo-keyword (-some--> (org-element-property :todo-keyword element)
                           (org-ql-view--add-todo-face it)))
           (tag-list (if org-use-tag-inheritance
                         (if-let ((marker (or (org-element-property :org-hd-marker element)
                                              (org-element-property :org-marker element))))
                             (with-current-buffer (marker-buffer marker)
                               (org-with-wide-buffer
                                (goto-char marker)
                                (cl-loop for type in (org-ql--tags-at marker)
                                         unless (or (eq 'org-ql-nil type)
                                                    (not type))
                                         append type)))
                           (display-warning 'org-ql (format "No marker found for item: %s" title))
                           (org-element-property :tags element))
                       (org-element-property :tags element)))
           (tag-string (when tag-list
                         (--> tag-list
                              (s-join ":" it)
                              (s-wrap it ":")
                              (org-add-props it nil 'face 'org-tag))))
           ;;  (category (org-element-property :category element))
           (priority-string (-some->> (org-element-property :priority element)
                              (char-to-string)
                              (format "[#%s]")
                              (org-ql-view--add-priority-face)))
           (clock-string (let ((effort (org-element-property :EFFORT element))
                               (clocked (org-element-property my/org-clock-total-prop element)))
                           (cond
                            ((and clocked effort) (format "[%s/%s]" clocked effort))
                            ((and clocked (not effort) (format "[%s]" clocked)))
                            ((and (not clocked) effort) (format "[EST: %s]" effort)))))
           (habit-property (org-with-point-at (or (org-element-property :org-hd-marker element)
                                                  (org-element-property :org-marker element))
                             (when (org-is-habit-p)
                               (org-habit-parse-todo))))
           (due-string (pcase (org-element-property :relative-due-date element)
                         ('nil "")
                         (string (format " %s " (org-add-props string nil 'face 'org-ql-view-due-date)))))
           (string (s-join " " (-non-nil (list todo-keyword priority-string title due-string clock-string tag-string)))))
      (remove-list-of-text-properties 0 (length string) '(line-prefix) string)
      (--> string
           (concat "  " it)
           (org-add-props it properties
             'org-agenda-type 'search
             'todo-state todo-keyword
             'tags tag-list
             'org-habit-p habit-property)))))

(with-eval-after-load 'org-ql
  (advice-add #'org-ql-view--format-element :override #'my/org-ql-view--format-element-override))

(use-package org-habit-stats
  :straight (:host github :repo "ml729/org-habit-stats")
  :after (org)
  :config
  (general-define-key
   :keymaps '(org-habit-stats-mode-map)
   :states '(normal emacs)
   "q" #'org-habit-stats-exit
   "<" #'org-habit-stats-calendar-scroll-left
   ">" #'org-habit-stats-calendar-scroll-right
   "[" #'org-habit-stats-scroll-graph-left
   "]" #'org-habit-stats-scroll-graph-right
   "{" #'org-habit-stats-scroll-graph-left-big
   "}" #'org-habit-stats-scroll-graph-right-big
   "." #'org-habit-stats-view-next-habit
   "," #'org-habit-stats-view-previous-habit)
   (add-hook 'org-after-todo-state-change-hook 'org-habit-stats-update-properties))

(defun my/org-match-at-point-p (match)
  "Return non-nil if headline at point matches MATCH.
Here MATCH is a match string of the same format used by
`org-tags-view'."
  (funcall (cdr (org-make-tags-matcher match))
           (org-get-todo-state)
           (org-get-tags-at)
           (org-reduced-level (org-current-level))))

(defun my/org-agenda-skip-without-match (match)
  "Skip current headline unless it matches MATCH.

Return nil if headline containing point matches MATCH (which
should be a match string of the same format used by
`org-tags-view').  If headline does not match, return the
position of the next headline in current buffer.

Intended for use with `org-agenda-skip-function', where this will
skip exactly those headlines that do not match."
  (save-excursion
    (unless (org-at-heading-p) (org-back-to-heading))
    (let ((next-headline (save-excursion
                           (or (outline-next-heading) (point-max)))))
      (if (my/org-match-at-point-p match) nil next-headline))))

(defun my/org-scheduled-get-time ()
  (let ((scheduled (org-get-scheduled-time (point))))
    (if scheduled
        (format-time-string "%Y-%m-%d" scheduled)
      "")))

(setq org-agenda-hide-tags-regexp (rx (or "org" "refile" "proj" "habit")))

(setq org-agenda-custom-commands
      `(("p" "My outline"
         ((agenda "" ((org-agenda-skip-function '(my/org-agenda-skip-without-match "-habit"))))
          (tags-todo "inbox"
                     ((org-agenda-overriding-header "Inbox")
                      (org-agenda-prefix-format " %i %-12:c")
                      (org-agenda-hide-tags-regexp ".")))
          (tags-todo "+waitlist+SCHEDULED<=\"<+14d>\""
                     ((org-agenda-overriding-header "Waitlist")
                      (org-agenda-hide-tags-regexp "waitlist")
                      (org-agenda-prefix-format " %i %-12:c %-12(my/org-scheduled-get-time)")))
          (tags-todo "habit+SCHEDULED<=\"<+0d>\""
                     ((org-agenda-overriding-header "Habits")
                      (org-agenda-prefix-format " %i %-12:c")
                      (org-agenda-hide-tags-regexp ".")))))))

(use-package org-yaap
  :straight (org-yaap :type git :host gitlab :repo "SqrtMinusOne/org-yaap")
  :after (org)
  :if (not my/nested-emacs)
  :disabled t
  :config
  (org-yaap-mode 1)
  (setq org-yaap-alert-before '(10 1))
  (setq org-yaap-alert-title "PROXIMITY ALERT")
  (setq org-yaap-todo-keywords-only '("FUTURE")))

(setq my/org-alert-notify-times '(600 60))

(setq my/org-alert--alerts (make-hash-table :test #'equal))

(defun my/org-alert--is-scheduled (label time)
  "Check if LABEL is scheduled to be shown an TIME."
  (gethash (cons label time)
           my/org-alert--alerts nil))

(defun my/org-alert--schedule (label time)
  "Schedule LABEL to be shown at TIME, unless it's already scheduled."
  (unless (my/org-alert--is-scheduled label time)
    (puthash (cons label time)
             (run-at-time time
                          nil
                          (lambda ()
                            (alert label
                                   :title "PROXIMITY ALERT")))
             my/org-alert--alerts)))

(defun my/org-alert-cleanup (&optional keys)
  "Unschedule items that do not appear in KEYS.

KEYS is a list of cons cells like (<label> . <time>)."
  (let ((existing-hash (make-hash-table :test #'equal)))
    (cl-loop for key in keys
             do (puthash key t existing-hash))
    (cl-loop for key being the hash-keys of my/org-alert--alerts
             unless (gethash key existing-hash)
             do (progn
                  (cancel-timer (gethash key my/org-alert--alerts))
                  (remhash key my/org-alert--alerts)))))

(defun my/org-alert--update-today-alerts ()
  (when-let* ((files (org-agenda-files))
              (items
               (org-ql-query
                 :select 'element
                 :from files
                 :where `(and
                          (todo "FUTURE")
                          (ts-active :from ,(format-time-string "%Y-%m-%d %H:%M")
                                     :to ,(format-time-string
                                           "%Y-%m-%d"
                                           (time-add
                                            (current-time)
                                            (* 60 60 24)))
                                     :with-time t))
                 :order-by 'date)))
    (let (scheduled-keys)
      (cl-loop
       for item in items
       for scheduled = (org-timestamp-to-time (org-element-property :scheduled item))
       do (cl-loop
           for before-time in my/org-alert-notify-times
           for label = (format "%s at %s [%s min. remaining]"
                               (org-element-property :raw-value item)
                               (format-time-string "%H:%M" scheduled)
                               (number-to-string (/ before-time 60)))
           for time = (time-convert
                       (+ (time-convert scheduled 'integer) (- before-time)))
           do (progn
                (my/org-alert--schedule label time)
                (push (cons label time) scheduled-keys))))
      (my/org-alert-cleanup scheduled-keys))))

(setq my/org-alert--timer nil)

(define-minor-mode my/org-alert-mode ()
  :global t
  :after-hook
  (if my/org-alert-mode
      (progn
        (my/org-alert--update-today-alerts)
        (when (timerp my/org-alert--timer)
          (cancel-timer my/org-alert--timer))
        (setq my/org-alert--timer
              (run-at-time 600 t #'my/org-alert--update-today-alerts)))
    (when (timerp my/org-alert--timer)
      (cancel-timer my/org-alert--timer))
    (my/org-alert-cleanup)))

(with-eval-after-load 'org
  (if my/emacs-started
      (my/org-alert-mode)
    (add-hook 'emacs-startup-hook #'my/org-alert-mode)))

(defun my/org-clone-subtree-with-time-shift (n &optional shift)
  (interactive "nNumber of clones to produce: ")
  (unless (wholenump n) (user-error "Invalid number of replications %s" n))
  (when (org-before-first-heading-p) (user-error "No subtree to clone"))
  (let* ((beg (save-excursion (org-back-to-heading t) (point)))
         (end-of-tree (save-excursion (org-end-of-subtree t t) (point)))
         (shift
          (or shift
              (if (and (not (equal current-prefix-arg '(4)))
                       (save-excursion
                         (goto-char beg)
                         (re-search-forward org-ts-regexp-both end-of-tree t)))
                  (read-from-minibuffer
                   "Date shift per clone (e.g. +1w, empty to copy unchanged): ")
                "")))                   ;No time shift
         (doshift
          (and (org-string-nw-p shift)
               (or (string-match "\\`[ \t]*\\([+-]?[0-9]+\\)\\([hdwmy]\\)[ \t]*\\'"
                                 shift)
                   (user-error "Invalid shift specification %s" shift)))))
    (goto-char end-of-tree)
    (unless (bolp) (insert "\n"))
    (let* ((end (point))
           (template (buffer-substring beg end))
           (shift-n (and doshift (string-to-number (match-string 1 shift))))
           (shift-what (pcase (and doshift (match-string 2 shift))
                         (`nil nil)
                         ("h" 'hour)
                         ("d" 'day)
                         ("w" (setq shift-n (* 7 shift-n)) 'day)
                         ("m" 'month)
                         ("y" 'year)
                         (_ (error "Unsupported time unit"))))
           (nmin 1)
           (nmax n)
           (n-no-remove -1)
           (org-id-overriding-file-name (buffer-file-name (buffer-base-buffer)))
           (idprop (org-entry-get beg "ID")))
      (when (and doshift
                 (string-match-p "<[^<>\n]+ [.+]?\\+[0-9]+[hdwmy][^<>\n]*>"
                                 template))
        (delete-region beg end)
        (setq end beg)
        (setq nmin 0)
        (setq nmax (1+ nmax))
        (setq n-no-remove nmax))
      (goto-char end)
      (cl-loop for n from nmin to nmax do
               (insert
                ;; Prepare clone.
                (with-temp-buffer
                  (insert template)
                  (org-mode)
                  (goto-char (point-min))
                  (org-show-subtree)
                  (and idprop (if org-clone-delete-id
                                  (org-entry-delete nil "ID")
                                (org-id-get-create t)))
                  (unless (= n 0)
                    (while (re-search-forward org-clock-line-re nil t)
                      (delete-region (line-beginning-position)
                                     (line-beginning-position 2)))
                    (goto-char (point-min))
                    (while (re-search-forward org-drawer-regexp nil t)
                      (org-remove-empty-drawer-at (point))))
                  (goto-char (point-min))

                  (when doshift
                    (while (re-search-forward org-ts-regexp-both nil t)
                      (org-timestamp-change (* n shift-n) shift-what))
                    (save-excursion
                      (goto-char (point-min))
                      (evil-numbers/inc-at-pt n (point-min)))
                    (unless (= n n-no-remove)
                      (goto-char (point-min))
                      (while (re-search-forward org-ts-regexp nil t)
                        (save-excursion
                          (goto-char (match-beginning 0))
                          (when (looking-at "<[^<>\n]+\\( +[.+]?\\+[0-9]+[hdwmy]\\)")
                            (delete-region (match-beginning 1) (match-end 1)))))))
                  (buffer-string)))))
    (goto-char beg)))

(defun my/org--headings-in-outline ()
  (org-ql-query
    :select (lambda () (propertize
                        (substring-no-properties (org-get-heading t t t))
                        'marker (copy-marker (point))))
    :from (append
           (list (buffer-file-name))
           (let ((archive
                  (concat (file-name-directory (buffer-file-name))
                          "archive/"
                          (file-name-nondirectory (buffer-file-name)))))
             (when (file-exists-p archive)
               (list archive))))
    :where `(and (outline-path ,@(org-get-outline-path))
                 (level ,(org-current-level)))))

(defun my/org--heading-strip (heading)
  (thread-last
    heading
    (substring-no-properties)
    (replace-regexp-in-string (rx (| "(" "[") (+ nonl) (| "]" ")")) "")
    (replace-regexp-in-string (rx " " (+ (or digit "."))) " ")
    (replace-regexp-in-string (rx (+ " ")) " ")
    (string-trim)))

(defun my/org--headings-group-seq (headings)
  (thread-last
    headings
    (seq-group-by #'my/org--heading-strip)
    (seq-sort-by #'car #'string-lessp)
    (mapcar (lambda (group)
              (cons (car group)
                    (seq-sort-by
                     (lambda (heading)
                       (save-match-data
                         (or
                          (and (string-match (rx (group (+ digit)))
                                             heading)
                               (string-to-number (match-string 1 heading)))
                          -1)))
                     #'<
                     (cdr group)))))))

(defun my/org-headings-seq ()
  (interactive)
  (let* ((headings (my/org--headings-in-outline))
         (headings-seq (my/org--headings-group-seq headings))
         (buffer (generate-new-buffer "*Sequential Headings in Outline*")))
    (with-current-buffer buffer
      (outline-mode)
      (setq-local widget-push-button-prefix "")
      (setq-local widget-push-button-suffix "")
      (dolist (group headings-seq)
        (insert (format "* %s\n" (car group)))
        (dolist (heading (cdr group))
          (widget-create 'push-button
                         :marker (get-text-property 0 'marker heading)
                         :notify (lambda (widget &rest ignore)
                                   (let ((marker (widget-get widget :marker)))
                                     (pop-to-buffer (marker-buffer marker))
                                     (goto-char marker)))
                         (concat "** " (substring-no-properties heading)))
          (insert "\n")))
      (widget-setup)
      (setq buffer-read-only t)
      (goto-char (point-min)))
    (pop-to-buffer buffer)))

(defun my/org-heading-seq-insert ()
  (interactive)
  (let* ((headings (my/org--headings-in-outline))
         (headings-seq (my/org--headings-group-seq headings))
         (heading (completing-read "Headings: " headings-seq))
         (last-number
          (thread-last headings-seq
                       (assoc heading)
                       (cdr)
                       (mapcar (lambda (x)
                                 (save-match-data
                                   (or
                                    (when (string-match (rx (group (+ digit)))
                                                        x)
                                      (string-to-number (match-string 1 x)))
                                    1))))
                       (seq-max)
                       (1+))))
    (org-insert-heading '(4))
    (insert (format "FUTURE %s %s" heading last-number))))

(defun my/org-archive--get-file ()
  "Get an archive version of the file."
  (let ((archive-file
         (concat
          (file-name-directory (buffer-file-name))
          "archive/" (file-name-nondirectory (buffer-file-name)))))
    (unless (file-exists-p archive-file)
      (make-empty-file archive-file))
    archive-file))

(defun my/org-refile--assert-path-exists (refile-path)
  (cl-assert (equal org-refile-use-outline-path 'file))
  (let* ((parts (string-split refile-path "/"))
         (tbl (mapcar
               (lambda (x)
                 (cons (concat (car x) "/") (cdr x)))
               org-refile-target-table)))
    (cl-loop for i from 1
             for part in (cdr parts)
             for target = (org-refile--get-location
                           (string-join (seq-take parts (1+ i)) "/")
                           tbl)
             unless target
             do (let ((parent-target
                       (org-refile--get-location
                        (string-join (seq-take parts i) "/")
                        tbl)))
                  (push (org-refile-new-child parent-target part) tbl)))))

(defun my/org-archive-refile ()
  (interactive)
  (let* ((org-refile-targets `((,(my/org-archive--get-file) . (:maxlevel . 6))))
         (org-refile-target-table (org-refile-get-targets))
         (org-refile-history nil)
         (org-refile-use-outline-path 'file)
         (org-refile-allow-creating-parent-nodes t)
         (org-outline-path-complete-in-steps nil)
         (refile-path (string-join
                       (append
                        (list (file-name-nondirectory
                               (buffer-file-name)))
                        (org-get-outline-path nil t))
                       "/")))
    ;; The path is already known
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) refile-path)))
      (my/org-refile--assert-path-exists refile-path)
      (org-refile))))

(defun my/org-archive-refile-all (days)
  (interactive (list (read-number "Days: " 60)))
  (let ((records (org-ql-query
                   :select #'element-with-markers
                   :from (current-buffer)
                   :where `(and (ts :to ,(- days)) (done)))))
    (when (y-or-n-p (format "Archive %d records? " (length records)))
      (dolist (record records)
        (let ((marker (org-element-property :org-marker record)))
          (org-with-point-at marker
            (my/org-archive-refile)))))))

(defun my/org-agenda-and-archive ()
  (append
   (org-agenda-files)
   (thread-last "/projects/archive"
                (concat org-directory)
                (directory-files)
                (mapcar (lambda (f)
                          (concat
                           org-directory "/projects/archive/" f)))
                (seq-filter (lambda (f)
                              (not (file-directory-p f)))))))

(my-leader-def
  :infix "o"
  "" '(:which-key "org-mode")
  "c" 'org-capture
  "a" 'org-agenda
  "o" #'my/org-file-open
  "v" #'org-ql-view
  "q" #'org-ql-search)

(with-eval-after-load 'org
  (my-leader-def
    :infix "SPC"
    :keymaps '(org-mode-map)
    "i" #'org-clock-in
    "o" #'org-clock-out
    "O" #'org-clock-cancel
    "c" #'org-clock-goto
    "p" #'org-set-property
    "e" #'org-set-effort
    "r" #'org-priority
    "m" #'my/org-meeting-link))

(use-package org-journal
  :straight t
  :init
  (my-leader-def
    :infix "oj"
    "" '(:which-key "org-journal")
    "j" 'org-journal-new-entry
    "o" 'org-journal-open-current-journal-file
    "s" 'org-journal-tags-status)
  :after org
  :config
  (setq org-journal-dir (concat org-directory "/journal"))
  (setq org-journal-file-type 'weekly)
  (setq org-journal-file-format "%Y-%m-%d.org")
  (setq org-journal-date-format "%A, %Y-%m-%d")
  (setq org-journal-enable-encryption t)
  (setq org-journal-time-format-post-midnight "PM: %R "))

(use-package org-journal-tags
  :straight (:host github :repo "SqrtMinusOne/org-journal-tags")
  :after (org-journal)
  :config
  (org-journal-tags-autosync-mode)
  (general-define-key
   :keymaps 'org-journal-mode-map
   "C-c t" #'org-journal-tags-insert-tag))

(use-package request
  :straight t
  :defer t)

(defvar my/weather-last-time 0)
(defvar my/weather-value nil)

(defun my/weather-get ()
  (when (> (- (time-convert nil 'integer) my/weather-last-time)
           (* 60 5))
    (request (format "https://wttr.in/%s" my/location)
      :params '(("format" . "%l:%20%C%20%t%20%w%20%p"))
      :sync t
      :parser (lambda () (url-unhex-string (buffer-string)))
      :timeout 10
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setq my/weather-value data)
                  (setq my/weather-last-time (time-convert nil 'integer))))
      :error
      (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                     (message "Got error: %S" error-thrown)))))
  my/weather-value)

(defun my/set-journal-header ()
  (org-set-property "Emacs" emacs-version)
  (org-set-property "Hostname" (my/system-name))
  (org-journal-tags-prop-apply-delta :add (list (format "host.%s" (my/system-name))))
  (when (boundp 'my/location)
    (org-set-property "Location" my/location)
    (when-let ((weather (my/weather-get)))
      (org-set-property "Weather" weather)))
  (when (boundp 'my/loc-tag)
    (org-journal-tags-prop-apply-delta :add (list my/loc-tag)))
  (when (fboundp 'emms-playlist-current-selected-track)
    (let ((track (emms-playlist-current-selected-track)))
      (when track
        (let ((album (cdr (assoc 'info-album track)))
              (artist (or (cdr (assoc 'info-albumartist track))
                          (cdr (assoc 'info-album track))))
              (title (cdr (assoc 'info-title track)))
              (string ""))
          (when artist
            (setq string (concat string "[" artist "] ")))
          (when album
            (setq string (concat string album " - ")))
          (when title
            (setq string (concat string title)))
          (when (> (length string) 0)
            (org-set-property "EMMS_Track" string)))))))

(add-hook 'org-journal-after-entry-create-hook
          #'my/set-journal-header)

(defun my/org-journal-decrypt ()
  "Decrypt the current org journal file."
  (interactive)
  (org-journal-tags--ensure-decrypted))

(use-package citar
  :straight t
  :init
  (my-leader-def "fB" #'citar-open)
  :commands (citar-open citar-insert-citation)
  :config
  (setq
   org-cite-global-bibliography '("~/30-39 Life/32 org-mode/library.bib")
   org-cite-insert-processor 'citar
   org-cite-follow-processor 'citar
   org-cite-activate-processor 'citar
   citar-bibliography org-cite-global-bibliography)
  (setq org-cite-export-processors
        '((latex bibtex "numeric")))
  (setq citar-library-paths
        '("~/30-39 Life/33 Library/33.01 Documents/"))
  (add-hook 'latex-mode #'citar-capf-setup)
  (add-hook 'org-mode #'citar-capf-setup))

(use-package citar-embark
  :after (citar embark)
  :straight t
  :config
  (citar-embark-mode))

(use-package org-ref
  :straight (:files (:defaults "citeproc" (:exclude "*helm*")))
  :commands (org-ref-insert-link-hydra/body
             org-ref-bibtex-hydra/body)
  :init
  (setq bibtex-dialect 'biblatex)
  (add-hook 'bibtex-mode 'smartparens-mode)
  :after (org)
  :config
  (general-define-key
   :keymaps 'org-mode-map
   "C-c l" #'org-ref-insert-link-hydra/body)
  (general-define-key
   :keymaps 'bibtex-mode-map
   "M-RET" 'org-ref-bibtex-hydra/body)
  (setq org-ref-insert-cite-function
        (lambda ()
          (call-interactively #'citar-insert-citation))))

(use-package emacsql-sqlite
  :defer t
  :straight (:type built-in))

(use-package org-roam
  :straight (:host github :repo "org-roam/org-roam"
                   :files (:defaults "extensions/*.el"))
  :if (file-directory-p org-roam-directory)
  :after org
  :init
  (setq org-roam-file-extensions '("org"))
  (setq org-roam-v2-ack t)
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  :config
  (org-roam-setup)
  (require 'org-roam-protocol))

(setq org-roam-capture-templates
      `(("d" "default" plain "%?"
         :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
         :unnarrowed t)
        ("f" "fleeting" plain "%?"
         :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: :fleeting:\n")
         :unnarrowed t)
        ("e" "encrypted" plain "%?"
         :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org.gpg" "#+title: ${title}\n")
         :unnarrowed t)))

(use-package org-roam-ql
  :straight t
  :after (org-roam)
  :config
  (general-define-key
   :states '(normal visual)
   :keymaps '(org-roam-ql-mode-map)
   "s" #'org-roam-ql-buffer-dispatch))

(defun my/org-roam-node-find-permanent (&optional other-window)
  (interactive current-prefix-arg)
  (org-roam-node-find
   other-window
   nil
   (lambda (node)
     (not
      (seq-contains-p
       "fleeting"
       (org-roam-node-tags node))))))

(defun my/org-roam-node-insert-permanent ()
  (interactive)
  (org-roam-node-insert
   (lambda (node)
     (not
      (seq-contains-p
       (org-roam-node-tags node)
       "fleeting")))))

(defun my/org-roam-ql-fleeting ()
  (interactive)
  (org-roam-ql-search
   '(tags "fleeting")
   "Fleeting notes"))

(with-eval-after-load 'org-roam
  (my-leader-def
    :infix "or"
    "" '(:which-key "org-roam")
    "i" #'my/org-roam-node-insert-permanent
    "r" #'my/org-roam-node-find-permanent
    "g" #'org-roam-graph
    "c" #'org-roam-capture
    "b" #'org-roam-buffer-toggle
    "q" #'org-roam-ql-search
    "f" #'my/org-roam-ql-fleeting)
  (general-define-key
   :keymaps 'org-roam-mode-map
   :states '(normal)
   "TAB" #'magit-section-toggle
   "q" #'quit-window
   "k" #'magit-section-backward
   "j" #'magit-section-forward
   "gr" #'revert-buffer
   "RET" #'org-roam-buffer-visit-thing))

(with-eval-after-load 'org
  (my-leader-def
    :keymap 'org-mode-map
    :infix "or"
    "t" #'org-roam-tag-add
    "T" #'org-roam-tag-remove
    "s" #'org-roam-db-autosync-mode
    "a" #'org-roam-alias-add)
  (general-define-key
   :keymap 'org-mode-map
   "C-c i" #'my/org-roam-node-insert-permanent
   "C-c I" #'org-roam-node-insert))

(defface my/org-roam-count-overlay-face
  '((t :inherit tooltip))
  "Face for Org Roam count overlay.")

(defun my/org-roam--count-overlay-make (pos count)
  (let* ((overlay-value (concat
                         " "
                         (propertize
                          (format "%d" count)
                          'face 'my/org-roam-count-overlay-face)
                         " "))
         (ov (make-overlay pos pos (current-buffer) nil t)))
    (overlay-put ov 'roam-backlinks-count count)
    (overlay-put ov 'priority 1)
    (overlay-put ov 'after-string overlay-value)))

(defun my/org-roam--count-overlay-remove-all ()
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (overlay-get ov 'roam-backlinks-count)
      (delete-overlay ov))))

(defun my/org-roam--count-overlay-make-all ()
  (my/org-roam--count-overlay-remove-all)
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (elem)
      (when (string-equal (org-element-property :type elem) "id")
        (let* ((id (org-element-property :path elem))
               (count (caar
                       (org-roam-db-query
                        [:select (funcall count source)
                         :from links
                         :where (= dest $s1)
                         :and (= type "id")]
                        id))))
          (when (< 0 count)
            (my/org-roam--count-overlay-make
             (org-element-property :end elem)
             count)))))))

(define-minor-mode my/org-roam-count-overlay-mode
  "Display backlink count for org-roam links."
  :after-hook
  (if my/org-roam-count-overlay-mode
      (progn
        (my/org-roam--count-overlay-make-all)
        (add-hook 'after-save-hook #'my/org-roam--count-overlay-make-all nil t))
    (my/org-roam--count-overlay-remove-all)
    (remove-hook 'after-save-hook #'my/org-roam--count-overlay-remove-all t)))

(defun my/org-roam-extract-links ()
  (interactive)
  (let ((buffer (generate-new-buffer "*roam-links*"))
        elems)
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (elem)
        (when (string-equal (org-element-property :type elem) "id")
          (push elem elems))))
    (with-current-buffer buffer
      (cl-loop for elem in elems
               for file-name =
               (file-name-nondirectory
                (caar
                 (org-roam-db-query
                  [:select [file]
                           :from nodes
                           :where (= id $s1)]
                  (org-element-property :path elem))))
               do (insert file-name "\n")))
    (switch-to-buffer buffer)))

(use-package org-roam-ui
  :straight (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  ;; :hook (org-roam . org-roam-ui-mode)
  :init
  (my-leader-def "oru" #'org-roam-ui-mode))

(use-package deft
  :straight t
  :commands (deft)
  :after (org)
  :init
  (my-leader-def "ord" #'deft)
  :config
  (setq deft-directory org-roam-directory)
  (setq deft-recursive t)
  (setq deft-use-filter-string-for-filename t)
  (add-hook 'deft-mode-hook
            (lambda () (display-line-numbers-mode -1)))
  (general-define-key
   :keymaps 'deft-mode-map
   :states '(normal motion)
   "q" #'quit-window
   "r" #'deft-refresh
   "s" #'deft-filter
   "d" #'deft-filter-clear
   "y" #'deft-filter-yank
   "t" #'deft-toggle-incremental-search
   "o" #'deft-toggle-sort-method))

(setq deft-strip-summary-regexp
      (rx (or
           (: ":PROPERTIES:" (* anything) ":END:")
           (: "#+" (+ alnum) ":" (* nonl))
           (regexp "[\n\t]"))))

(defun my/deft-parse-summary-around (fun contents title)
  (funcall fun (org-link-display-format contents) title))

(with-eval-after-load 'deft
  (advice-add #'deft-parse-summary :around #'my/deft-parse-summary-around))

(defun my/deft-parse-title (file contents)
  (with-temp-buffer
    (insert contents)
    (goto-char (point-min))
    (if (search-forward-regexp (rx (| "#+title:" "#+TITLE:")) nil t)
        (string-trim (buffer-substring-no-properties (point) (line-end-position)))
      file)))

(defun my/deft-parse-title-around (fun file contents)
  (or (my/deft-parse-title file contents)
      (funcall fun file contents)))

(with-eval-after-load 'deft
  (advice-add #'deft-parse-title :around #'my/deft-parse-title-around))

(defun my/org-roam-node-setup ()
  (setq-local org-hide-emphasis-markers t)
  (org-appear-mode 1)
  (when (display-graphic-p)
    (org-fragtog-mode 1)
    (org-latex-preview '(16))))

(with-eval-after-load 'org
  (add-hook 'org-roam-find-file-hook 'my/org-roam-node-setup))

(setq my/git-diff-status
      '(("A" . added)
        ("C" . copied)
        ("D" . deleted)
        ("M" . modified)
        ("R" . renamed)
        ("R100" . moved)
        ("T" . type-changed)
        ("U" . unmerged)))

(defun my/get-files-status (rev)
  (let* ((rev-cmd (if (string-match-p "^@{" rev)
                      (concat "$(git rev-list -1 --before=\""
                              (substring rev 2 -1) "\" HEAD)")
                    rev))
         (files (shell-command-to-string
                 (concat "git diff --name-status " rev-cmd))))
    (mapcar
     (lambda (file)
       (let ((elems (split-string file "\t")))
         (cons
          (cdr (assoc (car elems) my/git-diff-status))
          (car (last elems)))))
     (split-string files "\n" t))))

(defun my/org-changed-files-since-date (date)
  (let ((default-directory org-directory))
    (my/get-files-status (format "@{%s}" date))))

(defun my/org-review--org-roam-get-changes (date)
  (let ((changes (my/org-changed-files-since-date date))
        (nodes (org-roam-node-list))
        (nodes-by-file (make-hash-table :test #'equal)))
    (cl-loop for node in nodes
             for file = (org-roam-node-file node)
             do (puthash file node nodes-by-file))
    (let* ((changed-nodes
            (thread-last
              changes
              (mapcar (lambda (c)
                        (cons (car c)
                              (gethash
                               (concat org-directory "/" (cdr c))
                               nodes-by-file))))
              (seq-filter #'cdr)))
           (changed-inbox
            (thread-last
              changes
              (seq-filter
               (lambda (file) (string-match-p (rx bos "inbox-notes") (cdr file))))))
           (changed-fleeting
            (thread-last
              changed-nodes
              (seq-filter (lambda (c)
                            (seq-contains-p (org-roam-node-tags (cdr c))
                                            "fleeting")))
              (seq-sort-by (lambda (c) (concat (symbol-name (car c))
                                               (org-roam-node-title (cdr c))))
                           #'string-lessp)))
           (changed-permanent
            (thread-last
              changed-nodes
              (seq-filter (lambda (c)
                            (not (seq-contains-p (org-roam-node-tags (cdr c))
                                                 "fleeting"))))
              (seq-sort-by (lambda (c) (concat (symbol-name (car c))
                                               (org-roam-node-title (cdr c))))
                           #'string-lessp))))
      (list
       changed-inbox
       changed-fleeting
       changed-permanent))))

(defun my/org-review-org-roam-format (date)
  (let* ((data (my/org-review--org-roam-get-changes date))
         (changed-inbox (nth 0 data))
         (changed-fleeting (nth 1 data))
         (changed-permanent (nth 2 data)))
    (concat
     "Changes in inbox:\n"
     (thread-last
       changed-inbox
       (mapcar (lambda (change)
                 (format "- %s :: %s\n"
                         (cond
                          ((or (member (car change) '(deleted moved))
                               (string-match-p "figured-out" (cdr change)))
                           "Processed")
                          (t (capitalize (symbol-name (car change)))))
                         (cdr change))))
       (apply #'concat))
     "\nChanges in fleeting notes:\n"
     (thread-last
       changed-fleeting
       (mapcar (lambda (c)
                 (format "- %s :: [[id:%s][%s]]\n"
                         (capitalize (symbol-name (car c)))
                         (org-roam-node-id (cdr c))
                         (org-roam-node-title (cdr c)))))
       (apply #'concat))
     "\nChanges in permanent notes:\n"
     (thread-last
       changed-permanent
       (mapcar (lambda (c)
                 (format "- %s :: [[id:%s][%s]]\n"
                         (capitalize (symbol-name (car c)))
                         (org-roam-node-id (cdr c))
                         (org-roam-node-title (cdr c)))))
       (apply #'concat)))))

(defun my/org-review-get-last-review-date (kind)
  (let* ((start-of-day (- (time-convert nil #'integer)
                          (% (time-convert nil #'integer)
                             (* 24 60 60))))
         (query-res (org-journal-tags-query
                     :tag-names (list (format "review.%s" kind))
                     :start-date (pcase kind
                                   ('weekly
                                    (- start-of-day
                                       (* 30 24 60 60)))
                                   ('zk
                                    (- start-of-day
                                       (* 365 24 60 60)))
                                   (_ (error "Unsupported kind: %s" kind)))
                     :location 'section
                     :order 'descending)))
    (if query-res
        (org-journal-tag-reference-date (car query-res))
      (pcase kind
        ('weekly (- start-of-day (* 7 24 60 60)))
        ('zk (- start-of-day (* 45 24 60 60)))))))

(defun my/org-review-set-weekly-record ()
  (save-excursion
    (let ((last-review-date (my/org-review-get-last-review-date 'weekly)))
      (org-journal-tags-prop-apply-delta :add '("review.weekly"))
      (insert "Weekly Review")
      (goto-char (point-max))

      (insert "Last review date: "
              (format-time-string
               "[%Y-%m-%d]"
               (seconds-to-time last-review-date)))
      (insert "

Review checklist (/delete this/):
- [ ] Clear email inbox
- [ ] Reconcile ledger
- [ ] Clear [[file:~/Downloads][downloads]] and [[file:~/00-Scratch][scratch]] folders
- [ ] Process [[file:~/30-39 Life/35 Photos/35.00 Inbox/][photo inbox]]
- [ ] Process [[file:../inbox.org][inbox]]
- [ ] Create [[file:../recurring.org][recurring tasks]] for next week
- [ ] Check agenda (-1 / +2 weeks): priorities, deadlines
- [ ] Check TODOs: priorities, deadlines
  - [[org-ql-search:todo%3A?buffers-files=%22org-agenda-files%22&super-groups=%28%28%3Aauto-outline-path-file%20t%29%29&sort=%28priority%20todo%20deadline%29][org-ql-search: All TODOs]]
  - [[org-ql-search:(and (todo) (not (tags \"nots\")) (not (ts :from -14)) (not (todo \"MAYBE\")))?buffers-files=%22org-agenda-files%22&super-groups=%28%28%3Aauto-outline-path-file%20t%29%29&sort=%28priority%20todo%20deadline%29][org-ql-search: Stale tasks]]
  - [[org-ql-search:todo%3AWAIT?buffers-files=%22org-agenda-files%22&super-groups=%28%28%3Aauto-outline-path-file%20t%29%29&sort=%28priority%20todo%20deadline%29][org-ql-search: WAIT]]
  - [[org-ql-search:todo%3AMAYBE?buffers-files=%22org-agenda-files%22&super-groups=%28%28%3Aauto-outline-path-file%20t%29%29&sort=%28priority%20todo%20deadline%29][org-ql-search: MAYBE]]
- [ ] Run auto-archiving
- [ ] Review journal records
")

      (insert "
*** Summary
TODO Write something, maybe? "))))

(defun my/org-review-weekly ()
  (interactive)
  (let ((org-journal-after-entry-create-hook
         `(,@org-journal-after-entry-create-hook
           my/org-review-set-weekly-record)))
    (org-journal-new-entry nil)
    (org-fold-show-subtree)))

(with-eval-after-load 'org-journal
  (my-leader-def "ojw" #'my/org-review-weekly))

(defun my/kill-messengers ()
  (interactive)
  (when (get-buffer telega-root-buffer-name)
    (telega-kill t))
  (call-process-shell-command "pkill -f rocketchat-desktop")
  (call-process-shell-command "pkill -f 'bwrap --args 36 element'")
  (call-process-shell-command "pkill -f element-desktop"))

(defun my/org-review-set-daily-record ()
  (let* ((today (format-time-string
                 "%Y-%m-%d"
                 (days-to-time
                  (- (org-today) (time-to-days 0)))))
         (roam-changes (my/org-review--org-roam-get-changes today)))
    (save-excursion
      (org-journal-tags-prop-apply-delta :add '("review.daily"))
      (insert "Daily Review")
      (goto-char (point-max))

      (insert "
Maintenance checklist (/delete this/):
- [ ] [[elisp:(my/kill-messengers)][Close all messengers]]
- [ ] Process [[file:../inbox.org][inbox]]
- [ ] Check if clocked tasks are properly annotated
  - [[elisp:(my/org-ql-clocked-today)][Tasks clocked today]]
  - [[elisp:(my/org-ql-closed-today)][Tasks closed today]]
- [ ] Check agenda for the current week

/Remember, all of the following headers are optional./

*** Happened today
Happened to me:
- /Anything interesting?/
Happened to the world:
- /Anything important?/

*** New ideas
/Write them down in org-roam with the \"fleeting\" tag; leave links here. Perhaps note what sparked that idea?/
"
              (thread-last
                (nth 1 roam-changes)
                (seq-filter (lambda (c) (eq 'added (car c))))
                (mapcar (lambda (c)
                          (format "- [[id:%s][%s]]\n"
                                  (org-roam-node-id (cdr c))
                                  (org-roam-node-title (cdr c)))))
                (apply #'concat))
              "
*** Interactions today
/Any meaninginful interactions, conflicts or tensions?/

*** Emotions today
/How did I feel?/
"))))

(defun my/org-review-daily ()
  (interactive)
  (let ((org-journal-after-entry-create-hook
         `(,@org-journal-after-entry-create-hook
           my/org-review-set-daily-record)))
    (org-journal-new-entry nil)
    (org-fold-show-subtree)))

(with-eval-after-load 'org-journal
  (my-leader-def "ojd" #'my/org-review-daily))

(defun my/org-review-org-roam-format-zk-before (date)
  (let* ((data (my/org-review--org-roam-get-changes date))
         (changed-inbox (nth 0 data))
         (changed-fleeting (nth 1 data))
         (changed-permanent (nth 2 data)))
    (concat
     (when changed-inbox
       (concat
        "Process these changes in inbox:\n"
        (thread-last
          changed-inbox
          (mapcar (lambda (change)
                    (format "- [ ] %s :: %s\n"
                            (cond
                             ((or (member (car change) '(deleted moved))
                                  (string-match-p "figured-out" (cdr change)))
                              "Processed")
                             (t (capitalize (symbol-name (car change)))))
                            (cdr change))))
          (apply #'concat))
        "\n"))
     (when changed-fleeting
       (concat
        "Process these fleeting notes:\n"
        (thread-last
          changed-fleeting
          (mapcar (lambda (c)
                    (format "- [ ] %s :: [[id:%s][%s]]\n"
                            (capitalize (symbol-name (car c)))
                            (org-roam-node-id (cdr c))
                            (org-roam-node-title (cdr c)))))
          (apply #'concat))
        "\n"))
     (when changed-permanent
       (concat
        "Check these changes in permanent notes:\n"
        (thread-last
          changed-permanent
          (mapcar (lambda (c)
                    (format "- [ ] %s :: [[id:%s][%s]]\n"
                            (capitalize (symbol-name (car c)))
                            (org-roam-node-id (cdr c))
                            (org-roam-node-title (cdr c)))))
          (apply #'concat)))))))

(defun my/org-review-org-roam-finish (date)
  (org-roam-db-sync)
  (save-excursion
    (org-back-to-heading)
    (replace-regexp
     (rx
      ":BEGIN_REVIEW:" (* anything) ":END_REVIEW:")
     (string-trim
      (my/org-review-org-roam-format date)))))

(defun my/org-review-set-zk-record ()
  (save-excursion
    (let ((last-review-date (my/org-review-get-last-review-date 'zk)))
      (org-journal-tags-prop-apply-delta :add '("review.zk"))
      (insert "Zettelkasten Review")
      (goto-char (point-max))

      (insert "Last review date: "
              (format-time-string
               "[%Y-%m-%d]"
               (seconds-to-time last-review-date)))

      (insert "\n\n:BEGIN_REVIEW:\n"
              "Process all the required categories in this block, then execute \"Finish review\".\n\n"
              (string-trim
               (my/org-review-org-roam-format-zk-before last-review-date))
              "\n\n[[elisp:(my/org-review-org-roam-finish \""
              (format-time-string "%Y-%m-%d" last-review-date)
              "\")][Finish review]]"
              "\n:END_REVIEW:\n"))))

(defun my/org-review-zk ()
  (interactive)
  (let ((org-journal-after-entry-create-hook
         `(,@org-journal-after-entry-create-hook
           my/org-review-set-zk-record)))
    (org-journal-new-entry nil)
    (org-fold-show-subtree)))

(with-eval-after-load 'org-journal
  (my-leader-def "ojz" #'my/org-review-zk))

(use-package org-contacts
  :straight (:type git :repo "https://repo.or.cz/org-contacts.git")
  :after (org)
  :config
  (setq org-contacts-files (list
                            (concat org-directory "/contacts.org")))
  (add-to-list 'org-contacts-completion-enabled-mode-list
               'notmuch-message-mode))

(defun my/calfw-setup-buffer ()
  (display-line-numbers-mode -1))

(use-package calfw
  :straight t
  :defer t
  :config
  (add-hook 'cfw:calendar-mode-hook #'my/calfw-setup-buffer))

(use-package calfw-org
  :after (calfw org)
  :straight t)

(defun my/org-timeblock-conf ()
  (display-line-numbers-mode -1))

(use-package org-timeblock
  :straight (:host github :repo "ichernyshovvv/org-timeblock")
  :commands (org-timeblock-mode)
  :init
  (my-leader-def "ot" #'org-timeblock)
  :config
  (add-hook 'org-timeblock-mode-hook #'my/org-timeblock-conf)
  (general-define-key
   :keymaps '(org-timeblock-mode-map)
   :states '(normal visual)
   "j" #'org-timeblock-forward-block
   "h" #'org-timeblock-backward-column
   "l" #'org-timeblock-forward-column
   "k" #'org-timeblock-backward-block
   "M-[" #'org-timeblock-day-earlier
   "M-]" #'org-timeblock-day-later
   "H" #'org-timeblock-day-earlier
   "L" #'org-timeblock-day-later
   "RET" #'org-timeblock-goto
   "t" #'org-timeblock-todo-set
   "q" #'quit-window))

(use-package org-drill
  :straight t
  :commands (org-drill)
  :after (org))

(provide 'sqrt-org-productivity)
