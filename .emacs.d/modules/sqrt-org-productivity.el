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

(setq org-todo-keyword-faces
      '(("WAIT" . font-lock-escape-face)
        ("DELEGATED" . font-lock-keyword-face)))

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

(setq org-priority-highest ?A)
(setq org-priority-lowest (+ ?A 9))

(with-eval-after-load 'org
  (add-to-list
   'org-global-properties
   '("Effort_ALL" . "0 0:05 0:10 0:15 0:20 0:30 0:45 1:00 1:30 2:00 3:00 4:00 8:00")))

(defun my/org-planned-effort--bounds ()
  "Return the boundaries for the current task's EFFORT drawer.

Return a list of (BEG CONTENT-BEG CONTENT-END END)"
  (save-excursion
    (org-back-to-heading t)
    (let ((entry-end (save-excursion
                       (if (outline-next-heading) (point) (point-max)))))
      (forward-line)
      (when (re-search-forward "^[ \t]*:EFFORT:[ \t]*$" entry-end t)
        (let ((beg (line-beginning-position))
              (content-beg (progn (forward-line) (point))))
          (unless (re-search-forward "^[ \t]*:END:[ \t]*$" entry-end t)
            (user-error "Unclosed EFFORT drawer"))
          (list beg content-beg (line-beginning-position)
                (progn (forward-line) (point))))))))

(defun my/org-planned-effort-parse-string (string)
  "Parse STRING as EFFORT drawer contents.

Return an alist of (TIMESTAMP . MINUTES)."
  (let (rows)
    (dolist (line (split-string string "\n"))
      (when (string-match-p "\\S-" line)
        (unless (string-match org-ts-regexp-both line)
          (user-error "Invalid effort line: %s" line))
        (let* ((timestamp (match-string 0 line))
               (timestamp-end (match-end 0))
               (day (org-time-string-to-absolute timestamp))
               (duration (string-trim-left (substring line timestamp-end) "[ \t:]+"))
               (minutes (round (org-duration-to-minutes duration)))
               (row (assq day rows)))
          (if row
              (setcdr (cdr row) minutes)
            (push (cons day (cons timestamp minutes)) rows)))))
    (mapcar #'cdr (sort rows (lambda (a b) (< (car a) (car b)))))))

(defun my/org-planned-effort-parse ()
  "Parse the current task's EFFORT drawer.

Return an alist of (TIMESTAMP . MINUTES)."
  (let ((bounds (my/org-planned-effort--bounds)))
    (and bounds
         (my/org-planned-effort-parse-string
          (buffer-substring-no-properties
           (nth 1 bounds) (nth 2 bounds))))))

(defun my/org-planned-effort--write (entries)
  "Rewrite the current task's EFFORT drawer with ENTRIES.

ENTRIES is as returned by `my/org-planned-effort-parse'."
  (let ((bounds (my/org-planned-effort--bounds))
        (inhibit-read-only t))
    (when bounds (delete-region (car bounds) (nth 3 bounds)))
    (when entries
      (goto-char (or (car bounds)
                    (save-excursion
                      (org-back-to-heading t)
                      (org-end-of-meta-data nil)
                      (point))))
      (insert
       ":EFFORT:\n"
       (mapconcat (lambda (entry)
                    (format "%s: %s"
                            (car entry)
                            (org-duration-from-minutes (cdr entry) 'h:mm)))
                  entries "\n")
       "\n:END:\n"))))

(defun my/org-planned-effort--put (timestamp minutes entries)
  "Return ENTRIES with TIMESTAMP's day set to MINUTES."
  (let* ((day (org-time-string-to-absolute timestamp))
         (old (cl-find day entries
                       :key (lambda (e) (org-time-string-to-absolute (car e))))))
    (if old
        (setcdr old minutes)
      (push (cons timestamp minutes) entries))
    (seq-sort-by (lambda (e)
                   (org-time-string-to-absolute (car e)))
                 #'< entries)))

(defun my/org-planned-effort-add-daily (date duration)
  "Add planned DURATION for DATE to the current Org task's EFFORT drawer."
  (interactive
   (list (org-read-date nil t nil "Date: ")
         (read-string "Effort: ")))
  (let* ((timestamp (format-time-string (org-time-stamp-format nil nil) date))
         (minutes (round (org-duration-to-minutes duration))))
    (my/org-planned-effort--write
     (my/org-planned-effort--put timestamp minutes
                                 (my/org-planned-effort-parse)))))

(defun my/org-planned-effort-remove-daily (date)
  "Remove planned effort for DATE from the current Org task's EFFORT drawer."
  (interactive (list (org-read-date nil t nil "Date: ")))
  (let* ((day (org-time-string-to-absolute
               (format-time-string (org-time-stamp-format nil nil) date)))
         (entries (my/org-planned-effort-parse))
         (kept (cl-remove day entries
                          :key (lambda (entry)
                                 (org-time-string-to-absolute (car entry))))))
    (when (= (length entries) (length kept))
      (user-error "No effort for that day"))
    (my/org-planned-effort--write kept)))

(with-eval-after-load 'org-ql
  (org-ql-defpred has-effort ()
    "Match entries with an effort property or an EFFORT drawer."
    :body
    (or (org-entry-get (point) "EFFORT")
        (my/org-planned-effort--bounds))))

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
          (not (todo "WAIT" "DELEGATED"))
          (not (deadline))
          (not (scheduled))
          (not (tags "nb"))
          (category "JOB" "TEACH" "EDU"))
    :title "Backlog"
    :sort '(todo priority)
    :super-groups '((:auto-priority-outline-path-file t))))

(setq org-ql-views
      (list
       (cons "Overview: All TODO" #'my/org-ql-all-todo)
       (cons "Overview: Backlog" #'my/backlog)
       (cons "Review: WAIT"
             (list :buffers-files #'org-agenda-files
                   :query '(todo "WAIT")
                   :sort '(todo priority date)
                   :super-groups '((:auto-outline-path-file t))))
       (cons "Review: Stale tasks"
             (list :buffers-files #'org-agenda-files
                   :query '(and (todo)
                                (clocked)
                                (not (priority))
                                (not (todo "WAIT"))
                                (not (todo "DELEGATED"))
                                (not (todo "PLAN"))
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
       (cons "Review: Missed deadlines"
             (list :buffers-files #'org-agenda-files
                   :query '(and (todo)
                                (not (todo "WAIT"))
                                (deadline :to -1))
                   :title "Review: Missed deadlines"
                   :sort '(todo priority date)
                   :super-groups '((:auto-outline-path-file t))))
       (cons "Fix: Missing timestamps"
             (list :buffers-files #'org-agenda-files
                   :query '(and (todo "TODO")
                                (not (scheduled))
                                (not (deadline))
                                (not (priority))
                                (not (tags "nots")))
                   :title "Fix: Missing timestamps"
                   :sort '(todo date)
                   :super-groups '((:auto-outline-path-file t))))
       (cons "Fix: Missing effort"
             (list :buffers-files #'org-agenda-files
                   :query '(and (not (has-effort))
                                (todo "TODO")
                                (or (scheduled) (deadline))
                                (not (habit))
                                (not (tags "nots")))
                   :title "Fix: Missing timestamps"
                   :sort '(todo date)
                   :super-groups '((:auto-outline-path-file t))))
       (cons "Fix: Passed events"
             (list
              :buffers-files #'org-agenda-files
              :query '(and (todo "FUTURE")
                           (scheduled :to -1))
              :title "Fix: passed events"
              :sort '(todo date)
              :super-groups '((:auto-outline-path-file t))))
       (cons "Focus: View focus" #'my/org-focus-view)
       (cons "Focus: Tasks without focus" #'my/org-focus-view-tasks-without-focus)))

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

(defun my/org-ql-store-link ()
  "Store an Org QL link, expanding function-valued `org-ql-view-buffers-files'."
  (interactive)
  (let ((org-ql-view-buffers-files
         (pcase org-ql-view-buffers-files
           ((or 'org-agenda-files '(function org-agenda-files))
            (org-agenda-files))
           ((pred functionp)
            (funcall org-ql-view-buffers-files))
           (_ org-ql-view-buffers-files))))
    (call-interactively #'org-store-link)))

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

(setq my/org-focus-file (concat org-directory "/planning/focus-pocus.org"))

(with-eval-after-load 'org-id
  (add-to-list 'org-id-files my/org-focus-file))

(with-eval-after-load 'org-capture
  (setf (alist-get "f" org-capture-templates nil nil #'equal)
        `("Focus" entry (file+olp ,my/org-focus-file "Focuses")
          ,(concat "** ACTIVE %?\n"
                   "%^{ID}p\n"
                   "/Entered on/ %U\n")
          :prepend t
          :jump-to-captured t)))

(defun my/org-focus--select-focus ()
  "Select focus at point."
  (save-excursion
    (let* ((headline (org-element-headline-parser)))
      (org-ql--add-markers headline)
      (org-element-put-property
       headline :outline-path
       (org-get-outline-path t))
      headline)))

(defun my/org-focus--find ()
  "Find all currently active org focuses."
  (org-ql-query
    :select #'my/org-focus--select-focus
    :from my/org-focus-file
    :where '(and (todo)
                 (outline-path "Focuses"))))

(defun my/org-focus--find-by-id (id)
  "Find focus by ID.  Return nil if not found."
  (car (org-ql-query
         :select 'element-with-markers
         :from my/org-focus-file
         :where `(property "ID" ,id))))

(defconst my/org-focus-prop "FOCUS")

(defmacro my/org-focus--with-element (&rest body)
  "Try to execute BODY on element under cursor.

If the element is an `org-agenda' element reference, resolve that."
  `(cond ((derived-mode-p #'org-agenda-mode)
          (if-let ((marker (get-text-property (point) 'org-hd-marker)))
              (org-super-agenda--when-with-marker-buffer marker
                ,@body)
            (user-error "No element at point!")))
         (t ,@body)))

(defun my/org-focus-assign-to-task (focus)
  "Assign FOCUS to task under cursor."
  (interactive (list (my/org-focus--pick)))
  (unless (org-element-property :ID focus)
    (user-error "Focus doesn't have an ID!"))
  (my/org-focus--with-element
   (org-set-property my/org-focus-prop (org-element-property :ID focus))))

(defun my/org-focus--at-point ()
  "Get focus under cursor or nil."
  (when (equal (buffer-file-name) my/org-focus-file)
    (save-excursion
      (org-back-to-heading t)
      (when (seq-contains-p (org-get-outline-path) "Focuses")
        (let ((elem (org-element-at-point)))
          (when (org-element-property :ID elem)
            elem))))))

(defun my/org-focus--week-range (year number)
  "Determine week boundaries by YEAR and its NUMBER.

NUMBER may range from 1 to 53

Return a cons cell of two time values:
- week start (by UTC),
- week end (by UTC).

If the week is incomplete (at a start or end of the year), return the
first or the last day of the year as the boundary."
  (let* ((year-start `(0 0 0 1 1 ,year nil -1 0))
         ;; to get dow
         (year-start (decode-time (encode-time year-start) t))
         ;; Monday - 0, Sunday - 6
         (dow (% (+ (decoded-time-weekday year-start) 6) 7))
         (year-week-start (time-add (encode-time year-start) (* (- dow) 60 60 24)))
         (week-start (time-add year-week-start (* (1- number) 60 60 24 7)))
         (week-end (time-add week-start (* 6 60 60 24)))
         (week-start-year (decoded-time-year (decode-time week-start)))
         (week-end-year (decoded-time-year (decode-time week-end))))
    (when (and (not (= year week-start-year))
               (not (= year week-end-year)))
      (error "Wrong week number %s: both boundaries are out of year %s (%s, %s)"
             number year week-start-year week-end-year))

    (when (not (= year week-start-year))
      (setq week-start (encode-time year-start)))
    (when (not (= year week-end-year))
      (setq week-end (encode-time `(0 0 0 31 12 ,year nil -1 0))))

    ;; Cast to epoch for consistency: `time-add' returns epochs
    ;; somehow...
    (cons
     (time-convert week-start 'integer)
     (time-convert week-end 'integer))))

(defun my/org-focus--pick-week ()
  "Prompt the user for a date and return its week.

Return a cons cell of two values:
- year (integer)
- week number (integer)."
  (let* ((time (org-read-date nil t))
         (week-year (string-to-number (format-time-string "%Y" time)))
         (week-number (1+ (string-to-number (format-time-string "%W" time)))))
    (cons week-year week-number)))

(setq my/org-focus-day-format "%A, %Y-%m-%d")

(defun my/org-focus--week-template ()
  "Return a template for the weekly focus planning section."
  (pcase-let* ((`(,year . ,week) (my/org-focus--pick-week))
               (`(,week-start . ,week-end) (my/org-focus--week-range year week))
               (fmt (org-time-stamp-format nil t))
               (add-saturday (y-or-n-p "Add Saturday?"))
               (add-sunday (and add-saturday
                                (y-or-n-p "Add Sunday?"))))
    ;; Forward ref: check if the week already exists
    (when (my/org-focus--find-week year week)
      (user-error "Week %s-W%s already exists" year week))
    (concat
     (format "** %d-W%d %s - %s\n"
             year week (format-time-string fmt week-start)
             (format-time-string fmt week-end))
     (mapconcat
      (lambda (dow)
        (let ((time (time-add week-start (* 60 60 24 dow))))
          (concat (format "*** %s\n" (format-time-string my/org-focus-day-format time))
                  "*Primary focuses*:\n"
                  "\n"
                  "*Secondary focuses*:\n")))
      (number-sequence
       0 (cond (add-sunday 6) (add-saturday 5) (t 4)))
      "\n"))))

(with-eval-after-load 'org
  (setf (alist-get "w" org-capture-templates nil nil #'equal)
        `("Focus week" entry (file+olp ,my/org-focus-file "Planning")
          (function my/org-focus--week-template)
          :prepend t
          ;; I prefer to edit that in the full org buffer
          :immediate-finish t
          :jump-to-captured t
          :unnarrowed t)))

(defun my/org-focus--pick ()
  "Pick an active focus."
  (let* ((coll (mapcar
                (lambda (f)
                  (cons (mapconcat
                         #'identity (org-element-property :outline-path f) " / ")
                        f))
                (my/org-focus--find)))
         (focus
          (alist-get
           (completing-read "Focus: " coll nil t)
           coll nil nil #'equal)))
    (unless focus
      (user-error "Focus not selected"))
    focus))

(defun my/org-focus-insert-link (focus)
  "Insert a link to a focus under cursor.

FOCUS is an org element with the selected focus heading."
  (interactive (list (my/org-focus--pick)))
  (insert
   (format "[[id:%s][%s]]"
           (org-element-property :ID focus)
           (org-element-property :raw-value focus))))

(defun my/org-focus--parse-link-list ()
  "Parse paths of links in the list under cursor.

An example list is:
- [[id:1][link 1]]
- [[id:2][link 2]]

The function will return 1, 2."
  (let (links)
    (while (looking-at-p "- ")
      (forward-char 2)
      (when-let ((link (org-element-link-parser)))
        (push (org-element-property :path link) links))
      (forward-line))
    (nreverse links)))

(defun my/org-focus--parse-week ()
  "Parse focus week data under cursor.

Return an alist with the following keys:
- `:week' - a list with two values, year and week number
- `:week-start' - start of the week (a time value)
- `:week-end' - end of the week (a time value)
- `:day-data' - an alist, where the keys are day timestamps, and the
  values are alists with the following keys:
  - `:heading' - a string with the day heading
  - `:primary-ids' - ids of primary focuses
  - `:secondary-ids' - ids of secondary focuses."
  (save-excursion
    (search-backward-regexp (rx bol "** " (group (= 4 digit))
                                "-W" (group (= 2 digit))))
    (pcase-let* ((year (string-to-number (match-string 1)))
                 (week (string-to-number (match-string 2)))
                 (`(,week-start . ,week-end) (my/org-focus--week-range year week))
                 (days (mapcar
                        (lambda (dow)
                          (let ((time (time-add week-start (* dow 60 60 24))))
                            (cons (format-time-string my/org-focus-day-format time)
                                  (time-convert time 'integer))))
                        (number-sequence 0 6)))
                 ;; XXX `pcase-let' doesn't support (let (arg))?
                 (day-data nil))
      (pcase-dolist (`(,heading . ,time) days)
        ;; Assuming all days are in order
        (let (primary-ids secondary-ids)
          (when (search-forward heading nil t)
            (save-excursion
              (when (search-forward "*Primary focuses*:" nil t)
                (forward-line)
                (setq primary-ids (my/org-focus--parse-link-list))))
            (save-excursion
              (when (search-forward "*Secondary focuses*:" nil t)
                (forward-line)
                (setq secondary-ids (my/org-focus--parse-link-list)))))
          (when (or primary-ids secondary-ids)
            (setf (alist-get time day-data)
                  `((:heading . ,heading)
                    (:primary-ids . ,primary-ids)
                    (:secondary-ids . ,secondary-ids))))))
      `((:week . (,year ,week))
        (:week-start . ,week-start)
        (:week-end . ,week-end)
        (:day-data . ,(nreverse day-data))))))

(defun my/org-focus--find-week-1 (year week)
  "Find the data for a particular week.

YEAR is the year, WEEK is the week number from 1 to 53.

If the week is found, return the data structure described in
`my/org-focus--parse-week'.  Otherwise, return nil."
  (with-current-buffer (find-file-noselect my/org-focus-file)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward
             (rx bol "** " (literal (format "%d-W%d" year week))) nil t)
        (my/org-focus--parse-week)))))

(defvar-local my/cached-fn-tick nil)
(defvar-local my/cached-fn-cache nil)

(defmacro my/make-cached-fn (name fn buf-fn)
  "Make a cached variant of the function FN.  Call it NAME.

This can only work if FN operates in one and only one buffer.  BUF-FN must return that buffer."
  (declare (indent 1))
  (let ((arglist (ad-arglist fn)))
    `(defun ,name (,@arglist)
       (with-current-buffer (funcall ,buf-fn)
         (let ((tick (buffer-chars-modified-tick))
               (key (format "%s-%s" ',name (list ,@arglist))))
           (when (not (eql tick my/cached-fn-tick))
             (setq my/cached-fn-tick tick
                   my/cached-fn-cache (make-hash-table :test #'equal)))
           (let ((cached-res (gethash key my/cached-fn-cache :cache-miss)))
             (if (not (eq cached-res :cache-miss))
                 cached-res
               (let ((res (funcall #',fn ,@arglist)))
                 (puthash key res my/cached-fn-cache)
                 res))))))))

(defun my/make-cached-fn-reset ()
  "Reset the buffer-local cache for `my/make-cached-fn' functions."
  (interactive)
  (setq my/cached-fn-tick -1))

(my/make-cached-fn my/org-focus--find-week
  my/org-focus--find-week-1
  (lambda () (find-file-noselect my/org-focus-file)))

(defun my/org-focus-view-tasks-dwim (focus)
  "View tasks in FOCUS."
  (interactive
   (list (or (my/org-focus--at-point)
             (my/org-focus--pick))))
  (let ((id (org-element-property :ID focus))
        (title (format "Focus: %s" (org-element-property :raw-value focus))))
    (org-ql-search (org-agenda-files)
      `(and (property ,my/org-focus-prop ,id))
      :title title
      :sort '(priority todo deadline)
      :super-groups '((:auto-outline-path-file t)))))

(defun my/org-focus--get-referenced-at-point ()
  "Get focus ID referenced by task at point."
  (cond
   ((derived-mode-p #'org-agenda-mode)
    (get-text-property (point) 'FOCUS))
   (t (save-excursion
        (org-back-to-heading t)
        (let* ((elem (org-element-at-point-no-context))
               (focus-id (org-element-property :FOCUS elem)))
          focus-id)))))

(defun my/org-focus-task-dwim (arg)
  "Assign a focus to the task under cursor or go to it.

If a focus is under cursor, open its view.

ARG is the prefix arg.  If it's 4, then always assign a new focus even
if it's already set."
  (interactive "p")
  (let ((focus-id (my/org-focus--get-referenced-at-point))
        (focus-object (my/org-focus--at-point)))
    (cond (focus-object (my/org-focus-view focus-object))
          ((or (null focus-id) (eq arg 4))
           (call-interactively #'my/org-focus-assign-to-task))
          ;; XXX I don't want `pop-to-buffer-same-window' from `org-id-goto'
          (t (let ((m (org-id-find focus-id 'marker)))
               (unless m
                 (error "Cannot find entry with ID \"%s\"" id))
               (pop-to-buffer (marker-buffer m))
               (goto-char m)
               (move-marker m nil)
               (org-fold-show-context))))))

(defun my/org-focus-view-tasks-without-focus ()
  "Find all relevant tasks without focus."
  (interactive)
  (org-ql-search (org-agenda-files)
    `(and (not (property ,my/org-focus-prop))
          (not (tags "nof" "nots"))
          (or (todo "TODO" "MAYBE" "HOLD" "WAIT" "PLAN")
              (and (todo "DONE")
                   (or (closed :from -31)
                       (clocked :from -31)))))
    :title "Tasks without focus"
    :sort '(priority todo deadline)
    :super-groups '((:auto-outline-path-file t))))

(defun my/org-focus-sparse-tree-tasks-without-focus ()
  "Create a sparse tree from all tasks without focus."
  (interactive)
  (org-ql-sparse-tree
   `(and (not (property ,my/org-focus-prop))
         (not (tags "nof" "nots"))
         (or (todo "TODO" "MAYBE" "HOLD" "WAIT" "PLAN")
             (and (todo "DONE")
                  (or (closed :from -31)
                      (clocked :from -31)))))))

(defun my/org-focus-sparse-tree-for-focus (focus)
  "Create a sparse tree for tasks in FOCUS."
  (interactive (list (my/org-focus--pick)))
  (let ((id (org-element-property :ID focus)))
    (org-ql-sparse-tree
     `(property ,my/org-focus-prop ,id))))

(defun my/org-focus--select-headline ()
  "Parse headline at point for use in the focus view.

Return an alist with the following values:
- `:headline' - an instance of headline with markers
- `:clocks' - org-clock data (see `org-clock-agg--parse-clocks')
- `:clocks-total' - total time clocked (in seconds)
- `:effort-daily' - planned daily effort per task (see
  `my/org-planned-effort-parse')
- `:effort-total' - total time planned (in seconds)."
  (save-excursion ;; Why?
    (let* ((headline (org-element-headline-parser))
           (clocks (when (org-element-property :contents-begin headline)
                     (org-clock-agg--parse-clocks headline)))
           (effort (my/org-planned-effort-parse)))
      (org-ql--add-markers headline)
      `((:headline . ,headline)
        (:clocks . ,clocks)
        (:clocks-total . ,(when clocks
                            (seq-reduce (lambda (acc v) (+ acc (alist-get :duration v)))
                                        clocks 0)))
        (:effort-daily . ,effort)
        (:effort-total
         . ,(or (when effort
                  (seq-reduce (lambda (acc e) (+ acc (* (cdr e) 60))) effort 0))
                (when-let (effort-prop (org-element-property :EFFORT headline))
                  (floor (* 60 (org-duration-to-minutes effort-prop))))))))))

(defun my/org-focus--process-focus (focus &optional archive)
  "Process FOCUS for display.

If ARCHIVE is non-nil, include archived tasks.

The return value is an alist with the following values:
- `:id' - focus id
- `:name' - focus heading
- `:text' - focus description"
  (with-current-buffer (find-file-noselect my/org-focus-file)
    (let ((id (org-element-property :ID focus)))
      `((:id . ,id)
        (:name . ,(org-element-property :raw-value focus))
        (:text . ,(string-trim
                   (buffer-substring-no-properties
                    (org-element-property :contents-begin focus)
                    (org-element-property :contents-end focus))))
        (:tasks . ,(org-ql-query
                     :select #'my/org-focus--select-headline
                     :from (append (org-agenda-files)
                                   (when archive
                                     (my/org-agenda-archive)))
                     :where `(property ,my/org-focus-prop ,id)))))))

(setq my/org-done-statuses '("NO" "DONE" "PASSED" "OFF"))

(cl-defun my/org-focus--render-task
    (task &optional (display
                     '(todo-keyword priority-string closed-string
                                    clock-string due-string title)))
  "Render TASK for display.

TASK is an element as returned by `my/org-focus--select-headline'.

DISPLAY is the display configuration.  See the default value for
possible elements."
  ;; `org-ql' modifes the element
  (let* ((element (copy-tree (alist-get :headline task)))
         (marker (org-element-property :org-marker element))
         (title (--> (org-ql-view--add-faces element)
                     (org-element-property :raw-value it)
                     (org-link-display-format it)))
         (org-done-keywords my/org-done-statuses)
         (todo-keyword (-some--> (org-element-property :todo-keyword element)
                         (org-ql-view--add-todo-face it)))
         (priority-string (-some->> (org-element-property :priority element)
                            (char-to-string)
                            (format "[#%s]")
                            (org-ql-view--add-priority-face)))
         (closed-string (-some--> (org-element-property :closed element)
                          (org-timestamp-to-time it)
                          (format-time-string "%Y-%m-%d" it)
                          (propertize it 'face 'org-done)))
         (clock-string (let ((effort (-some--> (alist-get :effort-total task)
                                       (org-duration-from-minutes (/ it 60.0))))
                             (clocked (-some--> (alist-get :clocks-total task)
                                        (org-duration-from-minutes (/ it 60.0)))))
                         (format "%s / %s"
                                 (propertize (or effort "--") 'face 'success)
                                 (propertize (or clocked "--") 'face 'warning))
                         (cond
                          ((and clocked effort) (format "[%s/%s]" clocked effort))
                          ((and clocked (not effort) (format "[%s]" clocked)))
                          ((and (not clocked) effort) (format "[EST: %s]" effort)))))
         (due-string
          (when-let (s (org-element-property :relative-due-date element))
            (org-add-props s nil 'face 'org-ql-view-due-date)))
         (due-string-when-open
          (when (not (member (substring-no-properties
                              (org-element-property :todo-keyword element))
                             my/org-done-statuses))
            due-string))
         (string (s-join " " (-non-nil (mapcar #'symbol-value display)))))
    (remove-list-of-text-properties 0 (length string) '(line-prefix) string)
    (propertize string
                'org-agenda-type 'search
                'todo-state todo-keyword
                'org-marker marker
                'org-hd-marker marker)))

(defun my/org-focus--sort-task-key (task values)
  "Get a sort key for TASK.

VALUES is a list of parameters to extract from the task."
  (let* ((element (alist-get :headline task))
         (todo-keyword (or
                        (cl-position
                         (substring-no-properties
                          (org-element-property :todo-keyword element))
                         my/org-focus--todo-keywords-order
                         :test #'equal)
                        99))
         (priority (org-element-property :priority element))
         (title (org-element-property :raw-value element))
         (deadline (org-element-property
                    :raw-value (org-element-property :deadline element)))
         (scheduled (org-element-property
                     :raw-value (org-element-property :scheduled element)))
         (closed (org-element-property
                  :raw-value (org-element-property :closed element))) )
    (mapconcat
     (lambda (key)
       (let ((value (symbol-value key)))
         (cond ((numberp value) (format "%010d" value))
               ((stringp value) value)
               ((null value) (make-string 1 0))
               (t (prin1-to-string value)))))
     values "--")))

(defun my/org-focus--sort-tasks-by (tasks values &optional reverse)
  "Sort TASKS by VALUES.

If REVERSE is non-nil, sort in the backward order."
  (let ((res (seq-sort-by
              (lambda (task) (my/org-focus--sort-task-key task values))
              #'string-lessp
              tasks)))
    (when reverse
      (setq res (nreverse res)))
    res))

(setq my/org-focus--todo-keywords-order
      '("PLAN" "BACKLOG" "TODO" "WAIT" "DELEGATED" "FUTURE" "EVENT" "DONE" "PASSED" "NO"))

(defun my/org-focus--group-tasks-in-focus (tasks)
  "Group TASKS and sort them in groups.

Return an alist, where the keys are group names, and the values are
lists of tasks."
  (let (groups)
    (dolist (task tasks)
      (let* ((element (alist-get :headline task))
             (todo-keyword (org-element-property :todo-keyword element))
             (deadline (org-element-property :deadline element)))
        (push
         task
         (alist-get
          (cond
           ((and (equal todo-keyword "TODO") deadline) "TODO")
           ((and (equal todo-keyword "TODO") (not deadline)) "BACKLOG")
           ((equal todo-keyword "FUTURE") "EVENT")
           (t todo-keyword))
          groups nil nil #'equal))))
    (mapcar
     (lambda (group)
       (let ((name (car group))
             (tasks (cdr group)))
         (cons
          name
          (cond ((member name '("PLAN" "BACKLOG"))
                 (my/org-focus--sort-tasks-by tasks '(priority title)))
                ((member name '("EVENT"))
                 (my/org-focus--sort-tasks-by tasks '(scheduled title)))
                ((member name '("DONE" "PASSED" "NO"))
                 (my/org-focus--sort-tasks-by tasks
                                              '(closed scheduled deadline title) t))
                (t
                 (my/org-focus--sort-tasks-by tasks '(deadline priority title)))))))
     (seq-sort-by (lambda (group)
                    (or (cl-position
                         (car group) my/org-focus--todo-keywords-order :test #'equal)
                        99))
                  #'< groups))))

(defun my/org-focus--render-task-focus-view (tasks)
  "Render TASKS for focus view."
  (let ((groups (my/org-focus--group-tasks-in-focus tasks)))
    (pcase-dolist (`(,todo-keyword . ,tasks-group) groups)
      (insert (propertize "** " 'face 'outline-2)
              (propertize (or todo-keyword "") 'face
                          (or
                           (org-face-from-face-or-color
                            'todo 'org-todo
                            (cdr (assoc todo-keyword org-todo-keyword-faces)))
                           (and (member todo-keyword my/org-done-statuses) 'org-done)
                           'org-todo))
              "\n")
      (dolist (task tasks-group)
        (insert (my/org-focus--render-task
                 task '(due-string closed-string clock-string priority-string title))
                "\n"))
      (insert "\n"))))

(defun my/org-focus--render-focus-data (focus-data)
  "Render FOCUS-DATA in current buffer.

FOCUS-DATA is as returned by `my/org-focus--process-focus'."
  (let ((inhibit-read-only t)
        (id (alist-get :id focus-data)))
    (erase-buffer)
    (insert (propertize
             (format "* Focus: %s\n" (alist-get :name focus-data))
             'face 'outline-1))
    (insert (with-temp-buffer
              (insert (alist-get :text focus-data))
              (let (org-mode-hook)
                (org-mode))
              (font-lock-ensure)
              (buffer-string))
            "\n\n")
    (insert (propertize "* Actions\n" 'face 'outline-1))
    (widget-create 'push-button
                   :focus-id id
                   :notify (lambda (widget &rest _)
                             (org-id-goto (widget-get widget :focus-id)))
                   "Goto")
    (insert " ")
    (widget-create 'push-button
                   :focus-id id
                   :notify (lambda (widget &rest _)
                             (my/org-focus-view-tasks-dwim
                              (my/org-focus--find-by-id
                               (widget-get widget :focus-id))))
                   "Org QL Tasks")
    (insert "\n\n")
    (insert (propertize "* Tasks\n" 'face 'outline-1))
    (my/org-focus--render-task-focus-view
     (alist-get :tasks focus-data))
    (widget-setup)))

(defun my/org-focus-ret ()
  (interactive)
  (cond ((get-char-property (point) 'button)
         (call-interactively #'widget-button-press))
        ((get-char-property (point) 'focus-id)
         (my/org-focus-view
          (my/org-focus--find-by-id (get-char-property (point) 'focus-id))))
        ((get-char-property (point) 'org-hd-marker)
         (call-interactively #'org-agenda-switch-to))
        (t (evil-ret))))

(defmacro my/org-focus--wrap-agenda-command (command)
  "Call COMMAND interactively and revert the buffer."
  `(lambda () (interactive)
     (call-interactively #',command)
     (revert-buffer)))

(with-eval-after-load 'org
  (define-derived-mode my/org-focus-view-mode fundamental-mode "Focus Pocus"
    (setq-local buffer-read-only t)
    (outline-minor-mode 1)
    (visual-line-mode -1)
    (toggle-truncate-lines 1))

  (defvar-keymap my/org-focus-view-mode-map
    :doc "Keymap for org-focus-view-mode")

  (general-define-key
   :keymaps '(my/org-focus-view-mode-map)
   :states '(normal emacs)
   "q" #'my/quit-window-and-buffer
   ;; "t" (my/org-focus--wrap-agenda-command org-agenda-todo)
   "t" #'org-agenda-todo
   "r" #'revert-buffer
   "L" (my/org-focus--wrap-agenda-command org-agenda-do-date-later)
   "H" (my/org-focus--wrap-agenda-command org-agenda-do-date-earlier)
   "RET" #'my/org-focus-ret))

(defun my/org-focus-view-revert (_ignore-auto _noconfirm)
  "Refresh the current focus view buffer."
  (interactive)
  (unless (derived-mode-p #'my/org-focus-view-mode)
    (user-error "Not it my/org-focus-view-mode"))
  (let ((id (org-element-property :ID my/org-focus-focus)))
    (unless id
      (user-error "Can't find the focus id"))
    (let* ((focus (my/org-focus--find-by-id id))
           (focus-data (my/org-focus--process-focus focus))
           (point (point)))
      (my/org-focus--render-focus-data focus-data)
      (goto-char (max (point-min) point)))))

(defvar-local my/org-focus-focus nil
  "A focus headline displayed in the current focus view.")

(defvar-local my/org-focus-focus-data nil
  "Focus data displayed in the current focus view.")

(defun my/org-focus-view (focus)
  "Display information about FOCUS."
  (interactive
   (list (or (my/org-focus--at-point)
             (my/org-focus--pick))))
  (require 'org-clock-agg)
  (let* ((focus-data (my/org-focus--process-focus focus))
         (buf (get-buffer-create
               (format "*Focus: %s*" (alist-get :name focus-data)))))
    (with-current-buffer buf
      (my/org-focus-view-mode)
      (setq-local my/org-focus-focus focus
                  my/org-focus-focus-data focus-data
                  revert-buffer-function #'my/org-focus-view-revert)
      (my/org-focus--render-focus-data focus-data)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(defun my/org-focus-overview--data ()
  "Get data for focus overview."
  (let* ((focuses (my/org-focus--find))
         (tasks
          (cl-loop with total = (seq-length focuses)
                   for focus in focuses
                   for i from 0
                   for focus-id = (org-element-property :ID focus)
                   ;; do (message "Collecting focus %d/%d" i total)
                   collect (cons focus-id
                                 (my/org-focus--sort-tasks-by
                                  (org-ql-query
                                    :select #'my/org-focus--select-headline
                                    :from (org-agenda-files)
                                    :where `(property "FOCUS" ,focus-id))
                                  '(todo-keyword closed deadline scheduled priority
                                                 title))))))
    (cons focuses tasks)))

(defun my/org-focus-overview--render (data)
  "Render focus overview data.

DATA is as returned by `my/org-focus-overview--data'."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (pcase-let ((`(,focuses . ,tasks) data)
                (outline-paths-written nil))
      (dolist (focus focuses)
        (let ((focus-id (org-element-property :ID focus)))
          (cl-loop
           with last-i = (seq-length (org-element-property :outline-path focus))
           for i from 1
           for header in (org-element-property :outline-path focus)
           for full-path =
           (mapconcat #'identity (seq-subseq (org-element-property :outline-path focus)
                                             0 i))
           for face = (intern (format "outline-%d" i))
           for prefix = (concat (make-string i ?*) " ")
           do (if (= i last-i)
                  (insert (propertize (concat prefix header)
                                      'face face 'focus-id focus-id)
                          "\n")
                (unless (member full-path outline-paths-written)
                  (insert (propertize (concat prefix header)
                                      'face face)
                          "\n")
                  (push full-path outline-paths-written))))
          (dolist (task (alist-get focus-id tasks nil nil #'equal))
            (let ((render (my/org-focus--render-task
                           task '(todo-keyword due-string-when-open closed-string
                                               clock-string priority-string title))))
              (insert
               (apply #'propertize " " (text-properties-at 0 render))
               render
               "\n"))))
        (insert "\n")))))

(defvar-local my/org-focus-overview-data nil
  "Focus overview data displayed in the current view.")

(defun my/org-focus-overview-revert (_ignore-auto _noconfirm)
  "Refresh the current focus oveview buffer."
  (interactive)
  (unless (derived-mode-p #'my/org-focus-view-mode)
    (user-error "Not it my/org-focus-view-mode"))
  (let ((data (my/org-focus-overview--data)))
    (setq my/org-focus-overview-data data)
    (my/org-focus-overview--render data)
    (goto-char (point-min))))

(defun my/org-focus-overview ()
  "View all focuses and their tasks."
  (interactive)
  (let ((buf (get-buffer-create "*Focus overview*")))
    (with-current-buffer buf
      (my/org-focus-view-mode)
      (my/org-focus-overview-revert nil nil)
      (setq-local revert-buffer-function #'my/org-focus-overview-revert))
    (pop-to-buffer buf)))

(defun my/org-refile-with-focus (arg)
  "Refile header at point.  Maybe ask for focus.

See `org-refile' for ARG."
  (interactive "P" org-mode)
  (let ((heading (save-excursion
                   (org-back-to-heading t)
                   (org-element-headline-parser)))
        (is-inbox (equal (buffer-file-name)
                         (concat org-directory "/inbox.org"))))
    (when (and is-inbox (not (org-element-property :FOCUS heading))
               (y-or-n-p "Pick focus?"))
      (my/org-focus-assign-to-task
       (my/org-focus--pick)))
    (org-refile arg)))

(with-eval-after-load 'org
  (general-define-key
   :keymaps '(org-mode-map)
   "C-c C-w" #'my/org-refile-with-focus))

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

(defun my/org-agenda-clock--effort-at-point (timestamp &optional headline)
  "Return the task's effort in minutes for TIMESTAMP's day.
Subtract earlier clocks from the estimate only on its scheduled or
deadline day.  Otherwise use the day's clocks, unless an explicit
daily EFFORT drawer entry exists.  HEADLINE may supply the parsed task."
  (let* ((elem (or headline (org-element-at-point)))
         (start (my/utils-ts-to-day-start timestamp))
         (end (my/utils-ts-to-day-end timestamp))
         (scheduled (org-element-property :scheduled elem))
         (deadline (org-element-property :deadline elem))
         (due (or (my/org-agenda-clock--org-timestamp-in-range
                   start end scheduled)
                  (my/org-agenda-clock--org-timestamp-in-range
                   start end deadline))))
    (or
     (when due
       (when-let ((estimate
                   (or
                    ;; Data from the scheduled property
                    (when-let* ((range-type (org-element-property :range-type scheduled))
                                (range-start (org-timestamp-to-time scheduled))
                                (range-end (org-timestamp-to-time scheduled t)))
                      (/ (- (time-convert range-end 'integer)
                            (time-convert range-start 'integer))
                         60))
                    ;; Data from the EFFORT property
                    (when-let ((duration (org-element-property :EFFORT elem)))
                      (floor (org-duration-to-minutes duration))))))
         (max 0 (- estimate
                   (or (my/org-agenda-clock--clocked-at-point nil start) 0)))))
     ;; Daily plans are independent of clocks on other days.
     (when-let (effort-data (my/org-planned-effort-parse))
       (alist-get
        (format-time-string (org-time-stamp-format nil nil) timestamp)
        effort-data nil nil #'equal))
     (unless due
       (my/org-agenda-clock--clocked-at-point start (1+ end))))))

(defun my/utils-ts-to-day-start (&optional timestamp)
  "Move TIMESTAMP to start of day."
  (let ((time (decode-time timestamp)))
    (setf (decoded-time-second time) 0
          (decoded-time-minute time) 0
          (decoded-time-hour time) 0)
    (time-convert (encode-time time) 'integer)))

(defun my/utils-ts-to-day-end (&optional timestamp)
  "Move TIMESTAMP to start of day."
  (let ((time (decode-time timestamp)))
    (setf (decoded-time-second time) 59
          (decoded-time-minute time) 59
          (decoded-time-hour time) 23)
    (time-convert (encode-time time) 'integer)))

(defun my/org-agenda-clock--headline-at-point ()
  (let ((element (org-element-at-point-no-context)))
    (when (and (eq (org-element-type element) 'headline)
               (org-element-property :contents-begin element)
               (org-element-property :contents-end element))
      element)))

(defun my/org-agenda-clock--clocked-at-point (start end)
  "Return minutes clocked at point in the half-open interval START to END.
When START is nil, include all clocked time before END."
  (save-excursion
    (when-let* ((headline (my/org-agenda-clock--headline-at-point))
                (clocks (org-clock-agg--parse-clocks headline)))
      (let ((seconds
             (cl-loop for clock in clocks
                      for clock-start = (alist-get :start clock)
                      for clock-end = (alist-get :end clock)
                      when (and clock-start clock-end)
                      sum (max 0 (- (min end clock-end)
                                    (if start
                                        (max start clock-start)
                                      clock-start))))))
        (when (> seconds 0)
          (floor seconds 60))))))

(setq my/org-agenda-hide-tags (list "org" "refile" "proj" "habit"))

(defun my/org-agenda-clock--org-timestamp-in-range (start end timestamp)
  (when timestamp
    (let ((epoch (time-convert (org-timestamp-to-time timestamp) 'integer)))
      (and (>= epoch start) (<= epoch end)))))

(defun my/org-agenda-clock--get-data (timestamp)
  (let* ((start (my/utils-ts-to-day-start timestamp))
         (end (my/utils-ts-to-day-end timestamp))
         (date (format-time-string "%F" timestamp))
         (data
          (org-ql-query
            :select
            (lambda ()
              (let* ((headline (org-element-at-point))
                     (status (substring-no-properties
                              (org-element-property :todo-keyword headline)))
                     (clocked (my/org-agenda-clock--clocked-at-point start (1+ end)))
                     (effort (my/org-agenda-clock--effort-at-point
                              timestamp headline))
                     (scheduled (org-element-property :scheduled headline))
                     (deadline (org-element-property :deadline headline)))
                `((:clocked . ,clocked)
                  (:element . ,(org-ql--add-markers headline))
                  (:effort . ,effort)
                  (:missed
                   . ,(and (not clocked) (not effort)
                           (or (my/org-agenda-clock--org-timestamp-in-range
                                start end scheduled)
                               (my/org-agenda-clock--org-timestamp-in-range
                                start end deadline))
                           (not (equal status "WAIT")))))))
            :from (org-agenda-files)
            :where `(and
                     (ts :from ,(format-time-string "%FT%T%z" start)
                         :to ,(format-time-string "%FT%T%z" end)))))
         (clocked 0) (effort 0) missed)
    (mapc (lambda (datum)
            (cl-incf clocked (or (alist-get :clocked datum) 0))
            (cl-incf effort (or (alist-get :effort datum) 0))
            (when (alist-get :missed datum)
              (push (alist-get :element datum) missed)))
          data)
    (list clocked effort missed)))

(defun my/org-agenda-clock-show-missed (data)
  "Show missed agenda items in an org-ql-like buffer.

DATA is (TIMESTAMP . ELEMENTS)."
  (pcase-let* ((`(,timestamp . ,elements) data)
               (date (format-time-string "%F" timestamp))
               (title (format "Missed effort on %s" date))
               (query `(my/org-agenda-missed :on ,date))
               (strings (mapcar #'org-ql-view--format-element elements))
               (org-ql-view-buffers-files (org-agenda-files))
               (org-ql-view-query query)
               (org-ql-view-sort nil)
               (org-ql-view-narrow nil)
               (org-ql-view-super-groups '((:auto-outline-path-file t)))
               (org-ql-view-title title))
    (when org-ql-view-super-groups
      (let ((org-super-agenda-groups org-ql-view-super-groups))
        (setq strings (org-super-agenda--group-items strings))))
    (org-ql-view--display
      :buffer (format "*Org QL View: %s*" title)
      :header (org-ql-view--header-line-format
               :buffers-files org-ql-view-buffers-files
               :query query
               :title title)
      :strings strings)))

(defun my/org-agenda-clock-format-date (date)
  "Format agenda DATE with clickable daily clock summary.

DATE is a calendar-style date list, as passed by
`org-agenda-format-date'.  This expects
`my/org-agenda-clock--get-data' to return an alist with keys
`:clocked', `:effort', and `:missed'."
  (require 'org-clock-agg)
  (pcase-let* ((timestamp (encode-time 0 0 0 (cadr date) (car date) (nth 2 date)))
               (`(,clocked ,effort ,missed)
                (my/org-agenda-clock--get-data timestamp)))
    (concat
     (string-pad (org-agenda-format-date-aligned date) 33)
     (when effort
       (propertize
        (format "%s / "
                (buttonize
                 (org-duration-from-minutes clocked 'h:mm)
                 #'my/org-ql-clocked-today
                 timestamp)
                )
        'my/org-agenda-clock-face
        'warning))
     (when clocked
       (concat
        ""
        (propertize
         (org-duration-from-minutes effort 'h:mm)
         'my/org-agenda-clock-face
         'success)
        " "))
     (when missed
       (propertize
        (buttonize
         (format "!%d" (length missed))
         #'my/org-agenda-clock-show-missed
         (cons timestamp missed))
        'my/org-agenda-clock-face
        'error)))))

(defun my/org-agenda-clock-apply-faces ()
  (let ((inhibit-read-only t)
        pos next face)
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (setq pos (point)
              next (next-single-property-change
                    pos 'my/org-agenda-clock-face nil (point-max))
              face (get-text-property pos 'my/org-agenda-clock-face))
        (when face
          (add-face-text-property pos next face))
        (goto-char next)))))

(add-hook 'org-agenda-finalize-hook #'my/org-agenda-clock-apply-faces)

(defvar my/org-agenda-clock-prefix-width 13)

(defun my/org-agenda-clock-prefix-placeholder ()
  (propertize
   (make-string my/org-agenda-clock-prefix-width ?\s)
   'my/org-agenda-clock-prefix t))

(defun my/org-agenda-clock--task-prefix (timestamp marker)
  (org-with-point-at marker
    (when-let ((headline (my/org-agenda-clock--headline-at-point)))
      (let* ((start (my/utils-ts-to-day-start timestamp))
             (end (my/utils-ts-to-day-end timestamp))
             (clocked (my/org-agenda-clock--clocked-at-point start (1+ end)))
             (effort (my/org-agenda-clock--effort-at-point timestamp headline)))
        (if (or clocked effort)
            (format "%s / %s"
                    (propertize
                     (if clocked
                         (org-duration-from-minutes clocked 'h:mm)
                       "--")
                     'face
                     'warning)
                    (propertize
                     (if effort
                         (org-duration-from-minutes effort 'h:mm)
                       "--")
                     'face
                     'success))
          "")))))

(defun my/org-agenda-clock-fill-prefixes ()
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while-let ((match (text-property-search-forward
                          'my/org-agenda-clock-prefix)))
        (let* ((beg (prop-match-beginning match))
               (end (prop-match-end match))
               (day (get-text-property beg 'day))
               (marker (or (get-text-property beg 'org-hd-marker)
                           (get-text-property beg 'org-marker)))
               (text (if (and day marker)
                         (or (my/org-agenda-clock--task-prefix
                              (org-time-from-absolute day)
                              marker)
                             "")
                       "")))
          (goto-char beg)
          (delete-region beg end)
          (insert
           (truncate-string-to-width
            text my/org-agenda-clock-prefix-width 0 ?\s)))))))

(add-hook 'org-agenda-finalize-hook #'my/org-agenda-clock-fill-prefixes)

(defun my/org-scheduled-get-time ()
  (let ((scheduled (org-get-scheduled-time (point))))
    (if scheduled
        (format-time-string "%Y-%m-%d" scheduled)
      "")))

(setq org-agenda-hide-tags-regexp (rx (eval `(or ,@my/org-agenda-hide-tags))))

(setq org-agenda-custom-commands
      `(("p" "My outline"
         ((agenda ""
                  ((org-agenda-skip-function '(my/org-agenda-skip-without-match "-habit"))
                   (org-agenda-format-date 'my/org-agenda-clock-format-date)
                   (org-agenda-prefix-format " %i %(my/org-agenda-clock-prefix-placeholder)%?-12t% s")))
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

(defun my/org-insert-heading-link (&optional arg)
  "Insert a link to an Org heading.

With prefix ARG, choose a file from `org-agenda-files'."
  (interactive "P")
  (let* ((file (if arg
                   (completing-read "File: " (org-agenda-files) nil t)
                 (buffer-file-name)))
         (headings (with-current-buffer (find-file-noselect file)
                     (org-map-entries
                      (lambda ()
                        (cons (string-join
                               (append (org-get-outline-path)
                                       (list (org-get-heading t nil t t)))
                               " / ")
                              (org-get-heading t t t t)))
                      nil 'file)))
         (heading (cdr (assoc (completing-read "Heading: " headings nil t)
                              headings))))
    (org-insert-link
     nil
     (format "file:%s::*%s" (file-relative-name file) heading)
     heading)))

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

(defun my/org-agenda-archive ()
  (append
   (thread-last "/projects/archive"
                (concat org-directory)
                (directory-files)
                (mapcar (lambda (f)
                          (concat
                           org-directory "/projects/archive/" f)))
                (seq-filter (lambda (f)
                              (not (file-directory-p f)))))))

(defun my/org-agenda-and-archive ()
  (append
   (org-agenda-files)
   (my/org-agenda-archive)))

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
    "E" #'my/org-planned-effort-add-daily
    "R" #'my/org-planned-effort-remove-daily
    "r" #'org-priority
    "l" #'my/org-insert-heading-link
    ;; Focus
    "f" #'my/org-focus-task-dwim)

  (my-leader-def
    :states '(normal motion)
    :keymaps '(org-agenda-mode-map)
    "" nil
    ;; Focus
    "SPC f" #'my/org-focus-task-dwim))

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

Review checklist (/delete this after review/):
- [ ] Clear email inbox
- [ ] Reconcile ledger
- [ ] Reconcile other stats
- [ ] Clear [[file:~/Downloads][downloads]] and [[file:~/00-Scratch][scratch]] folders
- [ ] Process [[file:~/30-39 Life/35 Photos/35.00 Inbox/][photo inbox]]
- [ ] Process [[file:../inbox.org][inbox]]
- [ ] Create [[file:../planning/recurring.org][recurring tasks]] for next week
- [ ] Task check:
  - [[elisp:(org-ql-view \"Focus: Tasks without focus\")][org-ql: unfocused tasks]] - focus them
  - [[elisp:(org-ql-view \"Review: WAIT\")][org-ql: WAIT]] - ich warte...
  - [[elisp:(org-ql-view \"Fix: Passed events\")][org-ql: Fix passed events]]
  - [[elisp:(org-ql-view \"Review: Missed deadlines\")][org-ql: Review missed deadlines]] - move to backlog or reschedule
  - [[elisp:(org-ql-view \"Review: Stale tasks\")][org-ql: Stale tasks]] - move to backlog or reschedule
- [ ] [[file:../planning/focus-pocus.org::*Focuses][Focus]] check:
  - Review currently active focuses
- [ ] Agenda check:
  - Check [[file:/home/planning/focus-pocus.org::*Planning][Focused planning]] with agenda (-1 / +2 weeks)
- [ ] Final check
  - [[elisp:(org-ql-view \"Fix: Missing timestamps\")][org-ql: Fix missing timestamps]]
  - [[elisp:(org-ql-view \"Fix: Missing effort\")][org-ql: Fix missing effort]]
  - [[elisp:(my/backlog)][org-ql: Backlog]] - check if something's missing
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
  (call-process-shell-command "pkill -f element-desktop")
  (ignore-errors
    (rocket-kill t))
  (ignore-errors
    (max-kill t)))

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

(use-package org-drill
  :straight t
  :commands (org-drill)
  :after (org))

(provide 'sqrt-org-productivity)
