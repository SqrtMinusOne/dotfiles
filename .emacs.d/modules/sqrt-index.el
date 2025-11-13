;;; -*- lexical-binding: t -*-
(use-package ini
  :mode "\\.ini\\'"
  :straight (:host github :repo "daniel-ness/ini.el"))

(defvar my/index-root (concat (getenv "HOME") "/"))

(with-eval-after-load 'org
  (defvar my/index-file
    (concat org-directory "/misc/index.org")))

(defun my/index--tree-get-recursive (heading &optional path)
  "Read the index tree recursively from HEADING.

HEADING is an org-element of type `headline'.

If PATH is provided, it is the path to the current node. If not
provided, it is assumed to be the root of the index.

The return value is an alist; see `my/index--tree-get' for details."
  (when (eq (org-element-type heading) 'headline)
    (let (val
          (new-path (concat
                     (or path my/index-root)
                     (org-element-property :raw-value heading)
                     "/")))
      (when-let* ((children (thread-last
                              (org-element-contents heading)
                              (mapcar (lambda (e)
                                        (my/index--tree-get-recursive
                                         e new-path)))
                              (seq-filter #'identity))))
        (setf (alist-get :children val) children))
      (when-let ((machine (org-element-property :MACHINE heading)))
        (setf (alist-get :machine val) (split-string machine)))
      (when-let ((symlink (org-element-property :SYMLINK heading)))
        (setf (alist-get :symlink val) symlink))
      (when (org-element-property :PROJECT heading)
        (setf (alist-get :project val) t))
      (when-let* ((kind-str (org-element-property :KIND heading)))
        (when (string-match-p (rx bos "rclone:") kind-str)
          (setf (alist-get :remote val)
                (substring kind-str 7))
          (setq kind-str "rclone"))
        (when (equal kind-str "git")
          (let ((remote (org-element-property :REMOTE heading)))
            (unless remote
              (user-error "No remote for %s" (alist-get :name val)))
            (setf (alist-get :remote val) remote)))
        (setf (alist-get :kind val) (intern kind-str)))
      (setf (alist-get :name val) (org-element-property :raw-value heading)
            (alist-get :path val) new-path)
      val)))

(defun my/index--tree-get ()
  "Read the index tree from the current org buffer.

The return value is a list of alists, each representing a
folder/node.  Alists can have the following keys:
- `:name'
- `:path'
- `:children' - child nodes
- `:machine' - list of machines on which the node is active
- `:symlink' - a symlink to create
- `:kind' - one of \"git\", \"mega\", or \"dummy\"
- `:remote' - the remote to use for git nodes"
  (let* ((tree
          (thread-last
            (org-element-map (org-element-parse-buffer) 'headline #'identity)
            (seq-filter (lambda (el)
                          (and
                           (= (org-element-property :level el) 1)
                           (seq-contains-p
                            (mapcar #'substring-no-properties (org-element-property :tags el))
                            "folder"))))
            (mapcar #'my/index--tree-get-recursive))))
    tree))

(defun my/index--extact-number (name)
  "Extract the number from the index NAME.

NAME is a string.  The number is the first sequence of digits, e.g.:
- 10-19
- 10.01
- 10.01.Y22.01"
  (save-match-data
    (string-match (rx bos (+ (| num alpha "." "-"))) name)
    (match-string 0 name)))

(defun my/tree--verfify-recursive (elem &optional current)
  "Verify that ELEM is a valid tree element.

CURRENT is the current number or name of the parent element."
  (let* ((name (alist-get :name elem))
         (number (my/index--extact-number name)))
    (unless number
      (user-error "Can't find number: %s" name))
    (cond
     ((and (listp current) (not (null current)))
      (unless (seq-some (lambda (cand) (string-prefix-p cand name)) current)
        (user-error "Name: %s doesn't match: %s" name current)))
     ((stringp current)
      (unless (string-prefix-p current name)
        (user-error "Name: %s doesn't match: %s" name current))))
    (let ((recur-value
           (if (string-match-p (rx (+ num) "-" (+ num)) number)
               (let* ((borders (split-string number "-"))
                      (start (string-to-number (nth 0 borders)))
                      (end (string-to-number (nth 1 borders))))
                 (cl-loop for i from start to (1- end) collect (number-to-string i)))
             number)))
      (mapcar (lambda (e) (my/tree--verfify-recursive e recur-value))
              (alist-get :children elem))))
  t)

(defun my/index--tree-verify (tree)
  "Verify that TREE is a valid tree.

Return t if it is valid, otherwise raise an error.

See `my/index--tree-get' for the format of TREE."
  (mapcar #'my/tree--verfify-recursive tree))

(defun my/index--tree-narrow-recursive (elem machine)
  "Remove all children of ELEM that are not active on MACHINE."
  (unless (when-let ((elem-machines (alist-get :machine elem)))
            (not (seq-some (lambda (elem-machine)
                             (string-equal elem-machine machine))
                           elem-machines)))
    (setf (alist-get :children elem)
          (seq-filter
           #'identity
           (mapcar (lambda (e)
                     (my/index--tree-narrow-recursive e machine))
                   (alist-get :children elem))))
    elem))

(defun my/index--tree-narrow (tree)
  "Remove all elements of TREE that are not active on machine."
  (seq-filter
   #'identity
   (mapcar
    (lambda (elem) (my/index--tree-narrow-recursive elem (my/system-name)))
    (copy-tree tree))))

(defvar my/index-keep-files
  '(".dtrash"))

(defun my/index--filesystem-tree-mapping (full-tree tree &optional active-paths)
  "Return a \"sync state\" between the filesystem and the tree.

FULL-TREE and TREE are forms as defined by `my/index--tree-get'.  TREE
is the narrowed FULL-TREE (returned by `my/index--tree-narrow').

ACTIVE-PATHS is a list of paths that are currently active.  If not
provided, it is computed from TREE.

The return value is a list of alists with the following keys:
- path - the path of the folder
- exists - whether the folder exists on the filesystem
- has-to-exist - whether the folder exists in the tree
- extra - if the folder exists in the filesystem but not in the tree.
- children - a list of alists with the same keys for the children of
  the folder."
  (let ((active-paths (or active-paths (my/index--tree-get-paths tree))))
    (cl-loop for elem in full-tree
             for path = (alist-get :path elem)
             for extra-folders = (when (and (alist-get :children elem)
                                            (file-directory-p path))
                                   (seq-difference
                                    (mapcar (lambda (d) (if (file-directory-p d)
                                                            (concat d "/")
                                                          d))
                                            (directory-files path t (rx (not ".") eos)))
                                    (cl-loop for child in (alist-get :children elem)
                                             collect (alist-get :path child))))
             for folder-exists = (file-directory-p path)
             for folder-has-to-exist = (seq-contains-p active-paths path)
             collect `((path . ,path)
                       (exists . ,folder-exists)
                       (has-to-exist . ,folder-has-to-exist)
                       (children . ,(append
                                     (cl-loop for f in extra-folders
                                              collect
                                              `((path . ,f)
                                                (exists . t)
                                                (has-to-exist
                                                 . ,(member
                                                     (file-name-nondirectory
                                                      (directory-file-name f))
                                                     my/index-keep-files))
                                                (extra . t)))
                                     (my/index--filesystem-tree-mapping
                                      (alist-get :children elem) tree active-paths)))))))

(defun my/index--filesystem-commands (mapping)
  "Get commands to sync filesystem with the tree.

MAPPING is a form generated by `my/index--filesystem-tree-mapping'
that describes the \"sync state\" between the filesystem and the
tree.

The return value is a list of commands as defined by
`my/index--commands-display'."
  (cl-loop for elem in mapping
           for path = (alist-get 'path elem)
           for exists = (alist-get 'exists elem)
           for has-to-exist = (alist-get 'has-to-exist elem)
           for extra = (alist-get 'extra elem)
           when (and (not exists) has-to-exist)
           collect (list (format "mkdir \"%s\"" path) "Make directories" 1)
           when (and exists (not has-to-exist))
           collect (list (format "rm -rf \"%s\"" path)
                         (if extra "Remove extra files" "Remove directories")
                         (if extra 20 10))
           append (my/index--filesystem-commands (alist-get 'children elem))))

(defun my/parse-table-str (string)
  "Convert a table-like STRING into alist.

The input format is as follows:
HEADER1 HEADER2 HEADER3
value1  value2  3
value4  value5  6

Which creates the following output:
\(((HEADER1. \"value1\") (HEADER2 . \"value2\") (HEADER3 . \"3\"))
 ((HEADER1. \"value4\") (HEADER2 . \"value5\") (HEADER3 . \"6\")))

The functions also skips lines in [square brackets] and ones that
start with more than 3 spaces."
  (when-let* ((lines (seq-filter
                      (lambda (s) (not (or (string-empty-p s)
                                           (string-match-p (rx bos "[" (* nonl) "]") s)
                                           (string-match-p (rx bos (>= 3 " ")) s))))
                      (split-string string "\n")))
              (first-line (car lines))
              (headers (split-string first-line))
              (header-indices (mapcar
                               (lambda (header)
                                 (cl-search header first-line))
                               headers)))
    (cl-loop for line in (cdr lines)
             collect (cl-loop for header in headers
                              for start in header-indices
                              for end in (append (cdr header-indices)
                                                 (list (length line)))
                              collect (cons
                                       (intern header)
                                       (string-trim
                                        (substring line start end)))))))

(defun my/index--mega-data-from-sync ()
  "Get the current MEGA sync status.

The return value is a list of alists with the following keys:
- path - path to file or directory
- enabled - whether the file or directory is enabled for sync"
  (let ((mega-result (my/parse-table-str
                      (shell-command-to-string "mega-sync --path-display-size=10000"))))
    (cl-loop for value in mega-result
             for localpath = (alist-get 'LOCALPATH value)
             collect `((path . ,(if (file-directory-p localpath)
                                    (concat localpath "/")
                                  localpath))
                       (enabled . ,(seq-contains-p
                                    '("Pending" "Loading" "Running")
                                    (alist-get 'RUN_STATE value)))))))

(defun my/index--tree-get-paths (tree &optional kind)
  "Get paths from TREE.

TREE is a form a defined by `my/index--tree-get'.  KIND is either a
filter by the kind attribute or nil, in which case all paths are
returned.

The return value is a list of strings."
  (cl-loop for elem in tree
           when (or (null kind) (eq (alist-get :kind elem) kind))
           collect (alist-get :path elem)
           append (my/index--tree-get-paths
                   (alist-get :children elem) kind)))

(defun my/index--mega-local-path (path)
  "Get path in the MEGA cloud by the local path PATH."
  (string-replace my/index-root "/" path))

(defun my/index--mega-commands (full-tree tree)
  "Get commands to sync the mega-sync state with TREE.

FULL-TREE and TREE are forms as defined by `my/index--tree-get'.  TREE
is the narrowed FULL-TREE (returned by `my/index--tree-narrow').

The return value is a list of commands as defined by
`my/index--commands-display'."
  (let* ((paths-all (my/index--tree-get-paths full-tree))
         (mega-paths-to-enable (my/index--tree-get-paths tree 'mega))
         (mega-info (my/index--mega-data-from-sync))
         (mega-paths-enabled (seq-map
                              (lambda (e) (alist-get 'path e))
                              (seq-filter (lambda (e) (alist-get 'enabled e))
                                          mega-info)))
         (mega-paths-disabled (seq-map
                               (lambda (e) (alist-get 'path e))
                               (seq-filter (lambda (e) (not (alist-get 'enabled e)))
                                           mega-info))))
    (append
     (cl-loop for path in (seq-difference mega-paths-to-enable mega-paths-enabled)
              if (seq-contains-p mega-paths-disabled path)
              collect (list (format "mega-sync -r \"%s\"" path) "Mega enable sync" 5)
              else append (list
                           (list (format "mega-mkdir -p \"%s\""
                                         (my/index--mega-local-path path))
                                 "Mega mkdirs" 4)
                           (list (format "mega-sync \"%s\" \"%s\""
                                         path (my/index--mega-local-path path))
                                 "Mega add sync" 5)))
     (cl-loop for path in (seq-difference
                           (seq-intersection mega-paths-enabled paths-all)
                           mega-paths-to-enable)
              collect (list
                       (format "mega-sync -d \"%s\""
                               (substring path 0 (1- (length path))))
                       "Mega remove sync" 4)))))

(defconst my/index--rclone-options
  `("--create-empty-src-dirs"
    "--resilient"
    "--metadata"
    "--filters-file"
    ,(expand-file-name "~/.config/rclone/filters-bisync")))

(defconst my/index--rclone-script-path "~/bin/rclone-scripts/")

(defun my/index--rclone-get-folders (tree)
  "Get TREE nodes to be synced with rclone.

Return a list of alists with the following keys:
- `:local-path' - path in the local filesystem
- `:remote-path' - path in the remote
- `:remote' - name of the remote."
  (cl-loop for node in tree
           if (eq (alist-get :kind node) 'rclone)
           collect
           `((:local-path . ,(file-name-as-directory (alist-get :path node)))
             (:remote-path
              . ,(concat (alist-get :remote node)
                         ":" (my/index--mega-local-path
                              (file-name-as-directory (alist-get :path node)))))
             (:remote . ,(alist-get :remote node)))
           append (my/index--rclone-get-folders
                   (alist-get :children node))))

(defun my/index--rclone-make-command (local-path remote-path remote)
  "Make a bisync command to sync LOCAL-PATH and REMOTE-PATH.

REMOTE is the name of the remote."
  (string-join
   `("rclone"
     "bisync"
     ,(format "\"%s\"" local-path)
     ,(format "\"%s\"" remote-path)
     ,@my/index--rclone-options
     "--check-filename"
     ,(format ".rclone-test-%s" remote))
   " "))

(defun my/index--rclone-script (remote folders)
  (let ((script "
import subprocess
import json
import sys

REMOTE = '<rclone-remote>'
FOLDERS = json.loads('<rclone-folders-json>')
OPTIONS = json.loads('<rclone-options>')


def rclone_make_command(local_path, remote_path, remote):
    return [
        'rclone',
        'bisync',
        local_path,
        remote_path,
        *OPTIONS,
        '--check-filename',
        f'.rclone-test-{REMOTE}',
        '--verbose',
        '--color',
        'NEVER',
        '--use-json-log',
        '--stats',
        '9999m'
    ]


def parse_rclone_stats(log_output):
    log = log_output.splitlines()
    log.reverse()
    for line in log:
        try:
            log_entry = json.loads(line)
            if 'stats' in log_entry:
                return log_entry['stats']
        except json.JSONDecodeError:
            continue

    return None


def process_output(output):
    if output is None:
        print('(empty)')
    for line in output.splitlines():
        try:
            datum = json.loads(line)
            print(datum['msg'])
        except Exception:
            print(line)

def rclone_run(folder):
    command = rclone_make_command(
        folder['local-path'], folder['remote-path'], folder['remote']
    )
    try:
        result = subprocess.run(command, check=True, capture_output=True, text=True)
    except subprocess.CalledProcessError as e:
        print(f'=== Error syncing {folder['local-path']} ===')
        print(f'Command: {' '.join(command)}')
        print(f'--- STDOUT ---')
        process_output(e.stdout)
        print(f'--- STDERR ---')
        process_output(e.stderr)
        return {'success': False, 'stats': {}}
    return {'success': True, 'stats': parse_rclone_stats(result.stderr)}


def notify(summary, body, level='normal', expire_time=5000):
    subprocess.run(['notify-send', '-u', level, '-t', str(expire_time), summary, body])

# Source: https://stackoverflow.com/questions/1094841/get-a-human-readable-version-of-a-file-size
def sizeof_fmt(num, suffix='B'):
    for unit in ('', 'Ki', 'Mi', 'Gi', 'Ti', 'Pi', 'Ei', 'Zi'):
        if abs(num) < 1024.0:
            return f'{num:3.1f}{unit}{suffix}'
        num /= 1024.0
    return f'{num:.1f}Yi{suffix}'


def rclone_run_all(folders):
    error_folders = []
    total_bytes = 0
    total_transfers = 0
    total_deleted = 0
    total_renamed = 0
    for folder in folders:
        print(f'Running rclone for {folder}')
        res = rclone_run(folder)
        if not res['success']:
            error_folders.append(folder['local-path'])
        else:
            total_bytes += res.get('stats', {}).get('bytes', 0)
            total_transfers += res.get('stats', {}).get('transfers', 0)
            total_deleted += res.get('stats', {}).get('deletes', 0)
            total_renamed += res.get('stats', {}).get('renames', 0)
    if len(error_folders) > 0:
        error_msg = f'Sync error for remote {REMOTE}!'
        for folder in error_folders:
            error_msg += '''\n- ''' + folder
        notify(f'rclone sync {REMOTE}', error_msg, level='critical')
    else:
        msg = ''
        if total_transfers > 0:
            msg += f'''Transferred {total_transfers} files ({sizeof_fmt(total_bytes)})\n'''
        if total_deleted > 0:
            msg += f'''Deleted {total_transfers} files\n'''
        if total_renamed > 0:
            msg += f'''Renamed {total_renamed} files\n'''
        if len(msg) > 0:
            notify(f'rclone sync {REMOTE}', msg)

if __name__ == '__main__':
    rclone_run_all(FOLDERS)
"))
    (setq script
          (thread-last script
                       (string-trim)
                       (string-replace "<rclone-remote>" remote)
                       (string-replace "<rclone-folders-json>"
                                       (json-encode folders))
                       (string-replace "<rclone-options>"
                                       (json-encode my/index--rclone-options))))
    script))

(defun my/index--rclone-script-loc (remote)
  (concat (file-name-as-directory
           (expand-file-name my/index--rclone-script-path))
          (format "rclone_%s.py" remote)))

(defun my/index--rclone-script-saved-p (remote folders)
  (let* ((script (my/index--rclone-script remote folders))
         (script-loc (my/index--rclone-script-loc remote)))
    (when (file-exists-p script-loc)
      (with-temp-buffer
        (insert-file-contents script-loc)
        (equal (string-trim (buffer-string)) script)))))

(defun my/index--rclone-commands (tree)
  "Get commands to set up sync with rclone in TREE.

TREE is a form a defined by `my/index--tree-get'.  This is supposed to
be the tree narrowed to the current machine (`my/index--tree-narrow').

The return value is a list of commands as defined by
`my/index--commands-display'."
  (let ((folders (my/index--rclone-get-folders tree))
        commands
        sync-items-per-remote)
    (dolist (folder folders)
      (pcase-let*
          ((`((:local-path . ,local-path) (:remote-path . ,remote-path)
              (:remote . ,remote))
            folder)
           (test-file-name (format ".rclone-test-%s" remote))
           (test-file-local (concat local-path test-file-name))
           (test-file-remote (concat remote-path test-file-name)))
        (unless (file-exists-p test-file-local)
          (push
           (list (format "touch \"%s\"" test-file-local) "Create local test files" 3)
           commands)
          (push
           (list (format "rclone mkdir \"%s\"" remote-path)
                 "Create remote directories" 3)
           commands)
          (push
           (list (format "rclone touch \"%s\"" test-file-remote) "Create remote test-file" 4)
           commands)
          (push
           (list
            (concat (my/index--rclone-make-command local-path remote-path remote)
                    " --resync")
            (format "Initial sync for %s" remote) 8)
           commands))
        (push folder
              (alist-get remote sync-items-per-remote nil nil #'equal))))
    (unless (file-exists-p my/index--rclone-script-path)
      (push (list (format "mkdir -p \"%s\"" (expand-file-name
                                             my/index--rclone-script-path))
                  "Create rclone sync scripts directory" 9)
            commands))
    (cl-loop for (remote . folders) in sync-items-per-remote
             unless (my/index--rclone-script-saved-p remote folders)
             do (push
                 (list
                  (format "cat <<EOF > %s\n%s\nEOF"
                          (my/index--rclone-script-loc remote)
                          (my/index--rclone-script remote (nreverse folders)))
                  "Update rclone sync script" 10)
                 commands))
    (nreverse commands)))

(defun my/index--git-commands (tree)
  "Get commands to clone the yet uncloned git repos in TREE.

TREE is a form a defined by `my/index--tree-get'.  This is supposed to
be the tree narrowed to the current machine (`my/index--tree-narrow').

The return value is a list of commands as defined by
`my/index--commands-display'."
  (cl-loop for elem in tree
           for path = (alist-get :path elem)
           when (and (eq (alist-get :kind elem) 'git)
                     (or (not (file-directory-p path))
                         (directory-empty-p path)))
           collect (list (format "git clone \"%s\" \"%s\""
                                 (alist-get :remote elem)
                                 path)
                         "Init git repos" 2)
           append (my/index--git-commands (alist-get :children elem))))

(defun my/index--bare-project-name (name)
  "Remove the alphanumeric prefix from NAME.

E.g. 10.03.R.01 Project Name -> Project Name."
  (replace-regexp-in-string
   (rx bos (+ (| num alpha "." "-")) space) "" name))

(defun my/index--wakatime-escape (string)
  "Escape STRING for use in a WakaTime config file."
  (thread-last
    string
    (replace-regexp-in-string (rx "'") "\\\\'")
    (replace-regexp-in-string (rx "(") "\\\\(")
    (replace-regexp-in-string (rx ")") "\\\\)")))

(defun my/index--wakatime-get-map-tree (tree)
  "Get a list of (folder-name . bare-project-name) pairs from TREE.

TREE is a form as defined by `my/index--tree-get'.
\"bare-project-name\" is project name without the alphanumeric
prefix."
  (cl-loop for elem in tree
           for name = (alist-get :name elem)
           if (eq (alist-get :kind elem) 'git)
           collect (cons (my/index--wakatime-escape name)
                         (my/index--wakatime-escape
                          (my/index--bare-project-name name)))
           if (and (eq (alist-get :kind elem) 'git)
                   (alist-get :symlink elem))
           collect (cons (my/index--wakatime-escape
                          ;; lmao
                          ;; /a/b/c/ -> c
                          ;; /a/b/c -> b
                          (file-name-nondirectory
                           (directory-file-name
                            (file-name-directory (alist-get :symlink elem)))))
                         (my/index--wakatime-escape
                          (my/index--bare-project-name name)))
           append (my/index--wakatime-get-map-tree (alist-get :children elem))))

(defun my/index--wakatime-commands (tree)
  "Get commands to update WakaTime config from TREE.

TREE is a form a defined by `my/index--tree-get'. The return value is
a list of commands as defined by `my/index--commands-display'."
  (require 'ini)
  (let* ((map-tree (my/index--wakatime-get-map-tree tree))
         (map-tree-encoding (ini-encode `(("projectmap" . ,map-tree))))
         (map-tree-saved (with-temp-buffer
                           (insert-file-contents (expand-file-name "~/.wakatime.cfg"))
                           (string-match-p (regexp-quote map-tree-encoding)
                                           (buffer-string)))))
    (unless map-tree-saved
      (let ((insert-command (list (format "echo \"\n\n%s\" >> ~/.wakatime.cfg"
                                          map-tree-encoding)
                                  "Update WakaTime config" 9)))
        (list (list (format "sed -i -z 's/\\[projectmap\\]\\n[^[]*//g' ~/.wakatime.cfg")
                    "Update WakaTime config" 9)
              insert-command)))))

(defun my/index-get-symlink-commands (tree)
  "Get commands to create symlinks from TREE.

TREE is a form a defined by `my/index--tree-get'. The return value is
a list of commands as defined by `my/index--commands-display'."
  (cl-loop for elem in tree
           for path = (alist-get :path elem)
           for symlink = (alist-get :symlink elem)
           when (and symlink (not (string-match-p (rx "/" eos) symlink)))
           do (user-error "Wrong symlink: %s (should be a directory)" symlink)
           when (and path symlink
                     (or (file-exists-p symlink)
                         (file-exists-p (substring symlink 0 -1)))
                     (not (file-symlink-p (substring symlink 0 -1))))
           collect (list (format "rm -rf %s" (substring symlink 0 -1))
                         "Remove files to make symlinks" 6)
           when (and path symlink
                     (not (file-symlink-p (substring symlink 0 -1))))
           collect (list (format "ln -s '%s' '%s'" path
                                 (substring symlink 0 -1))
                         "Make symlinks" 7)
           append (my/index-get-symlink-commands (alist-get :children elem))))

(defvar my/index-commands-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-c C-c") #'my/index-commands-exec)
    (define-key keymap (kbd "q") #'my/quit-window-and-buffer)
    (when (fboundp 'evil-define-key*)
      (evil-define-key* 'normal keymap
        "q" #'my/quit-window-and-buffer))
    keymap)
  "Keymap for `biome-api-error-mode'.")

(define-derived-mode my/index-commands-mode sh-mode "Index Commands"
  "A mode to display index commands.")

(defvar-local my/index-commands nil
  "Commands to be executed by `my/index-commands-exec'")

(defun my/index--commands-display (commands)
  "Display COMMANDS in a buffer.

COMMANDS is a list of commands as defined by `my/index--commands-display'."
  (unless commands
    (user-error "No commands to display"))
  (let ((buffer (get-buffer-create "*index commands*"))
        (groups (seq-sort-by
                 (lambda (g) (nth 2 (nth 1 g)))
                 #'<
                 (seq-group-by (lambda (c) (nth 1 c))
                               commands))))
    (with-current-buffer buffer
      (my/index-commands-mode)
      (let ((inhibit-read-only t)
            commands-sequence)
        (erase-buffer)
        (setq-local my/index-commands nil)
        (cl-loop for g in groups
                 for group-name = (car g)
                 for elems = (cdr g)
                 do (insert "# " group-name "\n")
                 do (cl-loop for elem in elems
                             do (push (nth 0 elem) my/index-commands)
                             do (insert (nth 0 elem) "\n")))
        (setq-local buffer-read-only t)))
    (switch-to-buffer buffer)))

(defun my/index-commands-exec ()
  (interactive)
  (unless (eq major-mode 'my/index-commands-mode)
    (user-error "Not shell mode"))
  (let ((filename (make-temp-file "index-commands-")))
    (write-region (point-min) (point-max) filename)
    (compile (concat "bash -x " filename))))

(defvar my/index--tree nil
  "The last version of the index tree.")

(defun my/index--tree-retrive ()
  "Retrive the last version of the index tree.

This function returns the last saved version of the index tree if it
is still valid. Otherwise, it re-parses the index file."
  (setq
   my/index--tree
   (cond ((string-equal (buffer-file-name) my/index-file)
          (my/index--tree-get))
         ((or (null my/index--tree)
              (file-has-changed-p my/index-file 'index))
          (with-temp-buffer
            (insert-file-contents my/index-file)
            (let ((buffer-file-name my/index-file))
              (my/index--tree-get))))
         (t my/index--tree))))

(defun my/index-commands-sync ()
  "Sync the filesystem with the index."
  (interactive)
  (let* ((full-tree (my/index--tree-retrive)))
    (my/index--tree-verify full-tree)
    (let* ((tree (my/index--tree-narrow full-tree))
           ;; (mega-commands (my/index--mega-commands full-tree tree))
           (rclone-commands (my/index--rclone-commands tree))
           (mapping (my/index--filesystem-tree-mapping full-tree tree))
           (folder-commands (my/index--filesystem-commands mapping))
           (git-commands (my/index--git-commands tree))
           (waka-commands (my/index--wakatime-commands tree))
           (symlink-commands (my/index-get-symlink-commands tree)))
      (my/index--commands-display (append rclone-commands folder-commands git-commands
                                          waka-commands symlink-commands)))))

(defun my/index--nav-extend (name path &optional project)
  "Find all index-related files in PATH.

NAME is the name of the root index entry, e.g. \"10.01
Something\".  If PATH containts folders like \"10.01.01
Something\", \"10.01.02 ...\", they will be returned.  PROJECT is the
project name.

The return value is a form as defined by `my/index--nav-get'."
  (when (file-directory-p path)
    (let* ((number (my/index--extact-number name))
           (files (mapcar
                   (lambda (f) (cons f (concat path f)))
                   (seq-filter (lambda (f) (not (string-prefix-p "." f)))
                               (directory-files path))))
           (matching-files
            (seq-filter
             (lambda (f) (and (file-directory-p (cdr f))
                              (string-prefix-p number (car f))))
             files)))
      (when (and (length> matching-files 0)
                 (length< matching-files (length files)))
        (user-error "Extraneuous files in %s" path))
      (cl-loop for (name-1 . path-1) in matching-files
               append (if-let ((child-files (my/index--nav-extend name-1 (concat path-1 "/"))))
                          (mapcar
                           (lambda (child-datum)
                             (push name-1 (alist-get :names child-datum))
                             child-datum)
                           child-files)
                        `(((:names . (,name-1))
                           (:path . ,(concat path-1 "/")))))))))

(defun my/index--nav-get (tree &optional names project)
  "Get the navigation structure from TREE.

TREE is a form as defined by `my/index--tree-get'.  NAMES is a
list of names of the parent entries, e.g. (\"10.01 Something\"), used
for recursive calls.

The result is a list of alists with the following keys:
- `:names` - list of names, e.g.
  (\"10.01 Something\" \"10.01.01 Something\")
- `:path` - path to the folder, e.g.
  \"/path/10 stuff/10.01 Something/10.01.01 Something/\"
- `:project` - project name."
  (seq-sort-by
   (lambda (item) (alist-get :path item))
   #'string-lessp
   (cl-reduce
    (lambda (acc elem)
      (let* ((name (alist-get :name elem))
             (path (alist-get :path elem)))
        (cond ((alist-get :children elem)
               (when-let (child-navs (my/index--nav-get
                                      (alist-get :children elem)
                                      `(,@names ,name)
                                      (or (when (alist-get :project elem)
                                            name)
                                          project)))
                 (cl-loop for child-nav in child-navs
                          do (push child-nav acc))))
              (t (if-let ((extended-nav (my/index--nav-extend name path project)))
                     (cl-loop for child-nav in extended-nav
                              do (setf (alist-get :names child-nav)
                                       (append names (list name)
                                               (alist-get :names child-nav)))
                              do (push child-nav acc))
                   (push `((:names . (,@names ,name))
                           (:path . ,path)
                           (:project . ,project))
                         acc))))
        acc))
    tree
    :initial-value nil)))

(defvar my/index--nav nil
  "Navigation stucture for the index.")

(defun my/index--nav-retrive ()
  "Retrive the navigation structure from the index file.

The return value is a form as defined by `my/index--nav-get'."
  (if (or (null my/index--nav)
          (file-has-changed-p my/index-file 'nav))
      (let ((tree (my/index--tree-retrive)))
        (setq my/index--nav (my/index--nav-get
                             (my/index--tree-narrow tree))))
    my/index--nav))

(defun my/index--nav-prompt (nav)
  "Prompt the user for the navigation item to select.

NAV is a structure as defined by `my/index--nav-get'."
  (let* ((collection
          (mapcar (lambda (item)
                    (cons (let ((name (car (last (alist-get :names item)))))
                            (if (alist-get :project item)
                                (format "%s / %s" (alist-get :project item) name)
                              name))
                          (alist-get :path item)))
                  nav))
         (vertico-sort-function nil))
    (cdr
     (assoc
      (completing-read "Index: " collection nil t)
      collection))))

(defun my/index--nav-find-path (nav path)
  "Find the navigation item in NAV with the given PATH.

NAV is a structure as defined by `my/index--nav-get'."
  (seq-find
   (lambda (item)
     (string-prefix-p (alist-get :path item) path))
   nav))

(defun my/index-nav (&optional func)
  "Navigate the filesystem index.

FUNC is the function to call with the selected path.  It defaults
to `dired' if used interactively."
  (interactive (list #'dired))
  (let* ((nav (my/index--nav-retrive))
         (selected (my/index--nav-find-path
                    nav
                    (my/index--nav-prompt nav))))
    (funcall func (alist-get :path selected))))

(defun my/index-nav-with-select-file ()
  (interactive)
  (my/index-nav
   (lambda (dir)
     (let ((default-directory dir))
       (projectile-find-file)))))

(defun my/index-open-file ()
  (interactive)
  (find-file my/index-file))

(my-leader-def
  :infix "i"
  "" '(:wk "index")
  "i" #'my/index-nav
  "s" #'my/index-commands-sync
  "p" #'my/index-nav-with-select-file
  "f" #'my/index-open-file)

(defun my/index-export (file)
  (interactive (list (read-file-name "File: " "~/logs-sync/data/index.json")))
  (let ((full-tree (my/index--tree-retrive)))
    (unless (file-exists-p (file-name-directory file))
      (make-directory (file-name-directory file) t))
    (with-temp-file file
      (insert (json-encode full-tree))
      (json-pretty-print-buffer))))

(provide 'sqrt-index)
