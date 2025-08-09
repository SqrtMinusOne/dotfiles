;;; -*- lexical-binding: t -*-
(use-package dap-mode
  :straight t
  :commands (dap-debug)
  :init
  (setq lsp-enable-dap-auto-configure nil)
  :config

  (setq dap-ui-variable-length 100)
  (setq dap-auto-show-output nil)
  (require 'dap-node)
  (dap-node-setup)

  (require 'dap-chrome)
  (dap-chrome-setup)

  (require 'dap-python)
  (require 'dap-php)

  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1))

(with-eval-after-load 'dap-mode
  (defmacro my/define-dap-ui-window-toggler (name)
    `(defun ,(intern (concat "my/dap-ui-toggle-" name)) ()
       ,(concat "Toggle DAP " name "buffer")
       (interactive)
       (if-let (window (get-buffer-window ,(intern (concat "dap-ui--" name "-buffer"))))
           (quit-window nil window)
         (,(intern (concat "dap-ui-" name))))))

  (my/define-dap-ui-window-toggler "locals")
  (my/define-dap-ui-window-toggler "expressions")
  (my/define-dap-ui-window-toggler "sessions")
  (my/define-dap-ui-window-toggler "breakpoints")
  (my/define-dap-ui-window-toggler "repl"))

(defhydra my/dap-hydra (:color pink :hint nil :foreign-keys run)
  "
^Stepping^         ^UI^                     ^Switch^                   ^Breakpoints^         ^Debug^                     ^Expressions
^^^^^^^^------------------------------------------------------------------------------------------------------------------------------------------
_n_: Next          _uc_: Controls           _ss_: Session              _bb_: Toggle          _dd_: Debug                 _ee_: Eval
_i_: Step in       _ue_: Expressions        _st_: Thread               _bd_: Delete          _dr_: Debug recent          _er_: Eval region
_o_: Step out      _ul_: Locals             _sf_: Stack frame          _ba_: Add             _dl_: Debug last            _es_: Eval thing at point
_c_: Continue      _ur_: REPL               _su_: Up stack frame       _bc_: Set condition   _de_: Edit debug template   _ea_: Add expression
_r_: Restart frame _uo_: Output             _sd_: Down stack frame     _bh_: Set hit count   _Q_:  Disconnect            _ed_: Remove expression
                 _us_: Sessions           _sF_: Stack frame filtered _bl_: Set log message                           _eu_: Refresh expressions
                 _ub_: Breakpoints                                                                               "

  ("n" dap-next)
  ("i" dap-step-in)
  ("o" dap-step-out)
  ("c" dap-continue)
  ("r" dap-restart-frame)
  ("uc" dap-ui-controls-mode)
  ("ue" my/dap-ui-toggle-expressions)
  ("ul" my/dap-ui-toggle-locals)
  ("ur" my/dap-ui-toggle-repl)
  ("uo" dap-go-to-output-buffer)
  ("us" my/dap-ui-toggle-sessions)
  ("ub" my/dap-ui-toggle-breakpoints)
  ("ss" dap-switch-session)
  ("st" dap-switch-thread)
  ("sf" dap-switch-stack-frame)
  ("sF" my/dap-switch-stack-frame)
  ("su" dap-up-stack-frame)
  ("sd" dap-down-stack-frame)
  ("bb" dap-breakpoint-toggle)
  ("ba" dap-breakpoint-add)
  ("bd" dap-breakpoint-delete)
  ("bc" dap-breakpoint-condition)
  ("bh" dap-breakpoint-hit-condition)
  ("bl" dap-breakpoint-log-message)
  ("dd" dap-debug)
  ("dr" dap-debug-recent)
  ("dl" dap-debug-last)
  ("de" dap-debug-edit-template)
  ("ee" dap-eval)
  ("ea" dap-ui-expressions-add)
  ("er" dap-eval-region)
  ("es" dap-eval-thing-at-point)
  ("ed" dap-ui-expressions-remove)
  ("eu" dap-ui-expressions-refresh)
  ("q" nil "quit" :color blue)
  ("Q" dap-disconnect :color red))

(my-leader-def "d" #'my/dap-hydra/body)

(defvar my/dap-mode-buffer-fixed nil)

(with-eval-after-load 'dap-mode
  (defmacro my/define-dap-tree-buffer-fixer (buffer-var buffer-name)
    `(defun ,(intern (concat "my/fix-dap-ui-" buffer-name "-buffer")) (&rest _)
       (with-current-buffer ,buffer-var
         (unless my/dap-mode-buffer-fixed
           (toggle-truncate-lines 1)
           (doom-modeline-set-modeline 'info)
           (setq-local my/dap-mode-buffer-fixed t)))))

  (my/define-dap-tree-buffer-fixer dap-ui--locals-buffer "locals")
  (my/define-dap-tree-buffer-fixer dap-ui--expressions-buffer "expressions")
  (my/define-dap-tree-buffer-fixer dap-ui--sessions-buffer "sessions")
  (my/define-dap-tree-buffer-fixer dap-ui--breakpoints-buffer "breakpoints")

  (advice-add 'dap-ui-locals :after #'my/fix-dap-ui-locals-buffer)
  (advice-add 'dap-ui-expressions :after #'my/fix-dap-ui-expressions-buffer)
  (advice-add 'dap-ui-sessions :after #'my/fix-dap-ui-sessions-buffer)
  (advice-add 'dap-ui-breakpoints :after #'my/fix-dap-ui-breakpoints-buffer))

(defun my/clear-bad-window-parameters ()
  "Clear window parameters that interrupt my workflow."
  (interactive)
  (let ((window (get-buffer-window (current-buffer))))
    (set-window-parameter window 'no-delete-other-windows nil)))

(defun my/dap-yank-value-at-point (node)
  (interactive (list (treemacs-node-at-point)))
  (kill-new (message (plist-get (button-get node :item) :value))))

(defun my/dap-display-value (node)
  (interactive (list (treemacs-node-at-point)))
  (let ((value (plist-get (button-get node :item) :value)))
    (when value
      (let ((buffer (generate-new-buffer "dap-value")))
        (with-current-buffer buffer
          (insert value))
        (select-window (display-buffer buffer))))))

(with-eval-after-load 'dap-mode
  (setq my/dap-stack-frame-filters
        `(("node_modules,node:internal" . ,(rx (or "node_modules" "node:internal")))
          ("node_modules" . ,(rx (or "node_modules")))
          ("node:internal" . ,(rx (or "node:internal")))))

  (setq my/dap-stack-frame-current-filter (cdar my/dap-stack-frame-filters))

  (defun my/dap-stack-frame-filter-set ()
    (interactive)
    (setq my/dap-stack-frame-current-filter
          (cdr
           (assoc
            (completing-read "Filter: " my/dap-stack-frame-filters)
            my/dap-stack-frame-filters))))

  (defun my/dap-stack-frame-filter (frame)
    (when-let (path (dap--get-path-for-frame frame))
      (not (string-match my/dap-stack-frame-current-filter path)))))

(defun my/dap-switch-stack-frame ()
  "Switch stackframe by selecting another stackframe stackframes from current thread."
  (interactive)
  (when (not (dap--cur-session))
    (error "There is no active session"))

  (-if-let (thread-id (dap--debug-session-thread-id (dap--cur-session)))
      (-if-let (stack-frames
                (gethash
                 thread-id
                 (dap--debug-session-thread-stack-frames (dap--cur-session))))
          (let* ((index 0)
                 (stack-framces-filtered
                  (-filter
                   #'my/dap-stack-frame-filter
                   stack-frames))
                 (new-stack-frame
                  (dap--completing-read
                   "Select active frame: "
                   stack-framces-filtered
                   (-lambda ((frame &as &hash "name"))
                     (if-let (frame-path (dap--get-path-for-frame frame))
                         (format "%s: %s (in %s)"
                                 (cl-incf index) name frame-path)
                       (format "%s: %s" (cl-incf index) name)))
                   nil
                   t)))
            (dap--go-to-stack-frame (dap--cur-session) new-stack-frame))
        (->> (dap--cur-session)
             dap--debug-session-name
             (format "Current session %s is not stopped")
             error))
    (error "No thread is currently active %s" (dap--debug-session-name (dap--cur-session)))))

(defun my/exwm-perspective-find-buffer (path)
  "Find a buffer with PATH in all EXWM perspectives.

Returns (<buffer> . <workspace-index>) or nil."
  (let* ((buf (cl-loop for buf being buffers
                       if (and (buffer-file-name buf)
                               (f-equal-p (buffer-file-name buf) path))
                       return buf))
         (target-workspace
          (and buf
               (cl-loop for frame in exwm-workspace--list
                        if (with-selected-frame frame
                             (cl-loop for persp-name being the hash-keys of (perspectives-hash)
                                      if (member buf (persp-buffers
                                                      (gethash persp-name (perspectives-hash))))
                                      return persp-name))
                        return (cl-position frame exwm-workspace--list)))))
    (when target-workspace (cons buf target-workspace))))

(defun my/dap--go-to-stack-frame-override (debug-session stack-frame)
  "Make STACK-FRAME the active STACK-FRAME of DEBUG-SESSION."
  (with-lsp-workspace (dap--debug-session-workspace debug-session)
    (when stack-frame
      (-let* (((&hash "line" line "column" column "name" name) stack-frame)
              (path (dap--get-path-for-frame stack-frame)))
        (setf (dap--debug-session-active-frame debug-session) stack-frame)
        ;; If we have a source file with path attached, open it and
        ;; position the point in the line/column referenced in the
        ;; stack trace.
        (if (and path (file-exists-p path))
            (progn
              (let ((exwm-target (my/exwm-perspective-find-buffer path)))
                (if exwm-target
                    (progn
                      (unless (= (cdr exwm-target) exwm-workspace-current-index)
                        (exwm-workspace-switch (cdr exwm-target)))
                      (persp-switch-to-buffer (car exwm-target)))
                  (select-window (get-mru-window (selected-frame) nil))
                  (find-file path)))
              (goto-char (point-min))
              (forward-line (1- line))
              (forward-char column))
          (message "No source code for %s. Cursor at %s:%s." name line column))))
    (run-hook-with-args 'dap-stack-frame-changed-hook debug-session)))

(with-eval-after-load 'exwm
  (with-eval-after-load 'dap-mode
    (advice-add #'dap--go-to-stack-frame :override #'my/dap--go-to-stack-frame-override)))

;; (advice-remove #'dap--go-to-stack-frame #'my/dap--go-to-stack-frame-override)

(with-eval-after-load 'dap-mode
  (dap-register-debug-template
   "Node::Nest.js"
   (list :type "node"
         :request "attach"
         :name "Node::Attach"
         :port 9229
         :outFiles ["${workspaceFolder}/dist/**/*.js"]
         :sourceMaps t
         :program "${workspaceFolder}/src/app.ts"))
  (dap-register-debug-template
   "Node::Babel"
   (list :type "node"
         :request "attach"
         :name "Node::Attach"
         :port 9229
         :program "${workspaceFolder}/dist/bin/www.js")))

(provide 'sqrt-dap)
