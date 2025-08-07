;;; -*- lexical-binding: t -*-
(use-package perspective
  :straight t
  :init
  ;; (setq persp-show-modestring 'header)
  (setq persp-sort 'created)
  (setq persp-suppress-no-prefix-key-warning t)
  :config
  (persp-mode)
  (my-leader-def "x" '(:keymap perspective-map :which-key "perspective"))
  (general-define-key
   :keymaps 'override
   :states '(normal emacs)
   "gt" 'persp-next
   "gT" 'persp-prev
   "gn" 'persp-switch
   "gN" 'persp-kill)
  (general-define-key
   :keymaps 'perspective-map
   "b" 'persp-switch-to-buffer
   "x" 'persp-switch-to-buffer*
   "u" 'persp-ibuffer))

(defun my/persp-move-window-and-switch ()
  (interactive)
  (let* ((buffer (current-buffer)))
    (call-interactively #'persp-switch)
    (persp-set-buffer (buffer-name buffer))
    (switch-to-buffer buffer)))

(defun my/persp-copy-window-and-switch ()
  (interactive)
  (let* ((buffer (current-buffer)))
    (call-interactively #'persp-switch)
    (persp-add-buffer (buffer-name buffer))
    (switch-to-buffer buffer)))

(with-eval-after-load 'perspective
  (general-define-key
   :keymaps 'perspective-map
   "m" #'my/persp-move-window-and-switch
   "f" #'my/persp-copy-window-and-switch))

(setq my/perspective-assign-alist '())

(defvar my/perspective-assign-ignore nil
  "If non-nil, ignore `my/perspective-assign'")

(defun my/perspective-assign ()
  (when-let* ((_ (not my/perspective-assign-ignore))
              (rule (alist-get major-mode my/perspective-assign-alist)))
    (let ((workspace-index (car rule))
          (persp-name (cadr rule))
          (buffer (current-buffer)))
      (if (fboundp #'perspective-exwm-assign-window)
          (progn
            (perspective-exwm-assign-window
             :workspace-index workspace-index
             :persp-name persp-name)
            (when workspace-index
              (exwm-workspace-switch workspace-index))
            (when persp-name
              (persp-switch persp-name)))
        (with-perspective persp-name
          (persp-set-buffer buffer))
        (persp-switch-to-buffer buffer)))))

(defun my/perspective-assign-ignore-advice (fun &rest args)
  (let ((my/perspective-assign-ignore t))
    (apply fun args)))

(add-hook 'after-change-major-mode-hook #'my/perspective-assign)

(defmacro my/persp-add-rule (&rest body)
  (declare (indent 0))
  (unless (= (% (length body) 3) 0)
    (error "Malformed body in my/persp-add-rule"))
  (let (result)
    (while body
      (let ((major-mode (pop body))
            (workspace-index (pop body))
            (persp-name (pop body)))
        (push
         `(add-to-list 'my/perspective-assign-alist
                       '(,major-mode . (,workspace-index ,persp-name)))
         result)))
    `(progn
       ,@result)))

(defmacro my/command-in-persp (command-name persp-name workspace-index &rest args)
  `'((lambda ()
       (interactive)
       (when (and ,workspace-index (fboundp #'exwm-workspace-switch-create))
         (exwm-workspace-switch-create ,workspace-index))
       (persp-switch ,persp-name)
       (delete-other-windows)
       ,@args)
     :wk ,command-name))

(provide 'sqrt-perspective)
