;;; -*- lexical-binding: t -*-
(unless (or my/remote-server my/nested-emacs)
  ;; (add-hook 'after-init-hook #'server-start)
  )

(defmacro i3-msg (&rest args)
  `(start-process "emacs-i3-windmove" nil "i3-msg" ,@args))

(defun my/emacs-i3-windmove (dir)
  (let ((other-window (windmove-find-other-window dir)))
    (if (or (null other-window) (window-minibuffer-p other-window))
        (i3-msg "focus" (symbol-name dir))
      (windmove-do-window-select dir))))

(defun my/emacs-i3-direction-exists-p (dir)
  (cl-some (lambda (dir)
          (let ((win (windmove-find-other-window dir)))
            (and win (not (window-minibuffer-p win)))))
        (pcase dir
          ('width '(left right))
          ('height '(up down)))))

(defun my/emacs-i3-move-window (dir)
  (let ((other-window (windmove-find-other-window dir))
        (other-direction (my/emacs-i3-direction-exists-p
                          (pcase dir
                            ('up 'width)
                            ('down 'width)
                            ('left 'height)
                            ('right 'height)))))
    (cond
     ((and other-window (not (window-minibuffer-p other-window)))
      (window-swap-states (selected-window) other-window))
     (other-direction
      (evil-move-window dir))
     (t (i3-msg "move" (symbol-name dir))))))

(defun my/emacs-i3-resize-window (dir kind value)
  (if (or (one-window-p)
          (not (my/emacs-i3-direction-exists-p dir)))
      (i3-msg "resize" (symbol-name kind) (symbol-name dir)
              (format "%s px or %s ppt" value value))
    (setq value (/ value 2))
    (pcase kind
      ('shrink
       (pcase dir
         ('width
          (evil-window-decrease-width value))
         ('height
          (evil-window-decrease-height value))))
      ('grow
       (pcase dir
         ('width
          (evil-window-increase-width value))
         ('height
          (evil-window-increase-height value)))))))

(use-package transpose-frame
  :straight t
  :commands (transpose-frame))

(defun my/emacs-i3-integration (command)
  (pcase command
    ((rx bos "focus")
     (my/emacs-i3-windmove
      (intern (elt (split-string command) 1))))
    ((rx bos "move")
     (my/emacs-i3-move-window
      (intern (elt (split-string command) 1))))
    ((rx bos "resize")
     (my/emacs-i3-resize-window
      (intern (elt (split-string command) 2))
      (intern (elt (split-string command) 1))
      (string-to-number (elt (split-string command) 3))))
    ("layout toggle split" (transpose-frame))
    ("split h" (evil-window-split))
    ("split v" (evil-window-vsplit))
    ("kill" (evil-quit))
    (- (i3-msg command))))

(provide 'sqrt-i3)
