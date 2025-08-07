;;; -*- lexical-binding: t -*-
(use-package doom-modeline
  :straight t
  ;; :if (not (display-graphic-p))
  :init
  (setq doom-modeline-env-enable-python nil)
  (setq doom-modeline-env-enable-go nil)
  (setq doom-modeline-buffer-encoding 'nondefault)
  (setq doom-modeline-hud t)
  (setq doom-modeline-persp-icon nil)
  (setq doom-modeline-persp-name nil)
  (setq doom-modeline-display-misc-in-all-mode-lines nil)
  (when my/is-termux
    (setopt doom-modeline-icon nil))
  :config
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-irc nil)
  (setq doom-modeline-buffer-state-icon nil)
  (doom-modeline-mode 1))

(defun my/tab-bar-mode-line--format ()
  (unless (derived-mode-p 'company-box-mode)
    (cl-letf (((symbol-function 'window-pixel-width)
               'frame-pixel-width)
              ((symbol-function 'window-margins)
               (lambda (&rest _)
                 (list nil))))
      (let ((doom-modeline-window-width-limit nil)
            (doom-modeline--limited-width-p nil))
        (format-mode-line
         '("%e"
           (:eval
            (doom-modeline-format--main))))))))

(defun my/hide-mode-line-if-only-window ()
  (let* ((windows (window-list))
         (hide-mode-line-p (length= windows 1)))
    (dolist (win windows)
      (with-current-buffer (window-buffer win)
        (unless (eq hide-mode-line-p hide-mode-line-mode)
          (hide-mode-line-mode
           (if hide-mode-line-p +1 -1)))))))

(define-minor-mode my/tab-bar-mode-line-mode
  "Use tab-bar as mode line mode."
  :global t
  (if my/tab-bar-mode-line-mode
      (progn
        (tab-bar-mode +1)
        (setq tab-bar-format '(my/tab-bar-mode-line--format))
        (set-face-attribute 'tab-bar nil :inherit 'mode-line)
        (add-hook 'window-configuration-change-hook #'my/hide-mode-line-if-only-window)

        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (doom-modeline-set-modeline 'minimal)))
        (doom-modeline-set-modeline 'minimal 'default)

        (dolist (frame (frame-list))
          (with-selected-frame frame
            (my/hide-mode-line-if-only-window))
          (when-let (cb-frame (company-box--get-frame frame))
            (set-frame-parameter cb-frame 'tab-bar-lines 0)))
        (setenv "POLYBAR_BOTTOM" "false")
        (when (fboundp #'my/exwm-run-polybar)
          (my/exwm-run-polybar)))
    (tab-bar-mode -1)
    (setq tab-bar-format
          '(tab-bar-format-history tab-bar-format-tabs tab-bar-separator tab-bar-format-add-tab))
    (set-face-attribute 'tab-bar nil :inherit 'default)
    (remove-hook 'window-configuration-change-hook #'my/hide-mode-line-if-only-window)
    (global-hide-mode-line-mode -1)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (doom-modeline-set-modeline 'main)))
    (doom-modeline-set-modeline 'main 'default)
    (setenv "POLYBAR_BOTTOM" "true")
    (when (fboundp #'my/exwm-run-polybar)
      (my/exwm-run-polybar))))

(provide 'sqrt-doom-modeline)
