;;; -*- lexical-binding: t -*-
(setq my/emacs-started nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)
            (setq my/emacs-started t)))

(setq use-package-verbose nil)

(setq use-package-compute-statistics t)

(setq gc-cons-threshold 80000000)
(setq read-process-output-max (* 1024 1024))

(add-hook 'emacs-startup-hook
          (lambda ()
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              (add-hook 'after-focus-change-function 'garbage-collect))))

(defun my/get-ram-usage-async (callback)
  (let* ((temp-buffer (generate-new-buffer "*ps*"))
         (proc (start-process "ps" temp-buffer "ps"
                              "-p" (number-to-string (emacs-pid)) "-o" "rss")))
    (set-process-sentinel
     proc
     (lambda (process _msg)
       (when (eq (process-status process) 'exit)
         (let* ((output (with-current-buffer temp-buffer
                          (buffer-string)))
                (usage (string-to-number (nth 1 (split-string output "\n")))))
           (ignore-errors
             (funcall callback usage)))
         (kill-buffer temp-buffer))))))

(defun my/ram-usage ()
  (interactive)
  (my/get-ram-usage-async
   (lambda (data)
     (message "%f Gb" (/ (float data) 1024 1024)))))

(provide 'sqrt-performance)
