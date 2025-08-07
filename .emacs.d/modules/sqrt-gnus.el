;;; -*- lexical-binding: t -*-
(use-package gnus
  :straight t
  :init
  (my-leader-def "au" #'gnus)
  :commands (gnus)
  :config
  (my/persp-add-rule
    gnus-summary-mode 0 "gnus"
    ;; gnus-article-edit-mode 0 "gnus"
    gnus-browse-mode 0 "gnus"
    gnus-server-mode 0 "gnus"
    gnus-article-mode 0 "gnus"
    gnus-group-mode 0 "gnus"
    gnus-category-mode 0 "gnus")
  (let ((gnus-directory (concat user-emacs-directory "gnus")))
    (unless (file-directory-p gnus-directory)
      (make-directory gnus-directory))
    (setq gnus-dribble-directory (concat gnus-directory "/dribble"))
    (setq gnus-init-file (concat gnus-directory "/gnus.el"))
    (setq gnus-startup-file (concat gnus-directory "/newsrc")))
  ;; Sources
  (setq gnus-select-method '(nntp "news.gwene.org"))
  ;; Dribble
  (setq gnus-always-read-dribble-file t)
  ;; Agent
  (setq gnus-agent-article-alist-save-format 1)
  (setq gnus-agent-cache t))

(defun my/gnus-topic-toggle-topic ()
  (interactive "" gnus-topic-mode)
  (when (gnus-group-topic-p)
    (let ((topic (gnus-topic-find-topology (gnus-current-topic))))
      (if (eq (cadadr topic) 'visible)
          (progn
            (gnus-topic-goto-topic (gnus-current-topic))
            (gnus-topic-remove-topic nil nil))
        (gnus-topic-remove-topic t nil)))))

(with-eval-after-load 'gnus-group
  ;; Group
  (add-hook 'gnus-group-mode-hook #'gnus-topic-mode)
  (general-define-key
   :states '(normal)
   :keymaps '(gnus-group-mode-map)
   "a" #'gnus-group-toggle-subscription-at-point)
  (general-define-key
   :states '(normal)
   :keymaps '(gnus-topic-mode-map)
   "TAB" #'my/gnus-topic-toggle-topic
   "r" #'gnus-topic-catchup-articles))

(with-eval-after-load 'gnus-summary
  (setq gnus-summary-line-format "%U%R%z%I%(%[%4L: %-23,23f%]%) %s\n")
  (setq gnus-sum-thread-tree-false-root "> ")
  (setq gnus-sum-thread-tree-indent "  ")
  (setq gnus-sum-thread-tree-single-indent " ")
  (setq gnus-sum-thread-tree-leaf-with-other "+-> ")
  (setq gnus-sum-thread-tree-root "> ")
  (setq gnus-sum-thread-tree-single-leaf "\\-> ")
  (setq gnus-sum-thread-tree-vertical "| "))

(provide 'sqrt-gnus)
