;;; -*- lexical-binding: t -*-
(use-package pg
  :straight (:type git :host github :repo "emarsden/pg-el")
  :defer t)

(use-package pgmacs
  :straight (:type git :host github :repo "SqrtMinusOne/pgmacs")
  :commands (pgmacs)
  :init
  (my/persp-add-rule
    pgmacs-mode 4 "db"
    pgmacs-paginated-mode 4 "db")
  (my-leader-def "aP" (my/command-in-persp "PGmacs" "db" 4
                                           (call-interactively #'pgmacs)))
  :config
  ;; Table list config (the main buffer)
  (general-define-key
   :keymaps '(pgmacs-table-list-map)
   :states '(normal)
   "e" #'pgmacs-run-sql
   "E" #'pgmacs-run-buffer-sql
   "q" #'bury-buffer
   "gr" #'pgmacs--table-list-redraw
   "T" #'pgmacs--switch-to-database-buffer)
  (general-define-key
   :keymaps '(pgmacs-table-list-map/table)
   "j" #'evil-next-line
   "g" nil)

  ;; Individual table config
  (general-define-key
   :keymaps '(pgmacs-row-list-map/table)
   "j" #'evil-next-line
   "J" #'pgmacs--row-as-json
   "k" #'evil-previous-line
   "K" #'pgmacs--copy-row
   "0" #'evil-beginning-of-line
   "w" #'evil-forward-word-begin
   "W" #'pgmacs--add-where-filter
   "O" #'pgmacs--edit-value-widget
   "e" #'pgmacs-run-sql
   "E" #'pgmacs-run-buffer-sql
   "gr" #'pgmacs--row-list-redraw
   "T" #'pgmacs--switch-to-database-buffer
   "V" #'pgmacs--view-row-transposed
   "L" #'pgmacs--view-linked-rows
   "s" #'pgmacs--add-order-by-interactive
   "M-s" #'pgmacs--add-order-by)

  (general-define-key
   :keymaps '(pgmacs-row-list-map)
   :states '(normal)
   "e" #'pgmacs-run-sql
   "E" #'pgmacs-run-buffer-sql
   "W" #'pgmacs--add-where-filter
   "o" #'pgmacs-open-table
   "T" #'pgmacs--switch-to-database-buffer
   "gr" #'pgmacs--row-list-redraw
   "s" #'pgmacs--add-order-by-interactive
   "M-s" #'pgmacs--add-order-by)

  (setq pgmacs-row-colors
        (list
         (my/color-value 'bg-alt)
         (my/color-value 'bg))))

(provide 'sqrt-db-programming)
