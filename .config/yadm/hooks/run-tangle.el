(require 'org)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)))

;; Do not ask to confirm evaluations
(setq org-confirm-babel-evaluate nil)

(defun my/extract-guix-dependencies (&optional category)
  (let ((dependencies '()))
    (org-table-map-tables
     (lambda ()
       (let* ((table
               (seq-filter
                (lambda (q) (not (eq q 'hline)))
                (org-table-to-lisp)))
              (dep-name-index
               (cl-position
                nil
                (mapcar #'substring-no-properties (nth 0 table))
                :test (lambda (_ elem)
                        (string-match-p "[G|g]uix.*dep" elem))))
              (category-name-index
               (cl-position
                nil
                (mapcar #'substring-no-properties (nth 0 table))
                :test (lambda (_ elem)
                        (string-match-p ".*[C|c]ategory.*" elem))))
              (disabled-name-index
               (cl-position
                nil
                (mapcar #'substring-no-properties (nth 0 table))
                :test (lambda (_ elem)
                        (string-match-p ".*[D|d]isabled.*" elem)))))
         (when dep-name-index
           (dolist (elem (cdr table))
             (when
                 (and
                  ;; Category
                  (or
                   ;; Category not set and not present in the table
                   (and
                    (or (not category) (string-empty-p category))
                    (not category-name-index))
                   ;; Category is set and present in the table
                   (and
                    category-name-index
                    (not (string-empty-p category))
                    (string-match-p category (nth category-name-index elem))))
                  ;; Not disabled
                  (or
                   (not disabled-name-index)
                   (string-empty-p (nth disabled-name-index elem))))
               (add-to-list
                'dependencies
                (substring-no-properties (nth dep-name-index elem)))))))))
    dependencies))
(defun my/format-guix-dependencies (&optional category)
  (mapconcat
   (lambda (e) (concat "\"" e "\""))
   (my/extract-guix-dependencies category)
   "\n"))

;; A few dummy modes to avoid being prompted for comment systax
(define-derived-mode fish-mode prog-mode "Fish"
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+[\t ]*"))

(define-derived-mode yaml-mode text-mode "YAML"
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+ *"))

(mapcar #'org-babel-tangle-file
        '("/home/pavel/Emacs.org"
          "/home/pavel/Desktop.org"
          "/home/pavel/Console.org"
          "/home/pavel/Guix.org"
          "/home/pavel/Mail.org"))
