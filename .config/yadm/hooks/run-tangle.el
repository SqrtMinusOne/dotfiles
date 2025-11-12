(require 'org)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)))

;; Do not ask to confirm evaluations
(setq org-confirm-babel-evaluate nil)

(defun my/extract-arch-dependencies (&optional category)
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
                        (string-match-p "[A|a]rch.*dep" elem))))
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
                        (string-match-p ".*[D|d]isabled.*" elem))))
              (source-index
               (cl-position
                nil
                (mapcar #'substring-no-properties (nth 0 table))
                :test (lambda (_ elem)
                        (string-match-p ".*[S|s]ource.*" elem)))))
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
               (let ((source
                      (or
                       (when (and source-index
                                  (not (string-empty-p (nth source-index elem))))
                         (substring-no-properties
                          (nth source-index elem)))
                       "arch")))
                 (push
                  (substring-no-properties (nth dep-name-index elem))
                  (alist-get source dependencies nil nil #'equal)))))))))
    dependencies))
(defun my/format-arch-dependencies (&optional category)
  (let ((data (my/extract-arch-dependencies category)))
    (with-temp-buffer
      (cl-loop for (backend . packages) in data
               do (insert (format "%s = [\n" backend)
                          (mapconcat (lambda (package)
                                       (format "\"%s\"," package))
                                     packages
                                     "\n")
                          "]"))
      (buffer-string))))

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
