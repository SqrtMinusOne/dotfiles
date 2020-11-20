(defun org-babel-tangle-collect-blocks-handle-tangle-list (&optional language tangle-file)
  "Can be used as :override advice for `org-babel-tangle-collect-blocks'.
Handles lists of :tangle files."
  (let ((counter 0) last-heading-pos blocks)
    (org-babel-map-src-blocks (buffer-file-name)
      (let ((current-heading-pos
         (org-with-wide-buffer
          (org-with-limited-levels (outline-previous-heading)))))
    (if (eq last-heading-pos current-heading-pos) (cl-incf counter)
      (setq counter 1)
      (setq last-heading-pos current-heading-pos)))
      (unless (org-in-commented-heading-p)
    (let* ((info (org-babel-get-src-block-info 'light))
           (src-lang (nth 0 info))
           (src-tfiles (cdr (assq :tangle (nth 2 info))))) ; Tobias: accept list for :tangle
      (unless (consp src-tfiles) ; Tobias: unify handling of strings and lists for :tangle
        (setq src-tfiles (list src-tfiles))) ; Tobias: unify handling
      (dolist (src-tfile src-tfiles) ; Tobias: iterate over list
        (unless (or (string= src-tfile "no")
            (and tangle-file (not (equal tangle-file src-tfile)))
            (and language (not (string= language src-lang))))
          ;; Add the spec for this block to blocks under its
          ;; language.
          (let ((by-lang (assoc src-lang blocks))
            (block (org-babel-tangle-single-block counter)))
        (setcdr (assoc :tangle (nth 4 block)) src-tfile) ; Tobias: 
        (if by-lang (setcdr by-lang (cons block (cdr by-lang)))
          (push (cons src-lang (list block)) blocks)))))))) ; Tobias: just ()
    ;; Ensure blocks are in the correct order.
    (mapcar (lambda (b) (cons (car b) (nreverse (cdr b)))) blocks)))

(defun org-babel-tangle-single-block-handle-tangle-list (oldfun block-counter &optional only-this-block)
  "Can be used as :around advice for `org-babel-tangle-single-block'.
If the :tangle header arg is a list of files. Handle all files"
  (let* ((info (org-babel-get-src-block-info))
     (params (nth 2 info))
     (tfiles (cdr (assoc :tangle params))))
    (if (null (and only-this-block (consp tfiles)))
    (funcall oldfun block-counter only-this-block)
      (cl-assert (listp tfiles) nil
         ":tangle only allows a tangle file name or a list of tangle file names")
      (let ((ret (mapcar
          (lambda (tfile)
            (let (old-get-info)
              (cl-letf* (((symbol-function 'old-get-info) (symbol-function 'org-babel-get-src-block-info))
                 ((symbol-function 'org-babel-get-src-block-info)
                  `(lambda (&rest get-info-args)
                     (let* ((info (apply 'old-get-info get-info-args))
                        (params (nth 2 info))
                        (tfile-cons (assoc :tangle params)))
                       (setcdr tfile-cons ,tfile)
                       info))))
            (funcall oldfun block-counter only-this-block))))
          tfiles)))
    (if only-this-block
        (list (cons (cl-caaar ret) (mapcar #'cadar ret)))
      ret)))))

(advice-add 'org-babel-tangle-collect-blocks :override #'org-babel-tangle-collect-blocks-handle-tangle-list)
(advice-add 'org-babel-tangle-single-block :around #'org-babel-tangle-single-block-handle-tangle-list)
