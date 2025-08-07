;;; -*- lexical-binding: t -*-
(use-package ox-hugo
  :straight t
  :if (not my/remote-server)
  :after ox)

(use-package ox-ipynb
  :straight (:host github :repo "jkitchin/ox-ipynb")
  :if (not my/remote-server)
  :disabled t
  :after ox)

(use-package htmlize
  :straight t
  :after ox
  :if (not my/remote-server)
  :config
  (setq org-html-htmlize-output-type 'css))

(with-eval-after-load 'org-ref
  (setq org-ref-csl-default-locale "ru-RU")
  (setq org-ref-csl-default-style (expand-file-name
                                   (concat user-emacs-directory
                                           "gost-r-7-0-5-2008-numeric.csl"))))

(defun my/setup-org-latex ()
  (setq org-latex-prefer-user-labels t)
  (setq org-latex-compiler "xelatex") ;; Probably not necessary
  (setq org-latex-pdf-process '("latexmk -outdir=%o %f")) ;; Use latexmk
  (setq org-latex-listings 'minted) ;; Use minted to highlight source code
  (setq org-latex-minted-options    ;; Some minted options I like
        '(("breaklines" "true")
          ("tabsize" "4")
          ("autogobble")
          ("linenos")
          ("numbersep" "0.5cm")
          ("xleftmargin" "1cm")
          ("frame" "single")))
  ;; Use extarticle without the default packages
  (add-to-list 'org-latex-classes
               '("org-plain-extarticle"
                 "\\documentclass{extarticle}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
               '("org-plain-extreport"
                 "\\documentclass{extreport}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")))
  ;; Use beamer without the default packages
  (add-to-list 'org-latex-classes
               '("org-latex-beamer"
                 "\\documentclass{beamer}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
                 ("beamer" "\\documentclass[presentation]{beamer}"
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))))

;; Make sure to eval the function when org-latex-classes list already exists
(with-eval-after-load 'ox-latex
  (my/setup-org-latex))

(with-eval-after-load 'ox
  (setq org-export-dictionary
        (cl-loop for item in org-export-dictionary collect
                 (cons
                  (car item)
                  (cl-loop for entry in (cdr item)
                           if (and (equal (car entry) "ru")
                                   (plist-get (cdr entry) :utf-8))
                           collect (list "ru" :default (plist-get (cdr entry) :utf-8))
                           else collect entry)))))

(provide 'sqrt-org-export)
