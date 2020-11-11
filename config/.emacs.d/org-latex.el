(setq org-latex-compiler "xelatex")
(add-to-list 'org-latex-classes
             '("extarticle"
               "\\documentclass[a4paper, 14pt]{extarticle}"
             ("\\section{%s}" . "\\section*{%s}")
             ("\\subsection{%s}" . "\\subsection*{%s}")
             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
             ("\\paragraph{%s}" . "\\paragraph*{%s}")
             ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
             )

;; (setq org-latex-packages-alist (list)) 
