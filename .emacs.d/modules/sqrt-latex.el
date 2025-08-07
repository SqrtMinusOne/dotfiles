;;; -*- lexical-binding: t -*-
(use-package tex
  :straight auctex
  :defer t
  :config
  (setq-default TeX-auto-save t)
  (setq-default TeX-parse-self t)
  (TeX-PDF-mode)
  ;; Use XeLaTeX & stuff
  (setq-default TeX-engine 'xetex)
  (setq-default TeX-command-extra-options "-shell-escape")
  (setq-default TeX-source-correlate-method 'synctex)
  (TeX-source-correlate-mode)
  (setq-default TeX-source-correlate-start-server t)
  (setq-default LaTeX-math-menu-unicode t)

  (setq-default font-latex-fontify-sectioning 1.3)

  ;; Scale preview for my DPI
  (setq-default preview-scale-function 1.4)
  (when (boundp 'tex--prettify-symbols-alist)
    (assoc-delete-all "--" tex--prettify-symbols-alist)
    (assoc-delete-all "---" tex--prettify-symbols-alist))

  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (TeX-fold-mode 1)
              (outline-minor-mode)))

  (add-to-list 'TeX-view-program-selection
               '(output-pdf "Zathura"))

  ;; Do not run lsp within templated TeX files
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (unless (string-match "\.hogan\.tex$" (buffer-name))
                (lsp))
              (setq-local lsp-diagnostic-package :none)
              (setq-local flycheck-checker 'tex-chktex)))

  (add-hook 'LaTeX-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'LaTeX-mode-hook #'smartparens-mode)
  (add-hook 'LaTeX-mode-hook #'prettify-symbols-mode)

  (my/set-smartparens-indent 'LaTeX-mode)
  (require 'smartparens-latex)

  (general-nmap
    :keymaps '(LaTeX-mode-map latex-mode-map)
    "RET" 'TeX-command-run-all
    "C-c t" 'orgtbl-mode)

  (setq my/greek-alphabet
        '(("a" . "\\alpha")
          ("b" . "\\beta" )
          ("g" . "\\gamma")
          ("d" . "\\delta")
          ("e" . "\\epsilon")
          ("z" . "\\zeta")
          ("h" . "\\eta")
          ("o" . "\\theta")
          ("i" . "\\iota")
          ("k" . "\\kappa")
          ("l" . "\\lambda")
          ("m" . "\\mu")
          ("n" . "\\nu")
          ("x" . "\\xi")
          ("p" . "\\pi")
          ("r" . "\\rho")
          ("s" . "\\sigma")
          ("t" . "\\tau")
          ("u" . "\\upsilon")
          ("f" . "\\phi")
          ("c" . "\\chi")
          ("v" . "\\psi")
          ("g" . "\\omega")))

  (setq my/latex-greek-prefix "'")

  ;; The same for capitalized letters
  (dolist (elem my/greek-alphabet)
    (let ((key (car elem))
          (value (cdr elem)))
      (when (string-equal key (downcase key))
        (add-to-list 'my/greek-alphabet
                     (cons
                      (capitalize (car elem))
                      (concat
                       (substring value 0 1)
                       (capitalize (substring value 1 2))
                       (substring value 2)))))))

  (yas-define-snippets
   'latex-mode
   (mapcar
    (lambda (elem)
      (list (concat my/latex-greek-prefix (car elem)) (cdr elem) (concat "Greek letter " (car elem))))
    my/greek-alphabet))
  (setq my/english-alphabet
        '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))

  (dolist (elem my/english-alphabet)
    (when (string-equal elem (downcase elem))
      (add-to-list 'my/english-alphabet (upcase elem))))

  (setq my/latex-mathbb-prefix "`")

  (yas-define-snippets
   'latex-mode
   (mapcar
    (lambda (elem)
      (list (concat my/latex-mathbb-prefix elem) (concat "\\mathbb{" elem "}") (concat "Mathbb letter " elem)))
    my/english-alphabet))
  (setq my/latex-math-symbols
        '(("x" . "\\times")
          ("." . "\\cdot")
          ("v" . "\\forall")
          ("s" . "\\sum_{$1}^{$2}$0")
          ("p" . "\\prod_{$1}^{$2}$0")
          ("d" . "\\partial")
          ("e" . "\\exists")
          ("i" . "\\int_{$1}^{$2}$0")
          ("c" . "\\cap")
          ("u" . "\\cup")
          ("0" . "\\emptyset")
          ("^" . "\\widehat{$1}$0")
          ("_" . "\\overline{$1}$0")
          ("~" . "\\sim")
          ("|" . "\\mid")
          ("_|" . "\\perp")))

  (setq my/latex-math-prefix ";")

  (yas-define-snippets
   'latex-mode
   (mapcar
    (lambda (elem)
      (let ((key (car elem))
            (value (cdr elem)))
        (list (concat my/latex-math-prefix key) value (concat "Math symbol " value))))
    my/latex-math-symbols))
  (setq my/latex-section-snippets
        '(("ch" . "\\chapter{$1}")
          ("sec" . "\\section{$1}")
          ("ssec" . "\\subsection{$1}")
          ("sssec" . "\\subsubsection{$1}")
          ("par" . "\\paragraph{$1}}")))

  (setq my/latex-section-snippets
        (mapcar
         (lambda (elem)
           `(,(car elem)
             ,(cdr elem)
             ,(progn
                (string-match "[a-z]+" (cdr elem))
                (match-string 0 (cdr elem)))))
         my/latex-section-snippets))

  (dolist (elem my/latex-section-snippets)
    (let* ((key (nth 0 elem))
           (value (nth 1 elem))
           (desc (nth 2 elem))
           (star-index (string-match "\{\$1\}" value)))
      (add-to-list 'my/latex-section-snippets
                   `(,(concat key "*")
                     ,(concat
                       (substring value 0 star-index)
                       "*"
                       (substring value star-index))
                     ,(concat desc " with *")))
      (add-to-list 'my/latex-section-snippets
                   `(,(concat key "l")
                     ,(concat value "%\n\\label{sec:$2}")
                     ,(concat desc " with label")))))

  (dolist (elem my/latex-section-snippets)
    (setf (nth 1 elem) (concat (nth 1 elem) "\n$0")))

  (yas-define-snippets
   'latex-mode
   my/latex-section-snippets))

(defun my/list-sty ()
  (reverse
   (sort
    (seq-filter
     (lambda (file) (if (string-match ".*\.sty$" file) 1 nil))
     (directory-files
      (seq-some
       (lambda (dir)
         (if (and
              (f-directory-p dir)
              (seq-some
               (lambda (file) (string-match ".*\.sty$" file))
               (directory-files dir))
              ) dir nil))
       (list "./styles" "../styles/" "." "..")) :full))
    (lambda (f1 f2)
      (let ((f1b (file-name-base f1))
            (f1b (file-name-base f2)))
        (cond
         ((string-match-p ".*BibTex" f1) t)
         ((and (string-match-p ".*Locale" f1) (not (string-match-p ".*BibTex" f2))) t)
         ((string-match-p ".*Preamble" f2) t)
         (t (string-lessp f1 f2))))))))

(defun my/import-sty ()
  (interactive)
  (insert
   (apply #'concat
          (cl-mapcar
           (lambda (file) (concat "\\usepackage{" (file-name-sans-extension (file-relative-name file default-directory)) "}\n"))
           (my/list-sty)))))

(defun my/import-sty-org ()
  (interactive)
  (insert
   (apply #'concat
          (cl-mapcar
           (lambda (file) (concat "#+LATEX_HEADER: \\usepackage{" (file-name-sans-extension (file-relative-name file default-directory)) "}\n"))
           (my/list-sty)))))

(setq my/greek-alphabet
      '(("a" . "\\alpha")
        ("b" . "\\beta" )
        ("g" . "\\gamma")
        ("d" . "\\delta")
        ("e" . "\\epsilon")
        ("z" . "\\zeta")
        ("h" . "\\eta")
        ("o" . "\\theta")
        ("i" . "\\iota")
        ("k" . "\\kappa")
        ("l" . "\\lambda")
        ("m" . "\\mu")
        ("n" . "\\nu")
        ("x" . "\\xi")
        ("p" . "\\pi")
        ("r" . "\\rho")
        ("s" . "\\sigma")
        ("t" . "\\tau")
        ("u" . "\\upsilon")
        ("f" . "\\phi")
        ("c" . "\\chi")
        ("v" . "\\psi")
        ("g" . "\\omega")))

(setq my/latex-greek-prefix "'")

;; The same for capitalized letters
(dolist (elem my/greek-alphabet)
  (let ((key (car elem))
        (value (cdr elem)))
    (when (string-equal key (downcase key))
      (add-to-list 'my/greek-alphabet
                   (cons
                    (capitalize (car elem))
                    (concat
                     (substring value 0 1)
                     (capitalize (substring value 1 2))
                     (substring value 2)))))))

(yas-define-snippets
 'latex-mode
 (mapcar
  (lambda (elem)
    (list (concat my/latex-greek-prefix (car elem)) (cdr elem) (concat "Greek letter " (car elem))))
  my/greek-alphabet))

(setq my/english-alphabet
      '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))

(dolist (elem my/english-alphabet)
  (when (string-equal elem (downcase elem))
    (add-to-list 'my/english-alphabet (upcase elem))))

(setq my/latex-mathbb-prefix "`")

(yas-define-snippets
 'latex-mode
 (mapcar
  (lambda (elem)
    (list (concat my/latex-mathbb-prefix elem) (concat "\\mathbb{" elem "}") (concat "Mathbb letter " elem)))
  my/english-alphabet))

(setq my/latex-math-symbols
      '(("x" . "\\times")
        ("." . "\\cdot")
        ("v" . "\\forall")
        ("s" . "\\sum_{$1}^{$2}$0")
        ("p" . "\\prod_{$1}^{$2}$0")
        ("d" . "\\partial")
        ("e" . "\\exists")
        ("i" . "\\int_{$1}^{$2}$0")
        ("c" . "\\cap")
        ("u" . "\\cup")
        ("0" . "\\emptyset")
        ("^" . "\\widehat{$1}$0")
        ("_" . "\\overline{$1}$0")
        ("~" . "\\sim")
        ("|" . "\\mid")
        ("_|" . "\\perp")))

(setq my/latex-math-prefix ";")

(yas-define-snippets
 'latex-mode
 (mapcar
  (lambda (elem)
    (let ((key (car elem))
          (value (cdr elem)))
      (list (concat my/latex-math-prefix key) value (concat "Math symbol " value))))
  my/latex-math-symbols))

(setq my/latex-section-snippets
      '(("ch" . "\\chapter{$1}")
        ("sec" . "\\section{$1}")
        ("ssec" . "\\subsection{$1}")
        ("sssec" . "\\subsubsection{$1}")
        ("par" . "\\paragraph{$1}}")))

(setq my/latex-section-snippets
      (mapcar
       (lambda (elem)
         `(,(car elem)
           ,(cdr elem)
           ,(progn
              (string-match "[a-z]+" (cdr elem))
              (match-string 0 (cdr elem)))))
       my/latex-section-snippets))

(dolist (elem my/latex-section-snippets)
  (let* ((key (nth 0 elem))
         (value (nth 1 elem))
         (desc (nth 2 elem))
         (star-index (string-match "\{\$1\}" value)))
    (add-to-list 'my/latex-section-snippets
                 `(,(concat key "*")
                   ,(concat
                     (substring value 0 star-index)
                     "*"
                     (substring value star-index))
                   ,(concat desc " with *")))
    (add-to-list 'my/latex-section-snippets
                 `(,(concat key "l")
                   ,(concat value "%\n\\label{sec:$2}")
                   ,(concat desc " with label")))))

(dolist (elem my/latex-section-snippets)
  (setf (nth 1 elem) (concat (nth 1 elem) "\n$0")))

(yas-define-snippets
 'latex-mode
 my/latex-section-snippets)

(provide 'sqrt-latex)
