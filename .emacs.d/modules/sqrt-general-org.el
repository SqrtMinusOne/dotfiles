;;; -*- lexical-binding: t -*-
(setq org-directory (expand-file-name "~/30-39 Life/32 org-mode"))

(use-package org
  :straight (:type built-in)
  :defer t
  :init
  (unless (file-exists-p org-directory)
    (mkdir org-directory t))
  :config
  (setq org-startup-indented (not my/is-termux))
  (setq org-return-follows-link t)
  (setq org-src-tab-acts-natively nil)
  (add-hook 'org-mode-hook 'smartparens-mode)
  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (visual-line-mode -1)
              (toggle-truncate-lines 1)
              (display-line-numbers-mode 0)))
  (add-hook 'org-mode-hook
            (lambda ()
              (rainbow-delimiters-mode -1))))

(with-eval-after-load 'org
  (require 'org-crypt)
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance '("crypt"))
  (setq org-crypt-key "C1EC867E478472439CC82410DE004F32AFA00205"))

(with-eval-after-load 'epg
  (setq epg-gpg-program "gpg")
  (setq epg-config--program-alist
        `((OpenPGP
           epg-gpg-program
           ;; ("gpg2" . ,epg-gpg2-minimum-version)
           ("gpg" . ((,epg-gpg-minimum-version . "2.0")
                     ,epg-gpg2-minimum-version)))
          (CMS
           epg-gpgsm-program
           ("gpgsm" . "2.0.4")))))

(defun my/epa--select-keys-around (fun prompt keys)
  (if (= (seq-length keys) 1)
      keys
    (funcall fun prompt keys)))

(with-eval-after-load 'epa
  (advice-add #'epa--select-keys :around #'my/epa--select-keys-around))

(unless my/remote-server
  (setq epa-file-encrypt-to '("DE004F32AFA00205")))

(use-package org-contrib
  :straight (org-contrib
             :type git
             :repo "https://git.sr.ht/~bzg/org-contrib"
             :build t)
  :after (org)
  :config
  (require 'ox-extra)
  (ox-extras-activate '(latex-header-blocks ignore-headlines)))

(with-eval-after-load 'org
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("sq" . "src sql")))

(use-package evil-org
  :straight t
  :hook (org-mode . evil-org-mode)
  :config
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme '(navigation insert textobjects additional calendar todo))))
  (add-to-list 'evil-emacs-state-modes 'org-agenda-mode)
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(defun my/export-rel-url (path desc format)
  (cl-case format
    (html (format "<a href=\"%s\">%s</a>" path (or desc path)))
    (latex (format "\\href{%s}{%s}" path (or desc path)))
    (otherwise path)))

(with-eval-after-load 'org
  (org-link-set-parameters "rel" :follow #'browse-url :export #'my/export-rel-url))

(defun my/outline-prev-or-up-heading ()
  (interactive)
  (if (outline-on-heading-p)
      (outline-up-heading 1)
    (outline-previous-visible-heading 1)))

(with-eval-after-load 'org
  (general-define-key
   :keymaps 'org-mode-map
   "C-c d" #'org-decrypt-entry
   "C-c e" #'org-encrypt-entry
   "M-p" #'org-latex-preview
   "M-o" #'org-redisplay-inline-images)

  (general-define-key
   :keymaps 'org-mode-map
   :states '(normal emacs)
   "L" #'org-shiftright
   "H" #'org-shiftleft
   "S-<next>" #'org-next-visible-heading
   "S-<prior>" #'org-previous-visible-heading
   "M-0" #'org-next-visible-heading
   "M-9" #'org-previous-visible-heading
   "C-0" #'org-forward-heading-same-level
   "C-9" #'org-backward-heading-same-level
   "(" #'my/outline-prev-or-up-heading
   "M-]" #'org-babel-next-src-block
   "M-[" #'org-babel-previous-src-block)

  (general-define-key
   :keymaps 'org-agenda-mode-map
   "M-]" #'org-agenda-later
   "M-[" #'org-agenda-earlier)

  (general-nmap :keymaps 'org-mode-map "RET" 'org-ctrl-c-ctrl-c))

(defun my/org-link-copy (&optional arg)
  "Extract URL from org-mode link and add it to kill ring."
  (interactive "P")
  (let* ((link (org-element-lineage (org-element-context) '(link) t))
         (type (org-element-property :type link))
         (url (org-element-property :path link))
         (url (concat type ":" url)))
    (kill-new url)
    (message (concat "Copied URL: " url))))

(with-eval-after-load 'org
  (general-nmap :keymaps 'org-mode-map
    "C-x C-l" 'my/org-link-copy))

(defun my/org-babel-next-visible-src-block (arg)
  "Move to the next visible source block.

With ARG, repeats or can move backward if negative."
  (interactive "p")
  (let ((regexp org-babel-src-block-regexp))
    (if (< arg 0)
        (beginning-of-line)
      (end-of-line))
    (while (and (< arg 0) (re-search-backward regexp nil :move))
      (unless (bobp)
        (while (pcase (get-char-property-and-overlay (point) 'invisible)
                 (`(outline . ,o)
                  (goto-char (overlay-start o))
                  (re-search-backward regexp nil :move))
                 (_ nil))))
      (cl-incf arg))
    (while (and (> arg 0) (re-search-forward regexp nil t))
      (while (pcase (get-char-property-and-overlay (point) 'invisible)
               (`(outline . ,o)
                (goto-char (overlay-end o))
                (re-search-forward regexp nil :move))
               (_ (end-of-line) nil)))
      (re-search-backward regexp nil :move)
      (cl-decf arg))
    (if (> arg 0) (goto-char (point-max)) (beginning-of-line))))

(defun my/org-babel-previous-visible-src-block (arg)
  "Move to the prevous visible source block.

With ARG, repeats or can move backward if negative."
  (interactive "p")
  (my/org-babel-next-visible-src-block (- arg)))

(with-eval-after-load 'org
  (general-define-key
   :keymaps 'org-mode-map
   :states '(normal emacs)
   "M-]" #'my/org-babel-next-visible-src-block
   "M-[" #'my/org-babel-previous-visible-src-block))

(defun my/org-file-open ()
  (interactive)
  (let* ((files
          (thread-last
            '("projects" "misc" "learning")
            (mapcar (lambda (f)
                      (directory-files (concat org-directory "/" f) t (rx ".org" eos))))
            (apply #'append)
            (mapcar (lambda (file)
                      (string-replace (concat org-directory "/") "" file)))
            (append
             '("inbox.org" "contacts.org" "recurring.org")))))
    (find-file
     (concat org-directory "/"
             (completing-read "Org file: " files)))))

(defun my/enable-org-latex ()
  (interactive)
  (customize-set-variable 'org-highlight-latex-and-related '(native))
  (add-hook 'org-mode-hook (lambda () (yas-activate-extra-mode 'LaTeX-mode)))
  (sp-local-pair 'org-mode "$" "$")
  (sp--remove-local-pair "'"))

(with-eval-after-load 'org
  (setq my/org-latex-scale 1.75)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale my/org-latex-scale)))

(with-eval-after-load 'org
  (setq my/latex-preview-header "\\documentclass{article}
\\usepackage[usenames]{color}
\\usepackage{graphicx}
\\usepackage{grffile}
\\usepackage{longtable}
\\usepackage{wrapfig}
\\usepackage{rotating}
\\usepackage[normalem]{ulem}
\\usepackage{amsmath}
\\usepackage{textcomp}
\\usepackage{amssymb}
\\usepackage{capt-of}
\\usepackage{hyperref}
\\pagestyle{empty}")

  (setq org-preview-latex-process-alist
        (mapcar
         (lambda (item)
           (cons
            (car item)
            (plist-put (cdr item) :latex-header my/latex-preview-header)))
         org-preview-latex-process-alist)))

(use-package org-superstar
  :straight t
  :disabled
  :hook (org-mode . org-superstar-mode))

(use-package org-bars
  :straight (:repo "tonyaldon/org-bars" :host github)
  :if (display-graphic-p)
  :hook (org-mode . org-bars-mode))

(unless (display-graphic-p)
  (add-hook 'org-mode-hook #'org-indent-mode))

(defun my/org-no-ellipsis-in-headlines ()
  (remove-from-invisibility-spec '(outline . t))
  (add-to-invisibility-spec 'outline))

(with-eval-after-load 'org-bars
  (add-hook 'org-mode-hook #'my/org-no-ellipsis-in-headlines)
  (when (eq major-mode 'org-mode)
    (my/org-no-ellipsis-in-headlines)))

(my/use-colors
 (org-block :background (my/color-value 'bg-other))
 (org-block-begin-line :background (my/color-value 'bg-other)
                       :foreground (my/color-value 'grey)))

(use-package org-appear
  :after (org)
  :straight t)

(use-package org-fragtog
  :after (org)
  :straight t)

(use-package hide-mode-line
  :straight t
  :commands (hide-mode-line-mode))

(defun my/present-next-with-latex ()
  (interactive)
  (org-present-next)
  (org-latex-preview '(16)))

(defun my/present-prev-with-latex ()
  (interactive)
  (org-present-prev)
  (org-latex-preview '(16)))

(use-package org-present
  :straight (:host github :repo "rlister/org-present")
  :commands (org-present)
  :config
  (general-define-key
   :keymaps 'org-present-mode-keymap
   "<next>" 'my/present-next-with-latex
   "<prior>" 'my/present-prev-with-latex)
  (setq org-present-mode-hook
        (list (lambda ()
                (blink-cursor-mode 0)
                (org-present-big)
                (org-bars-mode -1)
                ;; (org-display-inline-images)
                (org-present-hide-cursor)
                (org-present-read-only)
                (display-line-numbers-mode 0)
                (hide-mode-line-mode +1)
                (setq-local org-format-latex-options
                            (plist-put org-format-latex-options
                                       :scale (* org-present-text-scale my/org-latex-scale 0.5)))
                ;; (org-latex-preview '(16))
                ;; TODO ^somehow this stucks at running LaTeX^
                (setq-local olivetti-body-width 60)
                (olivetti-mode 1))))
  (setq org-present-mode-quit-hook
        (list (lambda ()
                (blink-cursor-mode 1)
                (org-present-small)
                (org-bars-mode 1)
                ;; (org-remove-inline-images)
                (org-present-show-cursor)
                (org-present-read-write)
                (display-line-numbers-mode 1)
                (hide-mode-line-mode 0)
                (setq-local org-format-latex-options (plist-put org-format-latex-options :scale my/org-latex-scale))
                (org-latex-preview '(64))
                (olivetti-mode -1)
                (setq-local olivetti-body-width (default-value 'olivetti-body-width))))))

(use-package org-attach-screenshot
  :commands (org-attach-screenshot)
  :straight t
  :config
  (setq org-attach-screenshot-auto-refresh 'never))

(use-package org-transclusion
  :after org
  :straight (:host github :repo "nobiot/org-transclusion")
  :config
  (add-to-list 'org-transclusion-extensions 'org-transclusion-indent-mode)
  (require 'org-transclusion-indent-mode)
  (general-define-key
   :keymaps '(org-transclusion-map)
   :states '(normal)
   "RET" #'org-transclusion-open-source
   "gr" #'org-transclusion-refresh)
  (general-define-key
   :keymaps '(org-mode-map)
   :states 'normal
   "C-c t a" #'org-transclusion-add
   "C-c t A" #'org-transclusion-add-all
   "C-c t t" #'org-transclusion-mode))

(defun my/export-org-tables-to-csv ()
  (interactive)
  (org-table-map-tables
   (lambda ()
     (when-let
         (name
          (plist-get (cadr (org-element-at-point)) :name))
       (org-table-export
        (concat
         (file-name-directory
          (buffer-file-name))
         name ".csv")
        "orgtbl-to-csv")))))

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

(setq my/org-config-files
      (mapcar
       #'expand-file-name
       '("~/Emacs.org"
         "~/Desktop.org"
         "~/Console.org"
         "~/Arch.org"
         "~/Guix.org"
         "~/Mail.org")))

(add-hook 'org-mode-hook
          (lambda ()
            (when (member (buffer-file-name) my/org-config-files)
              (setq-local org-confirm-babel-evaluate nil))))

(defun my/regenerate-desktop ()
  (interactive)
  (org-babel-tangle-file "/home/pavel/Desktop.org")
  (org-babel-tangle-file "/home/pavel/Console.org")
  (call-process "xrdb" nil nil nil "-load" "/home/pavel/.Xresources")
  (call-process "~/bin/polybar.sh")
  (call-process "pkill" nil nil nil "dunst")
  (call-process "herd" nil nil nil "restart" "xsettingsd")
  (when (fboundp #'my/exwm-set-alpha)
    (if (my/light-p)
        (my/exwm-set-alpha 100)
      (my/exwm-set-alpha 90))))

(provide 'sqrt-general-org)
