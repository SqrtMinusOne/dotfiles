;;; -*- lexical-binding: t -*-
(use-package markdown-mode
  :straight t
  :mode "\\.md\\'"
  :config
  (setq markdown-command
        (concat
         "pandoc"
         " --from=markdown --to=html"
         " --standalone --mathjax --highlight-style=pygments"
         " --css=pandoc.css"
         " --quiet"
         ))
  (setq markdown-live-preview-delete-export 'delete-on-export)
  (setq markdown-asymmetric-header t)
  (setq markdown-open-command "/home/pavel/bin/scripts/chromium-sep")
  (add-hook 'markdown-mode-hook #'smartparens-mode)
  (general-define-key
   :keymaps 'markdown-mode-map
   "M-<left>" 'markdown-promote
   "M-<right>" 'markdown-demote))

;; (use-package livedown
;;   :straight (:host github :repo "shime/emacs-livedown")
;;   :commands livedown-preview
;;   :config
;;   (setq livedown-browser "qutebrowser"))

(use-package adoc-mode
  :mode (rx (| ".asciidoc") eos)
  :straight t)

(use-package plantuml-mode
  :straight t
  :mode "(\\.\\(plantuml?\\|uml\\|puml\\)\\'"
  :config
  (setq plantuml-executable-path "/home/pavel/.guix-extra-profiles/emacs/emacs/bin/plantuml")
  (setq plantuml-default-exec-mode 'executable)
  (setq plantuml-indent-level 2)
  (setq my/plantuml-indent-regexp-return "^\s*return\s+.+$")
  (;; (add-to-list
   ;;  'plantuml-indent-regexp-end
   ;;  my/plantuml-indent-regexp-return)
   )
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  (add-to-list 'auto-mode-alist '("\\.uml\\'" . plantuml-mode))
  (add-hook 'plantuml-mode-hook #'smartparens-mode)
  (general-nmap
    :keymaps 'plantuml-mode-map
    "RET" 'plantuml-preview))

(use-package subed
  :straight (:host github :repo "rndusr/subed" :files ("subed/*.el")
                   :build (:not native-compile))
  ;; (cons (rx (| "srt" "vtt" "ass") eos) #'subed-mode)
  :mode ("\\(?:ass\\|\\(?:sr\\|vt\\)t\\)\\'" . subed-mode)
  :config
  (general-define-key
   :keymaps '(subed-mode-map subed-vtt-mode-map)
   :states '(normal)
   "gp" #'subed-mpv-toggle-pause))

(use-package lsp-ltex
  :straight t
  :after (lsp)
  :init
  (setq lsp-ltex-version "15.2.0")
  (setq lsp-ltex-check-frequency "save"))

(defun my/ltex-lang ()
  (interactive)
  (setq lsp-ltex-language (completing-read
                           "Language: "
                           '("en-GB" "ru-RU" "de-DE")))
  (lsp-workspace-restart (lsp--read-workspace)))

(defun my/ltex-need-p ()
  (let ((file-name (buffer-file-name)))
    (cond
     (my/is-termux nil)
     ((null file-name) nil)
     ((string-match-p (rx "/home/pavel/" (+ alnum) ".org" eos) file-name) nil)
     ((string-match-p (rx (literal org-directory) "/" (or "roam" "inbox-notes" "literature-notes" "journal")) file-name) t)
     ((string-match-p (rx (literal org-directory)) file-name) nil)
     ((string-match-p (rx (literal (expand-file-name user-emacs-directory))) file-name) nil)
     (t t))))

(defun my/text-mode-lsp-maybe ()
  (when (my/ltex-need-p)
    (lsp)))

;; (add-hook 'text-mode-hook #'my/text-mode-lsp-maybe)

(use-package langtool
  :straight t
  :commands (langtool-check)
  :config
  (setq langtool-language-tool-server-jar "/home/pavel/bin/LanguageTool-6.4/languagetool-server.jar")
  (setq langtool-mother-tongue "ru")
  (setq langtool-default-language "ru-RU"))

(my-leader-def
  :infix "L"
  "" '(:which-key "languagetool")
  "c" 'langtool-check
  "s" 'langtool-server-stop
  "d" 'langtool-check-done
  "n" 'langtool-goto-next-error
  "p" 'langtool-goto-previous-error
  "l" 'langtool-correct-buffer)

(use-package reverso
  :straight (:host github :repo "SqrtMinusOne/reverso.el")
  :init
  (my-leader-def "ar" #'reverso)
  :commands (reverso)
  :config
  (setq reverso-languages '(russian english german))
  (reverso-history-mode))

(provide 'sqrt-markup)
