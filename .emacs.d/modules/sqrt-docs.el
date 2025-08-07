;;; -*- lexical-binding: t -*-
(use-package tldr
  :straight t
  :commands (tldr)
  :config
  (setq tldr-source-zip-url "https://github.com/tldr-pages/tldr/archive/refs/heads/main.zip")

  (defun tldr-update-docs ()
    (interactive)
    (shell-command-to-string (format "curl -L %s --output %s" tldr-source-zip-url tldr-saved-zip-path))
    (when (file-exists-p "/tmp/tldr")
      (delete-directory "/tmp/tldr" t))
    (shell-command-to-string (format "unzip -d /tmp/tldr/ %s" tldr-saved-zip-path))
    (when (file-exists-p tldr-directory-path)
      (delete-directory tldr-directory-path 'recursive 'no-trash))
    (shell-command-to-string (format "mv %s %s" "/tmp/tldr/tldr-main" tldr-directory-path))))

(my-leader-def "hT" 'tldr)

(setq Man-width-max 180)
(my-leader-def "hM" 'woman)
(setq woman-fill-column 90)

(general-define-key
 :states '(normal)
 :keymaps 'Info-mode-map
 (kbd "RET") #'Info-follow-nearest-node
 "H" #'Info-history-back
 "L" #'Info-history-forward
 "n" #'Info-search-next
 "b" #'Info-search-backward
 "f" #'ace-link-info)

(defun my/man-fix-width (&rest _)
  (setq-local Man-width (- (window-width) 4)))

(advice-add #'Man-update-manpage :before #'my/man-fix-width)

(use-package devdocs-browser
  :straight t
  :commands (devdocs-browser-open
             devdocs-browser-open-in
             devdocs-browser-install-doc
             devdocs-browser-uninstall-doc
             devdocs-browser-download-offline-data
             devdocs-browser-remove-offline-data
             devdocs-browser-upgrade-all-docs
             devdocs-browser-update-docs)
  :init
  (my-leader-def
    :infix "hd"
    "" '(:wk "devdocs")
    "d" #'devdocs-browser-open
    "o" #'devdocs-browser-open-in
    "i" #'devdocs-browser-install-doc
    "n" #'devdocs-browser-uninstall-doc
    "o" #'devdocs-browser-download-offline-data
    "O" #'devdocs-browser-remove-offline-data
    "u" #'devdocs-browser-upgrade-all-docs
    "r" #'devdocs-browser-update-docs))

(provide 'sqrt-docs)
