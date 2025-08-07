;;; -*- lexical-binding: t -*-
(use-package lsp-mode
  :straight t
  :hook (
         (typescript-mode . lsp)
         (js-mode . lsp)
         (vue-mode . lsp)
         (go-mode . lsp)
         (svelte-mode . lsp)
         ;; (python-mode . lsp)
         (json-mode . lsp)
         (haskell-mode . lsp)
         (haskell-literate-mode . lsp)
         (java-mode . lsp)
         ;; (csharp-mode . lsp)
         )
  :commands lsp
  :init
  (setq lsp-keymap-prefix nil)
  :config
  (setq lsp-idle-delay 1)
  (setq lsp-eslint-server-command '("node" "/home/pavel/.emacs.d/.cache/lsp/eslint/unzipped/extension/server/out/eslintServer.js" "--stdio"))
  (setq lsp-eslint-run "onSave")
  (setq lsp-signature-render-documentation nil)
  ;; (lsp-headerline-breadcrumb-mode nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-volar-take-over-mode nil)
  (add-to-list 'lsp-language-id-configuration '(svelte-mode . "svelte")))

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-delay 2)
  (setq lsp-ui-sideline-show-hover nil))

(use-package all-the-icons
  :straight t)

(my-leader-def
  :infix "l"
  "" '(:which-key "lsp")
  "d" 'lsp-ui-peek-find-definitions
  "r" 'lsp-rename
  "u" 'lsp-ui-peek-find-references
  "s" 'lsp-ui-find-workspace-symbol
  "l" 'lsp-execute-code-action
  "e" 'list-flycheck-errors)

(defun my/lsp--progress-status ()
  "Returns the status of the progress for the current workspaces."
  (-let ((progress-status
          (s-join
           "|"
           (-keep
            (lambda (workspace)
              (let ((tokens (lsp--workspace-work-done-tokens workspace)))
                (unless (ht-empty? tokens)
                  (mapconcat
                   (-lambda ((&WorkDoneProgressBegin :message? :title :percentage?))
                     (concat (if percentage?
                                 (if (numberp percentage?)
                                     (format "%.0f%%%% " percentage?)
                                   (format "%s%%%% " percentage?))
                               "")
                             (let ((msg (url-unhex-string (or message\? title))))
                               (if (string-match-p "\\`file:///" msg)
                                   (file-name-nondirectory msg)))))
                   (ht-values tokens)
                   "|"))))
            (lsp-workspaces)))))
    (unless (s-blank? progress-status)
      (concat lsp-progress-prefix progress-status))))

(with-eval-after-load 'lsp-mode
  (advice-add 'lsp--progress-status :override #'my/lsp--progress-status))

(setq my/lsp--vue-diagnostics-last-update (make-hash-table :test #'equal))

(defun my/lsp--on-diagnostics (fn workspace params)
  (if (equal (gethash 'vue-semantic-server lsp-clients)
             (lsp--workspace-client workspace))
      (progn
        (let* ((is-empty (seq-empty-p (gethash "diagnostics" params)))
               (uri (gethash "uri" params))
               (last-update (gethash uri my/lsp--vue-diagnostics-last-update))
               (current-update (time-convert nil #'integer)))
          (unless is-empty
            (puthash uri current-update my/lsp--vue-diagnostics-last-update))
          (when (or (not is-empty)
                    (not last-update)
                    (> (- current-update (or last-update 0)) 5))
            (funcall fn workspace params))))
    (funcall fn workspace params)))

(with-eval-after-load 'lsp
  (advice-add #'lsp--on-diagnostics :around #'my/lsp--on-diagnostics))

(provide 'sqrt-lsp)
