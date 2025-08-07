;;; -*- lexical-binding: t -*-
(use-package treesit
  :straight (:type built-in)
  :if (featurep 'treesit)
  :config
  (setq treesit-language-source-alist
        (mapcar
         (lambda (item)
           (let ((lang (nth 0 item))
                 (url (nth 1 item))
                 (rev (nth 2 item))
                 (source-dir (nth 3 item)))
             `(,lang ,url ,rev ,source-dir
                     ,(executable-find "gcc") ,(executable-find "c++"))))
         '((bash "https://github.com/tree-sitter/tree-sitter-bash")
           (cmake "https://github.com/uyha/tree-sitter-cmake")
           (css "https://github.com/tree-sitter/tree-sitter-css")
           (elisp "https://github.com/Wilfred/tree-sitter-elisp")
           (go "https://github.com/tree-sitter/tree-sitter-go")
           (html "https://github.com/tree-sitter/tree-sitter-html")
           (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
           (json "https://github.com/tree-sitter/tree-sitter-json")
           (make "https://github.com/alemuller/tree-sitter-make")
           (markdown "https://github.com/ikatyang/tree-sitter-markdown")
           (python "https://github.com/tree-sitter/tree-sitter-python")
           (toml "https://github.com/tree-sitter/tree-sitter-toml")
           (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
           (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
           (yaml "https://github.com/ikatyang/tree-sitter-yaml"))))
  (setq treesit-font-lock-level 4)
  (setq major-mode-remap-alist
        '((typescript-mode . typescript-ts-mode)
          (js-mode . javascript-ts-mode)
          (python-mode . python-ts-mode)
          (json-mode . json-ts-mode)))
  (cl-loop for (old-mode . new-mode) in major-mode-remap-alist
           do (my/set-smartparens-indent new-mode)
           do (set (intern (concat (symbol-name new-mode) "-hook"))
                   (list
                    (eval `(lambda ()
                             (run-hooks
                              ',(intern (concat (symbol-name old-mode) "-hook")))))))))

(use-package treesit-fold
  :straight (treesit-fold :type git :host github :repo "emacs-tree-sitter/treesit-fold")
  :commands (treesit-fold-mode))

(defun my/treesit-fold--get-nodes-to-fold ()
  (when-let*
      ((node (ignore-errors (treesit-buffer-root-node)))
       (patterns (seq-mapcat (lambda (fold-range) `((,(car fold-range)) @name))
                             (alist-get major-mode treesit-fold-range-alist)))
       (query (ignore-errors
                (treesit-query-compile (treesit-node-language node)
                                       patterns)))
       (nodes-to-fold (treesit-query-capture node query))
       (mode-ranges (alist-get major-mode treesit-fold-range-alist))
       (nodes-to-fold
        (cl-remove-if (lambda (node)
                        (treesit-fold--non-foldable-node-p (cdr node) mode-ranges))
                      nodes-to-fold)))
    nodes-to-fold))

(defun my/treesit-fold-hide-children ()
  (interactive)
  (let* ((current-node (treesit-fold--foldable-node-at-pos))
         (all-nodes-to-fold (my/treesit-fold--get-nodes-to-fold))
         ;; Find foldable children of `current-node'
         (target-nodes-to-fold
          (seq-filter
           (lambda (n)
             (cl-block tree-iter
               (while n
                 (setq n (treesit-node-parent n))
                 (when (equal n current-node)
                   (cl-return-from tree-iter t)))))
           (mapcar #'cdr all-nodes-to-fold))))
    (dolist (node target-nodes-to-fold)
      (treesit-fold-close node))))

(defun my/evil-fold-hide-level ()
  (interactive)
  (cond
   (hs-minor-mode (hs-hide-level))
   (treesit-fold-mode (my/treesit-fold-hide-children))))

(with-eval-after-load 'treesit-fold
  (general-define-key
   :states '(normal)
   "ze" #'my/evil-fold-hide-level)
  (keymap-unset evil-motion-state-map "z e" t))

(use-package combobulate
  :straight (:host github :repo "mickeynp/combobulate")
  :commands (combobulate))

(provide 'sqrt-tree-sitter)
