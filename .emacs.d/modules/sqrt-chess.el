;;; -*- lexical-binding: t -*-
(use-package chess
  :commands (chess-pgn-mode)
  :straight t)

(setq my/chess-python "/home/pavel/.guix-extra-profiles/dev/dev/bin/python3")

(defun org-babel-execute:pgn (body params)
  (let ((out-file (or (alist-get :file params)
                      (org-babel-temp-file "pgn-" ".png"))))
    (org-babel-eval
     (format "%s %s '%s' '%s'" my/chess-python
             "~/bin/python-scripts/render_pgn.py"
             body out-file)
     "")
    nil))

(defvar org-babel-default-header-args:pgn
  '((:results . "file") (:exports . "results"))
  "Default arguments for evaluating a pgn source block.")

(defun org-babel-execute:fen (body params)
  (let ((out-file (or (alist-get :file params)
                      (org-babel-temp-file "fen-" ".png"))))
    (org-babel-eval
     (format "%s %s '%s' '%s' true" my/chess-python
             "~/bin/python-scripts/render_pgn.py"
             body out-file)
     "")
    nil))

(defvar org-babel-default-header-args:fen
  '((:results . "file") (:exports . "results"))
  "Default arguments for evaluating a pgn source block.")

(provide 'sqrt-chess)
