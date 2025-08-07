;;; -*- lexical-binding: t -*-
(defun my/toggle-shr-use-fonts ()
  "Toggle the shr-use-fonts variable in buffer"
  (interactive)
  (setq-local shr-use-fonts (not shr-use-fonts)))

(defface my/shr-face
  `((t :inherit variable-pitch))
  "Default face for shr rendering.")

(my/use-colors
  (my/shr-face :foreground (my/color-value 'blue)))

(defun my/shr-insert-around (fun &rest args)
  (let ((shr-current-font (or shr-current-font 'my/shr-face)))
    (apply fun args)))

(defun my/shr-urlify-around (fun start url &optional title)
  (funcall fun start url title)
  (let ((faces (get-text-property start 'face)))
    (put-text-property
     start (point)
     'face
     (mapcar
      (lambda (face)
        (if (eq face 'my/shr-face)
            'link
          face))
      (if (sequencep faces) faces (list faces))))))

(with-eval-after-load 'shr
  (advice-add #'shr-insert :around #'my/shr-insert-around)
  (advice-add #'shr-urlify :around #'my/shr-urlify-around))

(my-leader-def "aw" 'eww)
(my/persp-add-rule
  eww-mode 2 "browser")

(with-eval-after-load 'eww
  (general-define-key
   :keymaps '(eww-mode-map)
   :states '(normal emacs)
   "f" #'ace-link-eww
   "+" 'text-scale-increase
   "-" 'text-scale-decrease))

(use-package google-translate
  :straight t
  :if (not my/remote-server)
  :functions (my-google-translate-at-point google-translate--search-tkk)
  :commands (google-translate-at-point
             google-translate-at-point-reverse
             google-translate-query-translate
             google-translate-query-translate-reverse
             google-translate-smooth-translate)
  :custom
  (google-translate-backend-method 'curl)
  :config
  (require 'facemenu)
  (defun google-translate--search-tkk ()
    "Search TKK."
    (list 430675 2721866130))
  (defun my-google-translate-at-point()
    "reverse translate if prefix"
    (interactive)
    (if current-prefix-arg
        (google-translate-at-point)
      (google-translate-at-point-reverse)))
  (setq google-translate-translation-directions-alist
        '(("en" . "ru")
          ("ru" . "en")
          ("de" . "en")
          ("en" . "de"))))

(my-leader-def
  :infix "at"
  "" '(:which-key "google translate")
  "p" 'google-translate-at-point
  "P" 'google-translate-at-point-reverse
  "q" 'google-translate-query-translate
  "Q" 'google-translate-query-translate-reverse
  "t" 'google-translate-smooth-translate)

(use-package biome
  :straight t
  :commands (biome)
  :init
  (my-leader-def "ab" #'biome)
  (when my/is-termux
    (setq biome-query-tab-key "<TAB>")
    (setq biome-api-try-parse-error-as-response t))
  :config
  (add-to-list 'biome-query-coords
               '("Saint-Petersburg, Russia" 59.942651 30.229930))
  (add-to-list 'biome-query-coords
               '("Tyumen, Russia" 57.15222 65.52722)))

(provide 'sqrt-misc-internet)
