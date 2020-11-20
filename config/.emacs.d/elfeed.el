(defun elfeed-open-current-with-qutebrowser ()
  "Open current link with qutebrowser."
  (interactive)
  (let ((browse-url-generic-program "/usr/bin/qutebrowser"))
    (elfeed-show-visit t)))

(defun elfeed-browse-url-with-qutebrowser ()
  "Open current link with qutebrowser."
  (interactive)
  (let ((browse-url-generic-program "/usr/bin/qutebrowser"))
    (elfeed-search-browse-url t)))

(defun elfeed-open-current-with-chromium ()
  "Open current link with qutebrowser."
  (interactive)
  (let ((browse-url-generic-program "/usr/bin/chromium"))
    (elfeed-show-visit t)))

(defun elfeed-browse-url-with-chromium ()
  "Open current link with qutebrowser."
  (interactive)
  (let ((browse-url-generic-program "/usr/bin/chromium"))
    (elfeed-search-browse-url t)))


(general-define-key
 :keymaps 'elfeed-search-mode-map
 "f" 'elfeed-search-update--force
 "o" 'elfeed-browse-url-with-qutebrowser
 "S-o" 'elfeed-browse-url-with-chromium)

(general-define-key
 :keymaps 'elfeed-show-mode-map
 "o" 'elfeed-open-current-with-qutebrowser
 "S-o" 'elfeed-browse-url-with-chromium
)

(general-define-key
 :keymaps '(elfeed-search-mode-map elfeed-show-mode-map)
 "g" nil
 "gN" 'quit-window
 "q" 'quit-window
 "j" 'next-line
 "k" 'previous-line)
