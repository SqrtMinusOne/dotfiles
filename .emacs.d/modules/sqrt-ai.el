;;; -*- lexical-binding: t -*-
(use-package gptel
  :straight t
  :if (not my/is-termux)
  :init
  (my-leader-def
    :infix "ai"
    "" '(:wk "AI")
    "i" #'gptel)
  :commands (gptel gptel-send gptel-menu)
  :config
  (setq gptel-mode "llama3:latest")
  (setq gptel-track-media t)
  (setq gptel-backend
        (gptel-make-ollama "Ollama"
          :host "localhost:11434"
          :stream t
          :models '("llama3.1:8b" "deepseek-r1:32b"
                    "qwen2.5:32b" "qwen2.5-coder:32b"
                    "qwen3:30b" "qwen3:32b"
                    "eva-qwen2.5-q4_k_l-32b:latest"
                    (gemma3:27b
                     :capabilities (media)
                     :mime-types ("image/jpeg" "image/png")))))
  (gptel-make-openai "OpenRouter"
    :host "openrouter.ai/api"
    :key (lambda () (my/password-store-get-field
                     "My_Online/Accounts/openrouter" "api-key"))
    :stream t
    :models '("anthropic/claude-sonnet-4"
              "qwen/qwen3-coder"
              "qwen/qwen3-coder:free"))
  (setq gptel--known-backends
        (seq-filter
         (lambda (cell)
           (not (equal (car cell) "ChatGPT")))
         gptel--known-backends))
  (setq gptel-response-prefix-alist
        '((markdown-mode . "[Response] ")
          (org-mode . "*** Response: ")
          (text-mode . "[Response]")))

  (general-define-key
   :keymaps '(gptel-mode-map)
   :states '(insert normal)
   "C-<return>" 'gptel-send
   "M-o" #'gptel-menu))

(use-package ellama
  :straight t
  :init
  (setq ellama-language "English")
  :defer t
  :config
  (require 'llm-ollama)
  ;; I've looked for this option for 1.5 hours
  (setq ellama-long-lines-length 100000)

  (setq ellama-provider (make-llm-ollama
                         :chat-model "qwen2.5:32b"
                         :embedding-model "qwen2.5:32b"))
  (setq ellama-coding-provider (make-llm-ollama
                                :chat-model "qwen2.5-coder:32b"
                                :embedding-model "qwen2.5-coder:32b"))
  (setq ellama-providers
        `(("llama3.1:8b" . ,(make-llm-ollama
                             :chat-model "llama3.1:latest"
                             :embedding-model "llama3.1:latest"))
          ("phi4:latest" . ,(make-llm-ollama
                             :chat-model "phi4:latest"
                             :embedding-model "phi4:latest"))
          ("qwen2.5:32b" . ,(make-llm-ollama
                             :chat-model "qwen2.5:32b"
                             :embedding-model "qwen2.5:32b"))
          ("qwen2.5-coder:32b" . ,(make-llm-ollama
                                   :chat-model "qwen2.5-coder:32b"
                                   :embedding-model "qwen2.5-coder:32b")))))

(with-eval-after-load 'ellama
  (transient-define-prefix my/ellama-transient ()
    "Ellama actions."
    ["General"
     :class transient-row
     ("a" "Chat" ellama-chat)]
    ["Code"
     :class transient-row
     ("ca" "Add" ellama-code-add)
     ("cc" "Complete" ellama-code-complete)
     ("ce" "Edit" ellama-code-edit)
     ("cr" "Review" ellama-code-review)
     ("ci" "Improve" ellama-code-improve)]
    ["Natural Language"
     :class transient-row
     ("np" "Proof-read" my/ellama-proof-read)]
    ["Formatting"
     :class transient-row
     ("ff" "Format" ellama-make-format)
     ("fm" "List" ellama-make-list)
     ("ft" "Table" ellama-make-table)]
    ["Explain & Summarize"
     :class transient-row
     ("es" "Summarize" ellama-summarize)
     ("ea" "Ask about" ellama-ask-about)
     ("es" "Send to chat" ellama-ask-selection)
     ("ew" "Word definition" ellama-define-word)]
    ["Context"
     :class transient-row
     ("xb" "Add buffer" ellama-context-add-buffer)
     ("xf" "Add file" ellama-context-add-file)
     ("xi" "Add info" ellama-context-add-info-node)
     ("xs" "Add selection" ellama-context-add-selection)]
    ["Settings & Sessions"
     :class transient-row
     ("sp" "Provider" ellama-provider-select)
     ("ss" "Session" ellama-session-switch)
     ("sr" "Rename ression" ellama-session-rename)
     ("sd" "Delete session" ellama-session-remove)]))

(defun my/ellama ()
  (interactive)
  (require 'ellama)
  (call-interactively #'my/ellama-transient))

(my-leader-def "aie" #'my/ellama)

(defun my/diff-strings (str1 str2)
  (let ((file1 (make-temp-file "diff1"))
        (file2 (make-temp-file "diff2")))
    (unwind-protect
        (progn
          (with-temp-file file1
            (insert str1))
          (with-temp-file file2
            (insert str2))
          (with-temp-buffer
            (diff-mode)
            (diff-no-select file1 file2 (diff-switches) t (current-buffer))
            (font-lock-fontify-buffer)
            (buffer-string)))
      (delete-file file1)
      (delete-file file2))))

(defun my/ellama-proof-read--display (text is-org-mode prompt)
  (llm-chat-async
   ellama-provider
   (llm-make-chat-prompt
    (format prompt text))
   (lambda (response)
     (let* ((parts (split-string response "-FIXED TEXT ENDS-"))
            (changed-text (nth 0 parts))
            (comments (nth 1 parts))
            (buffer (generate-new-buffer "*ellama-diff*")))
       (when is-org-mode
         (setq changed-text (ellama--translate-markdown-to-org-filter changed-text)))
       (with-current-buffer buffer
         (text-mode)
         (insert
          (propertize "Changed text:\n" 'face 'transient-heading)
          (string-trim changed-text)
          "\n\n"
          (propertize "Comments:\n" 'face 'transient-heading)
          (string-trim comments)
          "\n\n"
          (propertize "Diff:\n" 'face 'transient-heading)
          (my/diff-strings text changed-text)))
       (display-buffer buffer)))
   (lambda (&rest err)
     (message "Error: %s" err))))

(setq my/ellama-proof-read-prompt
      "Proof-read the following text. Follow these rules:
- Fix all grammar errors
- Keep the original style and punctuation, including linebreaks.
- Use British spelling
- Do not replace ' with ’, and do not touch other such symbols

Output the following and nothing else:
- The fixed text
- The string -FIXED TEXT ENDS-
- List of found errors
- List of style suggestions
%s")

(defun my/ellama--text ()
  (if (region-active-p)
	  (buffer-substring-no-properties (region-beginning) (region-end))
	(buffer-substring-no-properties (point-min) (point-max))))

(defun my/ellama-proof-read (text is-org-mode)
  (interactive (list (my/ellama--text) (derived-mode-p 'org-mode)))
  (require 'ellama)
  (my/ellama-proof-read--display text is-org-mode my/ellama-proof-read-prompt))

(defun my/whisper--format-vtt-seconds (seconds)
  (if (numberp seconds)
      (let* ((hours (/ (floor seconds) (* 60 60)))
             (minutes (/ (- (floor seconds) (* hours 60 60)) 60))
             (sec (% (floor seconds) 60))
             (ms (floor (* 1000 (- seconds (floor seconds))))))
        (format "%.2d:%.2d:%.2d.%.3d" hours minutes sec ms))
    ""))

(defun my/whisper--save-chucks-vtt (path data)
  (with-temp-file path
    (insert "WEBVTT\n\n")
    (cl-loop for chunk across (alist-get 'chunks data)
             for start = (my/whisper--format-vtt-seconds
                          (aref (alist-get 'timestamp chunk) 0))
             for end = (my/whisper--format-vtt-seconds
                        (aref (alist-get 'timestamp chunk) 1))
             do (insert (format "%s --> %s" start end) "\n")
             do (insert (string-trim (alist-get 'text chunk)) "\n\n"))))

(defun my/whisper--save-speakers-vtt (path data)
  (with-temp-file path
    (insert "WEBVTT\n\n")
    (cl-loop for chunk across (alist-get 'speakers data)
             for start = (my/whisper--format-vtt-seconds
                          (aref (alist-get 'timestamp chunk) 0))
             for end = (my/whisper--format-vtt-seconds
                        (aref (alist-get 'timestamp chunk) 1))
             do (insert (format "%s --> %s" start end) "\n")
             do (insert
                 (format "<v %s>" (alist-get 'speaker chunk))
                 (string-trim (alist-get 'text chunk)) "\n\n"))))

(defun my/whisper--save-speakers-txt (path data)
  (with-temp-file path
    (cl-loop with prev-speaker
             for chunk across (alist-get 'speakers data)
             for speaker = (alist-get 'speaker chunk)
             if (not (equal speaker prev-speaker))
             do (progn
                  (when prev-speaker
                    (fill-region
                     (line-beginning-position)
                     (line-end-position))
                    (insert "\n\n"))
                  (insert (format "[%s]" speaker) "\n")
                  (setq prev-speaker speaker))
             do (insert (string-trim (alist-get 'text chunk)) " "))
    (fill-region
     (line-beginning-position)
     (line-end-position))))

(defun my/whisper--process-output (transcript-path)
  (let ((data (json-read-file transcript-path)))
    (when (alist-get 'text data)
      (with-temp-file (concat
                       (file-name-sans-extension transcript-path)
                       ".txt")
        (insert (string-trim (alist-get 'text data)))
        (do-auto-fill)))
    (unless (seq-empty-p (alist-get 'speakers data))
      (my/whisper--save-speakers-vtt
       (concat (file-name-sans-extension transcript-path) "-spk.vtt")
       data)
      (my/whisper--save-speakers-txt
       (concat (file-name-sans-extension transcript-path) "-spk.txt")
       data))
    (my/whisper--save-chucks-vtt
     (concat (file-name-sans-extension transcript-path) ".vtt")
     data)))

(defvar my/whisper-path
  "/home/pavel/micromamba/envs/insanely-fast-whisper/bin/insanely-fast-whisper")

(defun my/invoke-whisper (input output-dir &optional language num-speakers)
  (interactive
   (list
    (read-file-name "Input file:" nil nil t)
    (read-directory-name "Output-directory: ")
    (let ((lang (read-string "Language (optional): ")))
      (if (string-empty-p lang) nil lang))
    (let ((num (read-number "Number of speakers (optional): " 0)))
      (when (> num 0)
        (number-to-string num)))))
  (let* ((transcript-path (concat
                           (expand-file-name (file-name-as-directory output-dir))
                           (file-name-base input)
                           ".json"))
         (args
          `("--file-name" ,(expand-file-name input)
            "--transcript-path" ,transcript-path
            "--hf-token" ,(my/password-store-get-field "My_Online/Accounts/huggingface.co" "token")
            ,@(when language
                `("--language" ,language))
            ,@(when num-speakers
                `("--num-speakers" ,num-speakers))))
         (buffer (generate-new-buffer "*whisper*"))
         (proc (apply #'start-process "whisper" buffer my/whisper-path args)))
    (set-process-sentinel
     proc
     (lambda (process _msg)
       (let ((status (process-status process))
             (code (process-exit-status process)))
         (cond ((and (eq status 'exit) (= code 0))
                (my/whisper--process-output transcript-path)
                (notifications-notify :body "Audio conversion completed"
                                      :title "Whisper")
                (kill-buffer (process-buffer process)))
               ((or (and (eq status 'exit) (> code 0))
                    (eq status 'signal))
                (let ((err (with-current-buffer (process-buffer process)
                             (buffer-string))))
                  (user-error "Error in Whisper: %s" err)))))))))

(with-eval-after-load 'elfeed
  (defvar my/elfeed-whisper-podcast-files-directory
    (concat elfeed-db-directory "/podcast-files/")))

(defun my/elfeed-whisper-get-transcript-new (entry)
  (interactive (list elfeed-show-entry))
  (let* ((url (caar (elfeed-entry-enclosures entry)))
         (file-name (concat
                     (elfeed-ref-id (elfeed-entry-content entry))
                     "."
                     (file-name-extension url)))
         (file-path (expand-file-name
                     (concat
                      my/elfeed-whisper-podcast-files-directory
                      file-name))))
    (message "Download started")
    (unless (file-exists-p my/elfeed-whisper-podcast-files-directory)
      (mkdir my/elfeed-whisper-podcast-files-directory))
    (request url
      :type "GET"
      :encoding 'binary
      :complete
      (cl-function
       (lambda (&key data &allow-other-keys)
         (let ((coding-system-for-write 'binary)
               (write-region-annotate-functions nil)
               (write-region-post-annotation-function nil))
           (write-region data nil file-path nil :silent))
         (message "Conversion started")
         (my/invoke-whisper file-path my/elfeed-srt-dir)))
      :error
      (cl-function
       (lambda (&key error-thrown &allow-other-keys)
         (message "Error!: %S" error-thrown))))))

(defun my/elfeed-show-related-files (entry)
  (interactive (list elfeed-show-entry))
  (let* ((files
          (mapcar
           (lambda (file) (cons (file-name-extension file) file))
           (seq-filter
            (lambda (file)
              (string-match-p
               (rx bos (literal (elfeed-ref-id (elfeed-entry-content entry))) ".")
               file))
            (directory-files my/elfeed-srt-dir))))
         (buffer
          (find-file-other-window
           (concat
            my/elfeed-srt-dir
            (alist-get
             (completing-read "File: " files)
             files nil nil #'equal)))))
    (with-current-buffer buffer
      (setq-local elfeed-show-entry entry))))

(defun my/elfeed-whisper-get-transcript (entry)
  "Retrieve transcript for the enclosure of the current elfeed ENTRY."
  (interactive (list elfeed-show-entry))
  (let ((enclosure (caar (elfeed-entry-enclosures entry))))
    (unless enclosure
      (user-error "No enclosure found!"))
    (let ((srt-path (concat my/elfeed-srt-dir
                            (elfeed-ref-id (elfeed-entry-content entry))
                            ".srt")))
      (if (file-exists-p srt-path)
          (let ((buffer (find-file-other-window srt-path)))
            (with-current-buffer buffer
              (setq-local elfeed-show-entry entry)))
        (my/elfeed-whisper-get-transcript-new entry)))))

(defun my/elfeed-whisper-subed (entry)
  "Run MPV for the current Whisper-generated subtitles file.

ENTRY is an instance of `elfeed-entry'."
  (interactive (list elfeed-show-entry))
  (unless entry
    (user-error "No entry!"))
  (unless (derived-mode-p 'subed-mode)
    (user-error "Not subed mode!"))
  (setq-local subed-mpv-video-file
              (expand-file-name
               (concat my/elfeed-whisper-podcast-files-directory
                       (my/get-file-name-from-url
                        (caar (elfeed-entry-enclosures entry))))))
  (subed-mpv--play subed-mpv-video-file))

(defun my/whisper-url (url file-name output-dir &optional language num-speakers)
  (interactive
   (list (read-from-minibuffer "URL: ")
         (read-from-minibuffer "File name: ")
         (read-directory-name "Output directory: ")
         (let ((lang (read-string "Language (optional): ")))
           (if (string-empty-p lang) nil lang))
         (let ((num (read-number "Number of speakers (optional): " 0)))
           (when (> num 0)
             (number-to-string num)))))
  (let ((file-path
         (concat output-dir file-name "." (file-name-extension url))))
    (message "Download started")
    (request url
      :type "GET"
      :encoding 'binary
      :complete
      (cl-function
       (lambda (&key data &allow-other-keys)
         (let ((coding-system-for-write 'binary)
               (write-region-annotate-functions nil)
               (write-region-post-annotation-function nil))
           (write-region data nil file-path nil :silent))
         (message "Conversion started")
         (my/invoke-whisper file-path output-dir language num-speakers)))
      :error
      (cl-function
       (lambda (&key error-thrown &allow-other-keys)
         (message "Error!: %S" error-thrown))))))

(provide 'sqrt-ai)
