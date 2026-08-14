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
        (gptel-make-openai "llama-cpp"
          :stream t
          :protocol "http"
          :host "localhost:8033"
          :models '(test)))
  (gptel-make-openai "OpenRouter"
    :host "openrouter.ai/api"
    :key (lambda () (my/password-store-get-field
                     "Accounts/openrouter" "api-key-kek"))
    :stream t
    :models '("anthropic/claude-sonnet-4.6"
              "deepseek/deepseek-v3.2"
              "openai/gpt-5.4"
              "x-ai/grok-4.1-fast"))
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

(defun my/gptel-add-current-file ()
  (interactive)
  (gptel-context-add-file (buffer-file-name)))

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

(defvar my/whisper-path
  "/home/pavel/10-19 Code/13 Other Projects/13.15 whisper-cli/.venv/bin/whisper-cli")

(defun my/whisper--sentinel (process event)
  (when (memq (process-status process) '(exit signal))
    (let ((buffer (process-buffer process)))
      (if (and (eq (process-status process) 'exit)
               (= (process-exit-status process) 0))
          (progn
            (notifications-notify :body "Audio conversion completed"
                                  :title "Whisper")
            (message "Whisper conversion completed")
            (when (buffer-live-p buffer)
              (kill-buffer buffer)))
        (notifications-notify
         :body (format "Conversion failed: %s" (string-trim event))
         :title "Whisper")
        (when (buffer-live-p buffer)
          (display-buffer buffer))
        (message "Whisper failed: %s" (string-trim event))))))

(defun my/whisper--read-diarization ()
  "Prompt for optional diarization and return nil, `auto', or a speaker count."
  (when (y-or-n-p "Enable speaker diarization? ")
    (let ((num (read-number "Number of speakers (0 for automatic): " 0)))
      (if (> num 0) num 'auto))))

(defun my/whisper--start (source-args output-dir &optional language num-speakers)
  "Start Whisper for SOURCE-ARGS.
NUM-SPEAKERS is nil to disable diarization, `auto' to infer the count, or a number."
  (let* ((args (append
                source-args
                (list "--output" (expand-file-name output-dir))
                (when language
                  (list "--language" language))
                (when num-speakers
                  (append
                   (list "--diarize")
                   (when (numberp num-speakers)
                     (list "--num-speakers" (format "%s" num-speakers)))))))
         (buffer (generate-new-buffer "*whisper*"))
         (process-environment (copy-sequence process-environment)))
    (dolist (variable '("http_proxy" "https_proxy" "HTTP_PROXY"
                        "HTTPS_PROXY" "all_proxy" "ALL_PROXY"))
      (setenv variable nil))
    (when num-speakers
      (setenv "HF_TOKEN"
              (my/password-store-get-field
               "Accounts/huggingface.co" "token")))
    (let ((process
           (make-process
            :name "whisper"
            :buffer buffer
            :command (cons my/whisper-path args)
            :connection-type 'pipe
            :noquery t
            :sentinel #'my/whisper--sentinel)))
      (display-buffer buffer)
      process)))

(defun my/invoke-whisper (input output-dir &optional language num-speakers)
  (interactive
   (list
    (read-file-name "Input file:" nil nil t)
    (read-directory-name "Output-directory: ")
    (let ((lang (read-string "Language (optional): ")))
      (if (string-empty-p lang) nil lang))
    (my/whisper--read-diarization)))
  (unless (file-readable-p input)
    (user-error "Input file is not readable: %s" input))
  (my/whisper--start
   (list "--file" (expand-file-name input))
   output-dir language num-speakers))

(defun my/whisper-url (url file-name output-dir &optional language num-speakers)
  (interactive
   (list (read-from-minibuffer "URL: ")
         (read-from-minibuffer "File name: ")
         (read-directory-name "Output directory: ")
         (let ((lang (read-string "Language (optional): ")))
           (if (string-empty-p lang) nil lang))
         (my/whisper--read-diarization)))
  (my/whisper--start
   (list "--url" url "--name" file-name)
   output-dir language num-speakers))

(use-package agent-shell
  :straight t
  :init
  (my-leader-def "ais" #'agent-shell)
  (my/persp-add-rule
    agent-shell-mode nil "agent")
  :commands (agent-shell)
  :config
  (setq agent-shell-openai-codex-acp-command
        (list "/home/pavel/.local/bin/ai-proxy"
              "run"
              "/home/pavel/micromamba/envs/general/bin/codex-acp"))
  (setq agent-shell-openai-authentication
        (agent-shell-openai-make-authentication :login t))
  (setq agent-shell-session-restore-verbosity 'full))

(defun my/agent-shell-ret ()
  (interactive)
  (if (get-text-property (point) 'agent-shell-ui-state)
      (agent-shell-ui-toggle-fragment)
    (agent-shell-submit)))

(with-eval-after-load 'agent-shell
  (general-define-key
   :states '(insert normal)
   :keymaps 'agent-shell-mode-map
   "RET"        #'my/agent-shell-ret
   "<return>"   #'my/agent-shell-ret)

  (general-define-key
   :states '(insert)
   :keymaps 'agent-shell-mode-map
   "C-<return>" #'newline))

(defun my/agent-shell-trigger-completion ()
  (when (and (memq (char-before) '(?@ ?/))
             (or (= (point) (1+ (line-beginning-position)))
                 (memq (char-before (1- (point))) '(?\s ?\t ?\n))))
    (company-begin-backend 'company-capf)))

(defun my/agent-shell-company-completion ()
  (remove-hook 'post-self-insert-hook
               #'agent-shell--trigger-completion-at-point
               t)
  (add-hook 'post-self-insert-hook
            #'my/agent-shell-trigger-completion
            nil t))

(with-eval-after-load 'agent-shell
  (add-hook 'agent-shell-completion-mode-hook
            #'my/agent-shell-company-completion))

(provide 'sqrt-ai)
