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
  ;; I've looked for this option for 1.5 hours
  (setq ellama-long-lines-length 100000)

  (setq ellama-provider (make-llm-openai-compatible :url "localhost:8033")))

(defun my/diff-strings (str1 str2)
  (let ((file1 (make-temp-file "diff1"))
        (file2 (make-temp-file "diff2")))
    (unwind-protect
        (progn
          (with-temp-file file1
            (insert str1))
          (with-temp-file file2
            (insert str2))
          (ansi-color-apply
           (mapconcat
            #'identity
            (process-lines
             "difft" file1 file2 "--color" "always" "--display" "inline")
            "\n")))
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
      "You are a conservative proofreader. Your job is to correct genuine language errors while preserving the author's original wording and style as closely as possible.

The input may be in English or Russian. Detect the language of the input text and produce your entire response in that language.

## Rules for correcting the text

1. Fix all genuine grammar, spelling, punctuation, agreement, inflection, and syntax errors.

2. Preserve the original style.

   * Keep the author's word choice whenever it is grammatically acceptable.
   * Keep abbreviations, contractions, informal expressions, technical terminology, jargon, sentence structure, and level of formality.
   * Do not rewrite sentences merely to make them smoother, clearer, more elegant, concise, idiomatic, or literary.
   * Do not replace an unusual but valid expression with a more common one.

3. For English text, use British spelling and conventions where there is a difference between British and American English.

   * For example: \"colour\", \"organise\", \"centre\".
   * Do not otherwise rewrite vocabulary merely because another word would sound more British.

4. Preserve symbols exactly whenever possible.

   * Do not replace straight quotes with curly quotes or curly quotes with straight quotes.
   * Do not change quotation-mark style.
   * Do not replace hyphens, en dashes, em dashes, apostrophes, ellipses, brackets, slashes, or other typographical symbols with alternative forms.
   * Do not normalise typography.
   * You may move, add, or remove commas when required by grammar or punctuation rules.
   * Other punctuation may be corrected only when it is genuinely erroneous; preserve the original character/style where possible.

5. Preserve formatting, including paragraph breaks, Markdown, lists, and other structural formatting, unless a change is necessary to correct an error.

6. Make the smallest possible correction that fixes each error.

## Errors vs style suggestions

Treat these as two separate categories.

An **error** is something that should actually be corrected: incorrect grammar, spelling, punctuation, agreement, word form, syntax, or an objectively incorrect use of a word or construction.

A **style suggestion** is optional. It may make the text clearer, more idiomatic, less awkward, or easier to read, but the original is not actually wrong.

Do NOT apply style suggestions to the corrected text. The corrected text must contain only corrections of genuine errors.

Be conservative with style suggestions. Do not suggest changing wording merely because you personally prefer an alternative.

## Required output format

First output the complete corrected text, without a heading or introductory sentence.

Immediately after it, output this exact string on its own line:

-FIXED TEXT ENDS-

Then output:

Errors:

* List every genuine error you found and briefly explain the correction.
* Where useful, show the original and corrected fragments.
* If there were no errors, write that no errors were found.

Style suggestions:

* List optional style improvements separately.
* These suggestions must NOT have been applied to the corrected text.
* If there are no worthwhile style suggestions, write that there are none.

The \"Errors:\" and \"Style suggestions:\" headings and all explanations must be in the same language as the input text.

Do not add any other commentary before or after this format.

Here is the text to proofread:

%s")

(defun my/ellama--text ()
  (if (region-active-p)
	  (buffer-substring-no-properties (region-beginning) (region-end))
	(buffer-substring-no-properties (point-min) (point-max))))

(defun my/ellama-proof-read (text is-org-mode)
  (interactive (list (my/ellama--text) (derived-mode-p 'org-mode)))
  (require 'ellama)
  (my/ellama-proof-read--display text is-org-mode my/ellama-proof-read-prompt))

(setq my/ellama-analyze-journal-prompt
      "
You see my personal data for a given period, including some aggregated statistics and journal records. Your job is to summarize it.

## Required output format
Please output the summary with the following sections. Make each section a Markdown heading.

1. Happened to me. A bullet-point list of things happened to me.
2. Happened to the world. A bullet-point list of events happened in the world.
3. Interactions. Mentioned or inferred from statistics interactions, conflicts, tensions, etc.
4. Emotions. A bullet-point list of experienced emotions, moods, etc. and their causes.
5. Observations. A free-form text, whatever your thoughts on the above are.

## Data
%s
")

(defun my/ellama-analyze-journal ()
  (interactive)
  (require 'ellama)
  (let ((text (my/ellama--text)))
    (llm-chat-async
     ellama-provider
     (llm-make-chat-prompt
      (format my/ellama-analyze-journal-prompt text))
     (lambda (response)
       (let ((buf (generate-new-buffer "*ellama-response*")))
         (with-current-buffer buf
           (insert (ellama--translate-markdown-to-org-filter response))
           (org-mode))
         (display-buffer buf)))
     (lambda (&rest err)
       (message "Error: %s" err)))))

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
