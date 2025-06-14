;;; ai-enhanced-coding.el --- Practical AI coding assistant -*- lexical-binding: t; -*-

;;; Commentary:
;; Practical AI coding assistant with in-buffer chat interface.

;;; Code:

(require 'json)
(require 'url)

;; Configuration
(defgroup ai-coding nil
  "AI enhanced coding with local Ollama."
  :group 'programming)

(defcustom ai-coding-ollama-host "localhost:11434"
  "Ollama server host and port."
  :type 'string
  :group 'ai-coding)

(defcustom ai-coding-model "deepseek-coder:latest"
  "Ollama model to use for code assistance."
  :type 'string
  :group 'ai-coding)

(defcustom ai-coding-temperature 0.7
  "Temperature for AI responses (0.0 to 1.0)."
  :type 'float
  :group 'ai-coding)

;; Chat conversation management
(defvar ai-coding/conversations (make-hash-table :test 'equal)
  "Hash table storing conversation histories by session ID.")

(defvar ai-coding/current-session-id nil
  "Current active conversation session ID.")

(defvar ai-coding/chat-buffer-name "*AI Chat*"
  "Name of the AI chat buffer.")

(defvar ai-coding/max-conversation-length 20
  "Maximum number of messages to keep in conversation history.")

;; Buffer management variables
(defvar-local ai-coding/input-start-point nil
  "Point where input area starts.")

(defvar-local ai-coding/chat-end-point nil
  "Point where chat area ends.")

;; Session management
(defun ai-coding/get-session-id ()
  "Get or create a session ID for the current context."
  (or ai-coding/current-session-id
      (setq ai-coding/current-session-id
            (format "session-%d" (truncate (float-time))))))

(defun ai-coding/get-conversation-history (session-id)
  "Get conversation history for SESSION-ID."
  (gethash session-id ai-coding/conversations '()))

(defun ai-coding/add-to-conversation (session-id role content)
  "Add a message to the conversation history."
  (let* ((history (ai-coding/get-conversation-history session-id))
         (new-message `((role . ,role) (content . ,content)))
         (updated-history (append history (list new-message))))
    ;; Keep only recent messages
    (when (> (length updated-history) ai-coding/max-conversation-length)
      (setq updated-history (last updated-history ai-coding/max-conversation-length)))
    (puthash session-id updated-history ai-coding/conversations)))

(defun ai-coding/clear-conversation (session-id)
  "Clear conversation history for SESSION-ID."
  (remhash session-id ai-coding/conversations)
  (message "Conversation history cleared"))

;; Check if Ollama is running
(defun ai-coding/check-ollama ()
  "Check if Ollama is running and accessible."
  (condition-case nil
      (let ((url-request-method "GET")
            (url-request-extra-headers '(("Content-Type" . "application/json"))))
        (with-current-buffer (url-retrieve-synchronously
                             (format "http://%s/api/tags" ai-coding-ollama-host)
                             nil nil 5)
          (goto-char (point-min))
          (re-search-forward "^$" nil t)
          (let ((response (buffer-substring-no-properties (point) (point-max))))
            (and (> (length response) 0)
                 (json-read-from-string response)))))
    (error nil)))

;; Make API request to Ollama
(defun ai-coding/make-ollama-request (messages callback)
  "Make a request to Ollama with MESSAGES and call CALLBACK with response."
  (unless (ai-coding/check-ollama)
    (error "Ollama is not running. Please start it with: ollama serve"))

  (let* ((url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (payload `((model . ,ai-coding-model)
                   (messages . ,(vconcat messages))
                   (stream . :json-false)
                   (options . ((temperature . ,ai-coding-temperature)))))
         (url-request-data (json-encode payload)))

    (message "Requesting response from AI...")

    (url-retrieve (format "http://%s/api/chat" ai-coding-ollama-host)
                  (lambda (status)
                    (if (plist-get status :error)
                        (message "Error: %s" (plist-get status :error))
                      (goto-char (point-min))
                      (re-search-forward "^$" nil t)
                      (let* ((response-text (buffer-substring-no-properties (point) (point-max)))
                             (response-json (condition-case err
                                              (json-read-from-string response-text)
                                              (error
                                               (message "JSON parse error: %s" err)
                                               nil))))
                        (if response-json
                            (let ((content (alist-get 'content
                                                     (alist-get 'message response-json))))
                              (if content
                                  (funcall callback content)
                                (message "No content in response")))
                          (message "Failed to parse response"))))))))

;; Buffer setup and management
(defun ai-coding/setup-chat-buffer ()
  "Set up the AI chat buffer with proper sections."
  (let ((buffer (get-buffer-create ai-coding/chat-buffer-name)))
    (with-current-buffer buffer
      (when (= (buffer-size) 0)
        (ai-coding/initialize-buffer))

      ;; Ensure we're in the right mode
      (unless (eq major-mode 'ai-coding-chat-mode)
        (ai-coding-chat-mode))

      ;; Position cursor in input area
      (when ai-coding/input-start-point
        (goto-char ai-coding/input-start-point)))
    buffer))

(defun ai-coding/initialize-buffer ()
  "Initialize the chat buffer with header, chat area, and input area."
  (erase-buffer)

  ;; Header section
  (insert "┌────────────────────────────────────────────────────────────┐\n")
  (insert "│                    AI Coding Assistant                    │\n")
  (insert "├────────────────────────────────────────────────────────────┤\n")
  (insert (format "│ Model: %-50s │\n" ai-coding-model))
  (insert (format "│ Session: %-48s │\n" (ai-coding/get-session-id)))
  (insert (format "│ Started: %-48s │\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
  (insert "└────────────────────────────────────────────────────────────┘\n\n")

  ;; Instructions
  (insert "Instructions:\n")
  (insert "• Type your message in the input box below and press Enter\n")
  (insert "• Use C-c C-c to send message\n")
  (insert "• Use C-c C-n for new session\n")
  (insert "• Select code in other buffers and use 'SPC a S' to add context\n\n")
  (insert (make-string 80 ?═) "\n\n")

  ;; Chat area marker
  (setq ai-coding/chat-end-point (point-marker))

  ;; Input section
  (insert "\n" (make-string 80 ?─) "\n")
  (insert "Input (Type your message here and press Enter):\n")
  (insert (make-string 80 ?─) "\n")

  ;; Input area
  (setq ai-coding/input-start-point (point-marker))
  (insert "\n\n")
  (insert (make-string 80 ?─) "\n")
  (insert "Press Enter to send • C-c C-c to send • C-c C-n for new session\n")

  ;; Make everything read-only except input area
  (ai-coding/setup-text-properties))

(defun ai-coding/setup-text-properties ()
  "Set up text properties to make chat area read-only but input area writable."
  ;; Make everything read-only first
  (put-text-property (point-min) (point-max) 'read-only t)
  (put-text-property (point-min) (point-max) 'front-sticky '(read-only))
  (put-text-property (point-min) (point-max) 'rear-nonsticky '(read-only))

  ;; Make input area writable
  (when (and ai-coding/input-start-point (marker-position ai-coding/input-start-point))
    (let ((input-end (save-excursion
                       (goto-char ai-coding/input-start-point)
                       (forward-line 2)
                       (point))))
      (remove-text-properties ai-coding/input-start-point input-end '(read-only nil))
      (put-text-property ai-coding/input-start-point input-end 'field 'input)
      (put-text-property ai-coding/input-start-point input-end 'front-sticky nil)
      (put-text-property ai-coding/input-start-point input-end 'rear-nonsticky nil))))

(defun ai-coding/add-message-to-chat (role content)
  "Add a message to the chat area."
  (when (and ai-coding/chat-end-point (marker-position ai-coding/chat-end-point))
    (save-excursion
      (goto-char ai-coding/chat-end-point)

      ;; Temporarily remove read-only to insert message
      (let ((inhibit-read-only t))
        (let ((timestamp (format-time-string "%H:%M:%S"))
              (role-display (cond
                             ((string= role "user") "User")
                             ((string= role "assistant") "AI")
                             (t role))))
          (insert (format "[%s] %s:\n" timestamp role-display))
          (insert content)
          (insert "\n")
          (insert (make-string 60 ?─))
          (insert "\n\n"))

        ;; Update chat end marker
        (set-marker ai-coding/chat-end-point (point))

        ;; Reapply text properties
        (ai-coding/setup-text-properties)))

    ;; Auto-scroll if buffer is visible
    (when-let ((window (get-buffer-window (current-buffer))))
      (with-selected-window window
        (goto-char ai-coding/input-start-point)
        (recenter -8)))))

(defun ai-coding/get-input-text ()
  "Get the text from the input area."
  (when ai-coding/input-start-point
    (save-excursion
      (goto-char ai-coding/input-start-point)
      (let ((start (point))
            (end (save-excursion
                   (forward-line 2)
                   (point))))
        (string-trim (buffer-substring-no-properties start end))))))

(defun ai-coding/clear-input ()
  "Clear the input area."
  (when ai-coding/input-start-point
    (save-excursion
      (goto-char ai-coding/input-start-point)
      (let ((start (point))
            (end (save-excursion
                   (forward-line 2)
                   (point)))
            (inhibit-read-only t))
        (delete-region start end)
        (insert "\n\n")))))

(defun ai-coding/send-message ()
  "Send the message from the input area."
  (interactive)
  (let ((message (ai-coding/get-input-text)))
    (when (and message (not (string-empty-p message)))
      (let ((session-id (ai-coding/get-session-id)))
        ;; Add user message to chat
        (ai-coding/add-message-to-chat "user" message)

        ;; Clear input
        (ai-coding/clear-input)

        ;; Show AI thinking
        (ai-coding/add-message-to-chat "assistant" "Processing your request...")

        ;; Position cursor back in input area
        (goto-char ai-coding/input-start-point)

        ;; Make the request
        (ai-coding/make-chat-request session-id message
                                    (lambda (response)
                                      ;; Remove thinking message and add real response
                                      (ai-coding/remove-last-message)
                                      (ai-coding/add-message-to-chat "assistant" response)
                                      ;; Position cursor back in input area
                                      (goto-char ai-coding/input-start-point)))))))

(defun ai-coding/remove-last-message ()
  "Remove the last message from chat (for removing thinking indicator)."
  (when ai-coding/chat-end-point
    (save-excursion
      (goto-char ai-coding/chat-end-point)
      (let ((inhibit-read-only t))
        (when (re-search-backward "^\\[.*\\] AI:$" nil t)
          (let ((start (line-beginning-position)))
            (goto-char ai-coding/chat-end-point)
            (delete-region start (point))
            (set-marker ai-coding/chat-end-point start)))))))

;; Main chat function with conversation context
(defun ai-coding/make-chat-request (session-id user-message callback)
  "Make an AI request with full conversation context."
  ;; Add user message to conversation history
  (ai-coding/add-to-conversation session-id "user" user-message)

  ;; Get full conversation history
  (let* ((history (ai-coding/get-conversation-history session-id))
         (messages history))

    (message "Sending request (%d messages in history)..." (length history))

    (ai-coding/make-ollama-request messages
                                  (lambda (response)
                                    ;; Add AI response to conversation history
                                    (ai-coding/add-to-conversation session-id "assistant" response)
                                    (message "Response received")
                                    (funcall callback response)))))

;; Interactive commands
(defun ai-coding/open-chat ()
  "Open the interactive AI chat buffer."
  (interactive)
  (unless (ai-coding/check-ollama)
    (error "Ollama is not running. Please start it with: ollama serve"))

  (let ((chat-buffer (ai-coding/setup-chat-buffer)))
    (pop-to-buffer chat-buffer)
    (message "AI Chat ready. Type in the input area and press Enter.")))

(defun ai-coding/add-code-to-chat ()
  "Add selected code or current buffer to the chat as context."
  (interactive)
  (unless (ai-coding/check-ollama)
    (error "Ollama is not running. Please start it with: ollama serve"))

  (let* ((code (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (buffer-string)))
         (filename (or (buffer-file-name) (buffer-name)))
         (chat-buffer (ai-coding/setup-chat-buffer))
         (session-id (ai-coding/get-session-id)))

    ;; Switch to chat buffer
    (pop-to-buffer chat-buffer)

    ;; Add code context message
    (ai-coding/add-message-to-chat "user"
                                  (format "Code from %s:\n\n```%s\n%s\n```"
                                         filename
                                         (or (file-name-extension filename) "")
                                         code))

    ;; Add to conversation history
    (ai-coding/add-to-conversation session-id "user"
                                  (format "Analyze this code from %s:\n\n```%s\n%s\n```"
                                         filename
                                         (or (file-name-extension filename) "")
                                         code))

    ;; Show processing and make request
    (ai-coding/add-message-to-chat "assistant" "Analyzing your code...")
    (goto-char ai-coding/input-start-point)

    (ai-coding/make-chat-request
     session-id (format "Analyze this code from %s:\n\n```%s\n%s\n```"
                       filename
                       (or (file-name-extension filename) "")
                       code)
     (lambda (response)
       (ai-coding/remove-last-message)
       (ai-coding/add-message-to-chat "assistant" response)
       (goto-char ai-coding/input-start-point)
       (message "Code analysis complete. Ask follow-up questions in the input area.")))))

(defun ai-coding/new-chat-session ()
  "Start a new chat session."
  (interactive)
  (when ai-coding/current-session-id
    (ai-coding/clear-conversation ai-coding/current-session-id))
  (setq ai-coding/current-session-id nil)

  ;; Clear chat buffer and reinitialize
  (let ((chat-buffer (get-buffer ai-coding/chat-buffer-name)))
    (when chat-buffer
      (with-current-buffer chat-buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (ai-coding/initialize-buffer)))))

  (message "New chat session started")
  (ai-coding/open-chat))

(defun ai-coding/show-chat-history ()
  "Show conversation history for current session."
  (interactive)
  (let* ((session-id (ai-coding/get-session-id))
         (history (ai-coding/get-conversation-history session-id)))
    (if (null history)
        (message "No conversation history for current session")
      (message "Current session has %d messages" (length history)))))

;; Simple coding functions for separate buffers
(defun ai-coding/explain-code ()
  "Explain selected code in a separate buffer."
  (interactive)
  (unless (ai-coding/check-ollama)
    (error "Ollama is not running. Please start it with: ollama serve"))

  (let ((code (if (use-region-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (thing-at-point 'line t))))

    (message "AI is analyzing the code...")
    (ai-coding/make-ollama-request
     `[((role . "user") (content . ,(format "Explain this code in detail:\n\n```\n%s\n```" code)))]
     (lambda (response)
       (with-current-buffer (get-buffer-create "*AI Code Explanation*")
         (let ((inhibit-read-only t))
           (erase-buffer)
           (insert "AI Code Explanation\n")
           (insert (make-string 50 ?=) "\n\n")
           (insert "Original Code:\n")
           (insert "```\n" code "\n```\n\n")
           (insert "Explanation:\n")
           (insert response))
         (when (fboundp 'markdown-mode) (markdown-mode))
         (view-mode)
         (goto-char (point-min))
         (display-buffer (current-buffer)))))))

(defun ai-coding/refactor-code ()
  "Get refactoring suggestions for selected code in a separate buffer."
  (interactive)
  (unless (ai-coding/check-ollama)
    (error "Ollama is not running. Please start it with: ollama serve"))

  (let ((code (if (use-region-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (buffer-string))))

    (message "AI is analyzing code for refactoring opportunities...")
    (ai-coding/make-ollama-request
     `[((role . "user") (content . ,(format "Suggest refactoring improvements for this code:\n\n```\n%s\n```" code)))]
     (lambda (response)
       (with-current-buffer (get-buffer-create "*AI Refactor Suggestions*")
         (let ((inhibit-read-only t))
           (erase-buffer)
           (insert "AI Refactoring Suggestions\n")
           (insert (make-string 50 ?=) "\n\n")
           (insert response))
         (when (fboundp 'markdown-mode) (markdown-mode))
         (view-mode)
         (goto-char (point-min))
         (display-buffer (current-buffer)))))))

;; Debug function
(defun ai-coding/debug-status ()
  "Show debug information about the AI coding system."
  (interactive)
  (let* ((session-id (ai-coding/get-session-id))
         (history (ai-coding/get-conversation-history session-id))
         (ollama-status (if (ai-coding/check-ollama) "Running" "Not running")))
    (message "AI Coding Status - Session: %s | Messages: %d | Ollama: %s"
             session-id (length history) ollama-status)))

;; Chat mode definition
(define-derived-mode ai-coding-chat-mode fundamental-mode "AI-Chat"
  "Major mode for AI chat sessions."
  (setq-local scroll-conservatively 101)
  (setq-local auto-window-vscroll nil)
  (visual-line-mode 1)

  ;; Local keybindings
  (local-set-key (kbd "RET") 'ai-coding/send-message)
  (local-set-key (kbd "C-c C-c") 'ai-coding/send-message)
  (local-set-key (kbd "C-c C-n") 'ai-coding/new-chat-session)
  (local-set-key (kbd "C-c C-h") 'ai-coding/show-chat-history)
  (local-set-key (kbd "q") 'quit-window))

;; Chat history for minibuffer fallback
(defvar ai-coding-chat-history nil
  "History list for AI chat input.")

(provide 'ai-enhanced-coding)
;;; ai-enhanced-coding.el ends here
