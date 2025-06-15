;;; ai-enhanced-coding.el --- AI coding assistant with thinking mode -*- lexical-binding: t; -*-

;;; Code:

(require 'json)
(require 'url)
(require 'utils)

;; Configuration Variables
(defgroup ai-coding nil
  "AI enhanced coding with local Ollama."
  :group 'programming)

(defcustom ai-coding-ollama-host "localhost:11434"
  "Ollama server host and port."
  :type 'string
  :group 'ai-coding)

(defcustom ai-coding-model "deepseek-r1:8b-0528-qwen3-q4_K_M"
  "Ollama model to use for code assistance."
  :type 'string
  :group 'ai-coding)

(defcustom ai-coding-temperature 0.7
  "Temperature for AI responses (0.0 to 1.0)."
  :type 'float
  :group 'ai-coding)

;; Thinking Mode Configuration
(defcustom ai-coding-thinking-mode nil
  "Enable step-by-step reasoning mode for AI responses."
  :type 'boolean
  :group 'ai-coding)

(defcustom ai-coding-thinking-temperature 0.6
  "Temperature for thinking mode responses."
  :type 'float
  :group 'ai-coding)

;; Custom Instructions System
(defcustom ai-coding-base-instructions
  "You are a Senior Software Engineer assistant. Follow these guidelines:
1. You're a Senior Software Engineer - respond with appropriate technical depth
2. When you have doubts, ask the user for clarification
3. Always be highly technical in every response
4. Be objective and clear in your explanations
5. Comment every function or piece of code you modify with explanation of why
6. If you alter something, don't break the initial logic or functionality
7. Dont use emojis"
  "Base instructions that define the AI's personality and behavior."
  :type 'string
  :group 'ai-coding)

(defcustom ai-coding-context-instructions
  '((explain . "When explaining code, provide detailed technical analysis including purpose, key algorithms, potential issues, and suggestions for improvement.")
    (refactor . "When refactoring, preserve all original functionality while improving code quality. Explain each change and why it's beneficial.")
    (debug . "When debugging, systematically analyze the problem, identify root causes, and provide step-by-step solutions.")
    (chat . "Engage in technical discussion while maintaining your senior engineer perspective."))
  "Context-specific instructions for different AI functions."
  :type '(alist :key-type symbol :value-type string)
  :group 'ai-coding)

;; Session management variables
(defvar ai-coding/conversations (make-hash-table :test 'equal))
(defvar ai-coding/current-session-id nil)
(defvar ai-coding/chat-buffer-name "*AI Chat*")

;; Thinking Mode Management
;;;###autoload
(defun ai-coding/toggle-thinking-mode ()
  "Toggle between thinking mode and quick response mode."
  (interactive)
  (setq ai-coding-thinking-mode (not ai-coding-thinking-mode))
  (message "AI thinking mode %s"
           (if ai-coding-thinking-mode
               (propertize "ENABLED" 'face 'success)
             (propertize "DISABLED" 'face 'warning)))
  (force-mode-line-update))

(defun ai-coding/get-current-mode-indicator ()
  "Return string indicating current AI mode for display."
  (if ai-coding-thinking-mode "[THINKING]" "[QUICK]"))

;; Instruction Management System
(defun ai-coding/build-system-prompt (&optional context)
  "Build complete system prompt including base instructions and context."
  (let ((base ai-coding-base-instructions)
        (context-instruction (when context
                               (alist-get context ai-coding-context-instructions))))
    (concat base
            (when context-instruction
              (concat "\n\nFor this specific task: " context-instruction)))))

;;;###autoload
(defun ai-coding/edit-instructions ()
  "Interactively edit the base AI instructions."
  (interactive)
  (let ((buffer (utils/create-display-buffer "*AI Instructions Editor*")))
    (with-current-buffer buffer
      (insert ai-coding-base-instructions)
      (text-mode)
      (goto-char (point-min))
      (local-set-key (kbd "C-c C-c")
                     (lambda ()
                       (interactive)
                       (setq ai-coding-base-instructions
                             (buffer-substring-no-properties (point-min) (point-max)))
                       (message "AI instructions updated")
                       (kill-buffer)))
      (local-set-key (kbd "C-c C-k")
                     (lambda ()
                       (interactive)
                       (message "Instructions edit cancelled")
                       (kill-buffer)))
      (message "Edit instructions. C-c C-c to save, C-c C-k to cancel"))
    (display-buffer buffer)))

;; Use utility function for text cleaning
(defalias 'ai-coding/clean-control-characters 'utils/clean-ai-text)

;; Core functions
(defun ai-coding/check-ollama ()
  "Check if Ollama is running."
  (utils/with-error-handling "Failed to check Ollama status"
    (let ((url-request-method "GET"))
      (with-current-buffer (url-retrieve-synchronously
                           (format "http://%s/api/tags" ai-coding-ollama-host)
                           nil nil 5)
        (goto-char (point-min))
        (re-search-forward "^$" nil t)
        (> (- (point-max) (point)) 5)))))

(defun ai-coding/make-request (messages callback &optional context)
  "Make enhanced request with instructions and thinking mode integration."
  (let* ((system-prompt (ai-coding/build-system-prompt context))
         (enhanced-messages (vconcat `[((role . "system") (content . ,system-prompt))]
                                     messages))
         (temperature (if ai-coding-thinking-mode
                          ai-coding-thinking-temperature
                        ai-coding-temperature))
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (payload `((model . ,ai-coding-model)
                   (messages . ,enhanced-messages)
                   (stream . :json-false)
                   (temperature . ,temperature)
                   (think . ,(if ai-coding-thinking-mode t :json-false))))
         (url-request-data (json-encode payload)))

    (utils/run-command-async
     "curl"
     (lambda (response)
       (utils/with-error-handling "Failed to process AI response"
         (let* ((json-data (ignore-errors (json-read-from-string response)))
                (message-data (alist-get 'message json-data))
                (content (alist-get 'content message-data))
                (thinking (alist-get 'thinking message-data)))
           (when content
             (let ((cleaned-content (utils/clean-ai-text content))
                   (cleaned-thinking (when thinking
                                      (utils/clean-ai-text thinking))))
               (funcall callback cleaned-content cleaned-thinking))))))
     "-X" "POST"
     (format "http://%s/api/chat" ai-coding-ollama-host)
     "-H" "Content-Type: application/json"
     "-d" (json-encode payload))))

;; Session management
(defun ai-coding/get-session-id ()
  "Get current session ID."
  (or ai-coding/current-session-id
      (setq ai-coding/current-session-id
            (format "session-%d" (truncate (float-time))))))

(defun ai-coding/add-to-conversation (session-id role content)
  "Add message to conversation."
  (let* ((history (gethash session-id ai-coding/conversations '()))
         (new-message `((role . ,role) (content . ,content)))
         (updated (append history (list new-message))))
    (when (> (length updated) 20)
      (setq updated (last updated 20)))
    (puthash session-id updated ai-coding/conversations)))

;; Core AI functions using utilities
;;;###autoload
(defun ai-coding/explain-code ()
  "Explain selected code using context-specific instructions."
  (interactive)
  (unless (ai-coding/check-ollama)
    (user-error "Ollama not running"))
  (let ((code (if (use-region-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (thing-at-point 'line t))))
    (message "Analyzing code with %s mode..." (ai-coding/get-current-mode-indicator))
    (ai-coding/make-request
     `[((role . "user") (content . ,(format "Explain this code:\n\n```\n%s\n```" code)))]
     (lambda (response thinking)
       (utils/ai-display-response "*AI Code Explanation*"
                                  (ai-coding/get-current-mode-indicator)
                                  code response thinking))
     'explain)))

;;;###autoload
(defun ai-coding/refactor-code ()
  "Get refactoring suggestions using context-specific instructions."
  (interactive)
  (unless (ai-coding/check-ollama)
    (user-error "Ollama not running"))
  (let ((code (if (use-region-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (buffer-string))))
    (message "Getting refactoring suggestions with %s mode..." (ai-coding/get-current-mode-indicator))
    (ai-coding/make-request
     `[((role . "user") (content . ,(format "Suggest refactoring for:\n\n```\n%s\n```" code)))]
     (lambda (response thinking)
       (utils/ai-display-response "*AI Refactor Suggestions*"
                                  (ai-coding/get-current-mode-indicator)
                                  nil response thinking))
     'refactor)))

;; Chat Interface using utilities
;;;###autoload
(defun ai-coding/open-chat ()
  "Open AI chat interface with mode indicator."
  (interactive)
  (unless (ai-coding/check-ollama)
    (user-error "Ollama is not running. Start it with: ollama serve"))

  (let ((buffer (utils/create-display-buffer ai-coding/chat-buffer-name)))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (unless (eq major-mode 'ai-chat-mode)
        (ai-chat-mode))
      (when (= (buffer-size) 0)
        (insert "=== AI Chat ===\n")
        (insert "Mode: " (ai-coding/get-current-mode-indicator)
                " | Model: " ai-coding-model "\n")
        (insert "Type your message and press Enter to send.\n")
        (insert "Use C-c C-n for new session, C-c C-t to toggle thinking mode.\n\n"))
      (goto-char (point-max))
      (insert "\nYou: ")
      (setq-local ai-chat-prompt-start (point)))))

(defvar-local ai-chat-prompt-start nil)

(defun ai-chat-send ()
  "Send current message with context-aware processing."
  (interactive)
  (when ai-chat-prompt-start
    (let* ((message (buffer-substring-no-properties ai-chat-prompt-start (point-max)))
           (session-id (ai-coding/get-session-id)))
      (when (string-match "\\S-" message)
        (goto-char (point-max))
        (insert "\n\n" (ai-coding/get-current-mode-indicator) " AI: [thinking...]\n")
        (let ((thinking-start (save-excursion
                                (forward-line -1)
                                (point))))
          (ai-coding/add-to-conversation session-id "user" message)
          (ai-coding/make-request
           (vconcat (gethash session-id ai-coding/conversations '()))
           (lambda (response thinking)
             (with-current-buffer (get-buffer ai-coding/chat-buffer-name)
               (goto-char thinking-start)
               (delete-region thinking-start (point-max))
               (insert (ai-coding/get-current-mode-indicator) " AI: ")
               (when (and thinking ai-coding-thinking-mode)
                 (insert "\n--- Thinking ---\n" thinking "\n--- Response ---\n"))
               (insert response "\n\nYou: ")
               (ai-coding/add-to-conversation session-id "assistant" response)
               (setq ai-chat-prompt-start (point))
               (goto-char (point-max))))
           'chat))))))

(define-derived-mode ai-chat-mode fundamental-mode "AI-Chat"
  "Enhanced AI chat mode with thinking mode support."
  (visual-line-mode 1)
  (local-set-key (kbd "RET") 'ai-chat-send)
  (local-set-key (kbd "C-c C-c") 'ai-chat-send)
  (local-set-key (kbd "C-c C-n") 'ai-coding/new-chat-session)
  (local-set-key (kbd "C-c C-t") 'ai-coding/toggle-thinking-mode))

;; Utility functions
;;;###autoload
(defun ai-coding/add-code-to-chat ()
  "Add code to chat."
  (interactive)
  (let* ((code (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (buffer-string)))
         (filename (or (buffer-file-name) (buffer-name))))
    (ai-coding/open-chat)
    (insert (format "Here's code from %s:\n\n```\n%s\n```\n\nPlease analyze this code."
                   filename code))
    (ai-chat-send)))

;;;###autoload
(defun ai-coding/new-chat-session ()
  "Start new chat session."
  (interactive)
  (setq ai-coding/current-session-id nil)
  (when-let ((buffer (get-buffer ai-coding/chat-buffer-name)))
    (with-current-buffer buffer
      (erase-buffer)))
  (ai-coding/open-chat)
  (message "New chat session started"))

;;;###autoload
(defun ai-coding/show-chat-history ()
  "Show chat history."
  (interactive)
  (let* ((session-id (ai-coding/get-session-id))
         (history (gethash session-id ai-coding/conversations '())))
    (message "Current session has %d messages" (length history))))

;;;###autoload
(defun ai-coding/debug-status ()
  "Show debug status including thinking mode."
  (interactive)
  (let* ((session-id (ai-coding/get-session-id))
         (history (gethash session-id ai-coding/conversations '()))
         (ollama-status (if (ai-coding/check-ollama) "Running" "Not running")))
    (message "Session: %s | Messages: %d | Ollama: %s | Mode: %s"
             session-id (length history) ollama-status
             (ai-coding/get-current-mode-indicator))))

;; Performance monitoring
(defun ai-coding/performance-report ()
  "Show AI coding performance metrics."
  (interactive)
  (utils/with-performance-timing "AI coding performance check"
    (let* ((session-count (hash-table-count ai-coding/conversations))
           (total-messages (apply #'+ (mapcar #'length
                                             (hash-table-values ai-coding/conversations))))
           (ollama-status (ai-coding/check-ollama)))
      (message "Sessions: %d, Total messages: %d, Ollama: %s, Mode: %s"
               session-count total-messages
               (if ollama-status "Online" "Offline")
               (ai-coding/get-current-mode-indicator)))))

(provide 'ai-enhanced-coding)
;;; ai-enhanced-coding.el ends here
