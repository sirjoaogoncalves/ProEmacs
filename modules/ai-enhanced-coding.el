;;; ai-enhanced-coding.el --- AI coding assistant with thinking mode and custom instructions -*- lexical-binding: t; -*-

;;; Commentary:
;; Enhanced AI coding assistant with local Ollama support, featuring:
;; - Thinking mode toggle for step-by-step reasoning vs quick responses
;; - Customizable instruction system for consistent AI behavior
;; - Backward compatibility with existing functionality

;;; Code:

(require 'json)
(require 'url)

;; ╔══════════════════════════════════════════════════════════════════════════╗
;; ║ Configuration Variables                                                  ║
;; ╚══════════════════════════════════════════════════════════════════════════╝

;; Core configuration (unchanged for backward compatibility)
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

;; ╔══════════════════════════════════════════════════════════════════════════╗
;; ║ NEW: Thinking Mode Configuration                                         ║
;; ╚══════════════════════════════════════════════════════════════════════════╝

(defcustom ai-coding-thinking-mode nil
  "When non-nil, AI responses use step-by-step reasoning.
This enables 'thinking mode' where the model shows its reasoning process
before providing the final answer."
  :type 'boolean
  :group 'ai-coding)

(defcustom ai-coding-thinking-temperature 0.6
  "Temperature for thinking mode responses.
Lower than default to reduce repetition and incoherence in reasoning chains."
  :type 'float
  :group 'ai-coding)

;; ╔══════════════════════════════════════════════════════════════════════════╗
;; ║ NEW: Custom Instructions System                                          ║
;; ╚══════════════════════════════════════════════════════════════════════════╝

(defcustom ai-coding-base-instructions
  "You are a Senior Software Engineer assistant. Follow these guidelines:
1. You're a Senior Software Engineer - respond with appropriate technical depth
2. When you have doubts, ask the user for clarification
3. Always be highly technical in every response
4. Be objective and clear in your explanations
5. Comment every function or piece of code you modify with explanation of why
6. If you alter something, don't break the initial logic or functionality
7. Dont use emojis"
  "Base instructions that define the AI's personality and behavior.
These instructions are included in every request to ensure consistent behavior."
  :type 'string
  :group 'ai-coding)

(defcustom ai-coding-context-instructions
  '((explain . "When explaining code, provide detailed technical analysis including purpose, key algorithms, potential issues, and suggestions for improvement.")
    (refactor . "When refactoring, preserve all original functionality while improving code quality. Explain each change and why it's beneficial.")
    (debug . "When debugging, systematically analyze the problem, identify root causes, and provide step-by-step solutions.")
    (chat . "Engage in technical discussion while maintaining your senior engineer perspective."))
  "Context-specific instructions for different AI functions.
Each entry is (CONTEXT . INSTRUCTION) where CONTEXT matches function types."
  :type '(alist :key-type symbol :value-type string)
  :group 'ai-coding)

;; Variables (unchanged for backward compatibility)
(defvar ai-coding/conversations (make-hash-table :test 'equal))
(defvar ai-coding/current-session-id nil)
(defvar ai-coding/chat-buffer-name "*AI Chat*")

;; ╔══════════════════════════════════════════════════════════════════════════╗
;; ║ NEW: Thinking Mode Management                                            ║
;; ╚══════════════════════════════════════════════════════════════════════════╝

;;;###autoload
(defun ai-coding/toggle-thinking-mode ()
  "Toggle between thinking mode (step-by-step reasoning) and quick response mode.
In thinking mode, the AI shows its reasoning process before providing answers.
In quick mode, the AI provides direct responses for faster interaction."
  (interactive)
  (setq ai-coding-thinking-mode (not ai-coding-thinking-mode))
  (message "AI thinking mode %s"
           (if ai-coding-thinking-mode
               (propertize "ENABLED" 'face 'success)
             (propertize "DISABLED" 'face 'warning)))
  ;; Update mode line or other indicators if desired
  (force-mode-line-update))

(defun ai-coding/get-current-mode-indicator ()
  "Return a string indicating current AI mode for display purposes."
  (if ai-coding-thinking-mode
      "[THINKING]"
    "[QUICK]"))

;; ╔══════════════════════════════════════════════════════════════════════════╗
;; ║ NEW: Instruction Management System                                       ║
;; ╚══════════════════════════════════════════════════════════════════════════╝

(defun ai-coding/build-system-prompt (&optional context)
  "Build the complete system prompt including base instructions and context.
CONTEXT is an optional symbol (e.g., 'explain, 'refactor) that adds
context-specific instructions to the base instructions.
Note: Thinking mode is controlled via Ollama's native API, not prompts."
  (let ((base ai-coding-base-instructions)
        (context-instruction (when context
                               (alist-get context ai-coding-context-instructions))))
    (concat base
            (when context-instruction
              (concat "\n\nFor this specific task: " context-instruction)))))

;;;###autoload
(defun ai-coding/edit-instructions ()
  "Interactively edit the base AI instructions.
Opens a temporary buffer where you can modify how the AI behaves."
  (interactive)
  (let ((buffer (get-buffer-create "*AI Instructions Editor*")))
    (with-current-buffer buffer
      (erase-buffer)
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
    (pop-to-buffer buffer)))


(defun ai-coding/clean-control-characters (text)
  "Remove problematic control characters from AI response TEXT.
Removes C0 and C1 control characters that can cause display issues."
  (when text
    ;; Remove C1 control characters (U+0080 to U+009F) - these cause visible artifacts
    (setq text (replace-regexp-in-string "[\x80-\x9F]" "" text))

    ;; Remove other problematic control characters except common ones
    ;; Keep: \t (tab), \n (newline), \r (carriage return)
    ;; Remove: other C0 controls that shouldn't appear in text
    (setq text (replace-regexp-in-string "[\x00-\x08\x0B\x0C\x0E-\x1F\x7F]" "" text))

    ;; Clean up any resulting double spaces or excessive whitespace
    (setq text (replace-regexp-in-string "  +" " " text))

    ;; Remove leading/trailing whitespace from lines
    (setq text (replace-regexp-in-string "^[ \t]+" "" text))
    (setq text (replace-regexp-in-string "[ \t]+$" "" text))

    text))


;; Core functions (preserved for backward compatibility)
(defun ai-coding/check-ollama ()
  "Check if Ollama is running."
  (condition-case nil
      (let ((url-request-method "GET"))
        (with-current-buffer (url-retrieve-synchronously
                             (format "http://%s/api/tags" ai-coding-ollama-host)
                             nil nil 5)
          (goto-char (point-min))
          (re-search-forward "^$" nil t)
          (> (- (point-max) (point)) 5)))
    (error nil)))

(defun ai-coding/make-request (messages callback &optional context)
  "Enhanced request function that integrates instructions and thinking mode.
MESSAGES is the conversation history.
CALLBACK is called with the response.
CONTEXT is an optional symbol for context-specific instructions (e.g., 'explain)."
  (let* ((system-prompt (ai-coding/build-system-prompt context))
         ;; Prepend system message with our instructions
         (enhanced-messages (vconcat `[((role . "system") (content . ,system-prompt))]
                                     messages))
         ;; Use thinking mode temperature if thinking is enabled
         (temperature (if ai-coding-thinking-mode
                          ai-coding-thinking-temperature
                        ai-coding-temperature))
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         ;; Use Ollama's native thinking control
         (payload `((model . ,ai-coding-model)
                   (messages . ,enhanced-messages)
                   (stream . :json-false)
                   (temperature . ,temperature)
                   (think . ,(if ai-coding-thinking-mode t :json-false))))
         (url-request-data (json-encode payload)))

    (url-retrieve (format "http://%s/api/chat" ai-coding-ollama-host)
                  (lambda (status)
                    (unless (plist-get status :error)
                      (goto-char (point-min))
                      (re-search-forward "^$" nil t)
                      (let* ((response (buffer-substring-no-properties (point) (point-max)))
                             (json-data (ignore-errors (json-read-from-string response)))
                             (message-data (alist-get 'message json-data))
                             (content (alist-get 'content message-data))
                             (thinking (alist-get 'thinking message-data)))
                        ;; FIXED: Clean control characters before processing
                        (when content
                          (let ((cleaned-content (ai-coding/clean-control-characters content))
                                (cleaned-thinking (when thinking
                                                   (ai-coding/clean-control-characters thinking))))
                            (funcall callback cleaned-content cleaned-thinking)))))))))


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

;; Enhanced core functions with context support
;;;###autoload
(defun ai-coding/explain-code ()
  "Explain selected code using context-specific instructions.
Uses 'explain context for detailed technical analysis."
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
       (with-current-buffer (get-buffer-create "*AI Code Explanation*")
         (erase-buffer)
         (insert "=== Code Explanation ===\n\n")
         (insert "Mode: " (ai-coding/get-current-mode-indicator) "\n")
         (insert "Code:\n```\n" code "\n```\n\n")
         (when (and thinking ai-coding-thinking-mode)
           (insert "=== Thinking Process ===\n" thinking "\n\n"))
         (insert "=== Explanation ===\n" response)
         (goto-char (point-min))
         (display-buffer (current-buffer))))
     'explain))) ; Pass 'explain context

;;;###autoload
(defun ai-coding/refactor-code ()
  "Get refactoring suggestions using context-specific instructions.
Uses 'refactor context to ensure functionality preservation."
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
       (with-current-buffer (get-buffer-create "*AI Refactor Suggestions*")
         (erase-buffer)
         (insert "=== Refactoring Suggestions ===\n\n")
         (insert "Mode: " (ai-coding/get-current-mode-indicator) "\n\n")
         ;; FIXED: Show thinking process if available
         (when (and thinking ai-coding-thinking-mode)
           (insert "=== Thinking Process ===\n" thinking "\n\n"))
         (insert "=== Suggestions ===\n" response)
         (goto-char (point-min))
         (display-buffer (current-buffer))))
     'refactor))) ; Pass 'refactor context

;;;###autoload
(defun ai-coding/open-chat ()
  "Open AI chat interface with mode indicator."
  (interactive)
  (unless (ai-coding/check-ollama)
    (user-error "Ollama is not running. Start it with: ollama serve"))

  (let ((buffer (get-buffer-create ai-coding/chat-buffer-name)))
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

;; Enhanced chat mode with thinking mode integration
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
               ;; FIXED: Show thinking process if available in chat
               (when (and thinking ai-coding-thinking-mode)
                 (insert "\n--- Thinking ---\n" thinking "\n--- Response ---\n"))
               (insert response "\n\nYou: ")
               (ai-coding/add-to-conversation session-id "assistant" response)
               (setq ai-chat-prompt-start (point))
               (goto-char (point-max))))
           'chat)))))) ; Pass 'chat context

(define-derived-mode ai-chat-mode fundamental-mode "AI-Chat"
  "Enhanced AI chat mode with thinking mode support."
  (visual-line-mode 1)
  ;; Existing keybindings preserved
  (local-set-key (kbd "RET") 'ai-chat-send)
  (local-set-key (kbd "C-c C-c") 'ai-chat-send)
  (local-set-key (kbd "C-c C-n") 'ai-coding/new-chat-session)
  ;; New keybinding for thinking mode toggle
  (local-set-key (kbd "C-c C-t") 'ai-coding/toggle-thinking-mode))


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

(provide 'ai-enhanced-coding)
;;; ai-enhanced-coding.el ends here
