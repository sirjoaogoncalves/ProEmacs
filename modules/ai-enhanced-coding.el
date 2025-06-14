;;; ai-enhanced-coding.el --- Simple AI coding assistant -*- lexical-binding: t; -*-

;;; Commentary:
;; Simple AI coding assistant with easy-to-use chat interface.

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

(defcustom ai-coding-model "qwen2.5-coder:7b"
  "Ollama model to use for code assistance."
  :type 'string
  :group 'ai-coding)

(defcustom ai-coding-temperature 0.7
  "Temperature for AI responses (0.0 to 1.0)."
  :type 'float
  :group 'ai-coding)

;; Variables
(defvar ai-coding/conversations (make-hash-table :test 'equal))
(defvar ai-coding/current-session-id nil)
(defvar ai-coding/chat-buffer-name "*AI Chat*")

;; Check if Ollama is running
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

;; Make API request
(defun ai-coding/make-request (messages callback)
  "Make request to Ollama."
  (let* ((url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (payload `((model . ,ai-coding-model)
                   (messages . ,(vconcat messages))
                   (stream . :json-false)))
         (url-request-data (json-encode payload)))

    (url-retrieve (format "http://%s/api/chat" ai-coding-ollama-host)
                  (lambda (status)
                    (unless (plist-get status :error)
                      (goto-char (point-min))
                      (re-search-forward "^$" nil t)
                      (let* ((response (buffer-substring-no-properties (point) (point-max)))
                             (json-data (ignore-errors (json-read-from-string response)))
                             (content (and json-data
                                          (alist-get 'content
                                                    (alist-get 'message json-data)))))
                        (when content
                          (funcall callback content))))))))

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

;; Main chat function
;;;###autoload
(defun ai-coding/open-chat ()
  "Open AI chat interface."
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
        (insert "Type your message and press Enter to send.\n")
        (insert "Use C-c C-n for new session.\n\n"))
      (goto-char (point-max))
      (insert "\nYou: ")
      (setq-local ai-chat-prompt-start (point)))))

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
(defun ai-coding/explain-code ()
  "Explain selected code."
  (interactive)
  (unless (ai-coding/check-ollama)
    (user-error "Ollama not running"))
  (let ((code (if (use-region-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (thing-at-point 'line t))))
    (message "Analyzing code...")
    (ai-coding/make-request
     `[((role . "user") (content . ,(format "Explain this code:\n\n```\n%s\n```" code)))]
     (lambda (response)
       (with-current-buffer (get-buffer-create "*AI Code Explanation*")
         (erase-buffer)
         (insert "=== Code Explanation ===\n\n")
         (insert "Code:\n```\n" code "\n```\n\n")
         (insert "Explanation:\n" response)
         (goto-char (point-min))
         (display-buffer (current-buffer)))))))

;;;###autoload
(defun ai-coding/refactor-code ()
  "Get refactoring suggestions."
  (interactive)
  (unless (ai-coding/check-ollama)
    (user-error "Ollama not running"))
  (let ((code (if (use-region-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (buffer-string))))
    (message "Getting refactoring suggestions...")
    (ai-coding/make-request
     `[((role . "user") (content . ,(format "Suggest refactoring for:\n\n```\n%s\n```" code)))]
     (lambda (response)
       (with-current-buffer (get-buffer-create "*AI Refactor Suggestions*")
         (erase-buffer)
         (insert "=== Refactoring Suggestions ===\n\n")
         (insert response)
         (goto-char (point-min))
         (display-buffer (current-buffer)))))))

;;;###autoload
(defun ai-coding/debug-status ()
  "Show debug status."
  (interactive)
  (let* ((session-id (ai-coding/get-session-id))
         (history (gethash session-id ai-coding/conversations '()))
         (ollama-status (if (ai-coding/check-ollama) "Running" "Not running")))
    (message "Session: %s | Messages: %d | Ollama: %s"
             session-id (length history) ollama-status)))

;; Chat mode
(defvar-local ai-chat-prompt-start nil)

(defun ai-chat-send ()
  "Send current message."
  (interactive)
  (when ai-chat-prompt-start
    (let* ((message (buffer-substring-no-properties ai-chat-prompt-start (point-max)))
           (session-id (ai-coding/get-session-id)))
      (when (string-match "\\S-" message)
        (goto-char (point-max))
        (insert "\n\nAI: [thinking...]\n")
        (let ((thinking-start (save-excursion
                                (forward-line -1)
                                (point))))
          (ai-coding/add-to-conversation session-id "user" message)
          (ai-coding/make-request
           (vconcat (gethash session-id ai-coding/conversations '()))
           (lambda (response)
             (with-current-buffer (get-buffer ai-coding/chat-buffer-name)
               (goto-char thinking-start)
               (delete-region thinking-start (point-max))
               (insert "AI: " response "\n\nYou: ")
               (ai-coding/add-to-conversation session-id "assistant" response)
               (setq ai-chat-prompt-start (point))
               (goto-char (point-max))))))))))

(define-derived-mode ai-chat-mode fundamental-mode "AI-Chat"
  "Simple AI chat mode."
  (visual-line-mode 1)
  (local-set-key (kbd "RET") 'ai-chat-send)
  (local-set-key (kbd "C-c C-c") 'ai-chat-send)
  (local-set-key (kbd "C-c C-n") 'ai-coding/new-chat-session))

(provide 'ai-enhanced-coding)
;;; ai-enhanced-coding.el ends here
