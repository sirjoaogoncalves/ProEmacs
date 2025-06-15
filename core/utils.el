;;; utils.el --- Common utility functions -*- lexical-binding: t; -*-

;;; Code:

;; Directory and file utilities
(defun utils/current-directory ()
  "Get current directory intelligently from buffer, dired, or default."
  (or (when buffer-file-name
        (file-name-directory buffer-file-name))
      (when (derived-mode-p 'dired-mode)
        default-directory)
      default-directory))

(defun utils/ensure-directory-slash (dir)
  "Ensure DIR ends with a slash."
  (if (string-suffix-p "/" dir) dir (concat dir "/")))

(defun utils/prompt-directory (prompt)
  "Prompt for directory with PROMPT, using current directory as base."
  (let* ((base-dir (utils/current-directory))
         (prompt-dir (utils/ensure-directory-slash base-dir)))
    (read-directory-name prompt prompt-dir)))

(defun utils/prompt-file (prompt)
  "Prompt for file with PROMPT, using current directory as base."
  (let* ((base-dir (utils/current-directory))
         (prompt-dir (utils/ensure-directory-slash base-dir)))
    (read-string (format "%s%s: " prompt prompt-dir) prompt-dir)))

;; Package management utilities
(defun utils/ensure-packages (packages)
  "Ensure all PACKAGES are installed."
  (dolist (pkg packages)
    (unless (package-installed-p pkg)
      (message "Installing %s..." pkg)
      (package-install pkg)
      (message "%s installed" pkg))))

(defmacro utils/use-package-with-lsp (name &rest args)
  "Use-package macro with common LSP setup for NAME."
  (let ((mode-name (intern (format "%s-mode" name))))
    `(use-package ,name
       :ensure t
       :hook ((,mode-name . lsp-deferred)
              (,mode-name . my/enable-lsp-format-on-save))
       ,@args)))

;; Buffer management utilities
(defun utils/create-display-buffer (name)
  "Create buffer with NAME, erase it, and return it for display."
  (let ((buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (erase-buffer))
    buffer))

(defun utils/display-buffer-at-top (buffer)
  "Display BUFFER and move to beginning."
  (with-current-buffer buffer
    (goto-char (point-min))
    (display-buffer buffer)))

;; File operation utilities
(defun utils/safe-file-operation (operation filename &optional new-name)
  "Safely perform OPERATION on FILENAME with optional NEW-NAME."
  (unless (buffer-file-name)
    (user-error "Buffer is not visiting a file"))
  (pcase operation
    ('delete
     (when (yes-or-no-p (format "Really delete %s? " filename))
       (delete-file filename t)
       (message "File %s deleted" filename)
       (kill-buffer (current-buffer))))
    ('rename
     (when (and new-name (not (string-equal filename new-name)))
       (if (vc-backend filename)
           (vc-rename-file filename new-name)
         (rename-file filename new-name t)
         (set-visited-file-name new-name t t))))
    ('copy
     (when (and new-name (not (string-equal filename new-name)))
       (copy-file filename new-name t t t)
       (message "File copied to %s" new-name)
       (find-file new-name)))))

;; Font utilities
(defun utils/font-exists-p (font-name)
  "Check if FONT-NAME exists on the system."
  (and (display-graphic-p)
       (member font-name (font-family-list))))

;; Keybinding utilities
(defmacro utils/leader-group (prefix description &rest bindings)
  "Create leader key group with PREFIX and DESCRIPTION, containing BINDINGS."
  `(my-leader-keys
     ,prefix '(:ignore t :which-key ,description)
     ,@bindings))

(defmacro utils/define-prefix-commands (prefix description &rest commands)
  "Define multiple prefix commands under PREFIX with DESCRIPTION."
  `(utils/leader-group ,prefix ,description
     ,@(mapcar (lambda (cmd)
                 (let ((key (car cmd))
                       (func (cadr cmd))
                       (desc (caddr cmd)))
                   `,(concat prefix key) ,func :which-key ,desc))
               commands)))

;; LSP utilities
(defun utils/setup-lsp-hooks (mode)
  "Setup common LSP hooks for MODE."
  (add-hook (intern (format "%s-hook" mode)) #'lsp-deferred)
  (add-hook (intern (format "%s-hook" mode)) #'my/enable-lsp-format-on-save))

;; Directory operations with refresh
(defun utils/directory-operation-with-refresh (operation dir-name)
  "Perform OPERATION on DIR-NAME and refresh dired if needed."
  (if (listp dir-name)
      (apply operation dir-name)
    (funcall operation dir-name))
  (when (derived-mode-p 'dired-mode)
    (revert-buffer)))

;; Error handling wrapper
(defmacro utils/with-error-handling (error-message &rest body)
  "Execute BODY with error handling, showing ERROR-MESSAGE on failure."
  `(condition-case err
       (progn ,@body)
     (error
      (message "%s: %s" ,error-message (error-message-string err)))))

;; AI response utilities
(defun utils/clean-ai-text (text)
  "Clean problematic characters from AI response TEXT."
  (when text
    (setq text (replace-regexp-in-string "[\x80-\x9F]" "" text))
    (setq text (replace-regexp-in-string "[\x00-\x08\x0B\x0C\x0E-\x1F\x7F]" "" text))
    (setq text (replace-regexp-in-string "  +" " " text))
    (setq text (replace-regexp-in-string "^[ \t]+" "" text))
    (setq text (replace-regexp-in-string "[ \t]+$" "" text))
    text))

(defun utils/ai-display-response (buffer-name mode-indicator code response thinking)
  "Display AI response in BUFFER-NAME with MODE-INDICATOR, CODE, RESPONSE, and optional THINKING."
  (with-current-buffer (utils/create-display-buffer buffer-name)
    (insert "=== " (capitalize (replace-regexp-in-string "\\*\\|AI " "" buffer-name)) " ===\n\n")
    (insert "Mode: " mode-indicator "\n\n")
    (when code
      (insert "Code:\n```\n" code "\n```\n\n"))
    (when (and thinking mode-indicator (string-match-p "THINKING" mode-indicator))
      (insert "=== Thinking Process ===\n" thinking "\n\n"))
    (insert "=== " (if thinking "Response" "Analysis") " ===\n" response)
    (utils/display-buffer-at-top (current-buffer))))

;; Process utilities
(defun utils/run-command-async (command callback &rest args)
  "Run COMMAND with ARGS asynchronously and call CALLBACK with result."
  (let ((cmd (mapconcat 'identity (cons command args) " ")))
    (let ((process (start-process-shell-command "async-command" nil cmd)))
      (set-process-sentinel
       process
       (lambda (proc event)
         (when (string= event "finished\n")
           (with-current-buffer (process-buffer proc)
             (funcall callback (buffer-string)))))))))

;; Theme utilities
(defun utils/load-theme-safe (theme)
  "Safely load THEME with error handling."
  (utils/with-error-handling (format "Failed to load theme %s" theme)
    (load-theme theme t)
    (message "Loaded theme: %s" theme)))

;; Performance utilities
(defmacro utils/with-performance-timing (description &rest body)
  "Execute BODY and time the execution, showing DESCRIPTION."
  `(let ((start-time (current-time)))
     (prog1 (progn ,@body)
       (message "%s took %.2f seconds"
                ,description
                (float-time (time-subtract (current-time) start-time))))))

(provide 'utils)
;;; utils.el ends here
