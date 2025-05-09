;;; process-manager.el --- Intelligent process lifecycle management -*- lexical-binding: t; -*-

;;; Commentary:
;; Monitors and manages external processes to conserve system resources
;; Particularly useful for memory-intensive processes like LSP servers

;;; Code:

;; Registry to keep track of all managed processes
(defvar normal/process-registry (make-hash-table :test 'equal)
  "Registry of processes managed by the system.")

;; Configuration options
(defgroup normal/process-management nil
  "Process lifecycle management settings."
  :group 'convenience)

(defcustom normal/process-idle-time 300
  "Seconds of inactivity after which a process may be suspended.
Different process types may override this value."
  :type 'integer
  :group 'normal/process-management)

(defcustom normal/process-memory-threshold 256
  "Memory threshold in MB above which a process is considered high-memory.
High-memory processes are prioritized for suspension."
  :type 'integer
  :group 'normal/process-management)

(defcustom normal/process-check-interval 30
  "Interval in seconds between process status checks."
  :type 'integer
  :group 'normal/process-management)

(defcustom normal/process-types
  '((:type lsp-server
           :detect "\\`lsp-\\|\\`clangd\\|\\`pylsp\\|\\`typescript-language-server"
           :idle-time 600
           :strategy suspend)
    (:type syntax-checker
           :detect "\\`flycheck-\\|\\`flymake-\\|\\`eslint\\|\\`pylint"
           :idle-time 300
           :strategy terminate)
    (:type formatter
           :detect "\\`prettier\\|\\`black\\|\\`gofmt\\|\\`clang-format"
           :idle-time 60
           :strategy terminate)
    (:type indexer
           :detect "\\`ctags\\|\\`gtags\\|\\`ripgrep\\|\\`find"
           :idle-time 120
           :strategy terminate)
    (:type git
           :detect "\\`git"
           :idle-time 60
           :strategy terminate))
  "Configuration for different process types.
Each entry specifies:
- :type       Symbol identifying the process type
- :detect     Regexp to identify processes of this type
- :idle-time  Seconds of inactivity before management action
- :strategy   Action to take (suspend or terminate)"
  :type '(repeat (plist :key-type keyword :value-type sexp))
  :group 'normal/process-management)

;; Process registry functions
(defun normal/process-register (process &optional buffer type)
  "Register PROCESS for lifecycle management.
Associates with BUFFER and TYPE if provided."
  (when (process-live-p process)
    (let* ((proc-name (process-name process))
           (proc-type (or type (normal/process-detect-type process)))
           (proc-buffer (or buffer (process-buffer process)))
           (entry (list :process process
                        :name proc-name
                        :type proc-type
                        :buffer proc-buffer
                        :last-active (float-time)
                        :memory 0
                        :status 'active)))
      (puthash proc-name entry normal/process-registry)
      (add-hook 'kill-buffer-hook #'normal/process-buffer-killed nil t)
      entry)))

(defun normal/process-detect-type (process)
  "Detect the type of PROCESS based on its name or command."
  (let* ((proc-name (process-name process))
         (proc-cmd (mapconcat #'identity (process-command process) " ")))
    (catch 'found
      (dolist (type-def normal/process-types)
        (let ((pattern (plist-get type-def :detect)))
          (when (or (and proc-name (string-match-p pattern proc-name))
                    (and proc-cmd (string-match-p pattern proc-cmd)))
            (throw 'found (plist-get type-def :type)))))
      'generic))) ;; Default type if no match

(defun normal/process-update-activity (process-name)
  "Update the last-active timestamp for process with PROCESS-NAME."
  (let ((entry (gethash process-name normal/process-registry)))
    (when entry
      (plist-put entry :last-active (float-time))
      (puthash process-name entry normal/process-registry))))

(defun normal/process-buffer-killed ()
  "Handle buffer being killed - mark associated processes."
  (let ((current-buffer (current-buffer)))
    (maphash (lambda (proc-name entry)
               (when (eq (plist-get entry :buffer) current-buffer)
                 (plist-put entry :buffer nil)
                 (puthash proc-name entry normal/process-registry)
                 (run-with-timer 5 nil #'normal/process-check-orphans)))
             normal/process-registry)))

(defun normal/process-check-orphans ()
  "Check for and clean up processes with no associated buffer."
  (maphash (lambda (proc-name entry)
             (let ((process (plist-get entry :process))
                   (buffer (plist-get entry :buffer)))
               (when (and (process-live-p process)
                          (or (null buffer)
                              (not (buffer-live-p buffer))))
                 (normal/process-manage-lifecycle
                  proc-name 'terminate "Buffer killed"))))
           normal/process-registry))

;; Resource monitoring
(defun normal/process-update-resources ()
  "Update resource usage information for all managed processes."
  (maphash (lambda (proc-name entry)
             (let ((process (plist-get entry :process)))
               (when (process-live-p process)
                 (let ((mem (normal/process-memory-usage process)))
                   (plist-put entry :memory mem)
                   (puthash proc-name entry normal/process-registry)))))
           normal/process-registry))

(defun normal/process-memory-usage (process)
  "Get memory usage of PROCESS in MB.
Returns 0 if unable to determine."
  (let ((pid (process-id process)))
    (if (not pid)
        0
      ;; Try to get memory usage - implementation varies by platform
      (cond
       ;; Linux
       ((eq system-type 'gnu/linux)
        (string-to-number (shell-command-to-string
         (format "ps -o rss= -p %d | awk '{print $1/1024}'" pid))))
       ;; macOS
       ((eq system-type 'darwin)
        (string-to-number (shell-command-to-string
         (format "ps -o rss= -p %d | awk '{print $1/1024}'" pid))))
       ;; Windows
       ((eq system-type 'windows-nt)
        (string-to-number (shell-command-to-string
         (format "powershell -command \"(Get-Process -Id %d).WorkingSet / 1MB\"" pid))))
       ;; Default: unknown
       (t 0)))))

;; Process lifecycle management
(defun normal/process-manage-lifecycle (process-name action &optional reason)
  "Manage the lifecycle of process with PROCESS-NAME.
ACTION can be `suspend', `resume', or `terminate'.
Optional REASON provides context for the action."
  (let ((entry (gethash process-name normal/process-registry)))
    (when entry
      (let ((process (plist-get entry :process))
            (type (plist-get entry :type))
            (buffer (plist-get entry :buffer)))
        (when (process-live-p process)
          (pcase action
            ('suspend
             (when (and (memq (process-status process) '(run open))
                        (eq (plist-get entry :status) 'active))
               (ignore-errors
                 (signal-process process 'STOP))
               (plist-put entry :status 'suspended)
               (puthash process-name entry normal/process-registry)
               (message "Process '%s' suspended: %s" process-name (or reason "idle"))))

            ('resume
             (when (and (eq (process-status process) 'stop)
                        (eq (plist-get entry :status) 'suspended))
               (ignore-errors
                 (signal-process process 'CONT))
               (plist-put entry :status 'active)
               (plist-put entry :last-active (float-time))
               (puthash process-name entry normal/process-registry)
               (message "Process '%s' resumed" process-name)))

            ('terminate
             (when (process-live-p process)
               (ignore-errors
                 (delete-process process))
               (plist-put entry :status 'terminated)
               (puthash process-name entry normal/process-registry)
               (message "Process '%s' terminated: %s" process-name (or reason "idle"))))))))))

(defun normal/process-check-idle ()
  "Check for and manage idle processes."
  (let ((current-time (float-time)))
    (maphash (lambda (proc-name entry)
               (let ((process (plist-get entry :process)))
                 (when (process-live-p process)
                   (let* ((proc-type (plist-get entry :type))
                          (type-config (seq-find
                                        (lambda (tc) (eq (plist-get tc :type) proc-type))
                                        normal/process-types
                                        nil))
                          (idle-threshold (or (and type-config
                                                   (plist-get type-config :idle-time))
                                              normal/process-idle-time))
                          (strategy (or (and type-config
                                             (plist-get type-config :strategy))
                                        'suspend))
                          (last-active (plist-get entry :last-active))
                          (idle-time (- current-time last-active))
                          (memory (plist-get entry :memory))
                          (is-high-memory (>= memory normal/process-memory-threshold))
                          (status (plist-get entry :status)))

                     ;; First check if we need to suspend the process
                     (when (and (eq status 'active)
                                (> idle-time idle-threshold)
                                (or is-high-memory
                                    (eq strategy 'suspend)
                                    (eq strategy 'terminate)))
                       (if (eq strategy 'terminate)
                           (normal/process-manage-lifecycle
                            proc-name 'terminate
                            (format "Idle for %.1fs%s"
                                    idle-time
                                    (if is-high-memory " (high memory)" "")))
                         (normal/process-manage-lifecycle
                          proc-name 'suspend
                          (format "Idle for %.1fs%s"
                                  idle-time
                                  (if is-high-memory " (high memory)" "")))))))))
             normal/process-registry)))

(defun normal/process-activated-hook ()
  "Function called when a process needs to be activated."
  (let* ((this-command (or (and (symbolp this-command) (symbol-name this-command)) ""))
         (this-buffer (current-buffer)))

    ;; Resume processes associated with current buffer
    (maphash (lambda (proc-name entry)
               (let ((process (plist-get entry :process))
                     (buffer (plist-get entry :buffer))
                     (status (plist-get entry :status)))
                 (when (and (eq buffer this-buffer)
                            (eq status 'suspended)
                            (process-live-p process))
                   (normal/process-manage-lifecycle proc-name 'resume))))
             normal/process-registry)

    ;; Certain commands may need specific processes
    (when (string-match-p "\\`lsp-\\|\\`company-\\|\\`flycheck-\\|\\`find-" this-command)
      (maphash (lambda (proc-name entry)
                 (let ((process (plist-get entry :process))
                       (proc-type (plist-get entry :type))
                       (status (plist-get entry :status)))
                   (when (and (memq proc-type '(lsp-server syntax-checker))
                              (eq status 'suspended)
                              (process-live-p process))
                     (normal/process-manage-lifecycle proc-name 'resume))))
               normal/process-registry))))

;; Setup hooks and timers
(defvar normal/process-manager-timer nil
  "Timer for periodic process checks.")

(defun normal/process-manager-setup ()
  "Set up the process management system."
  (interactive)

  ;; Cancel existing timer if running
  (when normal/process-manager-timer
    (cancel-timer normal/process-manager-timer))

  ;; Set up new timer
  (setq normal/process-manager-timer
        (run-with-timer 60 normal/process-check-interval #'normal/process-lifecycle-check))

  ;; Set up hooks
  (add-hook 'pre-command-hook #'normal/process-activated-hook)

  ;; Register existing processes
  (dolist (process (process-list))
    (when (process-live-p process)
      (normal/process-register process)))

  (message "Process manager started"))

;; Main lifecycle check function
(defun normal/process-lifecycle-check ()
  "Run a complete lifecycle check on all managed processes."
  (normal/process-update-resources)
  (normal/process-check-idle)
  (normal/process-check-orphans))

;; Process tracking hooks for various systems

;; LSP mode integration
(with-eval-after-load 'lsp-mode
  (advice-add 'lsp--start-process :after
              (lambda (cmd &optional callback error-callback)
                "Register LSP server process."
                (when-let* ((proc (car (last (process-list)))))
                  (when (and (stringp (car cmd))
                             (string-match-p "lsp\\|language-server" (car cmd)))
                    (normal/process-register proc (current-buffer) 'lsp-server))))))

;; Flycheck integration
(with-eval-after-load 'flycheck
  (advice-add 'flycheck-start-command-checker :after
              (lambda (_checker _callback)
                "Register checker processes."
                (when-let* ((proc (car (last (process-list)))))
                  (normal/process-register proc (current-buffer) 'syntax-checker)))))

;; General process sentinel
(defun normal/process-sentinel (process event)
  "Track process status changes in the registry.
This is a sentinel function for PROCESS that receives EVENT."
  (let ((proc-name (process-name process)))
    (cond
     ((string-match-p "finished\\|exited\\|failed" event)
      (let ((entry (gethash proc-name normal/process-registry)))
        (when entry
          (plist-put entry :status 'terminated)
          (puthash proc-name entry normal/process-registry))))

     ((string= "killed" event)
      (let ((entry (gethash proc-name normal/process-registry)))
        (when entry
          (plist-put entry :status 'terminated)
          (puthash proc-name entry normal/process-registry))))

     ((string= "stopped" event)
      (let ((entry (gethash proc-name normal/process-registry)))
        (when entry
          (plist-put entry :status 'suspended)
          (puthash proc-name entry normal/process-registry))))

     ((string= "continued" event)
      (let ((entry (gethash proc-name normal/process-registry)))
        (when entry
          (plist-put entry :status 'active)
          (plist-put entry :last-active (float-time))
          (puthash proc-name entry normal/process-registry)))))))

;; Install sentinel on new processes
(defun normal/process-advise-make-process (_name _buffer _program &rest _)
  "Advice to run after make-process to register the new process."
  (when-let* ((proc (car (last (process-list)))))
    (set-process-sentinel proc #'normal/process-sentinel)
    (normal/process-register proc)))

(advice-add 'make-process :after #'normal/process-advise-make-process)

;; Start process manager on emacs startup
(add-hook 'after-init-hook #'normal/process-manager-setup)

;; User command to view process status
(defun normal/process-manager-status ()
  "Display status of all managed processes."
  (interactive)
  (with-current-buffer (get-buffer-create "*Process Manager*")
    (let ((inhibit-read-only t)
          (current-time (float-time)))
      (erase-buffer)
      (insert "┌─ Process Manager Status ─────────────────────────────────────────────┐\n")
      (insert "│ PID | Type          | Status    | Memory | Idle Time | Name          │\n")
      (insert "├─────┼───────────────┼───────────┼────────┼───────────┼───────────────┤\n")

      (let ((processes nil))
        ;; Convert hash table to list for sorting
        (maphash (lambda (name entry)
                   (push entry processes))
                 normal/process-registry)

        ;; Sort by status (active first), then by type
        (setq processes
              (sort processes
                    (lambda (a b)
                      (let ((status-a (plist-get a :status))
                            (status-b (plist-get b :status))
                            (type-a (plist-get a :type))
                            (type-b (plist-get b :type)))
                        (if (eq status-a status-b)
                            (string< (symbol-name type-a) (symbol-name type-b))
                          (not (eq status-a 'terminated)))))))

        (if (null processes)
            (insert "│ No processes being managed                                         │\n")
          (dolist (entry processes)
            (let* ((process (plist-get entry :process))
                   (status (plist-get entry :status))
                   (pid (and (process-live-p process) (process-id process)))
                   (type (plist-get entry :type))
                   (name (plist-get entry :name))
                   (last-active (plist-get entry :last-active))
                   (memory (plist-get entry :memory))
                   (idle-time (and last-active (- current-time last-active))))

              (insert (format "│ %3s | %-13s | %-9s | %6.1fM | %7.1fs | %-14s │\n"
                              (or pid "—")
                              (or (and type (symbol-name type)) "unknown")
                              (or (and status (symbol-name status)) "unknown")
                              (or memory 0)
                              (or idle-time 0)
                              (truncate-string-to-width
                               (or name "—") 14 nil nil "…"))))))

        (insert "└─────┴───────────────┴───────────┴────────┴───────────┴───────────────┘\n\n")
        (insert "Memory usage: ")
        (let ((total-memory 0))
          (dolist (entry processes)
            (when (eq (plist-get entry :status) 'active)
              (setq total-memory (+ total-memory (or (plist-get entry :memory) 0)))))
          (insert (format "%.1f MB active processes\n\n" total-memory)))

        (insert "Commands:\n")
        (insert "  [r] Resume suspended process   [s] Suspend active process\n")
        (insert "  [t] Terminate process          [R] Refresh list\n")
        (insert "  [q] Quit")

        (special-mode)
        (local-set-key (kbd "r") 'normal/process-manager-resume)
        (local-set-key (kbd "s") 'normal/process-manager-suspend)
        (local-set-key (kbd "t") 'normal/process-manager-terminate)
        (local-set-key (kbd "R") 'normal/process-manager-status)
        (local-set-key (kbd "q") 'quit-window)

        (goto-char (point-min))
        (display-buffer (current-buffer))))))

(defun normal/process-manager-get-process-at-point ()
  "Get process name at point in the process manager buffer."
  (save-excursion
    (beginning-of-line)
    (if (looking-at "│ +\\(?:[0-9]+\\|—\\) +| +[^ ]+ +| +[^ ]+ +| +[^ ]+ +| +[^ ]+ +| +\\([^ ]+\\) +│")
        (match-string 1)
      nil)))

(defun normal/process-manager-resume ()
  "Resume the process at point."
  (interactive)
  (let ((proc-name (normal/process-manager-get-process-at-point)))
    (when proc-name
      (normal/process-manage-lifecycle proc-name 'resume "User request")
      (normal/process-manager-status))))

(defun normal/process-manager-suspend ()
  "Suspend the process at point."
  (interactive)
  (let ((proc-name (normal/process-manager-get-process-at-point)))
    (when proc-name
      (normal/process-manage-lifecycle proc-name 'suspend "User request")
      (normal/process-manager-status))))

(defun normal/process-manager-terminate ()
  "Terminate the process at point."
  (interactive)
  (let ((proc-name (normal/process-manager-get-process-at-point)))
    (when proc-name
      (when (yes-or-no-p (format "Really terminate process '%s'? " proc-name))
        (normal/process-manage-lifecycle proc-name 'terminate "User request")
        (normal/process-manager-status)))))

;; Add keybinding
(with-eval-after-load 'general
  (my-leader-keys
    "m" '(:ignore t :which-key "processes")
    "ms" '(normal/process-manager-status :which-key "process status")))

(provide 'process-manager)
;;; process-manager.el ends here
