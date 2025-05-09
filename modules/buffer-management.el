;;; buffer-management.el --- Better buffer management -*- lexical-binding: t; -*-

;;; Commentary:
;; Enhanced buffer management with auto-cleanup, limiting, and organization

;;; Code:

;; Variables for configuration
(defvar normal/buffer-cleanup-idle-time 300
  "Seconds of idle time before cleaning up unused buffers.")

(defvar normal/buffer-cleanup-exclude
  '("\\*scratch\\*" "\\*Messages\\*" "\\*dashboard\\*" "\\*.*\\*" "magit:.*")
  "List of regexps matching buffer names that should never be auto-cleaned.")

(defvar normal/max-buffers-per-mode 20
  "Maximum number of buffers to keep per major mode.")

(defvar normal/buffer-cleanup-timer nil
  "Timer for automatic buffer cleanup.")

(defvar normal/excluded-modes-for-limiting
  '(dired-mode magit-mode dashboard-mode vterm-mode)
  "Modes that shouldn't have their buffers limited.")

;; Core buffer management functions

(defun normal/unused-buffer-p (buffer)
  "Return t if BUFFER is deemed unused and can be killed."
  (let ((name (buffer-name buffer)))
    (and
     ;; Not one of our excluded buffers
     (not (seq-some (lambda (pattern) (string-match-p pattern name)) 
                    normal/buffer-cleanup-exclude))
     ;; Not modified
     (not (buffer-modified-p buffer))
     ;; Not visible in any window or frame
     (not (get-buffer-window buffer t))
     ;; Not a special buffer (unless it's a dead process buffer)
     (or (not (string-match-p "^\\*" name))
         (and (string-match-p "^\\*" name)
              (eq (buffer-local-value 'major-mode buffer) 'fundamental-mode)))
     ;; Has existed for some time
     (let ((time-created nil))
       (condition-case nil
           (setq time-created (buffer-local-value 'normal/buffer-creation-time buffer))
         (error
          (with-current-buffer buffer
            (setq-local normal/buffer-creation-time (current-time))
            (setq time-created (current-time)))))
       (> (float-time (time-subtract (current-time) time-created)) 3600)))))

(defun normal/cleanup-buffers ()
  "Kill unused buffers that haven't been viewed in a while."
  (interactive)
  (let ((count 0))
    (dolist (buffer (buffer-list))
      (when (normal/unused-buffer-p buffer)
        (kill-buffer buffer)
        (setq count (1+ count))))
    (message "Cleaned up %d unused buffer(s)" count)))

(defun normal/limit-buffers-by-mode ()
  "Limit the number of buffers per major mode."
  (interactive)
  (let ((mode-buffers (make-hash-table :test 'eq))
        (buffers-to-kill nil))
    
    ;; Group buffers by major mode
    (dolist (buffer (buffer-list))
      (let ((mode (buffer-local-value 'major-mode buffer)))
        (unless (member mode normal/excluded-modes-for-limiting)
          (push buffer (gethash mode mode-buffers nil)))))
    
    ;; For each mode, limit buffers to max allowed
    (maphash (lambda (mode buffers)
               (when (> (length buffers) normal/max-buffers-per-mode)
                 ;; Sort by last viewed time (most recent first)
                 (setq buffers 
                       (sort buffers
                             (lambda (a b)
                               (let ((time-a (or (buffer-local-value 'normal/last-viewed-time a) 0))
                                     (time-b (or (buffer-local-value 'normal/last-viewed-time b) 0)))
                                 (> time-a time-b)))))
                 
                 ;; Keep the first N, mark the rest for killing
                 (setq buffers-to-kill 
                       (append buffers-to-kill
                               (nthcdr normal/max-buffers-per-mode buffers)))))
             mode-buffers)
    
    ;; Kill the buffers we've selected (if unmodified)
    (let ((killed 0))
      (dolist (buffer buffers-to-kill)
        (when (and (buffer-name buffer) 
                   (not (buffer-modified-p buffer))
                   (not (get-buffer-window buffer t)))
          (kill-buffer buffer)
          (setq killed (1+ killed))))
      (when (> killed 0)
        (message "Limited buffers: killed %d excess buffer(s)" killed)))))

;; Track buffer viewings to inform our buffer management
(defun normal/record-buffer-access ()
  "Record when a buffer was last accessed."
  (setq-local normal/last-viewed-time (float-time))
  
  ;; Ensure we have a creation time
  (unless (boundp 'normal/buffer-creation-time)
    (setq-local normal/buffer-creation-time (current-time))))

;; Setup hooks and timers
(defun normal/setup-buffer-management ()
  "Set up the buffer management system."
  (interactive)
  ;; Cancel any existing timer
  (when normal/buffer-cleanup-timer
    (cancel-timer normal/buffer-cleanup-timer))
  
  ;; Start a new timer for buffer cleanup
  (setq normal/buffer-cleanup-timer
        (run-with-idle-timer normal/buffer-cleanup-idle-time t #'normal/cleanup-buffers))
  
  ;; Track when buffers are viewed
  (add-hook 'buffer-list-update-hook #'normal/record-buffer-access)
  
  ;; Limit buffers periodically
  (run-with-idle-timer 600 t #'normal/limit-buffers-by-mode)
  
  ;; Set creation time for all existing buffers
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (unless (boundp 'normal/buffer-creation-time)
        (setq-local normal/buffer-creation-time (current-time))))))

;; Interactive functions for buffer management

(defun normal/kill-other-buffers ()
  "Kill all buffers except the current one and essential buffers."
  (interactive)
  (let ((current (current-buffer)))
    (dolist (buffer (buffer-list))
      (unless (or (eq buffer current)
                  (seq-some (lambda (pattern) 
                              (string-match-p pattern (buffer-name buffer)))
                            normal/buffer-cleanup-exclude))
        (kill-buffer buffer)))
    (message "Killed other non-essential buffers")))

(defun normal/kill-matching-buffers (regexp &optional internal-too)
  "Kill buffers whose name matches the specified REGEXP.
With prefix arg, kill internal buffers too."
  (interactive 
   (list (read-string "Kill buffers matching: ")
         current-prefix-arg))
  (let ((count 0))
    (dolist (buffer (buffer-list))
      (let ((name (buffer-name buffer)))
        (when (and name
                   (not (string-equal name ""))
                   (or internal-too
                       (not (string-match-p "^\\*" name)))
                   (string-match regexp name))
          (kill-buffer buffer)
          (setq count (1+ count)))))
    (message "Killed %d buffer(s)" count)))

(defun normal/kill-dired-buffers ()
  "Kill all dired buffers."
  (interactive)
  (normal/kill-buffers-by-mode 'dired-mode))

(defun normal/kill-buffers-by-mode (mode)
  "Kill all buffers with major mode MODE."
  (interactive 
   (list (intern (completing-read "Kill buffers in mode: "
                                  (delete-dups 
                                   (mapcar (lambda (buffer)
                                             (buffer-local-value 'major-mode buffer))
                                           (buffer-list)))))))
  (let ((count 0))
    (dolist (buffer (buffer-list))
      (when (eq (buffer-local-value 'major-mode buffer) mode)
        (kill-buffer buffer)
        (setq count (1+ count))))
    (message "Killed %d buffer(s) in %s" count mode)))

(defun normal/kill-buried-buffers ()
  "Kill buffers that haven't been displayed recently."
  (interactive)
  (let ((count 0)
        (buffers-by-last-viewed nil))
    
    ;; Group buffers by last viewed time
    (dolist (buffer (buffer-list))
      (let ((time (or (buffer-local-value 'normal/last-viewed-time buffer) 0)))
        (push (cons buffer time) buffers-by-last-viewed)))
    
    ;; Sort by time, oldest first
    (setq buffers-by-last-viewed 
          (sort buffers-by-last-viewed (lambda (a b) (< (cdr a) (cdr b)))))
    
    ;; Kill the oldest half that aren't excluded
    (let* ((to-consider 
            (seq-filter 
             (lambda (buf-time)
               (let ((buf (car buf-time)))
                 (not (or (buffer-modified-p buf)
                          (get-buffer-window buf t)
                          (seq-some 
                           (lambda (pattern) 
                             (string-match-p pattern (buffer-name buf)))
                           normal/buffer-cleanup-exclude)))))
             buffers-by-last-viewed))
           (to-kill (seq-take to-consider (/ (length to-consider) 2))))
      
      (dolist (buf-time to-kill)
        (kill-buffer (car buf-time))
        (setq count (1+ count))))
    
    (message "Killed %d buried buffer(s)" count)))

;; Buffer grouping function
(defun normal/switch-to-buffer-group ()
  "Switch to a buffer within a specific group."
  (interactive)
  (let* ((modes (delete-dups 
                 (mapcar (lambda (buffer)
                           (buffer-local-value 'major-mode buffer))
                         (buffer-list))))
         (mode (intern 
                (completing-read "Switch to buffer in mode: " 
                                 (mapcar #'symbol-name modes)
                                 nil t)))
         (buffers (seq-filter 
                   (lambda (buffer)
                     (eq (buffer-local-value 'major-mode buffer) mode))
                   (buffer-list)))
         (names (mapcar #'buffer-name buffers))
         (selection (completing-read (format "Switch to %s buffer: " mode) names nil t)))
    (when selection
      (switch-to-buffer selection))))

;; Initialize buffer management
(normal/setup-buffer-management)


(provide 'buffer-management)
;;; buffer-management.el ends here
