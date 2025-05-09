;;; threading-utils.el --- Worker thread system -*- lexical-binding: t; -*-

;;; Commentary:
;; This module provides a thread pool system for running tasks in background threads
;; Requires Emacs 28+ for threading capabilities

;;; Code:

;; Check if threading is available
(defconst normal/threading-available (and (fboundp 'make-thread)
                                         (>= emacs-major-version 28))
  "Whether threading is available in this Emacs build.")

;; Thread pool configuration
(defgroup normal/threading nil
  "Pro Emacs threading utilities."
  :group 'applications)

(defcustom normal/thread-pool-size
  (if (fboundp 'num-processors)
      (max 2 (- (num-processors) 1))  ;; N-1 cores
    4)  ;; Default to 4 if we can't detect
  "Number of worker threads in the thread pool."
  :type 'integer
  :group 'normal/threading)

(defcustom normal/thread-idle-timeout 300
  "Seconds of inactivity after which idle worker threads are terminated."
  :type 'integer
  :group 'normal/threading)

;; Thread pool state
(defvar normal/thread-pool nil
  "List of active worker threads.")

(defvar normal/thread-pool-queue nil
  "Queue of pending tasks.")

(defvar normal/thread-pool-running-p nil
  "Whether the thread pool is currently running.")

(defvar normal/thread-results (make-hash-table :test 'equal)
  "Hash table storing results of thread tasks by task ID.")

;; Mutex for thread synchronization
(defvar normal/thread-pool-mutex (when normal/threading-available
                                 (make-mutex "pro-thread-pool"))
  "Mutex for thread pool synchronization.")

;; Task ID counter
(defvar normal/thread-task-counter 0
  "Counter for generating unique task IDs.")

;; Initialize the thread pool
(defun normal/thread-pool-init ()
  "Initialize the thread pool."
  (interactive)
  (when normal/threading-available
    (unless normal/thread-pool-running-p
      (setq normal/thread-pool-queue (make-queue))
      (setq normal/thread-pool (make-list normal/thread-pool-size nil))
      (setq normal/thread-pool-running-p t)
      (dotimes (i normal/thread-pool-size)
        (setf (nth i normal/thread-pool)
              (make-thread #'normal/thread-worker
                          (format "pro-worker-%d" i))))
      (message "Thread pool initialized with %d workers" normal/thread-pool-size))))

;; Stop the thread pool
(defun normal/thread-pool-stop ()
  "Stop the thread pool."
  (interactive)
  (when normal/threading-available
    (when normal/thread-pool-running-p
      (setq normal/thread-pool-running-p nil)
      (dolist (thread normal/thread-pool)
        (when (thread-live-p thread)
          (thread-signal thread 'quit nil)))
      (setq normal/thread-pool nil)
      (message "Thread pool stopped"))))

;; Thread worker function
(defun normal/thread-worker ()
  "Worker function run by each thread in the pool."
  (let ((task nil))
    (while normal/thread-pool-running-p
      (with-mutex normal/thread-pool-mutex
        (unless (queue-empty-p normal/thread-pool-queue)
          (setq task (queue-dequeue normal/thread-pool-queue))))
      
      (if task
          (progn
            (condition-case err
                (let* ((task-id (plist-get task :id))
                       (func (plist-get task :func))
                       (args (plist-get task :args))
                       (callback (plist-get task :callback))
                       (result (apply func args)))
                  ;; Store result
                  (with-mutex normal/thread-pool-mutex
                    (puthash task-id result normal/thread-results))
                  ;; Schedule callback on main thread if provided
                  (when callback
                    (run-with-timer 0 nil callback result task-id)))
              (error
               (let ((task-id (plist-get task :id))
                     (error-callback (plist-get task :error-callback)))
                 (with-mutex normal/thread-pool-mutex
                   (puthash task-id (cons 'error err) normal/thread-results))
                 (when error-callback
                   (run-with-timer 0 nil error-callback err task-id)))))
            (setq task nil))
        ;; No task, sleep for a bit
        (sleep-for 0.1)))))

;; Submit a task to the thread pool
(defun normal/thread-submit (func &rest args)
  "Submit FUNC with ARGS to be executed in a worker thread.
Returns a task ID that can be used to retrieve the result."
  (unless normal/threading-available
    (error "Threading is not available in this Emacs build"))
  
  (unless normal/thread-pool-running-p
    (normal/thread-pool-init))
  
  (with-mutex normal/thread-pool-mutex
    (let ((task-id (format "task-%d" (cl-incf normal/thread-task-counter))))
      (queue-enqueue normal/thread-pool-queue
                    (list :id task-id
                          :func func
                          :args args))
      task-id)))

;; Submit a task with a callback
(defun normal/thread-submit-with-callback (func callback &optional error-callback &rest args)
  "Submit FUNC with ARGS to be executed in a thread, with CALLBACK on completion.
If ERROR-CALLBACK is provided, it will be called with any errors.
Returns a task ID that can be used to track the task."
  (unless normal/threading-available
    (error "Threading is not available in this Emacs build"))
  
  (unless normal/thread-pool-running-p
    (normal/thread-pool-init))
  
  (with-mutex normal/thread-pool-mutex
    (let ((task-id (format "task-%d" (cl-incf normal/thread-task-counter))))
      (queue-enqueue normal/thread-pool-queue
                    (list :id task-id
                          :func func
                          :args args
                          :callback callback
                          :error-callback error-callback))
      task-id)))

;; Get the result of a task
(defun normal/thread-result (task-id)
  "Get the result of the task with TASK-ID.
Returns nil if the task is not completed yet."
  (gethash task-id normal/thread-results))

;; Wait for a task to complete
(defun normal/thread-wait (task-id &optional timeout)
  "Wait for the task with TASK-ID to complete.
Optional TIMEOUT in seconds after which we stop waiting.
Returns the result of the task, or nil if timeout."
  (let ((end-time (when timeout (+ (float-time) timeout)))
        result)
    (while (and (not (setq result (normal/thread-result task-id)))
                (or (null end-time) (< (float-time) end-time)))
      (sleep-for 0.01))
    result))

;; Example functions that benefit from threading

;; Recursive file listing in a thread
(defun normal/thread-find-files (directory pattern)
  "Find files in DIRECTORY matching PATTERN using a background thread.
Returns a task ID that can be used to retrieve the results."
  (normal/thread-submit
   (lambda (dir pat)
     (let ((default-directory dir)
           (case-fold-search t)
           result)
       (dolist (file (directory-files-recursively dir pat nil))
         (push file result))
       (nreverse result)))
   directory pattern))

;; Count lines in files using multiple threads
(defun normal/thread-count-lines-in-files (files)
  "Count total lines in FILES using worker threads.
Returns a task ID that can be used to retrieve the result."
  (when (null files)
    (error "No files provided"))
  
  ;; Split files among available threads
  (let* ((chunk-size (/ (length files) normal/thread-pool-size))
         (chunks (normal/split-list files (max 1 chunk-size)))
         (subtasks (mapcar (lambda (chunk)
                            (normal/thread-submit
                             (lambda (file-list)
                               (let ((total 0))
                                 (dolist (file file-list)
                                   (when (file-readable-p file)
                                     (with-temp-buffer
                                       (insert-file-contents file)
                                       (setq total (+ total (count-lines (point-min) (point-max)))))))
                                 total))
                             chunk))
                          chunks)))
    
    ;; Submit a task that waits for all subtasks and combines results
    (normal/thread-submit
     (lambda (task-ids)
       (let ((total 0))
         (dolist (id task-ids)
           (let ((result (normal/thread-wait id 30)))
             (when (numberp result)
               (setq total (+ total result)))))
         total))
     subtasks)))

;; Utility to split a list into chunks
(defun normal/split-list (list n)
  "Split LIST into chunks of size N."
  (let (result)
    (while list
      (push (seq-take list n) result)
      (setq list (seq-drop list n)))
    (nreverse result)))

;; Queue implementation (simple)
(defun make-queue ()
  "Create a new queue data structure."
  (cons nil nil))

(defun queue-empty-p (queue)
  "Check if QUEUE is empty."
  (null (car queue)))

(defun queue-enqueue (queue element)
  "Add ELEMENT to the end of QUEUE."
  (if (car queue)
      (setcdr (cdr queue) (cons element nil))
    (setcar queue (cons element nil)))
  (unless (cdr queue)
    (setcdr queue (car queue)))
  queue)

(defun queue-dequeue (queue)
  "Remove and return the first element of QUEUE."
  (unless (queue-empty-p queue)
    (prog1
        (cadr queue)
      (setcdr queue (cddr queue))
      (when (null (cdr queue))
        (setcar queue nil)))))

;; Initialize when loading
(when normal/threading-available
  (add-hook 'after-init-hook #'normal/thread-pool-init)
  (add-hook 'kill-emacs-hook #'normal/thread-pool-stop))

(provide 'threading-utils)
;;; threading-utils.el ends here
