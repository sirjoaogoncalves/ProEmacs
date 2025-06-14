;;; docker-integration.el --- Docker workflow integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Lightweight Docker integration focused on common development workflows
;; Prioritizes speed and ergonomics over comprehensive Docker GUI

;;; Code:

(require 'json)

;; Core Docker functionality
(defgroup docker-integration nil
  "Docker integration for ProEmacs."
  :group 'tools)

(defcustom docker-integration-shell "/bin/bash"
  "Default shell to use when connecting to containers."
  :type 'string
  :group 'docker-integration)

(defcustom docker-integration-auto-refresh t
  "Whether to auto-refresh container lists."
  :type 'boolean
  :group 'docker-integration)

;; Docker commands with error handling
(defun docker/run-command (command &rest args)
  "Run docker COMMAND with ARGS and return output."
  (let ((cmd (mapconcat 'identity (cons "docker" (cons command args)) " ")))
    (condition-case err
        (string-trim (shell-command-to-string cmd))
      (error
       (message "Docker command failed: %s" (error-message-string err))
       nil))))

(defun docker/run-command-async (command callback &rest args)
  "Run docker COMMAND with ARGS asynchronously and call CALLBACK with result."
  (let ((cmd (mapconcat 'identity (cons "docker" (cons command args)) " ")))
    (let ((process (start-process-shell-command "docker-async" nil cmd)))
      (set-process-sentinel
       process
       (lambda (proc event)
         (when (string= event "finished\n")
           (with-current-buffer (process-buffer proc)
             (funcall callback (buffer-string)))))))))

;; Container operations
(defun docker/list-containers (&optional all)
  "List Docker containers. If ALL is non-nil, include stopped containers."
  (let* ((args (if all '("ps" "-a" "--format" "table {{.Names}}\t{{.Status}}\t{{.Image}}\t{{.Ports}}")
                 '("ps" "--format" "table {{.Names}}\t{{.Status}}\t{{.Image}}\t{{.Ports}}")))
         (output (apply #'docker/run-command args)))
    (when output
      (split-string output "\n"))))

(defun docker/container-running-p (container)
  "Check if CONTAINER is running."
  (let ((status (docker/run-command "ps" "-q" "-f" (format "name=%s" container))))
    (not (string-empty-p status))))

(defun docker/start-container (container)
  "Start CONTAINER."
  (interactive (list (docker/select-container nil)))
  (let ((result (docker/run-command "start" container)))
    (if result
        (message "Started container: %s" container)
      (message "Failed to start container: %s" container))))

(defun docker/stop-container (container)
  "Stop CONTAINER."
  (interactive (list (docker/select-container t)))
  (let ((result (docker/run-command "stop" container)))
    (if result
        (message "Stopped container: %s" container)
      (message "Failed to stop container: %s" container))))

(defun docker/restart-container (container)
  "Restart CONTAINER."
  (interactive (list (docker/select-container t)))
  (let ((result (docker/run-command "restart" container)))
    (if result
        (message "Restarted container: %s" container)
      (message "Failed to restart container: %s" container))))

(defun docker/remove-container (container)
  "Remove CONTAINER (with confirmation)."
  (interactive (list (docker/select-container nil)))
  (when (yes-or-no-p (format "Really remove container %s? " container))
    (let ((result (docker/run-command "rm" "-f" container)))
      (if result
          (message "Removed container: %s" container)
        (message "Failed to remove container: %s" container)))))

;; Container logs and shell access
(defun docker/container-logs (container &optional follow)
  "Show logs for CONTAINER. If FOLLOW is non-nil, follow logs."
  (interactive (list (docker/select-container t)
                     current-prefix-arg))
  (let* ((buffer-name (format "*Docker Logs: %s*" container))
         (buffer (get-buffer-create buffer-name))
         (args (if follow '("logs" "-f") '("logs" "--tail" "100"))))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (if follow
          ;; For following logs, use async process
          (let ((process (apply #'start-process "docker-logs" buffer "docker" (append args (list container)))))
            (set-process-filter process
                               (lambda (proc string)
                                 (with-current-buffer (process-buffer proc)
                                   (goto-char (point-max))
                                   (insert string))))
            (message "Following logs for %s (press C-c C-c to stop)" container))
        ;; For static logs, get them synchronously
        (let ((logs (apply #'docker/run-command (append args (list container)))))
          (when logs
            (insert logs))))
      (setq buffer-read-only t)
      (goto-char (point-max))
      (display-buffer buffer))))

(defun docker/container-shell (container)
  "Open shell in CONTAINER."
  (interactive (list (docker/select-container t)))
  (let ((default-directory "/")
        (shell-cmd (format "docker exec -it %s %s" container docker-integration-shell)))
    (term shell-cmd)))

(defun docker/container-inspect (container)
  "Inspect CONTAINER configuration."
  (interactive (list (docker/select-container t)))
  (let* ((buffer-name (format "*Docker Inspect: %s*" container))
         (buffer (get-buffer-create buffer-name))
         (config (docker/run-command "inspect" container)))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (when config
        (insert config)
        (json-mode)
        (json-pretty-print-buffer))
      (setq buffer-read-only t)
      (goto-char (point-min))
      (display-buffer buffer))))

;; Docker Compose operations
(defun docker/compose-up (&optional detached)
  "Run docker-compose up. If DETACHED is non-nil, run in background."
  (interactive "P")
  (let* ((compose-file (docker/find-compose-file))
         (args (if detached '("compose" "up" "-d") '("compose" "up")))
         (default-directory (file-name-directory compose-file)))
    (if detached
        (let ((result (apply #'docker/run-command args)))
          (message "Docker Compose started in background"))
      (let ((buffer-name "*Docker Compose Up*")
            (buffer (get-buffer-create "*Docker Compose Up*")))
        (with-current-buffer buffer
          (erase-buffer))
        (start-process "docker-compose" buffer "docker" "compose" "up")
        (display-buffer buffer)))))

(defun docker/compose-down ()
  "Run docker-compose down."
  (interactive)
  (let* ((compose-file (docker/find-compose-file))
         (default-directory (file-name-directory compose-file))
         (result (docker/run-command "compose" "down")))
    (message "Docker Compose services stopped")))

(defun docker/compose-logs (service)
  "Show logs for docker-compose SERVICE."
  (interactive (list (docker/select-compose-service)))
  (let* ((compose-file (docker/find-compose-file))
         (default-directory (file-name-directory compose-file))
         (buffer-name (format "*Docker Compose Logs: %s*" service))
         (buffer (get-buffer-create buffer-name))
         (logs (docker/run-command "compose" "logs" "--tail" "100" service)))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (when logs
        (insert logs))
      (setq buffer-read-only t)
      (goto-char (point-max))
      (display-buffer buffer))))

;; Image management
(defun docker/list-images ()
  "List Docker images."
  (let ((output (docker/run-command "images" "--format" "table {{.Repository}}\t{{.Tag}}\t{{.Size}}\t{{.CreatedAt}}")))
    (when output
      (split-string output "\n"))))

(defun docker/remove-image (image)
  "Remove Docker IMAGE."
  (interactive (list (docker/select-image)))
  (when (yes-or-no-p (format "Really remove image %s? " image))
    (let ((result (docker/run-command "rmi" image)))
      (if result
          (message "Removed image: %s" image)
        (message "Failed to remove image: %s" image)))))

(defun docker/pull-image (image)
  "Pull Docker IMAGE."
  (interactive "sImage name: ")
  (docker/run-command-async
   "pull"
   (lambda (result) (message "Pulled image: %s" image))
   image))

;; System operations
(defun docker/system-prune ()
  "Clean up Docker system (remove unused containers, networks, images)."
  (interactive)
  (when (yes-or-no-p "This will remove all unused Docker objects. Continue? ")
    (docker/run-command-async
     "system"
     (lambda (result) (message "Docker system cleanup completed"))
     "prune" "-f")))

(defun docker/system-info ()
  "Show Docker system information."
  (interactive)
  (let* ((buffer-name "*Docker System Info*")
         (buffer (get-buffer-create buffer-name))
         (info (docker/run-command "system" "info")))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (when info
        (insert info))
      (setq buffer-read-only t)
      (goto-char (point-min))
      (display-buffer buffer))))

;; Helper functions for selection
(defun docker/select-container (running-only)
  "Select a container interactively. If RUNNING-ONLY, show only running containers."
  (let* ((containers (docker/list-containers (not running-only)))
         (container-names (mapcar (lambda (line)
                                   (car (split-string line "\t")))
                                 (cdr containers))))  ;; Skip header
    (completing-read "Container: " container-names nil t)))

(defun docker/select-image ()
  "Select an image interactively."
  (let* ((images (docker/list-images))
         (image-names (mapcar (lambda (line)
                               (let ((parts (split-string line "\t")))
                                 (concat (car parts) ":" (cadr parts))))
                             (cdr images))))  ;; Skip header
    (completing-read "Image: " image-names nil t)))

(defun docker/select-compose-service ()
  "Select a docker-compose service interactively."
  (let* ((compose-file (docker/find-compose-file))
         (default-directory (file-name-directory compose-file))
         (services-output (docker/run-command "compose" "ps" "--services"))
         (services (when services-output (split-string services-output "\n"))))
    (if services
        (completing-read "Service: " services nil t)
      (error "No docker-compose services found"))))

(defun docker/find-compose-file ()
  "Find docker-compose.yml file in current directory or parents."
  (let ((compose-files '("docker-compose.yml" "docker-compose.yaml" "compose.yml" "compose.yaml")))
    (or (seq-find #'file-exists-p compose-files)
        (let ((parent (file-name-directory (directory-file-name default-directory))))
          (unless (string= parent default-directory)
            (let ((default-directory parent))
              (docker/find-compose-file))))
        (error "No docker-compose file found"))))

;; Docker dashboard
(defun docker/dashboard ()
  "Show Docker dashboard with containers and images."
  (interactive)
  (let* ((buffer-name "*Docker Dashboard*")
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)

      ;; Header
      (insert (propertize "Docker Dashboard\n" 'face 'bold))
      (insert (make-string 50 ?─) "\n\n")

      ;; Running containers
      (insert (propertize "Running Containers:\n" 'face 'bold))
      (let ((containers (docker/list-containers)))
        (if containers
            (dolist (container (cdr containers))  ;; Skip header
              (insert (format "  %s\n" container)))
          (insert "  No running containers\n")))
      (insert "\n")

      ;; Images
      (insert (propertize "Images:\n" 'face 'bold))
      (let ((images (docker/list-images)))
        (if images
            (dolist (image (seq-take (cdr images) 5))  ;; Show first 5, skip header
              (insert (format "  %s\n" image)))
          (insert "  No images\n")))
      (insert "\n")

      ;; Commands help
      (insert (propertize "Commands:\n" 'face 'bold))
      (insert "  SPC C C  - Dashboard\n")
      (insert "  SPC C s  - Start container\n")
      (insert "  SPC C S  - Stop container\n")
      (insert "  SPC C r  - Restart container\n")
      (insert "  SPC C l  - Container logs\n")
      (insert "  SPC C x  - Container shell\n")
      (insert "  SPC C u  - Compose up\n")
      (insert "  SPC C D  - Compose down\n")

      (setq buffer-read-only t)
      (goto-char (point-min))
      (display-buffer buffer))))

;; Dockerfile mode enhancements
(use-package dockerfile-mode
  :mode "Dockerfile\\'"
  :config
  ;; Enhanced dockerfile editing
  (defun dockerfile/build-image ()
    "Build Docker image from current Dockerfile."
    (interactive)
    (let* ((dockerfile (buffer-file-name))
           (directory (file-name-directory dockerfile))
           (image-name (read-string "Image name: " (file-name-nondirectory directory)))
           (default-directory directory))
      (docker/run-command-async
       "build"
       (lambda (result) (message "Built image: %s" image-name))
       "-t" image-name ".")))

  (define-key dockerfile-mode-map (kbd "C-c C-b") #'dockerfile/build-image))

;; Performance optimizations
(setq docker-integration--cache-timeout 30)
(defvar docker-integration--container-cache nil)
(defvar docker-integration--cache-time nil)

(defun docker/cached-containers ()
  "Get containers with caching for better performance."
  (when (or (null docker-integration--cache-time)
            (> (- (float-time) docker-integration--cache-time) docker-integration--cache-timeout))
    (setq docker-integration--container-cache (docker/list-containers t))
    (setq docker-integration--cache-time (float-time)))
  docker-integration--container-cache)

(provide 'docker-integration)
;;; docker-integration.el ends here
