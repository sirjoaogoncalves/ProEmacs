;;; init.el --- Main configuration loader -*- lexical-binding: t; -*-

;;; Code:

(setq large-file-warning-threshold (* 100 1024 1024))

;; Add load paths FIRST
(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Create var directory
(let ((var-dir (expand-file-name "var" user-emacs-directory)))
  (unless (file-exists-p var-dir)
    (make-directory var-dir t)))

;; Disable process manager entirely
(setq normal/process-manager-timer nil)

;; Load core configuration in proper order
(require 'packages)    ; This sets up package system and use-package
(require 'utils)       ; Load utilities AFTER packages are ready
(require 'defaults)
(require 'ui)
(require 'keybindings) ; This now works because utils is loaded

;; Load Evil mode configuration
(require 'evil-config)

;; Load completion and search
(require 'unicode-complete-fix)
(require 'completion)
(require 'advanced-search)

;; Load development tools
(require 'development)
(require 'git)
(require 'format-utils)

;; Load AI and automation
(require 'ai)
(require 'ai-enhanced-coding)

;; Load interface and organization
(require 'dashboard-config)
(require 'dired-config)
(require 'window-config)
(require 'org-config)
(require 'terminal)

;; Load system utilities
(require 'performance)
(require 'buffer-management)
(require 'modern-features)
(require 'process-manager)
(require 'remote-file-utils)

;; Load specialized modules
(require 'docker-integration)

;; Load custom configuration if it exists
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Startup performance reporting and GC reset
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216)
            (let ((startup-time (float-time (time-subtract after-init-time before-init-time))))
              (message "Emacs loaded in %.2f seconds with %d garbage collections."
                       startup-time gcs-done)
              ;; Display package statistics if available
              (when (and (fboundp 'use-package-statistics)
                         (> (length use-package-statistics) 0))
                (run-with-idle-timer 2 nil
                  (lambda ()
                    (let ((slow-packages (seq-take
                                         (sort (copy-sequence use-package-statistics)
                                               (lambda (a b) (> (nth 2 a) (nth 2 b))))
                                         5)))
                      (message "Slowest packages: %s"
                               (string-join
                                (mapcar (lambda (pkg)
                                         (format "%s (%.2fms)"
                                                (car pkg)
                                                (* 1000 (nth 2 pkg))))
                                       slow-packages)
                                ", ")))))))))

;; Set AI coding default provider
(with-eval-after-load 'ai-enhanced-coding
  (setq ai-coding-default-provider 'local))

(provide 'init)
;;; init.el ends here
