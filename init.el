;;; init.el --- Main configuration loader -*- lexical-binding: t; -*-

;; This file loads all the separate configuration files

;;; Commentary:

;;; Code:

(setq large-file-warning-threshold (* 100 1024 1024))

;; Add core and modules directories to load path
(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Create var directory if it doesn't exist
(let ((var-dir (expand-file-name "var" user-emacs-directory)))
  (unless (file-exists-p var-dir)
    (make-directory var-dir t)))

;; Load core configuration
(require 'packages)
(require 'defaults)
(require 'ui)
(require 'keybindings)

;; Load feature modules
(require 'evil-config)
(require 'completion)
(require 'dashboard-config)
(require 'development)
(require 'git)
(require 'ai)
(require 'terminal)
(require 'org-config)
(require 'dired-config)
(require 'window-config)
(require 'performance)
;; REMOVED: neotree-config requirement - this was causing icon function warnings
(require 'buffer-management)
(require 'modern-features)
(require 'process-manager)
(require 'format-utils)
(require 'remote-file-utils)

;; Load powerhouse modules
(require 'advanced-search)
(require 'docker-integration)
(require 'ai-enhanced-coding)

;; Load custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Reset GC threshold to reasonable value
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216) ;; 16mb
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(with-eval-after-load 'ai-enhanced-coding
  (setq ai-coding-default-provider 'local))

(provide 'init)
;;; init.el ends here
