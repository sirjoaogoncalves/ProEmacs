;;; dashboard-config.el --- Simple Dashboard -*- lexical-binding: t; -*-

;;; Commentary:
;; Simplified dashboard configuration with only recent files

;;; Code:

;; Dashboard configuration
(defun normal/configure-dashboard ()
  "Configure dashboard with recent files."
  (setq dashboard-center-content t
        dashboard-set-heading-icons nil
        dashboard-set-file-icons nil
        dashboard-set-navigator nil
        dashboard-set-init-info nil
        dashboard-set-footer t
        ;; Only show recent files
        dashboard-items '((recents . 5)) ; Displaying up to 5 recent files
        dashboard-footer-messages
        (list (format "Emacs v%s • %d packages • Init: %.2fs"
                     emacs-version
                     (length package-activated-list)
                     (float-time (time-subtract after-init-time before-init-time))))))

;; Setup Dashboard
(use-package dashboard
  :init
  ;; Configure dashboard before loading
  (setq dashboard-footer-icon "")

  :config
  ;; Apply the configuration
  (normal/configure-dashboard)

  ;; Set up dashboard hooks
  (dashboard-setup-startup-hook)

  ;; Ensure the init info is completely empty
  (setq dashboard-init-info ""))

;; Refresh dashboard after all other initialization
(add-hook 'window-setup-hook 'dashboard-refresh-buffer -90)

(provide 'dashboard-config)
;;; dashboard-config.el ends here
