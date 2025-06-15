;;; dashboard-config.el --- Dashboard configuration -*- lexical-binding: t; -*-

;;; Code:

(defun normal/configure-dashboard ()
  "Configure dashboard with recent files."
  (setq dashboard-center-content t
        dashboard-set-heading-icons nil
        dashboard-set-file-icons nil
        dashboard-set-navigator nil
        dashboard-set-init-info nil
        dashboard-set-footer t
        dashboard-items '((recents . 5))
        dashboard-footer-messages
        (list (format "Emacs v%s • %d packages • Init: %.2fs"
                     emacs-version
                     (length package-activated-list)
                     (float-time (time-subtract after-init-time before-init-time))))))

(use-package dashboard
  :init
  (setq dashboard-footer-icon "")
  :config
  (normal/configure-dashboard)
  (dashboard-setup-startup-hook)
  (setq dashboard-init-info ""))

(add-hook 'window-setup-hook 'dashboard-refresh-buffer -90)

(provide 'dashboard-config)
;;; dashboard-config.el ends here
