;;; defaults.el --- Better default settings -*- lexical-binding: t; -*-

;;; Code:
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

(setq inhibit-startup-message t)

(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)

(setq enable-recursive-minibuffers t)

(setq scroll-margin 3
      scroll-conservatively 101
      scroll-preserve-screen-position t)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq ring-bell-function 'ignore)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

(savehist-mode 1)
(setq history-length 25)

(save-place-mode 1)

(setq x-stretch-cursor nil)

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(show-paren-mode 1)

(global-hl-line-mode t)

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(fset 'yes-or-no-p 'y-or-n-p)

(setq message-log-max 10000)

(setq backup-directory-alist
      `((".*" . ,(expand-file-name "backups/" user-emacs-directory))))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save-list/" user-emacs-directory) t)))

;; Increase process output buffer size for LSP performance
(setq read-process-output-max (* 1024 1024))

;; Exclude special buffers from tab bar
(setq tab-bar-exclude-mode-regexps
      '("^\\*\\(scratch\\|Messages\\|Warnings\\|dashboard\\|Completions\\)\\*$"))

(setq initial-scratch-message nil)

;; Prevent scratch/Messages buffers from showing in buffer list
(setq display-buffer-alist
      '(("\\*scratch\\|\\*Messages\\|\\*Warnings\\*"
         (display-buffer-no-window)
         (allow-no-window . t))))

(provide 'defaults)
;;; defaults.el ends here
