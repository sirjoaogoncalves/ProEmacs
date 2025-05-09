;;; defaults.el --- Better default settings -*- lexical-binding: t; -*-
;;; Commentary:
;; This file contains better default settings for Emacs, including
;; file handling, display, scrolling, and history.

;;; Code:
;; Prevent Emacs from creating backup files
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

;; Don't show the startup screen
(setq inhibit-startup-message t)

;; Line numbers
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)

;; Allow minibuffer commands while in the minibuffer
(setq enable-recursive-minibuffers t)

;; Better scrolling
(setq scroll-margin 3
      scroll-conservatively 101
      scroll-preserve-screen-position t)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Silence bell
(setq ring-bell-function 'ignore)

;; Set UTF-8 as default
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; Recent files
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

;; Save history
(savehist-mode 1)
(setq history-length 25)

;; Remember cursor position in files
(save-place-mode 1)

;; Don't stretch the cursor to fit the glyph
(setq x-stretch-cursor nil)

;; Indentation
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; Always show matching parentheses
(show-paren-mode 1)

;; Highlight current line
(global-hl-line-mode t)

;; Make frame title more useful
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; More room for messages
(setq message-log-max 10000)

;; Store all backup and autosave files in one place
(setq backup-directory-alist
      `((".*" . ,(expand-file-name "backups/" user-emacs-directory))))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save-list/" user-emacs-directory) t)))

;; Better performance
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Don't show special buffers in Tab Bar
(setq tab-bar-exclude-mode-regexps 
      '("^\\*\\(scratch\\|Messages\\|Warnings\\|dashboard\\|Completions\\)\\*$"))

;; Don't show scratch buffer at startup
(setq initial-scratch-message nil)

;; Control which buffers show up in the buffer list
(setq display-buffer-alist
      '(("\\*scratch\\|\\*Messages\\|\\*Warnings\\*"
         (display-buffer-no-window)
         (allow-no-window . t))))

(provide 'defaults)
;;; defaults.el ends here
