;;; terminal.el -- Terminal support -*-
;;; Commentary:

;; This file provides terminal integration for Emacs.
;; It integrates with the built-in 'term' to provide a terminal emulator inside Emacs.

;; The previous configuration for vterm is not needed if you are
;; using the built-in 'term' exclusively for this functionality.
;; If you still want vterm available for other purposes, keep its block.

;; (use-package vterm
;;  :commands (vterm)
;;  :config
;;  ;; Use system's default shell
;;  (setq vterm-shell (getenv "SHELL"))
;;  ;; Set directory tracking
;;  (setq vterm-kill-buffer-on-exit t)
;;  ;; Set maximum scrollback
;;  (setq vterm-max-scrollback 10000))

;; Function to open terminal in current directory, split horizontally
;; Now uses the built-in 'term' which handles TRAMP remote directories.
(defun open-terminal-here ()
  "Open a terminal in the current directory using 'term'.
This function uses the built-in 'term' which automatically handles
opening the shell on a remote host when the current buffer's
default directory is a TRAMP path."
  (interactive)
  (let ((default-directory (or
                             ;; Use the directory of the current buffer if it has one
                             (when buffer-file-name
                               (file-name-directory buffer-file-name))
                             ;; Or use the current directory of dired
                             (when (eq major-mode 'dired-mode)
                               default-directory)
                             ;; Fall back to default-directory
                             default-directory)))
    (split-window-horizontally)
    (other-window 1)
    ;; Use the built-in 'term' command.
    ;; 'shell-file-name' is the command to run (usually the user's login shell).
    ;; 'term' automatically starts this command remotely via TRAMP
    ;; if default-directory is a remote path.
    (term shell-file-name))) ; Use 'term' with the default shell command

(provide 'terminal)
;;; terminal.el ends here
