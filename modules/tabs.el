;;; tabs.el --- Tab system configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for Emacs tab-bar-mode, excluding certain buffers.

;;; Code:

;; Enable tab bar mode
(unless (version< emacs-version "27.1")
  (tab-bar-mode 1))

;; Function to filter buffers from the tab bar
(defun my/tab-bar-buffer-filter (buffer)
  "Return non-nil if BUFFER should be shown in the tab bar."
  (let ((name (buffer-name buffer))
        (mode (buffer-local-value 'major-mode buffer)))
    (and
     ;; Exclude specific named buffers
     (not (or (string= name "*dashboard*")
              (string= name "*Messages*")
              (string= name "*scratch*")
              (string= name "*Completions*") ;; Exclude completions buffer
              (string= name "*ielm*") ;; Exclude ielm buffer if you use it
              (string= name "*Ephemeral scratch*") ;; Another common scratch buffer
              (string-prefix-p " *Ephemera" name) ;; Other ephemera buffers
              ))
     ;; Exclude buffers whose names start and end with '*' (most special buffers)
     (not (and (string-prefix-p "*" name)
               (string-suffix-p "*" name)
               ;; Add exceptions here if you want to include specific *buffers*
               ;; that you consider important for tabs, e.g.,
               ;; (not (string= name "*Dired*"))
               )))))

;; Set the filter function
(setq tab-bar-buffer-list-filter #'my/tab-bar-buffer-filter)

;; Optional: Customize how new tabs are created or switched
;; (setq tab-bar-new-button-show t)
;; (setq tab-bar-close-button-show t)
;; (setq tab-bar-show nil) ;; Hide tab bar when only one tab
;; (setq tab-bar-format nil) ;; Customize tab bar display

;; Provide this file
(provide 'tabs)
;;; tabs.el ends here
