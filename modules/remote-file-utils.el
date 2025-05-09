;;; remote-file-utils.el --- Utilities for handling remote files -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains functions for working with remote files,
;; specifically a function to "download" the current buffer
;; to a local file path when editing a remote file via Tramp.

;;; Code:

(require 'tramp) ;; Ensure Tramp is loaded

(defun my/download-buffer-locally ()
  "Save the current buffer's content to a local file path.
This is useful when editing a remote file via Tramp and you
want to save a local copy."
  (interactive)
  (let ((current-file (buffer-file-name)))
    (if (null current-file)
        ;; If the buffer is not visiting a file
        (message "Current buffer is not visiting a file.")
      (if (file-remote-p current-file)
          ;; If the file is remote (using Tramp)
          (let* ((remote-file-name (file-name-nondirectory current-file))
                 ;; Suggest saving in the user's home directory
                 (default-local-path (expand-file-name remote-file-name "~"))
                 ;; Prompt the user for the local file path
                 (local-file-path (read-file-name (format "Save '%s' locally to: " remote-file-name)
                                                  nil ;; DIR: Use nil to let read-file-name handle default-directory logic
                                                  default-local-path ;; DEFAULT: Default path to suggest
                                                  t ;; REQUIRE-CONFIRM: Require confirmation if file exists
                                                  nil ;; DEF-HIST: Use default history
                                                  (file-name-nondirectory current-file) ;; INITIAL: Initial input string for the prompt
                                                  nil))) ;; PREDICATE: No predicate function needed
            (when local-file-path
              (condition-case err
                  (progn
                    ;; Use write-file to save the buffer content to the local path
                    (write-file local-file-path)
                    (message "Buffer successfully downloaded to %s" local-file-path))
                (error
                 (message "Error downloading file: %s" (error-message err))))))
        ;; If the file is already local
        (message "Current buffer is already a local file: %s" current-file)))))

(provide 'remote-file-utils)
;;; remote-file-utils.el ends here
