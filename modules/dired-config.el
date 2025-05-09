;;; dired-config.el --- Enhanced directory editing -*- lexical-binding: t; -*-

;;; Commentary:
;; Comprehensive Dired enhancements for a better file management experience

;;; Code:

;; Basic built-in Dired improvements
(use-package dired
  :ensure nil  ;; dired is built-in
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom
  ;; Always delete and copy recursively
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  ;; Auto refresh dired when file changes
  (dired-auto-revert-buffer t)
  ;; Allow dired to delete or copy dirs
  (dired-allow-to-change-permissions t)
  :config
  ;; Use the same buffer for viewing directory
  (put 'dired-find-alternate-file 'disabled nil)
  
  ;; Use different switches based on OS
  (setq dired-listing-switches
        (if (eq system-type 'darwin)
            "-lahgG"
          "-lahgo --group-directories-first"))
  
  ;; Move files between split panes
  (setq dired-dwim-target t))

;; Colorful dired
(use-package diredfl
  :hook (dired-mode . diredfl-mode)
  :config
  ;; Use more subdued colors for better readability
  (set-face-attribute 'diredfl-dir-name nil :bold t))

;; Dired subtree - view subdirectories in-line
(use-package dired-subtree
  :after dired
  :bind (:map dired-mode-map
         ("<tab>" . dired-subtree-toggle)
         ("<backtab>" . dired-subtree-cycle))
  :custom
  (dired-subtree-use-backgrounds nil)
  (dired-subtree-line-prefix "  "))

;; Extra file operations
(use-package dired-aux
  :ensure nil
  :config
  ;; Define extra file handlers for better compression/archive support
  (setq dired-compress-files-alist
        '(("\\.tar\\.gz\\'" . "tar -cf - %i | gzip -c9 > %o")
          ("\\.tar\\.bz2\\'" . "tar -cf - %i | bzip2 -c9 > %o")
          ("\\.tar\\.xz\\'" . "tar -cf - %i | xz -c9 > %o")
          ("\\.zip\\'" . "zip -r9 %o %i"))))

;; Dired-x - additional Dired functionality
(use-package dired-x
  :ensure nil
  :hook (dired-mode . dired-omit-mode)
  :config
  ;; Omit dot files and backup/auto-save files
  (setq dired-omit-files
        (concat dired-omit-files "\\|^\\..+$\\|\\.bak$\\|\\.~$"))
  ;; Don't omit directories, even if they match omit rules
  (setq dired-omit-extensions
        (nconc dired-omit-extensions
               '(".pyc" ".elc" ".o" ".hi"))))

;; All-the-icons support for dired (if available)
(use-package all-the-icons-dired
  :if (and (display-graphic-p)
           (package-installed-p 'all-the-icons))
  :hook (dired-mode . all-the-icons-dired-mode))

;; Editable dired mode
(use-package wdired
  :ensure nil
  :after dired
  :bind (:map dired-mode-map
         ("e" . wdired-change-to-wdired-mode))
  :custom
  (wdired-allow-to-change-permissions t)
  (wdired-create-parent-directories t))

;; Functions to create/remove files and directories
(defun dired-create-empty-file (file)
  "Create an empty FILE."
  (interactive 
   (list (read-file-name "Create empty file: ")))
  (with-temp-buffer
    (write-file file))
  (revert-buffer))

(defun dired-do-touch ()
  "Touch marked files in dired."
  (interactive)
  (dolist (file (dired-get-marked-files))
    (shell-command (concat "touch " (shell-quote-argument file))))
  (revert-buffer))


(provide 'dired-config)
;;; dired-config.el ends here
