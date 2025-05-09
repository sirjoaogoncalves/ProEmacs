;;; neotree-config.el --- Neotree file browser configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; A NERDtree-like file browser for Emacs using neotree

;;; Code:

(use-package neotree
  :commands (neotree-toggle neotree-show neotree-find)
  :custom
  ;; General settings
  (neo-window-width 35)
  (neo-create-file-auto-open t)
  (neo-banner-message nil)
  (neo-show-hidden-files t)
  (neo-mode-line-type 'none)
  (neo-auto-indent-point t)
  ;; IMPORTANT: Don't use persist or it will remember paths
  (neo-persist-show nil)
  ;; Use current directory by default rather than project root
  (neo-smart-open nil)
  
  :config
  ;; Theme
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  
  ;; Make Neotree's window non-resizable
  (setq neo-window-fixed-size t)
  
  ;; Close neotree when a file is opened
  (setq neo-autoclose t)

  ;; Function to open neotree in current directory
  (defun neotree-open-in-current-dir ()
    "Open neotree in the current directory instead of project root."
    (interactive)
    (let ((current-dir (or (when buffer-file-name
                             (file-name-directory buffer-file-name))
                           (when (eq major-mode 'dired-mode)
                             default-directory)
                           default-directory)))
      ;; Close current neotree if open
      (when (neo-global--window-exists-p)
        (neotree-hide))
      ;; Open in the current directory
      (neotree-show)
      (neotree-dir current-dir)
      (neotree-find current-dir)))
  
  ;; Function to reveal current file
  (defun neotree-reveal-current-file ()
    "Show and find current file in neotree."
    (interactive)
    (if buffer-file-name
        (progn
          (neotree-show)
          (neotree-find buffer-file-name))
      (message "No file for current buffer")))
  
  ;; Make neotree use evil navigation keys like NERDtree
  (with-eval-after-load 'evil
    (add-hook 'neotree-mode-hook
              (lambda ()
                (evil-define-key 'normal neotree-mode-map
                  (kbd "RET") 'neotree-enter
                  (kbd "q") 'neotree-hide
                  (kbd "r") 'neotree-refresh
                  (kbd "a") 'neotree-create-node
                  (kbd "d") 'neotree-delete-node
                  (kbd "c") 'neotree-copy-node
                  (kbd "m") 'neotree-rename-node
                  (kbd "h") 'neotree-select-up-node
                  (kbd "l") 'neotree-enter
                  (kbd "s") 'neotree-enter-vertical-split
                  (kbd "v") 'neotree-enter-horizontal-split
                  (kbd "?") 'describe-mode)))))

;; Hide line numbers in neotree
(add-hook 'neotree-mode-hook
          (lambda ()
            (display-line-numbers-mode -1)))


;; Add projectile integration
(with-eval-after-load 'projectile
  (defun projectile-find-dir-in-neotree ()
    "Open a projectile project in neotree."
    (interactive)
    (let ((project-root (projectile-project-root)))
      (if project-root
          (progn
            (neotree-show)
            (neotree-dir project-root))
        (message "Not in a project")))))

(provide 'neotree-config)
;;; neotree-config.el ends here
