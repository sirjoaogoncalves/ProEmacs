;;; development.el --- Development tools -*- lexical-binding: t; -*-

;;; Code:

;; Projectile for project management
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (projectile-mode +1)
  :custom
  (projectile-completion-system 'default)
  :bind-keymap
  ("C-c p" . projectile-command-map))

;; Flycheck for syntax checking
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :diminish flycheck-mode)

;; Define function for enabling LSP format on save
(defun my/enable-lsp-format-on-save ()
  "Enable LSP format on save for current buffer."
  (add-hook 'before-save-hook #'lsp-format-buffer nil t))

;; LSP mode for language server support
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook ((python-mode . lsp-deferred)
         (js-mode . lsp-deferred)
         (php-mode . lsp-deferred)
         (web-mode . lsp-deferred)
         (c-mode . lsp-deferred)
         (c++-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (lua-mode . lsp-deferred)
         ;; Format on save hooks
         (python-mode . my/enable-lsp-format-on-save)
         (js-mode . my/enable-lsp-format-on-save)
         (php-mode . my/enable-lsp-format-on-save)
         (web-mode . my/enable-lsp-format-on-save)
         (c-mode . my/enable-lsp-format-on-save)
         (c++-mode . my/enable-lsp-format-on-save)
         (go-mode . my/enable-lsp-format-on-save)
         (lua-mode . my/enable-lsp-format-on-save))
  :init
  ;; Performance optimizations
  (setq gc-cons-threshold (* 100 1024 1024))
  (setq read-process-output-max (* 1024 1024))
  (setq lsp-idle-delay 0.6)
  (setq lsp-log-io nil)
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

;; Code snippets
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;; Language configurations

;; Python
(use-package python
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :custom (python-indent-offset 4))

;; JavaScript with js2-mode
(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :custom (js2-basic-offset 2))

;; PHP
(use-package php-mode
  :ensure t
  :mode "\\.php\\'")

;; Web mode for various web technologies
(use-package web-mode
  :ensure t
  :mode (("\\.html\\'" . web-mode)
         ("\\.css\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.blade\\.php\\'" . web-mode))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-engines-alist '(("php" . "\\.blade\\.php\\'"))))

;; C/C++ with cc-mode
(use-package cc-mode
  :ensure t
  :mode ("\\.c\\'" "\\.h\\'" "\\.cpp\\'" "\\.hpp\\'"))

;; Modern C++ font lock
(use-package modern-cpp-font-lock
  :ensure t
  :hook (c++-mode . modern-c++-font-lock-mode))

;; Go
(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :config
  (defun my/go-before-save-hooks ()
    "Setup Go-specific before-save hooks."
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'my/go-before-save-hooks))

;; Lua
(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'")

;; Additional development utilities

;; Enhanced syntax highlighting
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Highlight TODO keywords
(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode)
  :custom
  (hl-todo-keyword-faces
   '(("TODO"   . "#FF0000")
     ("FIXME"  . "#FF0000")
     ("DEBUG"  . "#A020F0")
     ("GOTCHA" . "#FF4500")
     ("NOTE"   . "#1E90FF"))))

;; Git integration
(use-package magit
  :ensure t
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package git-timemachine
  :ensure t
  :defer t)

(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :hook (prog-mode . git-gutter-mode)
  :custom
  (git-gutter:update-interval 0.02))

(use-package blamer
  :ensure t
  :defer t
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                   :background nil
                   :height 140
                   :italic t))))

;; Performance monitoring for development
(defun my/dev-performance-report ()
  "Show development-related performance metrics."
  (interactive)
  (let ((start-time (current-time)))
    (message "LSP servers: %d, Git status: %s, Flycheck: %s"
             (length (if (fboundp 'lsp-workspaces) (lsp-workspaces) '()))
             (if (and (fboundp 'magit-git-repo-p) (magit-git-repo-p default-directory)) "Yes" "No")
             (if (bound-and-true-p flycheck-mode) "Enabled" "Disabled"))
    (message "Development check took %.2f seconds"
             (float-time (time-subtract (current-time) start-time)))))

(provide 'development)
;;; development.el ends here
