;;; development.el --- Development tools -*- lexical-binding: t; -*-
;;; Commentary:
;; This file contains configurations for various development tools and language
;; support in Emacs.

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

;; Treemacs configuration moved to ide-config.el

;; Flycheck for syntax checking
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :diminish flycheck-mode)

;; LSP mode for language server support
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook ((python-mode . lsp-deferred)
         (js-mode . lsp-deferred)
         (php-mode . lsp-deferred)
         (web-mode . lsp-deferred)) ; Hook for web-mode
  :init
  ;; -- Performance Settings --
  ;; Increase garbage collection threshold during startup and file loads
  (setq gc-cons-threshold (* 100 1024 1024)) ; 100 MB
  ;; Increase amount of data read from processes
  (setq read-process-output-max (* 1024 1024)) ; 1 MB
  ;; Adjust delay before LSP actions trigger after idle (default is 0.5)
  (setq lsp-idle-delay 0.6)
  ;; Ensure logging is off for performance (it's nil by default, but good to be explicit)
  (setq lsp-log-io nil)
  ;; Set the LSP keymap prefix
  (setq lsp-keymap-prefix "C-c l")
  ;; -- End Performance Settings --
  :config
  (lsp-enable-which-key-integration t))

;; LSP UI enhancements
(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

;; Yasnippet for code templates
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))

;; Collection of snippets
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;; Language specific setup
;; Python
(use-package python
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :custom
  (python-indent-offset 4))

;; JavaScript/TypeScript
(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :custom
  (js2-basic-offset 2))

;; PHP
(use-package php-mode
  :ensure t
  :mode "\\.php\\'")

;; Web development (including Blade)
(use-package web-mode
 :ensure t
 :mode (("\\.html\\'" . web-mode)
        ("\\.css\\'" . web-mode)
        ("\\.jsx\\'" . web-mode)
        ("\\.tsx\\'" . web-mode)
        ("\\.blade\\.php\\'" . web-mode)) ; Added Blade support via web-mode
 :custom
 (web-mode-markup-indent-offset 2)
 (web-mode-css-indent-offset 2)
 (web-mode-code-indent-offset 2)
 ;; Optional: Ensure PHP engine is used for PHP blocks within web-mode
 (web-mode-engines-alist '(("php" . "\\.blade\\.php\\'"))))

;; C/C++ mode with LSP
(use-package cc-mode
  :ensure t
  :mode ("\\.c\\'" "\\.h\\'" "\\.cpp\\'" "\\.hpp\\'")
  :hook ((c-mode . lsp-deferred)
         (c++-mode . lsp-deferred)
         (c-mode . my/enable-lsp-format-on-save)
         (c++-mode . my/enable-lsp-format-on-save)))

;; Modern C++ features
(use-package modern-cpp-font-lock
  :ensure t
  :hook (c++-mode . modern-c++-font-lock-mode))

;; Go mode support
(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :hook ((go-mode . lsp-deferred)
         (go-mode . my/enable-lsp-format-on-save))
  :config
  ;; Set up before-save hooks to format buffer and add/delete imports
  (defun my/go-before-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'my/go-before-save-hooks))

(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'"
  :hook (lua-ts-mode . lsp-deferred)) ; Use lsp-deferred to start LSP server


(provide 'development)
;;; development.el ends here
