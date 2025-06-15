;;; packages.el --- Package management -*- lexical-binding: t; -*-

;;; Code:
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(unless (bound-and-true-p package--initialized)
  (package-initialize))

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-compute-statistics t)

;; Simple package installation function (defined here to avoid circular deps)
(defun ensure-packages (packages)
  "Ensure all PACKAGES are installed."
  (dolist (pkg packages)
    (unless (package-installed-p pkg)
      (message "Installing %s..." pkg)
      (package-install pkg)
      (message "%s installed" pkg))))

;; Install core packages
(ensure-packages
 '(exec-path-from-shell
   general
   which-key
   gcmh
   diminish
   doom-themes
   doom-modeline
   ;; Completion framework
   vertico
   marginalia
   consult
   orderless
   corfu
   cape
   ;; Navigation and search
   embark
   embark-consult
   wgrep
   avy
   ;; Development
   projectile
   flycheck
   lsp-mode
   lsp-ui
   yasnippet
   yasnippet-snippets
   magit
   git-timemachine
   git-gutter
   blamer
   ;; Language modes
   js2-mode
   php-mode
   web-mode
   modern-cpp-font-lock
   go-mode
   lua-mode
   dockerfile-mode
   markdown-mode
   ;; AI and automation
   minuet
   ;; UI enhancements
   rainbow-delimiters
   hl-todo
   all-the-icons
   all-the-icons-dired
   ;; Window management
   ace-window
   buffer-expose
   eyebrowse
   popper
   hydra
   winner
   ;; Org mode
   org-bullets
   visual-fill-column
   org-roam
   org-roam-ui
   evil-org
   ;; Evil mode
   evil
   evil-collection
   evil-surround
   evil-commentary
   evil-matchit
   ;; Dired enhancements
   diredfl
   dired-subtree
   wdired))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  (exec-path-from-shell-copy-env "GEMINI_API_KEY"))

(use-package general)

(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3)
  (diminish 'which-key-mode))

(use-package gcmh
  :config
  (gcmh-mode 1)
  (diminish 'gcmh-mode))

(use-package diminish)

;; Enable lexical binding for better performance
(with-eval-after-load 'prog-mode
  (condition-case nil
      (progn
        (require 'rainbow-delimiters nil t)
        (require 'hl-todo nil t))
    (error nil)))

;; Lazy load packages to improve startup time
(run-with-idle-timer 1 nil (lambda ()
                             (condition-case nil
                                 (require 'yasnippet nil t)
                               (error nil))))
(run-with-idle-timer 2 nil (lambda ()
                             (condition-case nil
                                 (require 'yasnippet-snippets nil t)
                               (error nil))))
(run-with-idle-timer 3 nil (lambda ()
                             (condition-case nil
                                 (require 'git-gutter nil t)
                               (error nil))))

(provide 'packages)
;;; packages.el ends here
