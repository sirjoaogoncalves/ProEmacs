;;; packages.el --- Package management -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;; Setup package repositories
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Debug: Show package initialization status
(message "Initializing packages...")

;; Initialize packages (required since package-enable-at-startup is nil in early-init.el)
(unless (bound-and-true-p package--initialized)
  (package-initialize)
  (message "Package system initialized"))

;; Refresh package contents if archives are empty (first-time setup)
(unless package-archive-contents
  (message "Refreshing package archives...")
  (package-refresh-contents)
  (message "Package archives refreshed"))

;; Setup use-package
(unless (package-installed-p 'use-package)
  (message "Installing use-package...")
  (package-refresh-contents)
  (package-install 'use-package)
  (message "use-package installed"))

(require 'use-package)
(setq use-package-always-ensure t) ;; Always ensure packages are installed

;; Enable use-package statistics to identify slow-loading packages
(setq use-package-compute-statistics t)

;; Core packages - ensure they're installed before configuration
(message "Checking core packages...")
(dolist (pkg '(exec-path-from-shell general which-key gcmh diminish doom-themes doom-modeline))
  (unless (package-installed-p pkg)
    (message "Installing %s..." pkg)
    (package-install pkg)
    (message "%s installed" pkg)))
(message "All core packages ready")

;; Core packages - load these immediately
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  (exec-path-from-shell-copy-env "GEMINI_API_KEY"))

(use-package general)   ;; Keybinding framework
(use-package which-key  ;; Key binding hints
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3)
  (diminish 'which-key-mode))

(use-package gcmh      ;; Garbage Collection Magic Hack
  :config
  (gcmh-mode 1)
  (diminish 'gcmh-mode))

;; Enable diminish to hide minor modes in modeline
(use-package diminish)

;; REMOVED: Neotree package loading - this was causing the icon function warnings

(with-eval-after-load 'prog-mode
  (require 'rainbow-delimiters nil t)
  (require 'hl-todo nil t))

;; Load some packages during idle time
(run-with-idle-timer 1 nil (lambda () (require 'yasnippet nil t)))
(run-with-idle-timer 2 nil (lambda () (require 'yasnippet-snippets nil t)))
(run-with-idle-timer 3 nil (lambda () (require 'git-gutter nil t)))

(provide 'packages)
;;; packages.el ends here
