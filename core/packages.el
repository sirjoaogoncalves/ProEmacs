;;; packages.el --- Package management -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;; Setup package repositories
(require 'package)

;; Add package-quickstart for faster loading
(setq package-quickstart t)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Setup use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t) ;; Always ensure packages are installed

;; Enable use-package statistics to identify slow-loading packages
(setq use-package-compute-statistics t)

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
  (setq which-key-idle-delay 0.3))

(use-package gcmh      ;; Garbage Collection Magic Hack
  :config
  (gcmh-mode 1))

;; Enable diminish to hide minor modes in modeline
(use-package diminish)


;; Neotree
(use-package neotree)

(with-eval-after-load 'prog-mode
  (require 'rainbow-delimiters nil t)
  (require 'hl-todo nil t))

;; Load some packages during idle time
(run-with-idle-timer 1 nil (lambda () (require 'yasnippet nil t)))
(run-with-idle-timer 2 nil (lambda () (require 'yasnippet-snippets nil t)))
(run-with-idle-timer 3 nil (lambda () (require 'git-gutter nil t)))

(provide 'packages)
;;; packages.el ends here
