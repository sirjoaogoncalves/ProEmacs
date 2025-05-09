;;; early-init.el --- Early Init File -*- lexical-binding: t -*-

;; Defer garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)

;; Package initialization occurs after loading early-init.el
;; Prevent package.el loading packages prior to init.el loading
(setq package-enable-at-startup nil)

;; Prevent the glimpse of un-styled Emacs by disabling UI elements
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Prevent loading packages right away
(setq package-quickstart-file (concat user-emacs-directory "var/package-quickstart.el"))

;; Resizing the Emacs frame can be a terribly expensive operation
(setq frame-inhibit-implied-resize t)

;; Disable UI elements early to avoid momentary display
(menu-bar-mode -1)
(unless (and (display-graphic-p) (eq system-type 'darwin))
  (push '(menu-bar-lines . 0) default-frame-alist))
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Explicitly set the preferred coding systems
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(when (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-16-le))

;; PERF: Prevent premature display engine initialization
(setq-default inhibit-redisplay t
              inhibit-message t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil
                          inhibit-message nil)
            (redisplay)))

;; PERF: Don't load site-run-file
(setq site-run-file nil)

;; PERF: Font-related performance settings
(setq inhibit-compacting-font-caches t)

;;; early-init.el ends here
