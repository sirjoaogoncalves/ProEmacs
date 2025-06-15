;;; early-init.el --- Early Init File -*- lexical-binding: t -*-

;; Defer garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)

;; Prevent package.el loading packages prior to init.el
(setq package-enable-at-startup nil)

;; Disable UI elements to prevent glimpse of un-styled Emacs
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq package-quickstart-file (concat user-emacs-directory "var/package-quickstart.el"))

;; Prevent expensive frame resize operations
(setq frame-inhibit-implied-resize t)

(menu-bar-mode -1)
(unless (and (display-graphic-p) (eq system-type 'darwin))
  (push '(menu-bar-lines . 0) default-frame-alist))
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; UTF-8 configuration
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(when (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-16-le))

;; Performance: prevent premature display initialization
(setq-default inhibit-redisplay t
              inhibit-message t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil
                          inhibit-message nil)
            (redisplay)))

(setq site-run-file nil)
(setq inhibit-compacting-font-caches t)

;;; early-init.el ends here
