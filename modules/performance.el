;;; performance.el --- Performance optimizations -*- lexical-binding: t; -*-

;;; Commentary:
;; Optimizations for better performance and faster startup

;;; Code:

;;; Garbage Collection Strategy

;; Use higher gc threshold during startup
;; The init.el file already sets it for startup, this makes it permanent
(defvar my-gc-cons-threshold (* 64 1024 1024))
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold my-gc-cons-threshold
                  gc-cons-percentage 0.3)))

;; Adopt a smarter GC strategy for better responsiveness
(use-package gcmh
  :diminish gcmh-mode
  :init
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold (* 32 1024 1024)  ;; 32MB
        gcmh-low-cons-threshold (* 16 1024 1024)   ;; 16MB
        gcmh-verbose nil)
  :config
  (gcmh-mode 1)
  
  ;; ENHANCEMENT: Add GC when Emacs loses focus
  (defun my/gcmh-focus-out-hook ()
    "Run garbage collection when frame loses focus."
    (garbage-collect))
  (add-hook 'focus-out-hook #'my/gcmh-focus-out-hook))

;; Temporarily disable GC during minibuffer use for better responsiveness
(defun my/minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my/minibuffer-exit-hook ()
  (setq gc-cons-threshold my-gc-cons-threshold))

(add-hook 'minibuffer-setup-hook #'my/minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my/minibuffer-exit-hook)

;;; Native Compilation Settings
(when (featurep 'native-compile)
  ;; Configure native compilation for better performance
  (setq native-comp-async-report-warnings-errors nil
        native-comp-deferred-compilation t
        native-comp-jit-compilation t
        native-comp-speed 2)  ;; Compilation level (0-3, higher = more optimization)

  ;; ENHANCEMENT: Set number of parallel jobs for native compilation
  (setq native-comp-async-jobs-number 
        (max 2 (- (num-processors) 1))) ;; Use N-1 cores

  ;; ENHANCEMENT: Exclude problematic packages from native compilation
  (when (boundp 'comp-deferred-compilation-deny-list)
    (setq comp-deferred-compilation-deny-list
          '("treemacs-all-the-icons" "evil-collection" "vterm"))))

;; Configure a fixed place for native compilation cache
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name "var/eln-cache/" user-emacs-directory))))

;;; IO & File Related Optimizations

;; Increase read chunks for faster loading of large files
(setq read-process-output-max (* 4 1024 1024)) ;; 4MB (from 1MB set in defaults.el)

;; Disable bidirectional text rendering for performance
(setq-default bidi-display-reordering nil)
(setq bidi-inhibit-bpa t)  ;; Potentially risky but major speed boost
(setq-default bidi-paragraph-direction 'left-to-right)

;; Disable expensive font operations for non-text modes
(setq inhibit-compacting-font-caches t)

;; Optimize process communications
(setq process-adaptive-read-buffering nil)  ;; Reduce lag for process interaction

;; Cache more recent files for faster access
(setq recentf-max-saved-items 200)
(setq recentf-exclude '("\\.git/" "\\.emacs.d/elpa/"))

;; ENHANCEMENT: Optimize recentf (can slow down startup and closing)
(setq recentf-auto-cleanup 'never)
(setq recentf-keep '(file-remote-p file-readable-p))
(setq recentf-auto-save-timer 
      (run-with-idle-timer 300 t 'recentf-save-list))

;;; Editor Performance Optimizations

;; Reduce rendering work
(setq auto-window-vscroll nil)           ;; Don't adjust window-vscroll to view tall lines
(setq fast-but-imprecise-scrolling t)    ;; Faster scrolling over unfontified regions
(setq scroll-conservatively 101)         ;; More conservative scrolling
(setq scroll-margin 1)                   ;; Lowered from defaults.el for performance

;; Improve long-line performance
(setq-default bidi-display-reordering nil)
(setq redisplay-skip-fontification-on-input t)

;; ENHANCEMENT: Better font lock performance
(setq jit-lock-stealth-time 0.5)
(setq jit-lock-chunk-size 1000)
(setq jit-lock-defer-time 0.05)

;; ENHANCEMENT: Use precision pixel-based scrolling in Emacs 29+
(when (>= emacs-major-version 29)
  (pixel-scroll-precision-mode 1)
  (setq pixel-scroll-precision-interpolate-page t)
  (setq pixel-scroll-precision-interpolation-factor 0.85))

;; Disable expensive minor modes when files are too large
(defun my/check-large-file ()
  "Check if file is large - if so, disable certain modes."
  (when (> (buffer-size) (* 1024 1024))
    (when (bound-and-true-p lsp-mode)
      (lsp-disconnect))
    (when (bound-and-true-p flycheck-mode)
      (flycheck-mode -1))
    (when (bound-and-true-p flymake-mode)
      (flymake-mode -1))
    (when (bound-and-true-p eldoc-mode)
      (eldoc-mode -1))
    (font-lock-mode -1)
    (setq-local line-number-mode nil)
    (setq-local global-hl-line-mode nil)
    (display-line-numbers-mode -1)
    (message "Large file detected. Disabled some features to improve performance.")))

(add-hook 'find-file-hook #'my/check-large-file)

;; Better byte compilation for packages
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

;;; Lazy Loading Features

;; Defer loading of some features until they're needed
(with-eval-after-load 'hydra
  (setq hydra-is-helpful t))  ;; Only compute helpful hints when hydra is loaded

;; Don't load heavy lisp packages until used
(setq elisp-flymake-byte-compile-load-path load-path)

;; ENHANCEMENT: Delay loading of non-essential packages
(defvar my/delayed-packages
  '(yasnippet yasnippet-snippets rainbow-delimiters hl-todo
    all-the-icons all-the-icons-dired diredfl git-gutter
    format-all buffer-expose eyebrowse org-bullets)
  "Packages that can be delayed during startup.")

(defun my/load-delayed-packages ()
  "Load packages that were delayed during startup."
  (dolist (package my/delayed-packages)
    (condition-case nil
        (require package nil t)
      (error nil))))

;; Load delayed packages when Emacs is idle for 2 seconds
(run-with-idle-timer 2 nil #'my/load-delayed-packages)

;;; LSP Optimizations
(with-eval-after-load 'lsp-mode
  ;; Optimize LSP for better performance
  (setq lsp-enable-file-watchers nil
        lsp-enable-folding nil
        lsp-enable-text-document-color nil
        lsp-headerline-breadcrumb-enable nil
        lsp-modeline-code-actions-enable nil
        lsp-modeline-diagnostics-enable nil
        lsp-keep-workspace-alive nil
        lsp-signature-auto-activate nil
        lsp-completion-provider :none   ;; Use Emacs' native completion
        lsp-idle-delay 0.2)
  
  ;; ENHANCEMENT: Additional LSP optimizations
  (setq lsp-response-timeout 5)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-enable-indentation nil)
  (setq lsp-enable-snippet nil)
  (setq lsp-eldoc-enable-hover nil)
  
  ;; ENHANCEMENT: LSP toggle functions
  (defun my/toggle-lsp-ui-doc ()
    "Toggle LSP UI Doc mode."
    (interactive)
    (when (fboundp 'lsp-ui-doc-mode)
      (if (bound-and-true-p lsp-ui-doc-mode)
          (progn (setq lsp-ui-doc-enable nil)
                 (lsp-ui-doc-mode -1)
                 (message "LSP UI Doc mode disabled"))
        (progn (setq lsp-ui-doc-enable t)
               (lsp-ui-doc-mode 1)
               (message "LSP UI Doc mode enabled")))))
  
  (defun my/toggle-lsp-ui-sideline ()
    "Toggle LSP UI Sideline mode."
    (interactive)
    (when (fboundp 'lsp-ui-sideline-mode)
      (if (bound-and-true-p lsp-ui-sideline-mode)
          (progn (setq lsp-ui-sideline-enable nil)
                 (lsp-ui-sideline-mode -1)
                 (message "LSP UI Sideline mode disabled"))
        (progn (setq lsp-ui-sideline-enable t)
               (lsp-ui-sideline-mode 1)
               (message "LSP UI Sideline mode enabled"))))))

;; Improve company-mode performance
(with-eval-after-load 'company
  (setq company-minimum-prefix-length 3
        company-idle-delay 0.2
        company-tooltip-limit 10))

;;; Minibuffer Optimizations

;; Smarter resizing of minibuffer
(setq resize-mini-windows 'grow-only)

;; Optimize minibuffer loading
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

;;; Improve Emacs Frame Performance

;; Reduce frame operations that cause redisplay
(setq frame-resize-pixelwise nil)
(setq frame-inhibit-implied-resize t)

;; ENHANCEMENT: Better dashboard timing display
(with-eval-after-load 'dashboard
  (setq dashboard-footer-messages 
        (list (format "Emacs v%s • %d packages • Init: %.2fs • Total: %.2fs" 
                     emacs-version 
                     (length package-activated-list)
                     (float-time (time-subtract after-init-time before-init-time))
                     (float-time (time-subtract (current-time) before-init-time))))))

;; ENHANCEMENT: Fast directory listing
(setq directory-free-space-args "-Pkh")
(setq list-directory-brief-switches "-CF")

;; ENHANCEMENT: Use fd with projectile when available
(when (executable-find "fd")
  (with-eval-after-load 'projectile
    (setq projectile-generic-command
          (if (eq system-type 'windows-nt)
              "fd . -0 --color=never -t f"
            "fd . -0 --type f --color=never"))))

;; ENHANCEMENT: Package loading statistics
(setq use-package-compute-statistics t)
(defun my/display-package-stats ()
  "Display the packages that took the longest to load."
  (interactive)
  (let ((stats (sort (copy-sequence use-package-statistics)
                    (lambda (a b) (> (nth 2 a) (nth 2 b))))))
    (with-current-buffer (get-buffer-create "*Package Load Times*")
      (erase-buffer)
      (insert "Package Load Times:\n\n")
      (insert (format "%-30s %10s %10s\n" "Package" "Load Time" "Status"))
      (insert (make-string 52 ?-) "\n")
      (dolist (entry (seq-take stats 20))
        (insert (format "%-30s %10.2fms %10s\n" 
                       (car entry) 
                       (* 1000 (nth 2 entry))
                       (if (nth 1 entry) "Loaded" "Not Loaded"))))
      (goto-char (point-min)))
    (display-buffer "*Package Load Times*")))



(defun memory-report ()
  "Report memory usage."
  (interactive)
  (message "Memory used: %d bytes" (memory-use-counts)))
  
;; Font performance optimization function
(defun normal/optimize-font-rendering ()
  "Optimize font rendering for performance."
  (interactive)
  (setq inhibit-compacting-font-caches t)
  
  ;; Disable expensive font operations
  (setq x-use-underline-position-properties nil)
  (setq underline-minimum-offset 0)
  
  ;; Reduce work for font backend
  (when (boundp 'x-drawing-uses-system-configuration)
    (setq x-drawing-uses-system-configuration nil))
  
  ;; Use a simpler approach to underlines
  (setq-default x-stretch-cursor nil)
  
  ;; Reduce font work in the mode line
  (setq-default mode-line-percent-position nil)
  
  ;; Text scaling increments
  (setq text-scale-mode-step 1.1)
  
  (message "Font rendering optimized for performance"))


(provide 'performance)
;;; performance.el ends here
