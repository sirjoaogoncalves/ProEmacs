;;; advanced-search.el --- Advanced search and navigation -*- lexical-binding: t; -*-

;;; Commentary:
;; Lightweight advanced search and navigation tools

;;; Code:

;; Embark for contextual actions
(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim))
  :config
  ;; Hide mode line in embark buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Embark-Consult integration
(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Wgrep for editing search results
(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-enable-key "e"))

;; Avy for fast navigation
(use-package avy
  :bind (("C-'" . avy-goto-char)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1))
  :config
  (setq avy-background t)
  (setq avy-style 'at-full))

;; Enhanced consult configuration
(with-eval-after-load 'consult
  ;; Performance optimizations
  (setq consult-ripgrep-args
        "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --with-filename --line-number --search-zip")
  (setq consult-preview-key 'any)
  (setq consult-narrow-key "<"))

;; Smart search functions
(defun advanced-search/smart-search ()
  "Context-aware search: line in buffer, or ripgrep in project."
  (interactive)
  (if (and (fboundp 'projectile-project-root) (projectile-project-root))
      (call-interactively 'consult-ripgrep)
    (call-interactively 'consult-line)))

(defun advanced-search/find-references-at-point ()
  "Find all references to symbol at point using ripgrep."
  (interactive)
  (if-let ((symbol (thing-at-point 'symbol t)))
      (consult-ripgrep (concat "\\b" (regexp-quote symbol) "\\b"))
    (call-interactively 'consult-ripgrep)))

(defun advanced-search/search-todos ()
  "Search for TODO/FIXME/NOTE comments in project."
  (interactive)
  (consult-ripgrep "\\b(TODO|FIXME|NOTE|HACK|XXX|BUG)\\b"))

(defun advanced-search/project-files ()
  "Find files in current project with preview."
  (interactive)
  (if (and (fboundp 'projectile-project-root) (projectile-project-root))
      (consult-find (projectile-project-root))
    (consult-find default-directory)))

(defun advanced-search/recent-files ()
  "Enhanced recent files with preview."
  (interactive)
  (consult-recent-file))

;; Specialized search functions
(defun consult-ripgrep-code ()
  "Search only in code files."
  (interactive)
  (let ((consult-ripgrep-args
         (concat consult-ripgrep-args " --type-add 'code:*.{js,ts,jsx,tsx,py,go,rs,c,cpp,h,hpp,java,php,rb,el}' --type code")))
    (consult-ripgrep)))

(defun consult-ripgrep-docs ()
  "Search only in documentation files."
  (interactive)
  (let ((consult-ripgrep-args
         (concat consult-ripgrep-args " --type-add 'docs:*.{md,org,txt,rst}' --type docs")))
    (consult-ripgrep)))

;; Performance optimizations
(setq read-process-output-max (* 1024 1024))
(setq process-adaptive-read-buffering nil)

(provide 'advanced-search)
;;; advanced-search.el ends here
