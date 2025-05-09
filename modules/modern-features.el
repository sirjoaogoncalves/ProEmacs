;;; modern-features.el --- Modern Emacs features -*- lexical-binding: t; -*-

;;; Commentary:
;; Implementation of modern Emacs features like so-long mode and tree-sitter

;;; Code:

;;; Long Line Handling with so-long mode
(when (fboundp 'global-so-long-mode)
  ;; so-long mode is built into Emacs 27+
  (global-so-long-mode 1)
  
  ;; Configure so-long mode for better performance
  (setq so-long-threshold 1000)  ;; Default is 250, higher values mean fewer buffers engage so-long
  
  ;; Additional modes that trigger so-long - safer implementation
  (with-eval-after-load 'so-long
    (when (boundp 'so-long-minor-modes)
      (add-to-list 'so-long-minor-modes 'display-line-numbers-mode)
      (add-to-list 'so-long-minor-modes 'hl-line-mode)
      (add-to-list 'so-long-minor-modes 'lsp-mode))
    
    ;; Only add mode replacements if the variable exists
    (when (boundp 'so-long-action-alist)
      (setq so-long-action-alist
            (append so-long-action-alist
                   '(("disable-indicator" . ((display-fill-column-indicator-mode . -1))))))))
)

;;; Tree-sitter for better syntax highlighting
(when (>= emacs-major-version 29)
  ;; Tree-sitter is built into Emacs 29+
  (setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (c++ "https://github.com/tree-sitter/tree-sitter-cpp")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (lua "https://github.com/Azganoth/tree-sitter-lua")
        (php "https://github.com/tree-sitter/tree-sitter-php")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

  ;; Function to install all tree-sitter languages
  (defun normal/treesit-install-all-languages ()
    "Install all languages specified in `treesit-language-source-alist'."
    (interactive)
    (let ((languages (mapcar 'car treesit-language-source-alist)))
      (dolist (lang languages)
        (unless (treesit-language-available-p lang)
          (message "Installing tree-sitter grammar for %s" lang)
          (treesit-install-language-grammar lang)))
      (message "Finished installing tree-sitter grammars")))

  ;; Auto-use tree-sitter modes when available
  (setq major-mode-remap-alist
        '((c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (css-mode . css-ts-mode)
          (js-mode . js-ts-mode)
          (javascript-mode . js-ts-mode)
          (js2-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (json-mode . json-ts-mode)
          (python-mode . python-ts-mode)
          (bash-mode . bash-ts-mode)
          (sh-mode . bash-ts-mode)
          (yaml-mode . yaml-ts-mode)
          (go-mode . go-ts-mode)
          (rust-mode . rust-ts-mode)
          (lua-mode . lua-ts-mode)))

  ;; Enhance tree-sitter highlighting
  (setq treesit-font-lock-level 4)  ;; Maximum highlighting

  ;; Detect language for mixed ts-modes like .tsx
  (defun normal/setup-treesit-auto-detection ()
    "Setup automatic detection of language for mixed files."
    (add-hook 'after-change-major-mode-hook
              (lambda ()
                (when (derived-mode-p 'js-mode 'javascript-mode 'js2-mode)
                  (when (buffer-file-name)
                    (cond
                     ((string-match-p "\\.tsx\\'" (buffer-file-name))
                      (when (fboundp 'tsx-ts-mode)
                        (tsx-ts-mode)))
                     ((string-match-p "\\.[jt]sx?\\'" (buffer-file-name))
                      (if (save-excursion
                            (goto-char (point-min))
                            (re-search-forward "\\<import\\>\\|\\<export\\>" nil t))
                          (when (fboundp 'typescript-ts-mode)
                            (typescript-ts-mode))
                        (when (fboundp 'js-ts-mode)
                          (js-ts-mode))))))))))
  (normal/setup-treesit-auto-detection)

  ;; For older Emacs versions, use external tree-sitter
  (unless (fboundp 'treesit-install-language-grammar)
    (use-package tree-sitter
      :config
      (global-tree-sitter-mode)
      (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
    
    (use-package tree-sitter-langs
      :after tree-sitter)))

;;; Other Modern Features

;; Modern variables for newer Emacs
;; Pixel-based smooth scrolling (Emacs 29+)
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1)
  (setq pixel-scroll-precision-interpolate-page t))

;; Tab Bar Configurable Popup (Emacs 28+)
(when (>= emacs-major-version 28)
  (setq tab-bar-show t)
  ;; Mouse hover shows tab names
  (setq tab-bar-tab-hints t)
  (setq tab-bar-auto-width-max '(200 10)))

;; Portable dumper options (Emacs 27+)
(when (boundp 'startup-redirect-eln-cache)
  (setq package-native-compile t)
  (setq native-comp-deferred-compilation t)
  (setq native-comp-async-report-warnings-errors 'silent))



(provide 'modern-features)
;;; modern-features.el ends here
