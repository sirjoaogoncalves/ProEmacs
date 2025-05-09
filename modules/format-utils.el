;;; format-utils.el --- On-demand code formatting -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides lightweight, on-demand code formatting that only loads formatters
;; when they're needed to keep performance optimal.

;;; Code:

;; Formatter registry - maps major modes to formatter functions
(defvar normal/formatters-alist
  '((c-mode . normal/format-c-like)
    (c++-mode . normal/format-c-like)
    (c-ts-mode . normal/format-c-like)
    (c++-ts-mode . normal/format-c-like)
    (css-mode . normal/format-css)
    (css-ts-mode . normal/format-css)
    (emacs-lisp-mode . normal/format-elisp)
    (go-mode . normal/format-go)
    (go-ts-mode . normal/format-go)
    (html-mode . normal/format-html)
    (java-mode . normal/format-c-like)
    (java-ts-mode . normal/format-c-like)
    (js-mode . normal/format-js)
    (js-ts-mode . normal/format-js)
    (js2-mode . normal/format-js)
    (json-mode . normal/format-json)
    (json-ts-mode . normal/format-json)
    (markdown-mode . normal/format-markdown)
    (org-mode . normal/format-org)
    (php-mode . normal/format-php)
    (python-mode . normal/format-python)
    (python-ts-mode . normal/format-python)
    (rust-mode . normal/format-rust)
    (rust-ts-mode . normal/format-rust)
    (sh-mode . normal/format-shell)
    (bash-ts-mode . normal/format-shell)
    (typescript-mode . normal/format-typescript)
    (typescript-ts-mode . normal/format-typescript)
    (web-mode . normal/format-web)
    (yaml-mode . normal/format-yaml)
    (yaml-ts-mode . normal/format-yaml))
  "Alist mapping major modes to formatting functions.")

;; External formatter detection
(defun normal/formatter-exists-p (formatter)
  "Check if external FORMATTER exists in the system path."
  (and (executable-find formatter) t))

;; Status tracking for format-on-save
(defvar normal/format-on-save nil
  "When non-nil, format buffers on save.")

;; Main formatting function
(defun normal/format-buffer ()
  "Format the current buffer using the appropriate formatter."
  (interactive)
  (if-let* ((formatter-func (alist-get major-mode normal/formatters-alist))
            (formatter-name (symbol-name formatter-func)))
      (progn
        (message "Formatting buffer using %s..." 
                 (replace-regexp-in-string "^normal/format-" "" formatter-name))
        (condition-case err
            (call-interactively formatter-func)
          (error (message "Formatting failed: %s" (error-message-string err))))
        (message "Formatting complete."))
    (message "No formatter available for %s" major-mode)))

;; Toggle format on save
(defun normal/toggle-format-on-save ()
  "Toggle automatic formatting on buffer save."
  (interactive)
  (setq normal/format-on-save (not normal/format-on-save))
  (if normal/format-on-save
      (add-hook 'before-save-hook #'normal/format-buffer)
    (remove-hook 'before-save-hook #'normal/format-buffer))
  (message "Format on save %s" (if normal/format-on-save "enabled" "disabled")))

;; Individual formatters - only loaded when needed
;; They check for external tools and fall back to internal ones when needed

(defun normal/format-elisp ()
  "Format current Emacs Lisp buffer."
  (interactive)
  (unless (eq major-mode 'emacs-lisp-mode)
    (error "Not in an Emacs Lisp buffer"))
  (let ((point (point)))
    (save-restriction
      (indent-region (point-min) (point-max)))
    (goto-char point)))

(defun normal/format-c-like ()
  "Format C-like code (C, C++, Java, etc.) using clang-format if available."
  (interactive)
  (if (normal/formatter-exists-p "clang-format")
      (progn
        (unless (require 'clang-format nil t)
          (package-install 'clang-format))
        (clang-format-buffer))
    ;; Fallback to internal formatting
    (indent-region (point-min) (point-max))))

(defun normal/format-python ()
  "Format Python code using black or yapf if available."
  (interactive)
  (cond
   ((normal/formatter-exists-p "black")
    (shell-command-on-region (point-min) (point-max) "black -q -" nil t))
   ((normal/formatter-exists-p "yapf")
    (shell-command-on-region (point-min) (point-max) "yapf" nil t))
   (t
    (message "No Python formatter found. Install black or yapf.")
    (indent-region (point-min) (point-max)))))

(defun normal/format-js ()
  "Format JavaScript code using prettier if available."
  (interactive)
  (if (normal/formatter-exists-p "prettier")
      (shell-command-on-region 
       (point-min) (point-max) 
       "prettier --parser babel" 
       nil t)
    (message "Prettier not found. Using internal formatter.")
    (indent-region (point-min) (point-max))))

(defun normal/format-typescript ()
  "Format TypeScript code using prettier if available."
  (interactive)
  (if (normal/formatter-exists-p "prettier")
      (shell-command-on-region 
       (point-min) (point-max) 
       "prettier --parser typescript" 
       nil t)
    (message "Prettier not found. Using internal formatter.")
    (indent-region (point-min) (point-max))))

(defun normal/format-json ()
  "Format JSON using prettier or jq if available."
  (interactive)
  (cond
   ((normal/formatter-exists-p "prettier")
    (shell-command-on-region 
     (point-min) (point-max) 
     "prettier --parser json" 
     nil t))
   ((normal/formatter-exists-p "jq")
    (shell-command-on-region 
     (point-min) (point-max) 
     "jq ." 
     nil t))
   (t
    (message "No JSON formatter found. Using internal formatter.")
    (indent-region (point-min) (point-max)))))

(defun normal/format-html ()
  "Format HTML using prettier if available."
  (interactive)
  (if (normal/formatter-exists-p "prettier")
      (shell-command-on-region 
       (point-min) (point-max) 
       "prettier --parser html" 
       nil t)
    (message "Prettier not found. Using internal formatter.")
    (indent-region (point-min) (point-max))))

(defun normal/format-css ()
  "Format CSS using prettier if available."
  (interactive)
  (if (normal/formatter-exists-p "prettier")
      (shell-command-on-region 
       (point-min) (point-max) 
       "prettier --parser css" 
       nil t)
    (message "Prettier not found. Using internal formatter.")
    (indent-region (point-min) (point-max))))

(defun normal/format-markdown ()
  "Format Markdown using prettier if available."
  (interactive)
  (if (normal/formatter-exists-p "prettier")
      (shell-command-on-region 
       (point-min) (point-max) 
       "prettier --parser markdown" 
       nil t)
    (message "Prettier not found. No reliable internal formatter available.")
    (fill-region (point-min) (point-max))))

(defun normal/format-yaml ()
  "Format YAML using prettier if available."
  (interactive)
  (if (normal/formatter-exists-p "prettier")
      (shell-command-on-region 
       (point-min) (point-max) 
       "prettier --parser yaml" 
       nil t)
    (message "Prettier not found. Using internal formatter.")
    (indent-region (point-min) (point-max))))

(defun normal/format-go ()
  "Format Go code using gofmt if available."
  (interactive)
  (if (normal/formatter-exists-p "gofmt")
      (shell-command-on-region 
       (point-min) (point-max) 
       "gofmt" 
       nil t)
    (message "gofmt not found. No reliable internal formatter available.")
    (indent-region (point-min) (point-max))))

(defun normal/format-rust ()
  "Format Rust code using rustfmt if available."
  (interactive)
  (if (normal/formatter-exists-p "rustfmt")
      (shell-command-on-region 
       (point-min) (point-max) 
       "rustfmt" 
       nil t)
    (message "rustfmt not found. No reliable internal formatter available.")
    (indent-region (point-min) (point-max))))

(defun normal/format-php ()
  "Format PHP code using php-cs-fixer if available."
  (interactive)
  (if (normal/formatter-exists-p "php-cs-fixer")
      (shell-command-on-region 
       (point-min) (point-max) 
       "php-cs-fixer fix --quiet -" 
       nil t)
    (message "php-cs-fixer not found. Using internal formatter.")
    (indent-region (point-min) (point-max))))

(defun normal/format-shell ()
  "Format shell scripts using shfmt if available."
  (interactive)
  (if (normal/formatter-exists-p "shfmt")
      (shell-command-on-region 
       (point-min) (point-max) 
       "shfmt -i 2" 
       nil t)
    (message "shfmt not found. Using internal formatter.")
    (indent-region (point-min) (point-max))))

(defun normal/format-web ()
  "Format web-mode buffer (detect HTML/JS/CSS and format appropriately)."
  (interactive)
  (if (normal/formatter-exists-p "prettier")
      (let ((file-ext (file-name-extension (or buffer-file-name ""))))
        (cond
         ((string= file-ext "html")
          (shell-command-on-region (point-min) (point-max) "prettier --parser html" nil t))
         ((string= file-ext "php")
          (if (normal/formatter-exists-p "php-cs-fixer")
              (shell-command-on-region (point-min) (point-max) "php-cs-fixer fix --quiet -" nil t)
            (indent-region (point-min) (point-max))))
         ((member file-ext '("js" "jsx"))
          (shell-command-on-region (point-min) (point-max) "prettier --parser babel" nil t))
         ((member file-ext '("ts" "tsx"))
          (shell-command-on-region (point-min) (point-max) "prettier --parser typescript" nil t))
         (t (indent-region (point-min) (point-max)))))
    (message "Prettier not found. Using internal formatter.")
    (indent-region (point-min) (point-max))))

(defun normal/format-org ()
  "Format org-mode buffer."
  (interactive)
  (org-indent-region (point-min) (point-max)))


(provide 'format-utils)
;;; format-utils.el ends here
