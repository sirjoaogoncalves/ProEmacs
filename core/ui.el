;;; ui.el --- UI configuration -*- lexical-binding: t; -*-

(require 'utils)

;;; Code:

(tooltip-mode -1)

(use-package which-key
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3)
  (diminish 'which-key-mode))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)

  (doom-themes-visual-bell-config)
  (doom-themes-org-config)

  (load-theme 'doom-one t))

(defun doom-theme-selector ()
  "Select and load a doom theme from popular options."
  (interactive)
  (unless (featurep 'doom-themes)
    (require 'doom-themes nil t))

  (if (featurep 'doom-themes)
      (let ((theme (completing-read "Choose Doom theme: "
                                   '("doom-one"
                                     "doom-vibrant"
                                     "doom-dracula"
                                     "doom-nord"
                                     "doom-solarized-dark"
                                     "doom-solarized-light"
                                     "doom-tomorrow-night"
                                     "doom-monokai-pro"
                                     "doom-city-lights"
                                     "doom-dark+"
                                     "doom-spacegrey"
                                     "doom-gruvbox"
                                     "doom-palenight"
                                     "doom-material"
                                     "doom-moonlight"))))
        (load-theme (intern theme) t)
        (message "Loaded theme: %s" theme))
    (message "doom-themes package not available. Use 'SPC t t' for standard theme selection.")))

;; Safe font configuration
(when (utils/font-exists-p "Cantarell")
  (set-face-attribute 'variable-pitch nil :font "Cantarell" :height 120 :weight 'regular))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package paren
  :config
  (set-face-attribute 'show-paren-match nil :weight 'bold)
  (setq show-paren-style 'parenthesis)
  (setq show-paren-delay 0)
  (show-paren-mode 1))

(column-number-mode 1)

(add-hook 'text-mode-hook 'visual-line-mode)

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
        '(("TODO"   . "#FF0000")
          ("FIXME"  . "#FF0000")
          ("DEBUG"  . "#A020F0")
          ("GOTCHA" . "#FF4500")
          ("NOTE"   . "#1E90FF"))))

;; Font rendering optimizations
(setq inhibit-compacting-font-caches t)

;; Enhanced font rendering for Emacs 28+
(when (and (fboundp 'set-fontset-font)
           (version<= "28.0" emacs-version))
  (setq use-default-font-for-symbols nil)
  (setq-default bidi-paragraph-direction 'left-to-right)

  (when (eq system-type 'darwin)
    (when (boundp 'mac-allow-anti-aliasing)
      (setq mac-allow-anti-aliasing t))
    (when (boundp 'mac-auto-operator-composition-mode)
      (setq mac-auto-operator-composition-mode t)))

  (when (eq system-type 'windows-nt)
    (when (boundp 'w32-enable-synthesized-fonts)
      (setq w32-enable-synthesized-fonts t))
    (when (boundp 'w32-use-native-image-API)
      (setq w32-use-native-image-API t))))

(when (display-graphic-p)
  ;; Emoji font fallback configuration
  (set-fontset-font t 'symbol
                    (font-spec :family "Noto Color Emoji") nil 'prepend)
  (set-fontset-font t 'emoji
                    (font-spec :family "Noto Color Emoji") nil 'prepend)

  (set-fontset-font t 'symbol
                    (font-spec :family "Segoe UI Emoji") nil 'append)
  (set-fontset-font t 'emoji
                    (font-spec :family "Apple Color Emoji") nil 'append))

;; Mode-specific font caching
(dolist (mode '(org-mode prog-mode text-mode))
  (add-hook (intern (format "%s-hook" mode))
            (lambda () (setq inhibit-compacting-font-caches t))))

(provide 'ui)
;;; ui.el ends here
