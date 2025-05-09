;;; ui.el --- UI configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains configuration for Emacs user interface elements,
;; including which-key, font settings, delimiters, line numbers,
;; and visual line mode.

;;; Code:

(tooltip-mode -1)

;; Enable which-key
(use-package which-key
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3)
  (diminish 'which-key-mode))

;; Font configuration - uncomment and modify as needed
;; (set-face-attribute 'default nil :font "Fira Code" :height 120)
;; (set-face-attribute 'fixed-pitch nil :font "Fira Code" :height 120)
;; (set-face-attribute 'variable-pitch nil :font "Cantarell" :height 120 :weight 'regular)


;; Rainbow delimiters for better bracket visibility
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Highlight matching parentheses
(use-package paren
  :config
  (set-face-attribute 'show-paren-match nil :weight 'bold)
  (setq show-paren-style 'parenthesis)
  (setq show-paren-delay 0)
  (show-paren-mode 1))

;; Display line and column numbers in mode-line
(column-number-mode 1)

;; Visual line mode (word wrap) for text modes
(add-hook 'text-mode-hook 'visual-line-mode)

;; Highlight TODO keywords
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
        '(("TODO"   . "#FF0000")
          ("FIXME"  . "#FF0000")
          ("DEBUG"  . "#A020F0")
          ("GOTCHA" . "#FF4500")
          ("NOTE"   . "#1E90FF"))))

;; Font configuration - uncomment and modify as needed
;; (set-face-attribute 'default nil :font "Fira Code" :height 120)
;; (set-face-attribute 'fixed-pitch nil :font "Fira Code" :height 120)
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 120 :weight 'regular)

;; Font rendering optimization
(setq inhibit-compacting-font-caches t)  ;; Don't compact font caches during GC

;; HarfBuzz text shaping for better font rendering (Emacs 28+)
(when (and (fboundp 'set-fontset-font)
           (version<= "28.0" emacs-version))
  ;; Enable HarfBuzz
  (setq use-default-font-for-symbols nil)
  
  ;; Use harfbuzz for font rendering
  (setq-default bidi-paragraph-direction 'left-to-right)
  
  ;; Configure hinting and antialiasing
  (when (eq system-type 'gnu/linux)
    (setq font-use-system-font t)
    ;; Fontconfig settings - uncomment if needed
    ;; (set-frame-parameter nil 'font-backend '(harfbuzz))
    ;; (setq font-render-setting "rgba=rgb,hinting=slight,hintstyle=hintslight,lcdfilter=default")
    )
  
  ;; MacOS specific font rendering
  (when (eq system-type 'darwin)
    (when (boundp 'mac-allow-anti-aliasing)
      (setq mac-allow-anti-aliasing t))
    (when (boundp 'mac-auto-operator-composition-mode)
      (setq mac-auto-operator-composition-mode t)))

  ;; Windows specific font rendering
  (when (eq system-type 'windows-nt)
    (when (boundp 'w32-enable-synthesized-fonts)
      (setq w32-enable-synthesized-fonts t))
    (when (boundp 'w32-use-native-image-API)
      (setq w32-use-native-image-API t))))

;; Font caching for specific modes
(dolist (mode '(org-mode prog-mode text-mode))
  (add-hook (intern (format "%s-hook" mode))
            (lambda () (setq inhibit-compacting-font-caches t))))

(provide 'ui)
;;; ui.el ends here
