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

;; Doom themes - modern, beautiful themes
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)

  ;; Load default theme - doom-one is active, comment/uncomment to change:
  (load-theme 'doom-one t)           ; Popular dark theme
  ;; (load-theme 'doom-vibrant t)       ; Vibrant colors
  ;; (load-theme 'doom-dracula t)       ; Dracula theme
  ;; (load-theme 'doom-nord t)          ; Nord theme
  ;; (load-theme 'doom-solarized-light t) ; Light theme
  )

;; Doom modeline - modern, informative modeline
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 25)
  (doom-modeline-bar-width 3)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count nil)
  (doom-modeline-buffer-encoding t)
  (doom-modeline-indent-info nil)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-vcs-max-length 12)
  (doom-modeline-env-version t)
  (doom-modeline-irc-stylize 'identity)
  (doom-modeline-github-timer nil)
  (doom-modeline-gnus-timer nil))

;; Function to easily switch between popular doom themes
(defun doom-theme-selector ()
  "Select and load a doom theme from popular options."
  (interactive)
  ;; Ensure doom-themes is loaded
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

;; Font configuration - safely check if fonts exist before setting them
;; (set-face-attribute 'default nil :font "Fira Code" :height 120)
;; (set-face-attribute 'fixed-pitch nil :font "Fira Code" :height 120)

;; FIXED: Check if Cantarell font exists before setting it
(when (and (display-graphic-p)
           (member "Cantarell" (font-family-list)))
  (set-face-attribute 'variable-pitch nil :font "Cantarell" :height 120 :weight 'regular))

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
