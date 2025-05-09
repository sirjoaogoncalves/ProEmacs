;;; evil-config.el --- Evil mode setup -*- lexical-binding: t; -*-

;; Evil mode configuration
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  
  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

;; Evil collection for consistent evil keybindings across Emacs
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Evil surround for quick surrounding of text objects
(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

;; Evil commentary for easy commenting
(use-package evil-commentary
  :after evil
  :diminish
  :config
  (evil-commentary-mode))

;; Evil matchit for enhanced % matching
(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode 1))

(provide 'evil-config)
;;; evil-config.el ends here
