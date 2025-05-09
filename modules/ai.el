;;; ai.el --- AI tools configuration -*- lexical-binding: t; -*-

;; Minuet for AI code completion
(use-package minuet
  :bind
  (("M-y" . #'minuet-complete-with-minibuffer)
   ("M-i" . #'minuet-show-suggestion)
   ("C-c m" . #'minuet-configure-provider)
   :map minuet-active-mode-map
   ("M-p" . #'minuet-previous-suggestion)
   ("M-n" . #'minuet-next-suggestion)
   ("M-A" . #'minuet-accept-suggestion)
   ("M-a" . #'minuet-accept-suggestion-line)
   ("M-e" . #'minuet-dismiss-suggestion)
   ("<tab>" . #'minuet-accept-suggestion)
   ("RET" . #'minuet-accept-suggestion-line))
  :init
  (add-hook 'prog-mode-hook #'minuet-auto-suggestion-mode)
  :config
  (setq minuet-provider 'gemini)
  (plist-put minuet-gemini-options :model "gemini-2.0-flash")
  (plist-put minuet-gemini-options :thinking nil)
  (setq minuet-auto-suggestion-debounce-delay 0.5)
  (setq minuet-auto-suggestion-throttle-delay 1.0)
  (setq minuet-n-completions 2))

(provide 'ai)
;;; ai.el ends here
