;;; git.el --- Git integration -*- lexical-binding: t; -*-

;; Magit - Git client for Emacs
(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Git time machine to navigate through file history
(use-package git-timemachine
  :defer t)

;; Display git changes in the gutter
(use-package git-gutter
  :diminish git-gutter-mode
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

;; Git blame information
(use-package blamer
  :defer t
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                   :background nil
                   :height 140
                   :italic t))))


(provide 'git)
;;; git.el ends here
