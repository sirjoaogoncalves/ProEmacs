;;; completion.el --- Optimized Completion Frameworks -*- lexical-binding: t; -*-

;;; Commentary:
;; Optimized completion configuration with Vertico, Marginalia, Consult,
;; Orderless, Corfu, Cape, intended to resolve double completion UI.

;;; Code:

;; Prescient for better sorting of completion candidates
;; (use-package prescient
;;   :config ; Moved mode activation to :config
;;   ;; Enable caching and ranking
;;   (prescient-mode +lambda)
;;   ;; Enable persisting the history and rankings
;;   (prescient-persist-mode))

;; Vertico for vertical completion UI (for minibuffer completion)
(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t) ; Enable cycling through candidates
  ;; Integrate prescient for sorting (optional, uncomment if using prescient)
  ;; (vertico-sort-function '(lambda (a b) (prescient-compare-candidates a b)))
  )

;; Marginalia for annotations in the minibuffer
(use-package marginalia
  :init
  (marginalia-mode))

;; Consult for enhanced search and navigation
(use-package consult
  :bind
  ;; Keep your existing keybindings or add more as needed
  (("C-s" . consult-line)           ;; Search current buffer
   ("C-x b" . consult-buffer)       ;; Switch buffer
   ("C-c f" . consult-find)         ;; Find file
   ("C-c g" . consult-grep))
  :config
  ;; Optional: Enable previews for certain consult commands
  ;; (consult-customize
  ;;  consult-ripgrep consult-git-grep consult-grep
  ;;  consult-multi-grep consult-bookmark consult-recent-file
  ;;  consult-dir consult-yank consult-outline consult-org-heading
  ;;  consult-fill-column consult-symbol consult-imenu consult-library
  ;;  consult-register-load consult-register-store
  ;;  :preview-key (kbd "M-.")) ; Example preview key
  )

;; Orderless for flexible matching
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Corfu for completion at point (in-buffer completion)
(use-package corfu
  :custom
  (corfu-cycle t)                ;; Enable cycling through candidates
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 2)          ;; Complete with 2 letters (adjust as needed)
  (corfu-auto-delay 0.0)         ;; No delay for auto completion (adjust as needed)
  (corfu-separator ?\s)          ;; Use space as separator
  (corfu-quit-at-boundary 'separator) ;; Don't quit at completion boundary
  ;; Integrate prescient for sorting (optional, uncomment if using prescient)
  ;; (corfu-sort-function '(lambda (a b) (prescient-compare-candidates a b)))
  :init
  (global-corfu-mode))

;; Cape for completion at point extensions (provides more completion sources for Corfu)
(use-package cape
  :init
  ;; Add Cape backends you need
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;; Add more as needed, e.g., #'cape-keyword, #'cape-symbol, etc.
  )

;; Company mode (Commented out - likely cause of double completion UI)
;; If you find that certain modes or packages require Company, you might
;; need to investigate further or configure Company to not conflict with Corfu.
;; (use-package company
;;   :hook (prog-mode . company-mode)
;;   :config
;;   (setq company-idle-delay 0)
;;   (setq company-minimum-prefix-length 1)
;;   :diminish)

(provide 'completion)
;;; completion.el ends here
