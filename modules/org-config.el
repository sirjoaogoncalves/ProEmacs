;;; org-config.el --- Org Mode Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for Org Mode - the ultimate organization tool in Emacs

;;; Code:

;; Basic Org Mode setup
(use-package org
  :defer t
  :hook (org-mode . visual-line-mode)
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))
  :custom
  ;; General Org settings
  (org-directory "~/org")                      ;; Set org directory
  (org-default-notes-file "~/org/inbox.org")  ;; Default capture target
  (org-ellipsis " ▾")                         ;; Use downward-pointing triangle for folded sections
  (org-hide-emphasis-markers t)               ;; Hide markup characters
  (org-fontify-done-headline t)               ;; Apply special face to DONE headlines
  (org-log-done 'time)                        ;; Add timestamp when marking task as done
  (org-log-into-drawer t)                     ;; Put state change notes into a drawer
  (org-pretty-entities t)                     ;; Display entities like \alpha as UTF-8 characters
  
  ;; Agenda settings
  (org-agenda-files '("~/org/inbox.org"
                      "~/org/gtd.org"
                      "~/org/projects.org"
                      "~/org/calendar.org"))
  (org-agenda-span 'day)                      ;; Show one day by default
  (org-agenda-start-with-log-mode t)          ;; Include log mode by default
  (org-agenda-skip-deadline-prewarning-if-scheduled t) ;; Skip deadline warnings for scheduled tasks
  (org-agenda-skip-scheduled-if-done t)       ;; Skip scheduled tasks that are done
  (org-agenda-todo-ignore-scheduled 'future)  ;; Ignore tasks scheduled in the future
  
  ;; Todo keywords
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "SOMEDAY(s)" "|" "DONE(d)" "CANCELLED(c)")))
  (org-todo-keyword-faces
   '(("TODO" . (:foreground "firebrick" :weight bold))
     ("NEXT" . (:foreground "blue" :weight bold))
     ("WAITING" . (:foreground "orange" :weight bold))
     ("SOMEDAY" . (:foreground "purple" :weight bold))
     ("DONE" . (:foreground "forest green" :weight bold))
     ("CANCELLED" . (:foreground "gray" :weight bold))))
  
  ;; Capture templates
  (org-capture-templates
   '(("t" "Todo" entry (file+headline "~/org/inbox.org" "Tasks")
      "* TODO %?\n%i\n%a" :empty-lines 1)
     ("n" "Note" entry (file+datetree "~/org/notes.org")
      "* %?\n%i\n%a" :empty-lines 1)
     ("j" "Journal" entry (file+datetree "~/org/journal.org")
      "* %?\nEntered on %U\n%i\n%a" :empty-lines 1)))
  
  ;; Refile settings
  (org-refile-targets '((nil :maxlevel . 3)
                         (org-agenda-files :maxlevel . 2)))
  (org-refile-use-outline-path 'file)        ;; Use filenames for refiling
  (org-outline-path-complete-in-steps nil)   ;; Show full paths for refiling
  
  :config
  ;; Create default org directories and files if they don't exist
  (unless (file-exists-p org-directory)
    (make-directory org-directory t))
  
  (dolist (file '("inbox.org" "gtd.org" "projects.org" "calendar.org" "notes.org" "journal.org"))
    (let ((path (expand-file-name file org-directory)))
      (unless (file-exists-p path)
        (with-temp-file path
          (insert (format "#+TITLE: %s\n#+AUTHOR: User\n#+STARTUP: overview\n\n" 
                          (capitalize (file-name-sans-extension file))))))))
  
  ;; Set visual settings
  (setq org-startup-indented t)              ;; Enable org-indent mode by default
  (setq org-startup-with-inline-images t)    ;; Show inline images by default
  (setq org-image-actual-width '(300))       ;; Set default image width
  
  ;; Enhance Org appearance
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  
  ;; Set up babel for literate programming
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t)
     (js . t)
     (css . t))))

;; Org bullets for prettier headers
(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Visual fill for better reading
(use-package visual-fill-column
  :hook (org-mode . visual-fill-column-mode)
  :custom
  (visual-fill-column-width 100)
  (visual-fill-column-center-text t))

;; Org-roam for knowledge management
(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/org/roam"))
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+date: %U\n#+filetags: \n\n")
      :unnarrowed t)
     ("r" "reference" plain
      "* Source\n\nAuthor: %^{Author}\nTitle: ${title}\nYear: %^{Year}\n\n* Summary\n\n%?"
      :target (file+head "references/${slug}.org"
                         "#+title: ${title}\n#+date: %U\n#+filetags: reference\n\n")
      :unnarrowed t)))
  :config
  (unless (file-exists-p org-roam-directory)
    (make-directory org-roam-directory t))
  (org-roam-db-autosync-mode))

;; Org-roam-ui for graph visualization
(use-package org-roam-ui
  :after org-roam
  ;; The package seems to use different function names than expected
  ;; So we'll use a simpler configuration that should work more reliably
  :custom
  (org-roam-ui-follow nil)        ;; don't auto-follow nodes
  (org-roam-ui-update-on-save t)) ;; update when saving notes



;; Enable evil-org for better evil integration
(use-package evil-org
  :after (evil org)
  :hook (org-mode . evil-org-mode)
  :config
  ;; Load the agenda module after evil-org is loaded
  (with-eval-after-load 'evil-org
    (require 'evil-org-agenda nil t)
    (when (featurep 'evil-org-agenda)
      (evil-org-agenda-set-keys))))

(provide 'org-config)
;;; org-config.el ends here
