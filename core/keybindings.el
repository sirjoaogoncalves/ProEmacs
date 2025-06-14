;;; keybindings.el --- Global keybindings -*- lexical-binding: t; -*-

(require 'general)

;;; Code:

(general-create-definer my-leader-keys
  :keymaps '(normal insert visual emacs)
  :prefix "SPC"
  :global-prefix "C-SPC")

;; Helper functions for file operations (fallback if utils not loaded)
(defun get-current-directory ()
  "Get current directory intelligently from buffer, dired, or default."
  (or (when buffer-file-name
        (file-name-directory buffer-file-name))
      (when (derived-mode-p 'dired-mode)
        default-directory)
      default-directory))

(defun ensure-directory-slash (dir)
  "Ensure DIR ends with a slash."
  (if (string-suffix-p "/" dir) dir (concat dir "/")))

;; File operations
(my-leader-keys
  "f" '(:ignore t :which-key "file")
  "ff" #'find-file :which-key "find file"
  "fs" #'save-buffer :which-key "save file"
  "fR" #'rename-file-and-buffer :which-key "rename file"
  "fd" #'dired :which-key "dired"
  "fF" #'normal/format-buffer :which-key "format buffer"
  "ft" #'normal/toggle-format-on-save :which-key "toggle format on save"
  "fl" #'my/download-buffer-locally :which-key "download locally"
  "fr" #'advanced-search/recent-files :which-key "recent files"
  "fp" #'advanced-search/project-files :which-key "project files")

(my-leader-keys
  "fn" '(:ignore t :which-key "new")
  "fnf" #'create-file-in-current-dir :which-key "new file"
  "fnd" #'create-directory-in-current-dir :which-key "new directory")

(my-leader-keys
  "fD" '(:ignore t :which-key "delete")
  "fDf" #'delete-current-file :which-key "delete file"
  "fDd" #'delete-directory-prompt :which-key "delete directory")

(my-leader-keys
  "fc" '(:ignore t :which-key "copy")
  "fcf" #'copy-file-current :which-key "copy file"
  "fcd" #'copy-directory-prompt :which-key "copy directory")

;; Buffer operations
(my-leader-keys
  "b" '(:ignore t :which-key "buffer")
  "bb" #'switch-to-buffer :which-key "switch buffer"
  "bd" #'kill-current-buffer :which-key "kill buffer"
  "bs" #'save-buffer :which-key "save buffer"
  "bR" #'rename-buffer :which-key "rename buffer"
  "br" #'revert-buffer :which-key "revert buffer")

(my-leader-keys
  "B" '(:ignore t :which-key "buffer management")
  "Bc" #'normal/cleanup-buffers :which-key "cleanup unused"
  "Bl" #'normal/limit-buffers-by-mode :which-key "limit by mode"
  "Bo" #'normal/kill-other-buffers :which-key "kill other buffers"
  "Bm" #'normal/kill-matching-buffers :which-key "kill matching"
  "Bd" #'normal/kill-dired-buffers :which-key "kill dired buffers"
  "BM" #'normal/kill-buffers-by-mode :which-key "kill by major mode"
  "Bb" #'normal/kill-buried-buffers :which-key "kill buried buffers"
  "Bg" #'normal/switch-to-buffer-group :which-key "group by mode")

;; Window operations
(my-leader-keys
  "w" '(:ignore t :which-key "window")
  "ww" #'hydra-window/body :which-key "window hydra"
  "wv" #'split-window-right :which-key "split vertical"
  "ws" #'split-window-below :which-key "split horizontal"
  "wh" #'windmove-left :which-key "window left"
  "wj" #'windmove-down :which-key "window down"
  "wk" #'windmove-up :which-key "window up"
  "wl" #'windmove-right :which-key "window right"
  "wq" #'delete-window :which-key "close window"
  "wo" #'delete-other-windows :which-key "close other windows"
  "wd" #'delete-window :which-key "delete window"
  "wD" #'kill-buffer-and-window :which-key "kill buffer and window"
  "wm" #'toggle-window-maximize :which-key "maximize toggle"
  "w=" #'balance-windows :which-key "balance windows"
  "wr" #'rotate-windows :which-key "rotate"
  "wu" #'winner-undo :which-key "undo window config"
  "wR" #'winner-redo :which-key "redo window config"
  "wt" #'toggle-window-split :which-key "toggle split direction"
  "wa" #'ace-window :which-key "ace window")

;; Tab operations
(my-leader-keys
  "TAB" '(:ignore t :which-key "tabs")
  "TAB TAB" #'tab-bar-switch-to-tab :which-key "switch tab"
  "TAB n" #'tab-bar-new-tab :which-key "new tab"
  "TAB c" #'tab-bar-close-tab :which-key "close tab"
  "TAB r" #'tab-bar-rename-tab :which-key "rename tab"
  "TAB ]" #'tab-bar-switch-to-next-tab :which-key "next tab"
  "TAB [" #'tab-bar-switch-to-prev-tab :which-key "prev tab"
  "TAB d" #'tab-bar-close-other-tabs :which-key "close other tabs"
  "TAB b" #'tab-bar-switch-to-recent-tab :which-key "recent tab")

;; Layout operations
(my-leader-keys
  "l" '(:ignore t :which-key "layouts")
  "lc" #'eyebrowse-create-window-config :which-key "create layout"
  "ln" #'eyebrowse-next-window-config :which-key "next layout"
  "lp" #'eyebrowse-prev-window-config :which-key "prev layout"
  "l0" #'eyebrowse-switch-to-window-config-0 :which-key "layout 0"
  "l1" #'eyebrowse-switch-to-window-config-1 :which-key "layout 1"
  "l2" #'eyebrowse-switch-to-window-config-2 :which-key "layout 2"
  "l3" #'eyebrowse-switch-to-window-config-3 :which-key "layout 3"
  "l4" #'eyebrowse-switch-to-window-config-4 :which-key "layout 4"
  "l5" #'eyebrowse-switch-to-window-config-5 :which-key "layout 5"
  "l6" #'eyebrowse-switch-to-window-config-6 :which-key "layout 6"
  "l7" #'eyebrowse-switch-to-window-config-7 :which-key "layout 7"
  "l8" #'eyebrowse-switch-to-window-config-8 :which-key "layout 8"
  "l9" #'eyebrowse-switch-to-window-config-9 :which-key "layout 9"
  "lr" #'eyebrowse-rename-window-config :which-key "rename layout"
  "ll" #'eyebrowse-last-window-config :which-key "last layout"
  "lx" #'eyebrowse-close-window-config :which-key "close layout")

;; Help/Documentation
(my-leader-keys
  "h" '(:ignore t :which-key "help")
  "hf" #'describe-function :which-key "describe function"
  "hv" #'describe-variable :which-key "describe variable"
  "hk" #'describe-key :which-key "describe key"
  "hm" #'describe-mode :which-key "describe mode")

;; Search and Replace
(my-leader-keys
  "s" '(:ignore t :which-key "search")
  "ss" #'advanced-search/smart-search :which-key "smart search"
  "sp" #'consult-ripgrep :which-key "project search"
  "sc" #'consult-ripgrep-code :which-key "search code"
  "sd" #'consult-ripgrep-docs :which-key "search docs"
  "sr" #'advanced-search/find-references-at-point :which-key "find references"
  "st" #'advanced-search/search-todos :which-key "search TODOs")

(my-leader-keys
  "sR" '(:ignore t :which-key "replace")
  "sRr" #'query-replace :which-key "replace interactive"
  "sRs" #'replace-string :which-key "replace string (all)"
  "sRe" #'query-replace-regexp :which-key "replace regexp"
  "sRE" #'replace-regexp :which-key "replace regexp (all)"
  "sRp" #'project-query-replace-regexp :which-key "replace in project")

;; Toggle operations
(my-leader-keys
  "t" '(:ignore t :which-key "toggle")
  "tt" #'load-theme :which-key "choose theme"
  "tT" #'doom-theme-selector :which-key "doom theme selector"
  "tl" #'display-line-numbers-mode :which-key "line numbers"
  "tm" #'menu-bar-mode :which-key "menu bar")

;; Open Configuration
(my-leader-keys
  "O" '(:ignore t :which-key "open")
  "Oc" #'(lambda () (interactive) (find-file "~/.emacs.d/init.el")) :which-key "open init.el"
  "Ot" #'open-terminal-here :which-key "open terminal")

;; AI Tools
(my-leader-keys
  "a" '(:ignore t :which-key "AI")
  "am" #'minuet-show-suggestion :which-key "minuet suggestion"
  "aM" #'minuet-complete-with-minibuffer :which-key "minuet minibuffer"
  "ac" #'minuet-configure-provider :which-key "configure minuet"
  "ae" #'ai-coding/explain-code :which-key "explain code"
  "ar" #'ai-coding/refactor-code :which-key "refactor code"
  "at" #'ai-coding/toggle-thinking-mode :which-key "toggle thinking mode"
  "ai" #'ai-coding/edit-instructions :which-key "edit AI instructions"
  "aC" #'ai-coding/open-chat :which-key "open chat interface"
  "aS" #'ai-coding/add-code-to-chat :which-key "add code to chat"
  "an" #'ai-coding/new-chat-session :which-key "new chat session"
  "ah" #'ai-coding/show-chat-history :which-key "show chat history"
  "ad" #'ai-coding/debug-status :which-key "debug status")

;; Completion Framework
(my-leader-keys
  "c" '(:ignore t :which-key "completion")
  "cc" #'completion-at-point :which-key "complete at point"
  "ci" #'consult-imenu :which-key "imenu"
  "cl" #'consult-line :which-key "search lines")

;; Dashboard Operations
(my-leader-keys
  "D" '(:ignore t :which-key "dashboard")
  "DD" #'dashboard-refresh-buffer :which-key "open dashboard")

;; Project Management
(my-leader-keys
  "p" '(:ignore t :which-key "project")
  "pp" #'projectile-switch-project :which-key "switch project"
  "pf" #'projectile-find-file :which-key "find file in project"
  "pb" #'projectile-switch-to-buffer :which-key "switch buffer in project"
  "pk" #'projectile-kill-buffers :which-key "kill project buffers"
  "pc" #'projectile-compile-project :which-key "compile project"
  "pa" #'projectile-add-known-project :which-key "add project")

;; LSP Features
(my-leader-keys
  "L" '(:ignore t :which-key "lsp")
  "La" #'lsp-execute-code-action :which-key "code action"
  "Lr" #'lsp-rename :which-key "rename"
  "Ld" #'lsp-find-definition :which-key "find definition"
  "LD" #'lsp-find-declaration :which-key "find declaration"
  "Li" #'lsp-find-implementation :which-key "find implementation"
  "LR" #'lsp-find-references :which-key "find references"
  "Lf" #'lsp-format-buffer :which-key "format buffer")

;; Dired Operations
(my-leader-keys
  "d" '(:ignore t :which-key "dired")
  "dd" #'dired :which-key "open dired"
  "dj" #'dired-jump :which-key "dired jump to current"
  "df" #'find-name-dired :which-key "find name"
  "dg" #'find-grep-dired :which-key "find grep")

(my-leader-keys
  "dm" '(:ignore t :which-key "mark")
  "dma" #'dired-mark-files-regexp :which-key "mark by regexp"
  "dme" #'dired-mark-extension :which-key "mark by extension"
  "dmd" #'dired-mark-directories :which-key "mark directories"
  "dmu" #'dired-unmark-all-marks :which-key "unmark all")

(my-leader-keys
  "do" '(:ignore t :which-key "operations")
  "doc" #'dired-do-copy :which-key "copy"
  "dom" #'dired-do-rename :which-key "move/rename"
  "dod" #'dired-do-delete :which-key "delete"
  "doz" #'dired-do-compress :which-key "compress"
  "dos" #'dired-do-symlink :which-key "symlink"
  "doh" #'dired-do-hardlink :which-key "hardlink"
  "dot" #'dired-do-touch :which-key "touch"
  "don" #'dired-create-empty-file :which-key "create empty file")

(my-leader-keys
  "dt" '(:ignore t :which-key "toggle")
  "dth" #'dired-hide-details-mode :which-key "hide details"
  "dti" #'all-the-icons-dired-mode :which-key "toggle icons"
  "dto" #'dired-omit-mode :which-key "toggle omit mode"
  "dte" #'wdired-change-to-wdired-mode :which-key "edit mode (wdired)")

;; Git Operations
(my-leader-keys
  "g" '(:ignore t :which-key "git")
  "gs" #'magit-status :which-key "git status"
  "gb" #'magit-blame :which-key "git blame"
  "gl" #'magit-log-current :which-key "git log"
  "gc" #'magit-commit :which-key "git commit"
  "gp" #'magit-push :which-key "git push"
  "gP" #'magit-pull :which-key "git pull"
  "gd" #'magit-diff :which-key "git diff"
  "gt" #'git-timemachine :which-key "git time machine"
  "gB" #'blamer-mode :which-key "toggle git blame"
  "gS" #'magit-stage-file :which-key "git stage file"
  "gU" #'magit-unstage-file :which-key "git unstage file")

;; Org Mode Operations (basic structure)
(my-leader-keys
  "o" '(:ignore t :which-key "org")
  "oa" #'org-agenda :which-key "agenda"
  "oc" #'org-capture :which-key "capture"
  "ol" #'org-store-link :which-key "store link"
  "oi" #'org-insert-link :which-key "insert link"
  "ot" #'org-todo :which-key "todo state"
  "os" #'org-schedule :which-key "schedule"
  "od" #'org-deadline :which-key "deadline"
  "op" #'org-priority :which-key "priority"
  "or" #'org-refile :which-key "refile"
  "oo" #'org-open-at-point :which-key "open link")

;; Performance Tools
(my-leader-keys
  "P" '(:ignore t :which-key "performance")
  "Pp" #'profiler-start :which-key "start profiler"
  "Ps" #'profiler-stop :which-key "stop profiler"
  "Pr" #'profiler-report :which-key "profiler report"
  "Pg" #'garbage-collect :which-key "garbage collect"
  "Pd" #'memory-report :which-key "memory usage report"
  "Pt" #'my/display-package-stats :which-key "package load times"
  "Pf" #'normal/optimize-font-rendering :which-key "optimize fonts")

;; Process Management
(my-leader-keys
  "m" '(:ignore t :which-key "processes")
  "ms" #'normal/process-manager-status :which-key "process status")

;; Jump operations
(my-leader-keys
  "j" '(:ignore t :which-key "jump")
  "jj" #'avy-goto-char :which-key "jump to char"
  "jl" #'avy-goto-line :which-key "jump to line"
  "jw" #'avy-goto-word-1 :which-key "jump to word")

;; Container/Docker Operations
(my-leader-keys
  "C" '(:ignore t :which-key "containers")
  "CC" #'docker/dashboard :which-key "docker dashboard"
  "Ci" #'docker/system-info :which-key "system info"
  "Cp" #'docker/system-prune :which-key "system prune"
  "Cs" #'docker/start-container :which-key "start container"
  "CS" #'docker/stop-container :which-key "stop container"
  "Cr" #'docker/restart-container :which-key "restart container"
  "CR" #'docker/remove-container :which-key "remove container"
  "Cl" #'docker/container-logs :which-key "container logs"
  "Cx" #'docker/container-shell :which-key "container shell"
  "Ct" #'docker/container-inspect :which-key "inspect container"
  "Cu" #'docker/compose-up :which-key "compose up"
  "Cd" #'docker/compose-down :which-key "compose down")

;; Unicode tools
(my-leader-keys
  "u" '(:ignore t :which-key "unicode")
  "uf" #'unicode-fix-setup-fonts :which-key "setup fonts"
  "ut" #'unicode-fix-test-characters :which-key "test characters"
  "uc" #'unicode-fix-check-character-at-point :which-key "check character"
  "ua" #'unicode-fix-setup-all :which-key "setup all")

;; File and directory utility functions
(defun rename-file-and-buffer ()
  "Rename the current buffer and the file it's visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (when (and new-name (not (string-equal filename new-name)))
          (if (vc-backend filename)
              (vc-rename-file filename new-name)
            (rename-file filename new-name t)
            (set-visited-file-name new-name t t)))))))

(defun create-file-in-current-dir ()
  "Create a new file in the current directory."
  (interactive)
  (let* ((base-dir (get-current-directory))
         (prompt-dir (ensure-directory-slash base-dir))
         (filename (read-string (format "Create file in %s: " prompt-dir) prompt-dir)))
    (when (and filename (not (string-equal filename prompt-dir)))
      (find-file filename))))

(defun create-directory-in-current-dir ()
  "Create a new directory in the current directory."
  (interactive)
  (let* ((base-dir (get-current-directory))
         (prompt-dir (ensure-directory-slash base-dir))
         (dir-name (read-directory-name "Create directory: " prompt-dir)))
    (when (and dir-name (not (string-equal dir-name prompt-dir)))
      (make-directory dir-name t)
      (message "Directory created: %s" dir-name)
      (when (derived-mode-p 'dired-mode)
        (revert-buffer)))))

(defun delete-current-file ()
  "Delete the current file and kill its buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer)))
    (if (not filename)
        (message "Buffer is not visiting a file!")
      (when (yes-or-no-p (format "Really delete %s? " filename))
        (delete-file filename t)
        (message "File %s deleted." filename)
        (kill-buffer buffer)))))

(defun delete-directory-prompt ()
  "Prompt for a directory to delete recursively."
  (interactive)
  (let* ((base-dir (get-current-directory))
         (prompt-dir (ensure-directory-slash base-dir))
         (dir-name (read-directory-name "Delete directory (recursively): " prompt-dir)))
    (when (and dir-name (file-directory-p dir-name) (not (string-equal dir-name prompt-dir)))
      (when (yes-or-no-p (format "Really delete directory %s and all its contents? " dir-name))
        (delete-directory dir-name t)
        (message "Directory deleted: %s" dir-name)
        (when (derived-mode-p 'dired-mode)
          (revert-buffer))))))

(defun copy-file-current ()
  "Copy the current file to a new location and visit the copy."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer is not visiting a file!")
      (let* ((new-name (read-file-name (format "Copy %s to: " filename) (file-name-directory filename))))
        (when (and new-name (not (string-equal filename new-name)))
          (copy-file filename new-name t t t)
          (message "File copied to %s" new-name)
          (find-file new-name))))))

(defun copy-directory-prompt ()
  "Prompt for a source and target directory to copy."
  (interactive)
  (let* ((base-dir (get-current-directory))
         (prompt-dir (ensure-directory-slash base-dir))
         (source-dir (read-directory-name "Copy directory: " prompt-dir))
         (target-dir (read-directory-name (format "Copy %s to: " source-dir) prompt-dir)))
    (when (and source-dir (file-directory-p source-dir)
               target-dir (not (string-equal source-dir target-dir)))
      (copy-directory source-dir target-dir nil t t)
      (message "Directory %s copied to %s" source-dir target-dir)
      (when (derived-mode-p 'dired-mode)
        (revert-buffer)))))

(provide 'keybindings)
;;; keybindings.el ends here
