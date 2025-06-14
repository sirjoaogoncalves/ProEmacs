;;; unicode-complete-fix.el --- Comprehensive Unicode rendering fix for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; Complete Unicode character rendering solution for Emacs.
;; Fixes rendering issues with dashes, symbols, mathematical notation,
;; arrows, box drawing, and all other Unicode blocks.
;; Works in both GUI and terminal Emacs.

;;; Code:

(require 'fontset)

;; ╔══════════════════════════════════════════════════════════════════════════╗
;; ║ Core Unicode Configuration                                               ║
;; ╚══════════════════════════════════════════════════════════════════════════╝

(defgroup unicode-fix nil
  "Comprehensive Unicode rendering fixes for Emacs."
  :group 'display
  :group 'i18n)

(defcustom unicode-fix-font-fallback-list
  '(;; Comprehensive Unicode fonts (ordered by preference)
    "Noto Sans Symbols"
    "Noto Sans Symbols2"
    "Symbola"
    "DejaVu Sans"
    "Liberation Sans"
    "Arial Unicode MS"
    "Lucida Grande"
    "Apple Color Emoji"
    "Noto Color Emoji"
    "Segoe UI Emoji"
    "Segoe UI Symbol"
    "Source Code Pro"
    "Menlo"
    "Monaco"
    "Consolas")
  "List of fonts to use as Unicode fallbacks, in order of preference."
  :type '(repeat string)
  :group 'unicode-fix)

;; ╔══════════════════════════════════════════════════════════════════════════╗
;; ║ Unicode Block Definitions                                                ║
;; ╚══════════════════════════════════════════════════════════════════════════╝

(defconst unicode-fix-unicode-blocks
  '(;; Common problematic blocks
    (latin-1-supplement        #x0080 #x00FF)
    (latin-extended-a           #x0100 #x017F)
    (latin-extended-b           #x0180 #x024F)
    (spacing-modifier-letters   #x02B0 #x02FF)
    (combining-diacritical      #x0300 #x036F)

    ;; Punctuation and symbols (most common issues)
    (general-punctuation        #x2000 #x206F)  ; includes em-dash, en-dash
    (superscripts-subscripts    #x2070 #x209F)
    (currency-symbols           #x20A0 #x20CF)
    (letterlike-symbols         #x2100 #x214F)
    (number-forms               #x2150 #x218F)
    (arrows                     #x2190 #x21FF)
    (mathematical-operators     #x2200 #x22FF)
    (miscellaneous-technical    #x2300 #x23FF)
    (control-pictures           #x2400 #x243F)
    (optical-character-recognition #x2440 #x245F)
    (enclosed-alphanumerics     #x2460 #x24FF)
    (box-drawing                #x2500 #x257F)  ; often used in CLI output
    (block-elements             #x2580 #x259F)
    (geometric-shapes           #x25A0 #x25FF)
    (miscellaneous-symbols      #x2600 #x26FF)
    (dingbats                   #x2700 #x27BF)
    (miscellaneous-mathematical-symbols-a #x27C0 #x27EF)
    (supplemental-arrows-a      #x27F0 #x27FF)
    (braille-patterns           #x2800 #x28FF)
    (supplemental-arrows-b      #x2900 #x297F)
    (miscellaneous-mathematical-symbols-b #x2980 #x29FF)
    (supplemental-mathematical-operators #x2A00 #x2AFF)
    (miscellaneous-symbols-and-arrows #x2B00 #x2BFF)

    ;; CJK and other scripts
    (cjk-symbols-punctuation    #x3000 #x303F)
    (hiragana                   #x3040 #x309F)
    (katakana                   #x30A0 #x30FF)
    (cjk-unified-ideographs     #x4E00 #x9FFF)

    ;; Mathematical notation
    (mathematical-alphanumeric-symbols #x1D400 #x1D7FF)

    ;; Emoji ranges
    (emoticons                  #x1F600 #x1F64F)
    (miscellaneous-symbols-and-pictographs #x1F300 #x1F5FF)
    (transport-and-map-symbols  #x1F680 #x1F6FF)
    (supplemental-symbols       #x1F900 #x1F9FF))
  "Unicode blocks that commonly have rendering issues.")

;; ╔══════════════════════════════════════════════════════════════════════════╗
;; ║ Font Detection and Setup                                                 ║
;; ╚══════════════════════════════════════════════════════════════════════════╝

(defun unicode-fix-font-exists-p (font-name)
  "Check if FONT-NAME exists on the system."
  (member font-name (font-family-list)))

(defun unicode-fix-get-available-fonts ()
  "Return list of available Unicode fonts from our fallback list."
  (seq-filter #'unicode-fix-font-exists-p unicode-fix-font-fallback-list))

(defun unicode-fix-setup-fontset (fontset-name available-fonts)
  "Setup Unicode font fallbacks for FONTSET-NAME using AVAILABLE-FONTS."
  (dolist (font available-fonts)
    ;; Set font for all Unicode blocks
    (dolist (block unicode-fix-unicode-blocks)
      (let ((start (nth 1 block))
            (end   (nth 2 block)))
        (condition-case nil
            (set-fontset-font fontset-name
                            (cons start end)
                            (font-spec :family font)
                            nil 'append)
          (error nil))))

    ;; Set font for general Unicode ranges
    (condition-case nil
        (progn
          (set-fontset-font fontset-name 'unicode
                          (font-spec :family font) nil 'append)
          (set-fontset-font fontset-name 'symbol
                          (font-spec :family font) nil 'append))
      (error nil))))

;; ╔══════════════════════════════════════════════════════════════════════════╗
;; ║ Main Unicode Fix Functions                                               ║
;; ╚══════════════════════════════════════════════════════════════════════════╝

;;;###autoload
(defun unicode-fix-setup-fonts ()
  "Setup comprehensive Unicode font fallbacks for Emacs."
  (interactive)
  (when (display-graphic-p)
    (let ((available-fonts (unicode-fix-get-available-fonts)))
      (if available-fonts
          (progn
            (message "Setting up Unicode fonts: %s"
                    (string-join available-fonts ", "))

            ;; Setup for default fontset
            (unicode-fix-setup-fontset t available-fonts)

            ;; Setup for startup fontset if it exists
            (when (fontset-name-p "fontset-startup")
              (unicode-fix-setup-fontset "fontset-startup" available-fonts))

            ;; Force fontset refresh
            (when (fboundp 'clear-font-cache)
              (clear-font-cache))

            (message "Unicode font setup complete"))
        (message "Warning: No Unicode fonts found. Install fonts like Noto Sans, DejaVu Sans, or Symbola")))))

;;;###autoload
(defun unicode-fix-setup-terminal ()
  "Setup Unicode for terminal Emacs."
  (interactive)
  (unless (display-graphic-p)
    ;; Ensure UTF-8 encoding
    (set-terminal-coding-system 'utf-8)
    (set-keyboard-coding-system 'utf-8)
    (prefer-coding-system 'utf-8)

    ;; Set up terminal-specific Unicode handling
    (when (getenv "TERM")
      (let ((term (getenv "TERM")))
        (cond
         ;; Modern terminals with good Unicode support
         ((string-match-p "xterm\\|alacritty\\|kitty\\|iterm" term)
          (set-terminal-parameter nil 'encode-terminal-output-buffer t))
         ;; Enable UTF-8 mouse support if available
         ((string-match-p "screen\\|tmux" term)
          (when (fboundp 'xterm-mouse-mode)
            (xterm-mouse-mode 1))))))

    (message "Terminal Unicode setup complete")))

;; ╔══════════════════════════════════════════════════════════════════════════╗
;; ║ Unicode Debugging and Utilities                                         ║
;; ╚══════════════════════════════════════════════════════════════════════════╝

;;;###autoload
(defun unicode-fix-test-characters ()
  "Display test Unicode characters to verify rendering."
  (interactive)
  (with-current-buffer (get-buffer-create "*Unicode Test*")
    (erase-buffer)
    (insert "=== Unicode Rendering Test ===\n\n")

    ;; Common problematic characters
    (insert "Dashes and punctuation:\n")
    (insert "Hyphen: - (U+002D)\n")
    (insert "En dash: – (U+2013)\n")
    (insert "Em dash: — (U+2014)\n")
    (insert "Horizontal bar: ― (U+2015)\n")
    (insert "Left single quote: ' (U+2018)\n")
    (insert "Right single quote: ' (U+2019)\n")
    (insert "Left double quote: " (U+201C)\n")
    (insert "Right double quote: " (U+201D)\n\n")

    ;; Mathematical symbols
    (insert "Mathematical symbols:\n")
    (insert "Multiplication: × (U+00D7)\n")
    (insert "Division: ÷ (U+00F7)\n")
    (insert "Plus-minus: ± (U+00B1)\n")
    (insert "Infinity: ∞ (U+221E)\n")
    (insert "Sum: ∑ (U+2211)\n")
    (insert "Integral: ∫ (U+222B)\n\n")

    ;; Arrows
    (insert "Arrows:\n")
    (insert "Left arrow: ← (U+2190)\n")
    (insert "Right arrow: → (U+2192)\n")
    (insert "Up arrow: ↑ (U+2191)\n")
    (insert "Down arrow: ↓ (U+2193)\n\n")

    ;; Box drawing (common in terminal output)
    (insert "Box drawing:\n")
    (insert "┌─┬─┐\n")
    (insert "│ │ │\n")
    (insert "├─┼─┤\n")
    (insert "│ │ │\n")
    (insert "└─┴─┘\n\n")

    ;; Currency and symbols
    (insert "Currency and symbols:\n")
    (insert "Euro: € (U+20AC)\n")
    (insert "Pound: £ (U+00A3)\n")
    (insert "Yen: ¥ (U+00A5)\n")
    (insert "Copyright: © (U+00A9)\n")
    (insert "Registered: ® (U+00AE)\n")
    (insert "Trademark: ™ (U+2122)\n\n")

    ;; Emoji (if supported)
    (insert "Emoji (if supported):\n")
    (insert "😊 🚀 💻 🔧 ⚡ 📝 🎯\n\n")

    (insert "If any characters appear as boxes, question marks, or are missing,\n")
    (insert "your Unicode font setup needs adjustment.\n")

    (goto-char (point-min))
    (display-buffer (current-buffer))))

;;;###autoload
(defun unicode-fix-check-character-at-point ()
  "Show information about the Unicode character at point."
  (interactive)
  (let* ((char (char-after))
         (char-code (when char (format "U+%04X" char)))
         (char-name (when char (get-char-code-property char 'name)))
         (font-info (when (and char (display-graphic-p))
                      (font-at (point)))))
    (if char
        (message "Character: %c | Code: %s | Name: %s | Font: %s"
                 char
                 (or char-code "unknown")
                 (or char-name "unknown")
                 (or font-info "default"))
      (message "No character at point"))))

;; ╔══════════════════════════════════════════════════════════════════════════╗
;; ║ Auto-setup and Integration                                               ║
;; ╚══════════════════════════════════════════════════════════════════════════╝

;;;###autoload
(defun unicode-fix-setup-all ()
  "Setup Unicode rendering for both GUI and terminal Emacs."
  (interactive)
  (if (display-graphic-p)
      (unicode-fix-setup-fonts)
    (unicode-fix-setup-terminal)))

;; Auto-setup on startup
(add-hook 'after-init-hook #'unicode-fix-setup-all)

;; Auto-setup for new frames
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (unicode-fix-setup-all))))

;; ╔══════════════════════════════════════════════════════════════════════════╗
;; ║ AI Response Unicode Cleaning (Optional)                                 ║
;; ╚══════════════════════════════════════════════════════════════════════════╝

(defcustom unicode-fix-clean-ai-responses nil
  "When non-nil, automatically convert fancy Unicode in AI responses to ASCII."
  :type 'boolean
  :group 'unicode-fix)

(defconst unicode-fix-ascii-replacements
  '(("—" . "--")    ; em-dash to double hyphen
    ("–" . "-")     ; en-dash to hyphen
    ("'" . "'")     ; left single quote
    ("'" . "'")     ; right single quote
    (""" . "\"")    ; left double quote
    (""" . "\"")    ; right double quote
    ("…" . "...")   ; ellipsis
    ("×" . "x")     ; multiplication
    ("÷" . "/")     ; division
    ("±" . "+/-")   ; plus-minus
    ("→" . "->")    ; right arrow
    ("←" . "<-")    ; left arrow
    ("↑" . "^")     ; up arrow
    ("↓" . "v"))    ; down arrow
  "Mapping of Unicode characters to ASCII equivalents.")

(defun unicode-fix-clean-unicode-string (text)
  "Replace fancy Unicode characters in TEXT with ASCII equivalents."
  (when unicode-fix-clean-ai-responses
    (dolist (replacement unicode-fix-ascii-replacements)
      (setq text (replace-regexp-in-string (car replacement) (cdr replacement) text))))
  text)

;; ╔══════════════════════════════════════════════════════════════════════════╗
;; ║ Integration with ai-enhanced-coding.el                                  ║
;; ╚══════════════════════════════════════════════════════════════════════════╝

;; Add advice to clean AI responses if desired
(defun unicode-fix-clean-ai-response-advice (orig-fun response &optional thinking)
  "Advice to clean Unicode in AI responses."
  (let ((cleaned-response (unicode-fix-clean-unicode-string response))
        (cleaned-thinking (when thinking
                           (unicode-fix-clean-unicode-string thinking))))
    (funcall orig-fun cleaned-response cleaned-thinking)))

;; Uncomment to enable AI response cleaning:
;; (advice-add 'ai-coding/explain-code :around #'unicode-fix-clean-ai-response-advice)
;; (advice-add 'ai-coding/refactor-code :around #'unicode-fix-clean-ai-response-advice)

;; ╔══════════════════════════════════════════════════════════════════════════╗
;; ║ Keybindings                                                              ║
;; ╚══════════════════════════════════════════════════════════════════════════╝

;; Add these to your keybindings.el if desired:
;; (global-set-key (kbd "C-c u f") #'unicode-fix-setup-fonts)
;; (global-set-key (kbd "C-c u t") #'unicode-fix-test-characters)
;; (global-set-key (kbd "C-c u c") #'unicode-fix-check-character-at-point)

(provide 'unicode-complete-fix)
;;; unicode-complete-fix.el ends here
