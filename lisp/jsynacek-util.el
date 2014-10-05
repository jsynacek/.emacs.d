;;
;; qwerty based keybinding layout
;;



;;; insertion
(defun jsynacek-insert-brackets ()
  (interactive)
  (insert-pair nil ?( ?)))

(defun jsynacek-insert-curly ()
  (interactive)
  (insert-pair nil ?{ ?}))

(defun jsynacek-insert-double-quotes ()
  (interactive)
  (insert-pair nil ?\" ?\"))

(defun jsynacek-insert-single-quotes ()
  (interactive)
  (insert-pair nil ?' ?'))

(defun jsynacek-open-below ()
  (interactive)
  (end-of-line)
  (newline-and-indent)
  (jsynacek-emacs-mode))

(defun jsynacek-open-above ()
  (interactive)
  (beginning-of-line)
  (backward-char)
  (newline-and-indent)
  (jsynacek-emacs-mode))

(defun jsynacek-change-char ()
  (interactive)
  (delete-char 1)
  (jsynacek-emacs-mode))

(defun jsynacek-change ()
  (interactive)
  (jsynacek-kill-line-or-region)
  (jsynacek-emacs-mode))

(defun jsynacek-copy-line-or-region ()
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (save-excursion
      (kill-ring-save (line-beginning-position) (1+ (line-end-position))))))

;;; marking
(defun jsynacek-mark-line ()
  "Select the current line"
  (interactive)
  (move-beginning-of-line nil)
  (set-mark (point))
  (move-end-of-line nil))

;; taken from ergoemacs (ergoemacs-select-current-block)
(defun jsynacek-mark-block ()
  "Select the current block between empty lines."
  (interactive)
  (let (p1)
    (if (re-search-backward "\n[ \t]*\n" nil "move")
        (progn (re-search-forward "\n[ \t]*\n")
               (setq p1 (point)))
      (setq p1 (point)))
    (if (re-search-forward "\n[ \t]*\n" nil "move")
        (re-search-backward "\n[ \t]*\n"))
    (set-mark p1)))

;;; killing
(defun jsynacek-kill-line-or-region (&optional arg)
  (interactive "p")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (kill-whole-line arg)))

(defun jsynacek-kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))



(setq jsynacek--chars
      (list
       "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p"
       "q" "r" "s" "t" "u" "v" "w" "x" "y" "z" "A" "B" "C" "D" "E" "F"
       "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V"
       "W" "X" "Y" "Z" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "/" ","
       "\\"))

(defun jsynacek-reset-keybindings (unbind)
  (dolist (c jsynacek--chars)
    (global-set-key c (or unbind
			  'self-insert-command))))

;;; remaps
(defun jsynacek-emacs-mode ()
  (interactive)
  (jsynacek-reset-keybindings nil)
  (setq jsynacek-current-mode 'emacs)
  (setq jsynacek-mode-line-mode-string "E "))

(defvar jsynacek-current-mode ()
  'emacs)

(defvar jsynacek-mode-line-mode-string "E ")

(define-prefix-command 'jsynacek-kill-keymap)
(define-prefix-command 'jsynacek-replace-keymap)
(define-prefix-command 'jsynacek-change-keymap)
(define-prefix-command 'jsynacek-sexp-keymap)
(define-prefix-command 'jsynacek-mark-keymap)

(defun jsynacek-command-mode ()
  (interactive)
  (progn
    (jsynacek-reset-keybindings t)
    (global-set-key "i" 'previous-line)
    (global-set-key "I" 'scroll-down)
    (global-set-key "j" 'backward-char)
    (global-set-key "J" 'beginning-of-line)
    (global-set-key "k" 'next-line)
    (global-set-key "K" 'scroll-up)
    (global-set-key "l" 'forward-char)
    (global-set-key "L" 'end-of-line)
    (global-set-key "u" 'backward-word)
    (global-set-key "o" 'forward-word)

    (global-set-key "g" 'jsynacek-sexp-keymap)
    (global-set-key "gi" 'backward-up-list)
    (global-set-key "gj" 'backward-sexp)
    (global-set-key "gk" 'down-list)
    (global-set-key "gl" 'forward-sexp)
    (global-set-key "gu" 'beginning-of-defun)
    (global-set-key "go" 'end-of-defun)

    (global-set-key "f" 'jsynacek-change-keymap)
    (global-set-key "ff" 'jsynacek-change)
    (global-set-key "fi" 'jsynacek-open-above)
    (global-set-key "fk" 'jsynacek-open-below)
    (global-set-key "fl" 'jsynacek-change-char)

    (global-set-key "d" 'jsynacek-kill-keymap)
    (global-set-key "dd" 'jsynacek-kill-line-or-region)
    (global-set-key "du" 'backward-kill-word)
    (global-set-key "do" 'kill-word)

    (global-set-key "m" 'jsynacek-mark-keymap)
    (global-set-key "mm" 'er/expand-region)
    (global-set-key "ml" 'jsynacek-mark-line)
    (global-set-key "mi" 'mark-defun)

    (global-set-key "x" 'jsynacek-kill-line-or-region)
    (global-set-key "c" 'jsynacek-copy-line-or-region)
    (global-set-key "v" 'yank)
    ;; (global-set-key "" 'f)

    (global-set-key "," 'ace-jump-mode)
    (global-set-key "/" 'isearch-forward)
    (global-set-key "\\" 'just-one-space)
    (global-set-key "z" 'undo)
    ;; (global-set-key "Z" 'redo)
    )
  (setq jsynacek-current-mode 'command)
  (setq jsynacek-mode-line-mode-string "C "))

(defun jsynacek-toggle-modes ()
  (interactive)
  (if (equal jsynacek-current-mode 'emacs)
      (progn
	(jsynacek-command-mode)
	(message "Command mode"))
    (progn
      (jsynacek-emacs-mode)
      (message "Emacs mode"))))

(global-set-key (kbd "<f12>") 'jsynacek-toggle-modes)
