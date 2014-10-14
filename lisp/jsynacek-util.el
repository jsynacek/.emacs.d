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

(defun jsynacek-open-before-char ()
  (interactive)
  (backward-char)
  (jsynacek-switch-to-emacs-mode))

(defun jsynacek-comment-line-or-region ()
  (interactive)
  (if (region-active-p)
      (comment-dwim nil)
    (progn
      (jsynacek-mark-line)
      (comment-dwim nil))))

;;; opening
(defun jsynacek-open-after-char ()
  (interactive)
  (forward-char)
  (jsynacek-switch-to-emacs-mode))

(defun jsynacek-open-before-word ()
  (interactive)
  (backward-word)
  (jsynacek-switch-to-emacs-mode))

(defun jsynacek-open-after-word ()
  (interactive)
  (forward-word)
  (jsynacek-switch-to-emacs-mode))

(defun jsynacek-open-beginning-of-line () ; TODO optional universal arg to make it jump back to indentation?
  (interactive)
  (beginning-of-line)
  (jsynacek-switch-to-emacs-mode))

(defun jsynacek-open-end-of-line ()
  (interactive)
  (end-of-line)
  (jsynacek-switch-to-emacs-mode))

(defun jsynacek-open-below ()
  (interactive)
  (end-of-line)
  (newline-and-indent)
  (jsynacek-switch-to-emacs-mode))

(defun jsynacek-open-above ()
  (interactive)
  (beginning-of-line)
  (if (= (point) 1)
      (progn
	(newline)
	(backward-char))
    (progn
      (backward-char)
      (newline-and-indent)))
  (jsynacek-switch-to-emacs-mode))


(defun jsynacek-change-char ()
  (interactive)
  (delete-char 1)
  (jsynacek-switch-to-emacs-mode))

; TODO pretty ineffective to the point where it's visible
(defun jsynacek-change-word ()
  (interactive)
  (er/mark-word)
  (jsynacek-change-line-or-region))

(defun jsynacek-change-line-or-region ()
  (interactive)
  (jsynacek-kill-line-or-region)
  (jsynacek-switch-to-emacs-mode))

(defun jsynacek-replace-char (c)
  (interactive "cReplace with:")
  (delete-char 1)
  (insert c)
  (backward-char))

(defun jsynacek-join-lines ()
  (interactive)
  (delete-indentation -1))

(defun jsynacek-copy-line-or-region ()
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (save-excursion
      (kill-ring-save (line-beginning-position) (1+ (line-end-position))))))

(defun jsynacek-yank ()
  (interactive)
  (let ((text (current-kill 0)))
    ;; (when (and (stringp text)
    ;;            (string-equal (substring text -1) "\n"))
    ;;   (end-of-line)
    ;;   (newline))
    (insert-for-yank (current-kill 0)))) ; TODO fixme vim-like?

(defun jsynacek-exchange-point-and-mark ()
  (interactive)
  (if (region-active-p)
      (exchange-point-and-mark)))

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
(defun jsynacek-kill-line-backward ()
  (interactive)
  (kill-region (line-beginning-position) (point)))

(defun jsynacek-kill-line-or-region (&optional arg)
  (interactive "p")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (kill-whole-line arg)))

(defun jsynacek-kill-defun ()
  (interactive)
  (mark-defun)
  (kill-region (region-beginning) (region-end)))

(defun jsynacek-kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))



(defconst jsynacek--chars
      (list
       "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p"
       "q" "r" "s" "t" "u" "v" "w" "x" "y" "z" "A" "B" "C" "D" "E" "F"
       "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V"
       "W" "X" "Y" "Z" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "." ","
       "/" " " ":" ";" "{" "}" "[" "]" "<" ">" "\\"))

(defvar jsynacek-current-mode () 'emacs)

(defun jsynacek-reset-keybindings (unbind)
  (dolist (c jsynacek--chars)
    (global-set-key c (or unbind
                          'self-insert-command))))

;;; remaps
(defun jsynacek-emacs-mode ()
  (jsynacek-reset-keybindings nil))

(define-prefix-command 'jsynacek-kill-keymap)
(define-prefix-command 'jsynacek-replace-keymap)
(define-prefix-command 'jsynacek-change-keymap)
(define-prefix-command 'jsynacek-sexp-keymap)
(define-prefix-command 'jsynacek-mark-keymap)
(define-prefix-command 'jsynacek-switch-keymap)
(define-prefix-command 'jsynacek-comment-keymap)

(defun jsynacek-command-mode ()
  (progn
    (jsynacek-reset-keybindings t)
    (global-set-key "i" 'previous-line)
    (global-set-key "I" 'scroll-down)
    (global-set-key "j" 'backward-char)
    (global-set-key "J" 'beginning-of-line)
    (global-set-key "k" 'next-line)	; TODO doesn't work in man-mode, dired, ...
    (global-set-key "K" 'scroll-up)
    (global-set-key "l" 'forward-char)	; TODO breaks in info-mode
    (global-set-key "L" 'end-of-line)
    (global-set-key "u" 'backward-word)
    (global-set-key "U" 'backward-paragraph)
    (global-set-key "o" 'forward-word)
    (global-set-key "O" 'forward-paragraph)
    (global-set-key "r" 'jsynacek-replace-char)
    (global-set-key "[" 'beginning-of-defun)
    (global-set-key "]" 'end-of-defun)
    (global-set-key "<" 'beginning-of-buffer)
    (global-set-key ">" 'end-of-buffer)
    (global-set-key "e" 'delete-forward-char)

    (global-set-key "x" 'jsynacek-kill-line-or-region)
    (global-set-key "c" 'jsynacek-copy-line-or-region)
    (global-set-key "v" 'jsynacek-yank)

    (global-set-key "." 'jsynacek-exchange-point-and-mark)
    (global-set-key "," 'ace-jump-mode)
    (global-set-key "/" 'isearch-forward)
    (global-set-key "\\" 'fixup-whitespace)

    (global-set-key "z" 'undo-tree-undo)
    (global-set-key "Z" 'undo-tree-redo)

    (global-set-key "g" 'jsynacek-sexp-keymap)
    (global-set-key "gg" 'ace-jump-mode)
    (global-set-key "gi" 'backward-up-list)
    (global-set-key "gj" 'backward-sexp)
    (global-set-key "gk" 'down-list)
    (global-set-key "gl" 'forward-sexp)
    (global-set-key "gL" 'goto-line)
    (global-set-key "go" 'end-of-defun)
    (global-set-key "gu" 'beginning-of-defun)

    (global-set-key "f" 'jsynacek-change-keymap)
    (global-set-key "fi" 'jsynacek-join-lines)
    (global-set-key "ff" 'jsynacek-change-line-or-region)
    (global-set-key "fl" 'jsynacek-change-char)
    (global-set-key "fo" 'jsynacek-change-word)

    (global-set-key "d" 'jsynacek-kill-keymap)
    (global-set-key "dd" 'jsynacek-kill-line-or-region)
    (global-set-key "di" 'jsynacek-kill-defun)
    (global-set-key "do" 'kill-word)
    (global-set-key "du" 'backward-kill-word)
    (global-set-key "dJ" 'jsynacek-kill-line-backward)
    (global-set-key "dL" 'kill-line)

    (global-set-key "m" 'jsynacek-mark-keymap) ; TODO breaks in notmuch
    (global-set-key "mm" 'er/expand-region)
    (global-set-key "m " 'set-mark-command)
    (global-set-key "mi" 'mark-defun)
    (global-set-key "mk" 'jsynacek-mark-block)
    (global-set-key "ml" 'jsynacek-mark-line)

    (global-set-key " " 'jsynacek-switch-keymap)
    (global-set-key "  " 'jsynacek-switch-to-emacs-mode)
    (global-set-key " i" 'jsynacek-open-above)
    (global-set-key " k" 'jsynacek-open-below)
    (global-set-key " j" 'jsynacek-open-before-char)
    (global-set-key " J" 'jsynacek-open-beginning-of-line)
    (global-set-key " l" 'jsynacek-open-after-char)
    (global-set-key " L" 'jsynacek-open-end-of-line)
    (global-set-key " o" 'jsynacek-open-after-word)
    (global-set-key " u" 'jsynacek-open-before-word)

    (global-set-key ";" 'jsynacek-comment-keymap)
    (global-set-key ";;" 'jsynacek-comment-line-or-region)
    (global-set-key ";l" 'comment-dwim)

    ;; . tag related?
    ))

(defun jsynacek-switch-to-emacs-mode ()
  (interactive)
  (progn
    (jsynacek-emacs-mode)
    (setq jsynacek-current-mode 'emacs)
    (modify-all-frames-parameters (list (cons 'cursor-color "#93a1a1")
                                        (cons 'cursor-type 'bar))) ; TODO use set-face-attribute
    ))

(defun jsynacek-switch-to-command-mode ()
  (interactive)
  (progn
    (jsynacek-command-mode)
    (setq jsynacek-current-mode 'command)
    (modify-all-frames-parameters (list (cons 'cursor-color "#268bd2")
                                        (cons 'cursor-type 'box))) ; TODO use set-face-attribute
    ))

(defun jsynacek-toggle-modes ()
  (interactive)
  (if (equal jsynacek-current-mode 'emacs)
      (jsynacek-switch-to-command-mode)
    (jsynacek-switch-to-emacs-mode)))

(add-hook 'minibuffer-setup-hook #'jsynacek-switch-to-emacs-mode)
(add-hook 'minibuffer-exit-hook #'jsynacek-switch-to-command-mode) ; TODO make this correctly restore the last state?
(jsynacek-switch-to-command-mode)

(global-set-key (kbd "M-SPC") 'jsynacek-toggle-modes) ; was just-one-space
