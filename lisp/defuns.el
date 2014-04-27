(defun vi-open-line-above ()
  "Insert a newline above the current line and put point at beginning."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun vi-open-line-below ()
  "Insert a newline below the current line and put point at beginning."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun describe-thing-at-point ()
  (interactive)
  (let ((function (function-called-at-point))
        (variable (variable-at-point)))
    (cond
     ((/= variable 0) (describe-variable variable))
     (function (describe-function function)))))

(defun my-multi-occur-in-matching-buffers (regexp &optional allbufs)
  "Show all lines matching REGEXP in all buffers."
  (interactive (occur-read-primary-args))
  (multi-occur-in-matching-buffers ".*" regexp))

(defun font-lock-restart ()
  (interactive)
  (setq font-lock-mode-major-mode nil)
  (font-lock-fontify-buffer))

(defun hippie-expand-line ()
  (interactive)
  (let ((hippie-expand-try-functions-list '(try-expand-line
                                            try-expand-line-all-buffers)))
    (hippie-expand nil)))

(defun save-region-or-current-line (arg)
  (interactive "P")
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (kill-ring-save (line-beginning-position) (line-end-position))))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

;; full screen magit-status
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(defun magit-toggle-whitespace ()
  (interactive)
  (if (member "-w" magit-diff-options)
      (magit-dont-ignore-whitespace)
    (magit-ignore-whitespace)))

(defun magit-ignore-whitespace ()
  (interactive)
  (add-to-list 'magit-diff-options "-w")
  (magit-refresh))

(defun magit-dont-ignore-whitespace ()
  (interactive)
  (setq magit-diff-options (remove "-w" magit-diff-options))
  (magit-refresh))

(defun google-search ()
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string
     (if mark-active
         (buffer-substring (region-beginning) (region-end))
       (read-string "Google:"))))))

(defun sudo-edit (file)
  (interactive "fSudo edit: ")
  (let ((sudo-prefix "/sudo:root@localhost:"))
    (find-file (concat sudo-prefix file))))

(defun zap-up-to-char (arg char)
  "Kill up to ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found."
  (interactive (list (prefix-numeric-value current-prefix-arg)
             (read-char "Zap to char: " t)))
  ;; Avoid "obsolete" warnings for translation-table-for-input.
  (with-no-warnings
    (if (char-table-p translation-table-for-input)
	(setq char (or (aref translation-table-for-input char) char))))
  (kill-region (point) (progn
             (search-forward (char-to-string char) nil nil arg)
             (goto-char (1- (point)))
             (point))))

;;; hooks
(defun my-rpm-hook-defaults ()
  (setq tab-width 4
        indent-tabs-mode t))
(add-hook 'rpm-spec-mode-hook 'my-rpm-hook-defaults)

(defun my-c-mode-hook-defaults ()
  (setq c-default-style "linux"
        indent-tabs-mode t
        subword-mode t))
(add-hook 'c-mode-hook 'my-c-mode-hook-defaults)

(defun my-sh-mode-hook-defaults ()
  (setq sh-indentation 2))
(add-hook 'sh-mode-hook 'my-sh-mode-hook-defaults)

;; highlight code annotations
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(FIXME\\|TODO\\|BUG\\|XXX\\)" 1 font-lock-warning-face t)))))

(defun highlight-trailing-whitespace ()
  (highlight-regexp "\\s-+$" 'hi-pink))
;; highlight trailing whitespace with ugly pink
(add-hook 'prog-mode-hook 'highlight-trailing-whitespace)

(global-set-key [remap goto-line] 'goto-line-with-feedback)
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))


(defun browse-rhbz-at-point ()
  (interactive)
  (browse-url-generic
   (concat "https://bugzilla.redhat.com/show_bug.cgi?id="
           (number-to-string (thing-at-point 'number)))))

(provide 'defuns)
