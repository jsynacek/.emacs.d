;; -*- lexical-binding: t; -*-

(defun cmd/mark-defun ()
  (interactive)
  (mark-defun)
  (while (looking-at "[^(]")
    (forward-char)))

(defun cmd/eval-defun ()
  "Eval defun and pulse for visual confirmation."
  (interactive)
  (save-excursion
    (cmd/mark-defun)
    (let ((start (region-beginning))
          (end (region-end)))
      (deactivate-mark)
      (pulse-momentary-highlight-region start end 'my-pulse))))

(defun cmd/eval-region-or-last-sexp ()
  "Eval region if active, otherwise eval the last sexp."
  (interactive)
  (if (region-active-p)
      (progn
        (eval-region (region-beginning) (region-end))
        (deactivate-mark))
    (call-interactively 'eval-last-sexp)))

(defun cmd/info-elisp ()
  "Show Elisp Info in a buffer called *info-elisp*. If it already exists, switch to it
instead of creating or resetting it.
"
  (interactive)
  (let* ((name "*info-elisp*")
         (buffer (get-buffer name)))
    ;; Manual says that `(info "Elisp" name)' switches to the buffer if it already exists.
    ;; However, it switches to the buffer *and* resets the elisp manual to the toplevel,
    ;; which is something that I don't want.
    (if buffer
        (switch-to-buffer buffer)
      (info "Elisp" name))))

(provide 'jsynacek-elisp)
