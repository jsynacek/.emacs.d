;; -*- lexical-binding: t; -*-

(defun cmd/start-of-line ()
  "Move to the beginning of indentation or line. Toggle between the positions if the point
is already on either of them."
  (interactive)
  (let ((b (line-beginning-position))
        (i (current-indentation))
        (p (point)))
    (cond ((= p (+ b i))
           (goto-char b))
          (t
           (back-to-indentation)))))

;; TODO: The wrapping code can very likely be substituted with what electric-pair-mode does. Investigate.
(defun jsynacek-wrap (left &optional right)
  (when (region-active-p)
    (let ((start (region-beginning))
          (end (region-end)))
      (save-excursion
        (goto-char end)
        (insert (or right left)))
      (save-excursion
        (goto-char start)
        (insert left)))))

(defun cmd/insert-quote-or-wrap-with-quotes ()
  (interactive)
  (if (region-active-p)
      (jsynacek-wrap "\"")
    (self-insert-command 1)))

(defun cmd/insert-paren-or-wrap-with-parens ()
  (interactive)
  (if (region-active-p)
      (jsynacek-wrap "(" ")")
    (self-insert-command 1)))

(defun cmd/cut-line-or-region ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (kill-whole-line)))

(defun cmd/copy-line-or-region ()
  (interactive)
  (if (region-active-p)
      (progn
	(kill-ring-save (region-beginning) (region-end))
	(pulse-momentary-highlight-region (region-beginning) (region-end)))
    (kill-ring-save (line-beginning-position) (line-end-position))
    (pulse-momentary-highlight-one-line (point))))

(defun cmd/paste ()
  (interactive)
  (let ((p (point)))
    (yank)
    (pulse-momentary-highlight-region p (point))))

(defun cmd/sum-and-replace-region ()
  "Sum numbers in the active region and replace the region with the result."
  (interactive)
  (if (region-active-p)
      (let* ((region-str
              (buffer-substring-no-properties
               (region-beginning)
               (region-end)))
             (number-list (mapcar #'string-to-number
                                  (split-string region-str " "))))
        (save-excursion
          (delete-active-region)
          (insert (format "%s" (apply #'+ number-list)))))
    (user-error "Region not active")))

(provide 'jsynacek-text)
