;; -*- lexical-binding: t; -*-

(defun cmd/eval-region-or-last-sexp ()
  (interactive)
  (if (region-active-p)
      (progn
        (eval-region (region-beginning) (region-end))
        (deactivate-mark))
    (call-interactively 'eval-last-sexp)))

(provide 'jsynacek-elisp)
