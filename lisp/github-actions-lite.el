(define-derived-mode github-actions-lite-mode nil "-lite "
  "GitHub Actions lite mode"
  :syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  (setq-local comment-start "#"
              ;; TODO: Add highlighting for some "keywords" (at least the top level ones)
              font-lock-defaults '((nil))))

(add-to-list 'auto-mode-alist '("ya?ml\\'" . github-actions-lite-mode))

(provide 'github-actions-lite)
