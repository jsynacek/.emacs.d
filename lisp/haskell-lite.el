(define-derived-mode haskell-lite-mode nil "λ"
  "Haskell lite mode."
  :syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?- "_<12" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  (setq-local comment-start "--"
              font-lock-defaults '((nil))))

(add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-lite-mode))

(provide 'haskell-lite)
