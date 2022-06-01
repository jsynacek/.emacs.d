(defconst +cabal-lite--keywords-rx+
  (rx line-start
      (or "common" "executable" "library" "test-suite" "benchmark")
      word-boundary))

(define-derived-mode cabal-lite-mode nil "Cabal-lite"
  "Cabal lite mode."
  :syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?- "_<12" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  (setq-local comment-start "--"
              font-lock-defaults
              `(((,+cabal-lite--keywords-rx+ . font-lock-function-name-face)))))

(add-to-list 'auto-mode-alist '("\\.cabal\\'" . cabal-lite-mode))

(provide 'cabal-lite)
