(define-derived-mode cabal-project-lite-mode nil "Cabal-project-lite"
  "Cabal project lite mode."
  :syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?- "_<12" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  (setq-local comment-start "--"
              font-lock-defaults
              (let* ((options-rx
                      (rx word-boundary
                          (or "allow-newer" "constraints" "ghc-options" "packages"
                              "write-ghc-environment-files")
                          ":"))
                     (stanzas-rx
                      (rx line-start
                          (or "package" "source-repository-package")
                          word-boundary))
                     (keywords
                      ;; TODO: Just make the 'bold face work...
                      `((,options-rx . font-lock-function-name-face)
                        (,stanzas-rx . font-lock-function-name-face))))
                (list keywords))))

(add-to-list 'auto-mode-alist '("cabal\\.project\\(?:\\.local\\)?\\'" . cabal-project-lite-mode))

(provide 'cabal-project-lite)
