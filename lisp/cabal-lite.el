(define-derived-mode cabal-lite-mode nil "Cabal-lite"
  "Cabal lite mode."
  :syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?- "_<12" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  (setq-local comment-start "--"
              font-lock-defaults
              (let* ((package-properties-rx
                      (rx line-start
                          (or "name" "version" "cabal-version" "build-type" "license"
                              "license-file" "license-files" "copyright" "author" "maintainer"
                              "stability" "homepage" "bug-reports" "package-url" "synopsis"
                              "description" "category" "tested-with" "data-files" "data-dir"
                              "extra-source-files" "extra-doc-files" "extra-tmp-files"
                              ;; Technically not a package property, but I want it here.
                              "common")
                          word-boundary))
                     (component-rx
                      (rx line-start
                          (or "library" "executable" "test-suite" "benchmark" "foreign-library")
                          word-boundary))
                     (keywords
                      `((,package-properties-rx . font-lock-function-name-face)
                        (,component-rx . font-lock-function-name-face))))
                (list keywords))))

(add-to-list 'auto-mode-alist '("\\.cabal\\'" . cabal-lite-mode))

(provide 'cabal-lite)
