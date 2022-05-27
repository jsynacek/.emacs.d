;;; Dockerfile lite.

(defconst +dockerfile-lite--name+
  (if (seq-filter #'(lambda (font)
                      (string-match "Font Awesome" font))
                  (x-list-fonts ":"))
      ""
    "🐋")
  "The name of `dockerfile-lite` major mode. If Font Awesome is
installed, use the Docker character. Otherwise use the WHALE
unicode character.")

(define-derived-mode dockerfile-lite-mode nil +dockerfile-lite--name+
  "Dockumentation."
  :syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  (setq-local font-lock-defaults
              '((("^\s*FROM\s+" . font-lock-function-name-face)
                 ("^\s*ARG\s+" . font-lock-variable-name-face)))
              imenu-generic-expression
              '((nil "^\s*FROM\s+\\(.+\\)$" 1))))

(add-to-list 'auto-mode-alist '("Dockerfile\\(?:[.-].+\\)?\\'" . dockerfile-lite-mode))

(provide 'dockerfile-lite)
