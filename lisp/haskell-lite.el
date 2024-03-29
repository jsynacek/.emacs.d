(require 'open-color)

(defface haskell-cpp-conditional-face
  `((t :foreground ,(oc-color 'cyan7)))
  "Font lock face for CPP conditionals in Haskell source code.")
(defvar haskell-cpp-conditional-face 'haskell-cpp-conditional-face)

(defface haskell-type-annotation-face
  `((t :foreground ,(oc-color 'cyan7)
       :weight bold))
  "Font lock face for type annotations in Haskell source code.")
(defvar haskell-type-annotation-face 'haskell-type-annotation-face)

(define-skeleton skel/haskell-language-pragma "Insert haskell LANGUAGE pragma"
  (completing-read
   "Extension: "
   (with-temp-buffer
     (call-process "ghc" nil t nil "--supported-extensions")
     (string-lines (buffer-string) t))
   nil t)
  "{-# LANGUAGE " str " #-}"
  \n)

(define-derived-mode haskell-lite-mode nil "λ"
  "Haskell lite mode."
  :syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?- "_<12" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  (setq-local comment-start "--"
              font-lock-defaults
              (let* ((cpp-rx (rx line-start
                                 "#" (or "if" "ifdef" "else" "elif" "endif")
                                 word-boundary))
                     (type-annotation-rx "::")
                     (keywords
                      `((,cpp-rx . haskell-cpp-conditional-face)
                        (,type-annotation-rx . haskell-type-annotation-face))))
                (list keywords))))

(add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-lite-mode))

(provide 'haskell-lite)
