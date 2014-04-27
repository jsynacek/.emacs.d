(require 'smartparens)
(require 'smartparens-config)

(sp-with-modes sp--lisp-modes
  (sp-local-pair "`" "'")
  (sp-local-pair "'" nil :actions nil))

(define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp)
(define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
(define-key sp-keymap (kbd "<C-left>") 'sp-forward-barf-sexp)
(define-key sp-keymap (kbd "<C-right>") 'sp-forward-slurp-sexp)
(define-key sp-keymap (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
(define-key sp-keymap (kbd "C-M-<right>") 'sp-backward-barf-sexp)
(define-key sp-keymap (kbd "M-F") 'sp-forward-symbol)
(define-key sp-keymap (kbd "M-B") 'sp-backward-symbol)
(define-key sp-keymap (kbd "C-}") 'sp-select-next-thing-exchange)
(define-key sp-keymap (kbd "C-{") 'sp-select-previous-thing)
(define-key sp-keymap (kbd "C-M-}") 'sp-select-next-thing)

(provide 'setup-smartparens)

