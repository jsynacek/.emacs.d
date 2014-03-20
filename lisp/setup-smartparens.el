(require 'smartparens)
(require 'smartparens-config)

(sp-with-modes sp--lisp-modes
  (sp-local-pair "`" "'")
  (sp-local-pair "'" nil :actions nil))

(provide 'setup-smartparens)

