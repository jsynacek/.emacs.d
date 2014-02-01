(require 'bbdb)
(bbdb-initialize 'gnus 'message 'pgp)

;; add bindings for default keys to Gnus and notify BBDB when new messages are
;; loaded
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)

;(setq bbdb-file "~/.emacs.d/bbdb")

(provide 'setup-bbdb)
