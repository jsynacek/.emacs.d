(require 'notmuch)

;; add 'archive' tag when archiving
(setq notmuch-archive-tags '("-inbox" "+archive"))

(provide 'setup-notmuch)
