(require 'notmuch)

;; add 'archived' tag when archiving
(setq notmuch-archive-tags '("-inbox" "+archived"))

(provide 'setup-notmuch)
