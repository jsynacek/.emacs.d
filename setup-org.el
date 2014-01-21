(require 'org)

;(setq org-agenda-include-diary t)
;(setq org-agenda-include-all-todo t)
(setq org-completion-use-ido t)
(setq org-log-done 'time)

(setq calendar-week-start-day 1)

;; TODO figure this out
;; (defmacro org-level-set-face-font (level font)
;;   (let ((org-level-variable (make-symbol  (concat "org-level-" (number-to-string level)))))
;;     `(set-face-attribute (quote ,org-level-variable) nil :inherit ,font)))

(defun jsynacek/org-level-set-headings-font ()
  "Fix default font for org-level-X (where X is 1 to 7) variables.

By default, 'variable-pitch is used. That screws tag alignment.

Use favorite font instead. Default is `jsynacek/font', if bound, otherwise
'fixed-pitch."
  (let ((font (if (boundp 'jsynacek/font)
                  jsynacek/font
                'fixed-pitch)))
    (set-face-attribute 'org-level-1 nil :inherit font)
    (set-face-attribute 'org-level-2 nil :inherit font)
    (set-face-attribute 'org-level-3 nil :inherit font)
    (set-face-attribute 'org-level-4 nil :inherit font)
    (set-face-attribute 'org-level-5 nil :inherit font)
    (set-face-attribute 'org-level-6 nil :inherit font)
    (set-face-attribute 'org-level-7 nil :inherit font)))

; use my favorite monospace font, please
(jsynacek/org-level-set-headings-font)
; try to set org's default column for tags
(if (boundp 'jsynacek/org-tags-column)
    (setq org-tags-column jsynacek/org-tags-column))
; try to set org's default notes file
(setq org-default-notes-file (if (boundp 'jsynacek/org-default-notes-file)
                                 jsynacek/org-default-notes-file))

(setq org-agenda-files `(,org-default-notes-file)
      org-capture-templates '(("t" "New TODO item into Inbox" entry (file+headline org-agenda-files "Inbox") "** TODO %?\nadded:%U"))
      org-todo-keywords '((sequence "TODO" "STARTED" "WAITING" "|" "DONE" "CANCELLED" ))
      org-todo-keyword-faces '(("STARTED" . (:foreground "#af8700" :weight bold))))

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c b") 'org-iswitchb)

;; (add-hook 'org-mode-hook 'org-indent-mode)

(provide 'setup-org)
