(require 'erc)
(require 'erc-autoaway)

(setq erc-nick "jsynacek")
(setq erc-away-nickname "jsynacek|away")

(add-to-list 'erc-modules 'notifications 'autoaway)

(setq erc-log-insert-log-on-open nil)
(setq erc-log-channels t)
(setq erc-log-channels-directory "~/.irclogs/")
(setq erc-save-buffer-on-part t)
(setq erc-hide-timestamps nil)
(setq erc-fill-column 100)

(defadvice save-buffers-kill-emacs (before save-logs (arg) activate)
  (save-some-buffers t (lambda () (when (and (eq major-mode 'erc-mode)
                                             (not (null buffer-file-name)))))))

(add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)
(add-hook 'erc-mode-hook '(lambda () (when (not (featurep 'xemacs))
                                       (set (make-variable-buffer-local
                                             'coding-system-for-write)
                                            'emacs-mule))))

; add channels to the mode-line (in the corresponding face) only when
; the current nickname or any of your keywords are mentioned.
; http://www.emacswiki.org/emacs/ErcChannelTracking
(setq erc-current-nick-highlight-type 'nick)
; (setq erc-keywords '("\\berc[-a-z]*\\b" "\\bemms[-a-z]*\\b"))

(setq erc-track-exclude-types '("JOIN" "PART" "QUIT" "NICK" "MODE"))
(setq erc-track-faces-priority-list
      '(erc-current-nick-face erc-keyword-face))
(setq erc-track-priority-faces-only 'all)

;; autoaway stuff
; automatically remove away status when sending something to the server
(setq erc-auto-discard-away t)

(setq erc-autoaway-idle-seconds 1200)
;(setq erc-autoaway-message)
(setq erc-auto-set-away t)

(define-key erc-mode-map (kbd "RET") nil)
(define-key erc-mode-map (kbd "C-<return>") 'erc-send-current-line)

(provide 'setup-erc)
