;; do not kill emacs that easily
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c") nil)

(global-set-key (kbd "C-x C-m") 'eshell)
;; transpose stuff with M-t (stolen from Magnar Sveen)
(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t s") 'transpose-sexps)

;; View occurrence in occur mode
(define-key occur-mode-map (kbd "v") 'occur-mode-display-occurrence)
(define-key occur-mode-map (kbd "n") 'next-line)
(define-key occur-mode-map (kbd "p") 'previous-line)

;; (global-set-key (kbd "M-<SPC>") 'set-mark-command)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-S-o") 'vi-open-line-above)
(global-set-key (kbd "C-o") 'vi-open-line-below)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)
;(global-set-key (kbd "M-j")
;'                (lambda ()
;                  (interactive)
;                  (join-line -1)))
(global-set-key (kbd "C-x e") 'eval-and-replace)
(global-set-key (kbd "<f5>") 'compile)
(global-set-key (kbd "<f6>") 'recompile)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-c i") 'imenu)
(global-set-key (kbd "C-.") 'describe-thing-at-point)
(global-set-key (kbd "C-M-/") 'hippie-expand-line)
(global-set-key (kbd "M-w") 'save-region-or-current-line)

(global-set-key (kbd "M-2") 'delete-window)
(global-set-key (kbd "M-3") 'delete-other-windows)
(global-set-key (kbd "<XF86Mail>") 'gnus)
(define-prefix-command 'menukey-prefix-map)
; custom prefix
(global-set-key (kbd "<menu>") 'menukey-prefix-map)
(define-key menukey-prefix-map (kbd "r") 'rgrep)
; my multi-occur
;(global-set-key (kbd "M-s /") 'my-multi-occur-in-matching-buffers)
; highlight-symbol
;; (global-set-key (kbd "M-n") 'highlight-symbol-next)
;; (global-set-key (kbd "M-p") 'highlight-symbol-prev)

                                        ;
;; windmove-default-keybindings
(global-set-key (kbd "S-<right>") 'windmove-right)
(global-set-key (kbd "S-<left>") 'windmove-left)
(global-set-key (kbd "S-<down>") 'windmove-down)
(global-set-key (kbd "S-<up>") 'windmove-up)
; buffer-move
(global-set-key (kbd "<C-S-up>")    'buf-move-up)
(global-set-key (kbd "<C-S-down>")  'buf-move-down)
(global-set-key (kbd "<C-S-left>")  'buf-move-left)
(global-set-key (kbd "<C-S-right>") 'buf-move-right)
; elisp
(define-key emacs-lisp-mode-map (kbd "M-.") 'find-function-at-point)
(provide 'keybindings)
