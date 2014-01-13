;; ;;; general - inspired by ergoemacs
;; (global-unset-key (kbd "C-b"))
;; (global-set-key (kbd "M-j") 'backward-char)
;; (global-unset-key (kbd "C-f"))
;; (global-set-key (kbd "M-l") 'forward-char)
;; (global-unset-key (kbd "C-p"))
;; (global-set-key (kbd "M-i") 'previous-line)
;; (global-unset-key (kbd "C-n"))
;; (global-set-key (kbd "M-k") 'next-line)
;; (global-set-key (kbd "M-J") 'backward-open-bracket)
;; (global-set-key (kbd "M-L") 'forward-open-bracket)
;; (global-set-key (kbd "M-I") 'scroll-down)
;; (global-set-key (kbd "M-K") 'scroll-up)
;; ; origin: upcase-word
;; (global-set-key (kbd "M-u") 'backward-word)
;; ; origin: facemenu-keymap
;; (global-set-key (kbd "M-o") 'forward-word)
;; (global-set-key (kbd "M-U") 'backward-paragraph)
;; (global-set-key (kbd "M-O") 'forward-paragraph)
;; ; origin: kill-word
;; (global-set-key (kbd "M-d") 'delete-backward-char)
;; ; origin: forward-word
;; (global-set-key (kbd "M-f") 'delete-char)
;; ; origin: forward-sentence
;; (global-set-key (kbd "M-e") 'backward-kill-word)
;; ; origin: move-to-window-line-top-bottom
;; (global-set-key (kbd "M-r") 'kill-word)

;; ; origin: prefix for occur and hi-lock commands
;; (global-set-key (kbd "M-s") 'other-window)
;; (global-set-key (kbd "M-S") (lambda ()
;;                               (interactive)
;;                               (other-window -1)))
;; ; origin: backward-sentence
;; (global-set-key (kbd "M-a") 'eshell)
;; (global-set-key (kbd "M-A") 'shell-command)

;; do not kill emacs that easily
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c") nil)

(if (fboundp 'smex)
    (global-set-key (kbd "M-x") 'smex))

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
(global-set-key (kbd "<f5>") 'recompile)
(global-set-key (kbd "<f6>") 'compile)
(global-set-key (kbd "<f7>") 'whitespace-cleanup)
(global-set-key (kbd "<f8>") 'eval-buffer)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)
(global-set-key (kbd "C-x w") 'write-region)
(global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)
(global-set-key (kbd "C-x C-k") 'kill-buffer-and-window)
(global-set-key (kbd "C-x \\") 'align-regexp)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c i") 'imenu)
(global-set-key (kbd "C-.") 'describe-thing-at-point)
(global-set-key (kbd "C-M-/") 'hippie-expand-line)
(global-set-key (kbd "M-w") 'save-region-or-current-line)
(global-set-key (kbd "M-8") 'er/expand-region)
(global-set-key (kbd "M-9")
                (lambda ()
                  (interactive)
                  (er/expand-region -1)))
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(define-prefix-command 'menukey-prefix-map)
; custom prefix
(global-set-key (kbd "<menu>") 'menukey-prefix-map)
(define-key menukey-prefix-map (kbd "r") 'rgrep)
; my multi-occur
;(global-set-key (kbd "M-s /") 'my-multi-occur-in-matching-buffers)
; highlight-symbol
(global-set-key (kbd "M-n") 'highlight-symbol-next)
(global-set-key (kbd "M-p") 'highlight-symbol-prev)
; numbers
(global-set-key (kbd "C-c +") 'increment-number-at-point)
(global-set-key (kbd "C-c -") 'decrement-number-at-point)
; windmove-default-keybindings
(global-set-key (kbd "S-<right>") 'windmove-right)
(global-set-key (kbd "S-<left>") 'windmove-left)
(global-set-key (kbd "S-<down>") 'windmove-down)
(global-set-key (kbd "S-<up>") 'windmove-up)
; buffer-move
(global-set-key (kbd "<C-S-up>")    'buf-move-up)
(global-set-key (kbd "<C-S-down>")  'buf-move-down)
(global-set-key (kbd "<C-S-left>")  'buf-move-left)
(global-set-key (kbd "<C-S-right>") 'buf-move-right)
; magit
(global-set-key (kbd "C-x g") 'magit-status)
(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
(define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)
; smartparens
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
; python-mode
(define-key python-mode-map (kbd "C-c d") 'pydoc)
(define-key python-mode-map (kbd "M-e") 'python-next-statement)
(define-key python-mode-map (kbd "M-a") 'python-previous-statement)
; etags-select
; TODO this breaks ggtags' M-. ?
(if (fboundp 'etags-select-find-tag)
    (global-set-key (kbd "M-.") 'etags-select-find-tag))
; erc
(define-key erc-mode-map (kbd "RET") nil)
(define-key erc-mode-map (kbd "C-<return>") 'erc-send-current-line)
(provide 'keybindings)
