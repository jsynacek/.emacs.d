;;; general
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))
(add-to-list 'load-path "/usr/share/emacs/site-lisp") ; notmuch.el
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e") ; mu4e.el
(add-to-list 'load-path "~/work/git/upstream/org-mode/contrib/lisp/") ; org-notmuch.el

(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))
(setq smex-save-file "~/.emacs.d/.smex-items")

(setq tramp-default-method "ssh")
(setq send-mail-function 'smtpmail-send-it)

;;; ui
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area t)
(setq initial-scratch-message nil)

(blink-cursor-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode t)

(setq ring-bell-function 'ignore)
(prefer-coding-system 'utf-8)

(setq Man-width 80)

(defun jsynacek-highlight-trailing-whitespace ()
  (setq-local show-trailing-whitespace t))
(add-hook 'find-file-hook #'jsynacek-highlight-trailing-whitespace)

(global-unset-key (kbd "C-h")) ; learn to use f1
(global-unset-key (kbd "C-z")) ; no suspend

;;; editing
(setq global-auto-revert-mode t)
(setq mouse-yank-at-point t)

(defalias 'dtw 'delete-trailing-whitespace)
(defalias 'plp 'package-list-packages)
(defalias 'yes-or-no-p 'y-or-n-p)

(defun jsynacek-ggtags-mode ()
  (ggtags-mode 1))
(add-hook 'c-mode-hook #'jsynacek-ggtags-mode)

(defadvice eval-region (after jsynacek-eval-region-advice-after activate)
  (deactivate-mark))

;;; packages
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(require 'ace-jump-mode)
;; (require 'bbdb)
;; (bbdb-initialize 'message)
;; (bbdb-mua-auto-update-init 'message)
;; (setq bbdb-update-records-p 'create)

(require 'dired)
(setq dired-listing-switches "-al --group-directories-first")
(require 'dired-x)

(require 'elfeed)
(define-key elfeed-search-mode-map "i"
  (lambda ()
    (interactive)
    (goto-line 2)))
(define-key elfeed-search-mode-map "k" 'elfeed-search-untag-all-unread)
(define-key elfeed-search-mode-map "r" nil)

(require 'erc)
(setq erc-nick-uniquifier "_")
(setq erc-current-nick-highlight-type 'nick)
(setq erc-track-exclude-types '("JOIN" "KICK" "MODE" "NICK" "PART" "QUIT"))
(setq erc-hide-timestamps nil)
(setq erc-fill-function #'erc-fill-static)
(setq erc-fill-static-center 15)
(setq erc-fill-column 90)

(add-to-list 'erc-modules 'notifications)
(define-key erc-mode-map (kbd "RET") nil)
(define-key erc-mode-map (kbd "C-<return>") 'erc-send-current-line)

(require 'expand-region)
(setq expand-region-contract-fast-key "M")

(require 'helm)
;; (helm-mode t)
;; (add-to-list 'helm-completing-read-handlers-alist '(find-file . nil))
;; (add-to-list 'helm-completing-read-handlers-alist '(find-file-other-window . nil))
;; (add-to-list 'helm-completing-read-handlers-alist '(find-alternate-file . nil))
;; (add-to-list 'helm-completing-read-handlers-alist '(kill-buffer . nil))

(require 'helm-git-grep)
(global-set-key (kbd "M-s M-g") 'helm-git-grep)

(require 'notmuch)
(setq notmuch-search-oldest-first nil)
(setq notmuch-fcc-dirs "Sent")
(setq notmuch-show-logo nil)

(defun jsynacek-notmuch-mark-read-and-archive ()
  (interactive)
  (notmuch-search-tag '("-unread"))
  (notmuch-search-archive-thread))
(global-set-key (kbd "<XF86Mail>") 'notmuch)
(define-key notmuch-search-mode-map "k" 'jsynacek-notmuch-mark-read-and-archive)
(defun jsynacek-notmuch-search-unread ()
  (interactive)
  (notmuch-hello-search "tag:unread"))
(define-key notmuch-hello-mode-map "u" 'jsynacek-notmuch-search-unread)
(defun jsynacek-notmuch-search-inbox ()
  (interactive)
  (notmuch-hello-search "tag:inbox"))
(define-key notmuch-hello-mode-map "i" 'jsynacek-notmuch-search-inbox)

(require 'org)
(setq org-agenda-files '("~/SpiderOak Hive/orgfiles/inbox.org.gpg"
                         "~/SpiderOak Hive/orgfiles/birthday.org"))

(require 'org-notmuch)

(require 'solarized)
(setq solarized-use-variable-pitch nil)
(setq custom-safe-themes
      '("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default))
(load-theme 'solarized-light)

(require 'undo-tree)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

(load "jsynacek-util.el")
(require 'private)

(global-set-key [remap list-buffers] 'ibuffer)
(global-set-key (kbd "C-o") 'find-file) ; was open-line
(global-set-key (kbd "C-b") 'helm-mini) ; was backward-char
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-d") 'dired)     ; was delete-char
(global-set-key (kbd "C-w") 'jsynacek-kill-current-buffer) ; was kill-region
(global-set-key (kbd "<f2>") 'save-buffer)
(global-set-key (kbd "M-2") 'delete-window)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-c") 'jsynacek-copy-line-or-region) ; was capitalize-word
(global-set-key (kbd "M-v") 'jsynacek-yank) ; was scroll-down
(global-set-key (kbd "M-z") 'undo-tree-undo) ; was zap-to-char
(global-set-key (kbd "M-3") 'delete-other-windows)
(global-set-key (kbd "M-,") 'ace-jump-mode) ; was indent-new-comment-line
(global-set-key (kbd "M-y") 'helm-show-kill-ring) ; was yank-pop
(global-set-key (kbd "<f1> a") 'helm-apropos)      ; was apropos-command
(global-set-key (kbd "<f1> l") 'helm-locate-library) ; was view-lossage

(global-set-key (kbd "M-/") 'isearch-forward) ; was dabbrev-expand
(global-set-key (kbd "M-?") 'isearch-backward)
(define-key isearch-mode-map (kbd "M-/") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "M-?") 'isearch-repeat-backward)

(global-set-key (kbd "M-i") 'previous-line) ; was tab-to-tab-stop
(global-set-key (kbd "M-I") 'scroll-down)
(global-set-key (kbd "M-j") 'backward-char) ; was indent-new-comment-line
(global-set-key (kbd "M-k") 'next-line) ; was kill-sentence
(global-set-key (kbd "M-K") 'scroll-up)
(global-set-key (kbd "M-l") 'forward-char) ; was downcase-region

(define-prefix-command 'jsynacek-apps-keymap)
(global-set-key (kbd "M-a") 'jsynacek-apps-keymap) ; was backward-sentence
(global-set-key (kbd "M-a M-a") 'magit-status)	   ; default app
(global-set-key (kbd "M-a a") 'magit-status)	   ; default app
(global-set-key (kbd "M-a d") 'dired)
(global-set-key (kbd "M-a g") 'rgrep)
(global-set-key (kbd "M-a m") 'notmuch)		   ; email
(global-set-key (kbd "M-a s") 'shell)		   ; shell

(define-prefix-command 'jsynacek-window-keymap)
(global-set-key (kbd "M-w") 'jsynacek-window-keymap)
(global-set-key (kbd "M-w i") 'windmove-up)
(global-set-key (kbd "M-w j") 'windmove-left)
(global-set-key (kbd "M-w k") 'windmove-down)
(global-set-key (kbd "M-w l") 'windmove-right)

(define-prefix-command 'jsynacek-insert-keymap)
(global-set-key (kbd "M-p") 'jsynacek-insert-keymap)
(global-set-key (kbd "M-p j") 'jsynacek-insert-brackets)
(global-set-key (kbd "M-p k") 'jsynacek-insert-curly)
(global-set-key (kbd "M-p u") 'jsynacek-insert-double-quotes)
(global-set-key (kbd "M-p i") 'jsynacek-insert-single-quotes)

(define-prefix-command 'jsynacek-menu-keymap)
(global-set-key (kbd "<menu>") 'jsynacek-menu-keymap)
;; M-x
(global-set-key (kbd "<menu> M-x") 'helm-M-x)
;; apps
(global-set-key (kbd "<menu> a c") 'calc)
(global-set-key (kbd "<menu> a e") 'eshell)
(global-set-key (kbd "<menu> a g") 'magit-status)
;; evaluations

(global-set-key (kbd "<menu> e b") 'eval-buffer)
(global-set-key (kbd "<menu> e d") 'eval-defun)
(global-set-key (kbd "<menu> e e") 'eval-last-sexp)
(global-set-key (kbd "<menu> e r") 'eval-region)

(define-prefix-command 'jsynacek-transpose-keymap)
(global-set-key (kbd "M-t") 'jsynacek-transpose-keymap)
(global-set-key (kbd "M-t t") 'transpose-chars)
(global-set-key (kbd "M-t M-t") 'transpose-chars)
(global-set-key (kbd "M-t s") 'transpose-sexps)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t l") 'transpose-lines)

;; searching
(global-set-key (kbd "M-u") 'isearch-backward) ; was upcase-word
(define-key isearch-mode-map (kbd "M-u") 'isearch-repeat-backward)

(global-set-key (kbd "M-s b") 'helm-bookmarks)
(global-set-key (kbd "M-s g") 'rgrep)
(global-set-key (kbd "M-s i") 'helm-semantic-or-imenu)
(global-set-key (kbd "M-s m") 'helm-man-woman)
(global-set-key (kbd "M-s s") 'helm-swoop)

