;;; general
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))
(add-to-list 'load-path "/usr/share/emacs/site-lisp") ; notmuch.el
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

(global-unset-key (kbd "C-z"))

;;; editing
(setq global-auto-revert-mode t)
(setq mouse-yank-at-point t)

(global-set-key [remap list-buffers] 'ibuffer)

(define-prefix-command 'jsynacek-menu-keymap)
(global-set-key (kbd "<menu>") 'jsynacek-menu-keymap)
;; M-x
(global-set-key (kbd "<menu> M-x") 'execute-extended-command)
;; apps
(global-set-key (kbd "<menu> a c") 'calc)
(global-set-key (kbd "<menu> a e") 'eshell)
(global-set-key (kbd "<menu> a g") 'magit-status)
;; evaluations
(global-set-key (kbd "<menu> e b") 'eval-buffer)
(global-set-key (kbd "<menu> e d") 'eval-defun)
(global-set-key (kbd "<menu> e e") 'eval-last-sexp)
(global-set-key (kbd "<menu> e r") 'eval-region)
;; marking
(global-set-key (kbd "<menu> m d") 'mark-defun)
;; transpositions
(global-set-key (kbd "<menu> t l") 'transpose-lines)
(global-set-key (kbd "<menu> t s") 'transpose-sexps)
(global-set-key (kbd "<menu> t w") 'transpose-words)

(defalias 'dtw 'delete-trailing-whitespace)
(defalias 'plp 'package-list-packages)
(defalias 'yes-or-no-p 'y-or-n-p)

(defun jsynacek-ggtags-mode ()
  (ggtags-mode 1))
(add-hook 'c-mode-hook #'jsynacek-ggtags-mode)

;;; packages
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(require 'bbdb)
(bbdb-initialize 'message)
(bbdb-mua-auto-update-init 'message)
(setq bbdb-update-records-p 'create)

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

(require 'dired)
(setq dired-listing-switches "-al --group-directories-first")

(require 'helm)
(helm-mode t)
(add-to-list 'helm-completing-read-handlers-alist '(find-file . nil))
(add-to-list 'helm-completing-read-handlers-alist '(find-file-other-window . nil))
(add-to-list 'helm-completing-read-handlers-alist '(find-alternate-file . nil))
(add-to-list 'helm-completing-read-handlers-alist '(kill-buffer . nil))
(global-set-key (kbd "M-x") 'helm-M-x)		  ; was execute-extended-command
(global-set-key (kbd "M-y") 'helm-show-kill-ring) ; was yank-pop
(global-set-key (kbd "C-h a") 'helm-apropos)	  ; was apropos-command
(global-set-key (kbd "C-h l") 'helm-locate-library) ; was view-lossage
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "M-s a") 'ag)
(global-set-key (kbd "M-s b") 'helm-bookmarks)
(global-set-key (kbd "M-s g") 'helm-do-grep)
(global-set-key (kbd "M-s i") 'helm-semantic-or-imenu)
(global-set-key (kbd "M-s m") 'helm-man-woman)
(global-set-key (kbd "M-s o") 'helm-occur) ; was occur
(global-set-key (kbd "M-s s") 'helm-swoop)

(require 'helm-git-grep)
(global-set-key (kbd "M-s M-g") 'helm-git-grep)

;; (require 'ido)
;; (ido-mode t)

(require 'notmuch)
(setq notmuch-search-oldest-first nil)
(setq notmuch-fcc-dirs "Sent")
(setq notmuch-show-logo nil)

(require 'org)
(setq org-agenda-files '("~/SpiderOak Hive/orgfiles/inbox.org.gpg"
			 "~/SpiderOak Hive/orgfiles/birthday.org"))

(require 'org-notmuch)

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

;; (require 'smex)
;; (smex-initialize)

(require 'solarized)
(setq solarized-use-variable-pitch nil)
(setq custom-safe-themes
      '("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default))
(load-theme 'solarized-light)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

(require 'private)
