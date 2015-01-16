; TODO map C-x 4 s to new eshell window

;;; general
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))
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
(abbrev-mode t)
(recentf-mode t)
(column-number-mode 1)

(setq ring-bell-function 'ignore)
(prefer-coding-system 'utf-8)

(setq Man-width 90)

;;; faces
(eval-after-load "diff-mode"
  '(set-face-foreground 'diff-hunk-header-face "#2aa198")) ; solarized cyan

(defun jsynacek-highlight-trailing-whitespace ()
  (setq-local show-trailing-whitespace t))
(add-hook 'find-file-hook #'jsynacek-highlight-trailing-whitespace)

(global-unset-key (kbd "C-z")) ; no suspend

;;; editing
(setq global-auto-revert-mode t)
(setq mouse-yank-at-point t)

(defalias 'dtw 'delete-trailing-whitespace)
(defalias 'plp 'package-list-packages)
(defalias 'yes-or-no-p 'y-or-n-p)

(defun jsynacek-c-mode-setup ()
  (c-set-style "linux")
  (ggtags-mode 1))
(add-hook 'c-mode-hook #'jsynacek-c-mode-setup)

(defadvice eval-region (after jsynacek-eval-region-advice-after activate)
  (deactivate-mark))

;;; packages
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(require 'ace-jump-mode)
;; (require 'bbdb)
;; (bbdb-initialize 'message)
;; (bbdb-mua-auto-update-init 'message)
;; (setq bbdb-update-records-p 'create)

(require 'dired)
(require 'dired-x)
(setq dired-listing-switches "-al --group-directories-first")
(define-key dired-mode-map "P" 'dired-up-directory)

(require 'elfeed)
(define-key elfeed-search-mode-map "c"
  (lambda ()
    (interactive)
    (goto-line 2)))
(define-key elfeed-search-mode-map "t" 'elfeed-search-untag-all-unread)
(define-key elfeed-search-mode-map "r" nil)
(define-key elfeed-search-mode-map "n" 'elfeed-search-browse-url)

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

(require 'eshell)
(defun pcomplete/eshell-mode/virsh ()
  (pcomplete-here '("destroy"
		    "list"
		    "start"
		    "shutdown"
		    "reboot"))
  (pcomplete-here
   (let ((last-cmd (nth (1- pcomplete-last) pcomplete-args)))
     (cond
      ((equal "list" last-cmd)
       '("--all"))
      (t
       '("f20"
	 "f21"
	 "systemd-devel"
	 "rhel-6"
	 "rhel-7")))))) ; TODO parse this information from 'virsh list --all --name'

(require 'expand-region)
(global-set-key (kbd "C-c -") 'er/expand-region)
(setq expand-region-contract-fast-key "_")

(require 'helm)
;; (helm-mode t)
;; (add-to-list 'helm-completing-read-handlers-alist '(find-file . nil))
;; (add-to-list 'helm-completing-read-handlers-alist '(find-file-other-window . nil))
;; (add-to-list 'helm-completing-read-handlers-alist '(find-alternate-file . nil))
;; (add-to-list 'helm-completing-read-handlers-alist '(kill-buffer . nil))

(require 'helm-git-grep)
(global-set-key (kbd "M-s M-g") 'helm-git-grep)

(require 'ido)
(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-use-url-at-point t)
(setq ido-auto-merge-work-directories-length -1)

(require 'magit)
(setq magit-auto-revert-mode-lighter nil)
; kill magit windows when quitting
(define-key magit-mode-map "q" #'(lambda ()
				   (interactive)
				   (magit-mode-quit-window t)))

(require 'notmuch)
(setq notmuch-fcc-dirs "Sent")
(setq notmuch-show-logo nil)

(defun jsynacek-notmuch-mark-read-and-archive ()
  (interactive)
  (notmuch-search-tag '("-unread"))
  (notmuch-search-archive-thread))
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
(setq org-default-notes-file "~/SpiderOak Hive/orgfiles/inbox.org.gpg")
(setq org-capture-templates '(("t" "New item into Inbox" entry
			       (file org-agenda-files)
			       "** %?\n   added:%U" :empty-lines-after 1)))
(setq org-catch-invisible-edits 'show-and-error)
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAITING(w@/!)" "|" "DONE(d!)")))
(setq org-clock-out-when-done t)
(setq org-agenda-span 14)
(setq org-agenda-start-on-weekday nil)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))
(setq org-src-fontify-natively t)

(require 'org-notmuch)

(require 'paredit)
(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode 1)))

(require 'solarized)
(setq solarized-use-variable-pitch nil)
(setq solarized-height-minus-1 1.0)
(setq solarized-height-plus-1 1.0)
(setq solarized-height-plus-2 1.0)
(setq solarized-height-plus-3 1.0)
(setq solarized-height-plus-4 1.0)
(setq custom-safe-themes
      '("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" ; solarized-light
	"8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" ; solarized-dark
	default))
(load-theme 'solarized-dark)

(require 'recentf)
(setq recentf-max-saved-items 50)

;; (require 'smartparens-config)
;; (define-global-minor-mode jsynacek-smartparens-mode smartparens-mode
;;   (lambda ()
;;     (when (memq major-mode '(emacs-lisp-mode c-mode))
;;       (smartparens-strict-mode 1))))
;; (jsynacek-smartparens-mode 1)
;; (define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
;; (define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)
;; (define-key sp-keymap (kbd "C-M-d") 'sp-down-sexp)
;; (define-key sp-keymap (kbd "C-M-u") 'sp-backward-up-sexp)
;; (define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
;; ;(define-key sp-keymap (kbd "C-k") 'sp-kill-hybrid-sexp)
;; (define-key sp-keymap (kbd "M-D") 'sp-splice-sexp)
;; (define-key sp-keymap (kbd "M-0") 'sp-forward-slurp-sexp)
;; (define-key sp-keymap (kbd "M-9") 'sp-forward-barf-sexp)
;; (define-key sp-keymap (kbd "M-(") 'sp-backward-slurp-sexp)
;; (define-key sp-keymap (kbd "M-)") 'sp-backward-barf-sexp)


(require 'undo-tree)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

(load "jsynacek-util.el")
(require 'private)

;;; keybindings
; basic
(global-set-key [remap list-buffers] 'ibuffer)
(defun jsynacek-find-file (arg)
  (interactive "P")
  (if current-prefix-arg
      (find-file (ido-completing-read "Open recent: "
				      (recentf-elements recentf-max-saved-items)))
    (call-interactively #'find-file)))
;; (global-set-key (kbd "C-o") 'jsynacek-find-file) ; was open-line
;; (global-set-key (kbd "C-b") 'switch-to-buffer) ; was backward-char
;; (global-set-key (kbd "C-w") 'jsynacek-kill-current-buffer) ; was kill-region
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-h r") 'helm-info-emacs)
(global-set-key (kbd "<f2>") 'save-buffer)
(global-set-key (kbd "M-2") 'delete-window)
(global-set-key (kbd "M-3") 'delete-other-windows)
(global-set-key (kbd "M-4") 'switch-to-buffer-other-window)
(global-set-key (kbd "M-5") 'find-file-other-window)
(global-set-key (kbd "M-\\") 'fixup-whitespace) ; was delete-horizontal-space
(global-set-key (kbd "C-c /") 'hippie-expand)
(global-set-key (kbd "C-c x") (if (fboundp 'helm-M-x)
				  'helm-M-x
				'execute-extended-command))
					;(global-set-key (kbd "M-x") 'jsynacek-kill-line-or-region)
(define-key minibuffer-local-completion-map (kbd "M-v") 'jsynacek-yank)
(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "C-S-z") 'undo-tree-redo)
(global-set-key (kbd "C-j") 'ace-jump-mode)
(global-set-key (kbd "M-y") 'helm-show-kill-ring) ; was yank-pop
(global-set-key (kbd "C-h a") 'helm-apropos)      ; was apropos-command
(global-set-key (kbd "C-h l") 'helm-locate-library) ; was view-lossage
(global-set-key (kbd "C-h n") 'man)		     ; was view-emacs-news
(global-set-key (kbd "C-h M") 'helm-man-woman)
; movement
(global-set-key (kbd "M-c") 'previous-line)
(global-set-key (kbd "M-C") 'scroll-down)
(global-set-key (kbd "M-h") 'backward-char)
(global-set-key (kbd "M-t") 'next-line)
(global-set-key (kbd "M-T") 'scroll-up)
(global-set-key (kbd "M-n") 'forward-char)
(global-set-key (kbd "M-r") 'forward-word)
; selection
(define-prefix-command 'jsynacek-selection-map)
(global-set-key (kbd "M-;") 'jsynacek-selection-map)
(global-set-key (kbd "M-; o") 'er/mark-word)
(global-set-key (kbd "M-; O") 'er/mark-paragraph)
(global-set-key (kbd "M-; k") 'jsynacek-mark-line) ; TODO this seems really not needed
; windows
;; (define-prefix-command 'jsynacek-window-keymap)
;; (global-set-key (kbd "M-w") 'jsynacek-window-keymap)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <down>") 'windmove-down)
(global-set-key (kbd "C-c <right>") 'windmove-right)
; evaluation
(define-prefix-command 'jsynacek-eval-keymap)
(global-set-key (kbd "M-e") 'jsynacek-eval-keymap)
(global-set-key (kbd "M-e M-e") 'eval-last-sexp)
(global-set-key (kbd "M-e b") 'eval-buffer)
(global-set-key (kbd "M-e d") 'eval-defun)
(global-set-key (kbd "M-e e") 'eval-last-sexp)
(global-set-key (kbd "M-e r") 'eval-region)
; transposition
(define-prefix-command 'jsynacek-transpose-keymap)
(global-set-key (kbd "C-c t") 'jsynacek-transpose-keymap)
(global-set-key (kbd "C-c t t") 'transpose-chars)
(global-set-key (kbd "C-c t s") 'transpose-sexps)
(global-set-key (kbd "C-c t w") 'transpose-words)
(global-set-key (kbd "C-c t l") 'transpose-lines)
; searching
(define-prefix-command 'jsynacek-search-keymap)
(global-set-key (kbd "C-c s") 'jsynacek-search-keymap)
(global-set-key (kbd "C-c s g") 'rgrep)
(global-set-key (kbd "C-c s s") 'helm-swoop)
(global-set-key (kbd "C-c s .") 'isearch-forward-symbol-at-point)
; inserting and removing
(global-set-key (kbd "C-c o") 'jsynacek-open-below)
(global-set-key (kbd "C-c O") 'jsynacek-open-above)
(global-set-key (kbd "C-c d") 'kill-whole-line)

; code navigation
;;(global-set-key (kbd "M-m r") 'ggtags-find-reference)
;;(global-set-key (kbd "M-m f") 'ggtags-find-tag-dwim)
(global-set-key (kbd "C-c i") 'helm-semantic-or-imenu)
; mail
(define-prefix-command 'jsynacek-mail-keymap)
(global-set-key (kbd "C-c m") 'jsynacek-mail-keymap)
(global-set-key (kbd "C-c m m") 'notmuch)
(global-set-key (kbd "C-c m n") 'jsynacek-mail-get)
(global-set-key (kbd "C-c m s") 'jsynacek-mail-send)
; org
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
; apps
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c s") 'eshell)
(global-set-key (kbd "C-c p") 'proced)
; comments
(global-set-key (kbd "C-c ;") 'jsynacek-comment-line-or-region)
; buffer
;;(global-set-key (kbd "M-m b b") 'switch-to-buffer)
(global-set-key (kbd "C-c R") 'revert-buffer)
;;(global-set-key (kbd "M-m b k") 'jsynacek-kill-current-buffer)
; file and bookmarks
(global-set-key (kbd "C-c b") 'helm-bookmarks)
; version control
(global-set-key (kbd "C-c v l") 'magit-log)
; other
(global-set-key (kbd "C-c r") 'recompile)

;;; enable "dangerous" commands
(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
