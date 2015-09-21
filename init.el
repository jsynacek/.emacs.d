;;; bootstrap
;(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area t)
(setq initial-scratch-message nil)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode t)
(column-number-mode 1)
(setq-default show-trailing-whitespace t)

(setq ring-bell-function 'ignore)
(prefer-coding-system 'utf-8)

(setq global-auto-revert-mode t)
(setq mouse-yank-at-point t)

(setq backup-directory-alist '(("." . "~/.emacs.d/.cache/")))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/.cache/\\1" t)))
(make-directory "~/.emacs.d/.cache/" t)

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(defalias 'yes-or-no-p 'y-or-n-p)

(defadvice eval-region (after jsynacek-eval-region-advice-after activate)
  (deactivate-mark))

;;; packages
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; builtin
(use-package cc-mode
  :config
  (add-hook 'c-mode-hook
	    #'(lambda () (setq c-default-style "linux"))))

(use-package dired
  :config
  (use-package dired-x)
  (setq dired-listing-switches "-al --group-directories-first")
  (define-key dired-mode-map "P" 'dired-up-directory))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward))

(use-package recentf
  :config
  (progn
    (setq recentf-max-saved-items 50)
    (recentf-mode)))

;; external
(use-package avy
  :ensure t
  :config
  (progn
    (setq avy-background t)
    (setq avy-style 'at-full))
  :bind ("C-." . avy-goto-subword-1))

(use-package company
  :ensure t
  :config

  (defun jsynacek-enable-company-mode ()
    (company-mode 1))

  (setq company-idle-delay 0.1)

  (use-package company-c-headers
    :ensure t
    :config
    (add-to-list 'company-backends 'company-c-headers))

  (add-hook 'c-mode-hook 'jsynacek-enable-company-mode)
  (add-hook 'emacs-lisp-mode-hook 'jsynacek-enable-company-mode))

(use-package helm
  :ensure t
  :bind (("C-x b" . helm-mini)
	 ("C-c x" . helm-M-x)
	 ("C-c i" . helm-imenu)
	 ("C-c b" . helm-bookmarks)))

(use-package helm-git-grep
  :ensure t
  :bind ("C-c f" . helm-git-grep))

(define-prefix-command 'tags-prefix)
(global-set-key (kbd "C-t") 'tags-prefix)

(use-package helm-gtags
  :ensure t
  :bind
  (("C-t n" . helm-gtags-dwim)
   ("C-t r" . helm-gtags-find-tag)
   ("C-t g" . helm-gtags-find-rtag)
   ("C-t C-t" . helm-gtags-pop-stack)
   ("C-t /" . helm-gtags-select)
   ("C-t l" . helm-gtags-show-stack)
   ("C-t m" . helm-gtags-update-tags)))

(use-package helm-swoop
  :ensure t)

(use-package magit
  :ensure t
  :bind ("C-c g" . magit-status)
  :config
  (magit-define-popup-switch 'magit-log-popup
			     ?m "Omit merge commits" "--no-merges"))

(use-package notmuch
  :bind ("C-c m m" . jsynacek-notmuch)
  :config
  (setq notmuch-fcc-dirs "Sent")
  (setq notmuch-show-logo nil)

  (defun jsynacek-notmuch ()
    (interactive)
    (delete-other-windows)
    (notmuch))

  (defun jsynacek-notmuch-mark-read-and-archive ()
    (interactive)
    (notmuch-search-tag '("-unread"))
    (notmuch-search-archive-thread))
  (define-key notmuch-search-mode-map "k" 'jsynacek-notmuch-mark-read-and-archive)
  (defun jsynacek-notmuch-mark-read-and-delete ()
    (interactive)
    (notmuch-search-tag '("-unread" "+delete"))
    (notmuch-search-archive-thread))
  (define-key notmuch-search-mode-map "K" 'jsynacek-notmuch-mark-read-and-delete)
  (defun jsynacek-notmuch-search-unread ()
    (interactive)
    (notmuch-hello-search "tag:unread"))
  (define-key notmuch-hello-mode-map "u" 'jsynacek-notmuch-search-unread)
  (defun jsynacek-notmuch-search-inbox ()
    (interactive)
    (notmuch-hello-search "tag:inbox"))
  (define-key notmuch-hello-mode-map "i" 'jsynacek-notmuch-search-inbox)

  (use-package org-notmuch))

(use-package org
  :bind (("C-c a" . org-agenda)
	 ("C-c c" . org-capture)
	 ("C-c l" . org-store-link))
  :config
  (progn
    (setq org-agenda-files '("~/SpiderOak Hive/orgfiles/inbox.org.gpg"
			     "~/SpiderOak Hive/orgfiles/birthday.org"
			     "~/SpiderOak Hive/orgfiles/work-todo.org"
			     "~/SpiderOak Hive/orgfiles/rhel7.org"))
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
    (setq org-src-fontify-natively t)))

(use-package undo-tree
  :ensure t
  :bind (("C-z" . undo-tree-undo)
	 ("C-S-z" . undo-tree-redo)))

(use-package private
  :demand t
  :load-path "lisp"
  :bind (("C-c m n" . jsynacek-mail-get)
	 ("C-c m s" . jsynacek-mail-send)))

(bind-key* "C-c <" 'beginning-of-buffer)
(bind-key* "C-c >" 'end-of-buffer)
(bind-key* "C-c r" 'revert-buffer)
(bind-key* "C-S-s" 'isearch-forward-regexp)
(bind-key* "C-S-r" 'isearch-backward-regexp)
(bind-key* "C-c s" 'query-replace)
(bind-key* "C-c S" 'query-replace-regexp)
(bind-key* "C-/" 'hippie-expand)
(bind-key* "C-c C-f" 'ffap)

(defun jsynacek-duplicate-line ()
  (interactive)
  (kill-whole-line)
  (yank)
  (yank)
  (backward-char))
(bind-key* "C-c C-d" 'jsynacek-duplicate-line)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   (quote
    ((setq indent-tabs-mode nil)
     (nxml-child-indent . 2))))
 '(send-mail-function (quote smtpmail-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
