;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; init
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path (expand-file-name "~/.emacs.d/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/emacs-color-theme-solarized/"))

(require 'idomenu)

(require 'color-theme-solarized)
(color-theme-solarized-dark)

(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(require 'expand-region)

(require 'uniquify)

(require 'ace-jump-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc defuns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun vi-open-line-above ()
  "Insert a newline above the current line and put point at beginning."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun vi-open-line-below ()
  "Insert a newline below the current line and put point at beginning."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun spec-changelog-entry ()
  (interactive)
  (let ((ts (current-time-string)))
    (insert (concat "* "
                    (substring ts 0 10)
                    (substring ts 19)
                    " Jan Synáček <jsynacek@redhat.com> - "))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "borrowed" from emacs starter kit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
	     (current-buffer))
    (error (message "Invalid expression")
	   (insert (current-kill 0)))))

(defun lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
	  "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
	  "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
	  "aliquip ex ea commodo consequat. Duis aute irure dolor in "
	  "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
	  "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
	  "culpa qui officia deserunt mollit anim id est laborum."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq inhibit-startup-message t
      inhibit-startup-echo-area t)

;(blink-cursor-mode -1)
(ffap-bindings)

(set-default-font "Terminus-12")
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(auto-fill-mode 1)

(prefer-coding-system 'utf-8)

(setq case-fold-search nil)

(ido-mode 1)

(size-indication-mode t)

(setq org-todo-keywords
      '((sequence "TODO" "INPROGRESS" "|" "DONE" )))

(setq org-todo-keyword-faces
      '(("INPROGRESS" . (:foreground "#af8700" :weight bold))))

(add-hook 'after-save-hook 'whitespace-cleanup)
;; todo set global auto revert buffer
;; todo org mode dont export postamble
;; todo auto-revert-non-file-buffers (dired?)
;; todo ido-goto-symbol
;; todo bind meta-tab to complete-tag?
;; todo sudo-editb

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "M-O") 'vi-open-line-above)
(global-set-key (kbd "M-o") 'vi-open-line-below)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "M-j")
		(lambda ()
		  (interactive)
		  (join-line -1)))
(global-set-key (kbd "C-c e") 'eval-and-replace)
(global-set-key (kbd "<f5>") 'recompile)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)
; org-mode
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)
; ido-find-file
(global-set-key (kbd "C-x C-f") 'ido-find-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; customized
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-directory-alist (quote (("." . "/home/jsynacek/emacsbackup"))))
 '(column-number-mode t)
 '(confirm-kill-emacs nil)
 '(electric-pair-mode nil)
 '(fill-column 80)
 '(global-auto-revert-mode t)
 '(ido-case-fold nil)
 '(ido-create-new-buffer (quote always))
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(indent-tabs-mode nil)
 '(org-enforce-todo-dependencies t)
 '(org-tags-column -70)
 '(scroll-conservatively 10)
 '(scroll-margin 2)
 '(scroll-preserve-screen-position 1)
 '(show-paren-mode t)
 '(split-height-threshold nil)
 '(split-width-threshold 140)
 '(tab-width 4)
 '(tramp-default-method "ssh")
 '(transient-mark-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
