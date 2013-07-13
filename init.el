;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; init
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq user-full-name "Jan Synáček")
(setq user-mail-address "jsynacek@redhat.com")
(setq user-nick "jsynacek")

(add-to-list 'load-path (expand-file-name "~/.emacs.d/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/emacs-color-theme-solarized/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/ido-hacks"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/gitsum"))

(require 'idomenu)

(require 'ido-hacks)

(require 'color-theme-solarized)
(color-theme-solarized-dark)

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(defun install-my-packages ()
  (interactive)
  (let ((pkg-list '(ace-jump-mode
                    buffer-move
                    cl-lib
                    expand-region
                    fill-column-indicator
                    highlight-symbol
                    magit
                    paredit
                    undo-tree
                    yasnippet)))
    (dolist (pkg pkg-list)
      (if (package-installed-p pkg)
          (message (concat (symbol-name pkg) " already installed."))
        (package-install pkg)))))
;; TODO run this every start up?
;(instal-my-packages)


(require 'expand-region)

(require 'uniquify)

(require 'ace-jump-mode)

;(require 'auto-complete-config)

(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)
(setq browse-kill-ring-quit-action 'save-and-restore)

(setq edebug-trace t)
;(define-key emacs-lisp-mode-map (kbd "C-c .") 'find-function-at-point)

(setq display-time-day-and-date t
      display-time-24hr-format t
      display-time-default-load-average nil)
(display-time-mode t)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function (if (> (frame-width) 140)
                                      'split-window-horizontally
                                    'split-window-vertically))

(setq diff-switches "-u")

;;; server
(setenv "EDITOR" "emacsclient")
(require 'server)
(unless (server-running-p)
  (server-start))

(setq frame-title-format
      '((:eval (concat "e: " (if (buffer-file-name)
                                 (abbreviate-file-name (buffer-file-name))
                               "%b")))))

(require 'highlight-symbol)

(require 'gitsum)

;; (defvar python-mode-home (expand-file-name "~/.emacs.d/python-mode.el-6.1.1/"))
;; (add-to-list 'load-path python-mode-home)
;; (setq py-install-directory python-mode-home)
;; (require 'python-mode)

(require 'buffer-move)
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

(defun my-multi-occur-in-matching-buffers (regexp &optional allbufs)
  "Show all lines matching REGEXP in all buffers."
  (interactive (occur-read-primary-args))
  (multi-occur-in-matching-buffers ".*" regexp))

(defun kill-all-dired-buffers()
  "Kill all dired buffers."
  (interactive)
  (save-excursion
    (let ((count 0))
      (dolist (buffer (buffer-list))
        (set-buffer buffer)
        (when (equal major-mode 'dired-mode)
          (setq count (1+ count))
          (kill-buffer buffer)))
      (message "Killed %i dired buffer(s)." count ))))

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
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(size-indication-mode t)
(set-default-font "Terminus-12")
;(blink-cursor-mode -1)
;(ffap-bindings)

(auto-fill-mode 1)
(setq fill-column 80)
(prefer-coding-system 'utf-8)

(ido-mode 1)
(setq ido-case-fold nil
      ido-create-new-buffer 'always
      ido-enable-flex-matching t
      ido-everywhere t)
(ido-hacks-mode 1)

(setq org-todo-keywords
      '((sequence "TODO" "INPROGRESS" "|" "DONE" ))
      org-todo-keyword-faces
      '(("INPROGRESS" . (:foreground "#af8700" :weight bold))))

;; fill column indicator
(setq fci-rule-width 5
      fci-rule-color "#073642")
;(fci-mode t)

; auto-complete-mode
(setq ac-auto-show-menu nil)

(fset 'yes-or-no-p 'y-or-n-p)

;;; Shut up compile saves
(setq compilation-ask-about-save nil)

(defun my-rpm-hook-defaults ()
  (setq tab-width 4
        indent-tabs-mode t))
(add-hook 'rpm-spec-mode-hook 'my-rpm-hook-defaults)

(defun my-c-mode-hook-defaults ()
  (setq c-default-style "linux"
        c-basic-offset 4
        indent-tabs-mode t
        subword-mode t))
(add-hook 'c-mode-hook 'my-c-mode-hook-defaults)


; TODO this does not work at startup with the scratch buffer
;(add-hook 'emacs-lisp-mode-hook
;         (lambda ()
;           eldoc-mode))

;(add-hook 'after-save-hook 'whitespace-cleanup)
;; todo org mode dont export postamble
;; todo auto-revert-non-file-buffers (dired?)
;; todo ido-goto-symbol
;; todo bind meta-tab to complete-tag?
;; todo sudo-editb

(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0123456789")
  (or (looking-at "[0123456789]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(defun decrement-number-at-point ()
  (interactive)
  (skip-chars-backward "0123456789")
  (or (looking-at "[0123456789]+")
      (error "No number at point"))
  (replace-match (number-to-string (1- (string-to-number (match-string 0))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "M-O") 'vi-open-line-above)
(global-set-key (kbd "M-o") 'vi-open-line-below)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-c j")
                (lambda ()
                  (interactive)
                  (join-line -1)))
(global-set-key (kbd "C-c e") 'eval-and-replace)
(global-set-key (kbd "<f5>") 'recompile)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)
(global-set-key (kbd "C-x w") 'write-region)
; org-mode
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)
; my multi-occur
(global-set-key (kbd "M-s /") 'my-multi-occur-in-matching-buffers)
; highlight-symbol
(global-set-key (kbd "C-x *") 'highlight-symbol-at-point)
(global-set-key (kbd "M-n") 'highlight-symbol-next)
(global-set-key (kbd "M-p") 'highlight-symbol-prev)
; other
(global-set-key (kbd "C-x \\") 'align-regexp)
(global-set-key (kbd "C-x m") 'eshell)
(global-set-key (kbd "M-\\") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer-other-window)
(global-set-key (kbd "C-x g") 'magit-status)
; numbers
(global-set-key (kbd "C-c +") 'increment-number-at-point)
(global-set-key (kbd "C-c -") 'decrement-number-at-point)
(require 'python)
(define-key python-mode-map (kbd "M-e") 'python-next-statement)
(define-key python-mode-map (kbd "M-a") 'python-previous-statement)
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

(global-set-key (kbd "M-i") 'idomenu)

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
 '(ediff-diff-options "")
 '(electric-pair-mode nil)
 '(global-auto-revert-mode t)
 '(indent-tabs-mode nil)
 '(org-export-html-postamble-format (quote (("en" "<p class=\"author\">Author: %a (%e)</p>
<p class=\"date\">Date: %d</p>"))))
 '(org-export-html-style-include-default nil)
 '(org-export-html-table-tag "<table border=\"2\">")
 '(scroll-conservatively 10)
 '(scroll-margin 2)
 '(scroll-preserve-screen-position 1)
 '(show-paren-mode t)
 '(split-height-threshold nil)
 '(split-width-threshold 140)
 '(tab-width 4)
 '(tramp-default-method "ssh")
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(whitespace-style (quote (face tabs spaces trailing lines newline empty space-after-tab space-mark tab-mark newline-mark))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
