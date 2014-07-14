;;; init

(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))
(setq custom-file (expand-file-name "emacs-custom.el" user-emacs-directory))
(require 'package)
(package-initialize)
(load custom-file)

;; basic settings
(setq inhibit-startup-message t
      inhibit-startup-echo-area t
      initial-scratch-message nil)
(setq frame-title-format
      '((:eval (concat "e: " (if (buffer-file-name)
                                 (abbreviate-file-name (buffer-file-name))
                               "%b")))))
(setq ring-bell-function 'ignore)
(prefer-coding-system 'utf-8)

(setq jsynacek/org-default-notes-file "~/Dropbox/orgfiles/inbox.org.gpg")

(setq user-full-name "Jan Synáček")
(setq user-nick "jsynacek")

;; host specific preconfig
(if (string= (car (split-string (system-name) "\\."))
             "jsynacek-ntb-home")
    ;; home config
    (progn
      (setq jsynacek/font "Terminus 14")
      (setq user-mail-address "jan.synacek@gmail.com")
      (setq jsynacek/org-tags-column -80))
    ;; work config
  (progn
    (setq jsynacek/font "Terminus")
    (setq user-mail-address "jsynacek@redhat.com")
    (setq jsynacek/org-tags-column -80)))

(set-default-font jsynacek/font)
(setq default-frame-alist `((width . 110)
                            (fullscreen . fullheight)
                            (vertical-scroll-bars . nil)
                            (font . ,jsynacek/font)))

(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program "google-chrome")

;; modes
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)
(electric-indent-mode -1)
(auto-fill-mode 1)
(delete-selection-mode t)
(winner-mode t)

;; common aliases
(defalias 'ar 'align-regexp)
(defalias 'dtw 'delete-trailing-whitespace)
(defalias 'eb 'eval-buffer)
(defalias 'eis 'elisp-index-search)
(defalias 'er 'eval-region)
(defalias 'plp 'package-list-packages)
(defalias 'yes-or-no-p 'y-or-n-p)

;;; packages

(require 'use-package)

(let ((pkg-list '(ace-link
                  ace-jump-mode
                  ace-window
                  diminish
                  discover-my-major
                  helm
                  magit
                  projectile
                  pydoc-info
                  smex
                  smartparens
                  solarized-theme
                  undo-tree
                  visual-regexp
                  yasnippet)))
  (dolist (pkg pkg-list)
    (unless (package-installed-p pkg)
      (package-install pkg))))

;; (setq ergoemacs-theme nil)
;; (setq ergoemacs-keyboard-layout "us")
;; (use-package ergoemacs-mode
;;   :config
;;   (progn
;;     (bind-key "M-;" 'comment-dwim)
;;     ;; TODO rebind M-e/M-r to backward-kill-sexp/kill-sexp ?
;;     (bind-key "M-D" 'backward-kill-sexp)
;;     (bind-key "M-F" 'kill-sexp)
;;     (ergoemacs-mode -11)))

(use-package server
  :init
  (unless (server-running-p)
    (server-start)))

(require 'ace-link)
(ace-link-setup-default)

(use-package calendar
  :config
  (setq holiday-local-holidays
        '((holiday-fixed 1 1 "Nový rok")
          (holiday-easter-etc +1 "Velikonoční pondělí")
          (holiday-fixed 5 1 "Svátek práce")
          (holiday-fixed 5 8 "Den vítězství")
          (holiday-fixed 7 5 "Den slovanských věrozvěstů Cyrila a Metoděje")
          (holiday-fixed 7 6 "Den upálení mistra Jana Husa")
          (holiday-fixed 9 28 "Den české státnosti")
          (holiday-fixed 10 28 "Den vzniku samostatného československého státu")
          (holiday-fixed 11 17 "Den boje za svobodu a demokracii")
          (holiday-fixed 12 24 "Štědrý den")
          (holiday-fixed 12 25 "1. svátek vánoční")
          (holiday-fixed 12 26 "2. svátek vánoční"))
        holiday-christian-holidays nil
        holiday-general-holidays nil
        holiday-hebrew-holidays nil
        holiday-islamic-holidays nil
        holiday-oriental-holidays nil
        holiday-bahai-holidays nil))

(use-package diminish
  :config
  (progn
    (diminish 'auto-fill-function)
    (eval-after-load 'hi-lock
      '(diminish 'hi-lock-mode))
    ))

(use-package discover-my-major)

(use-package dired
  :config
  (progn
    (require 'dired-x)
    (setq dired-dwim-target t)
    ;; isearch filenames in dired
    (add-hook 'dired-mode-hook 'dired-isearch-filenames-mode)))

(use-package erc
  :config
  (progn
    (require 'erc-autoaway)
    (add-to-list 'erc-modules 'notifications 'autoaway)

    (setq erc-nick "jsynacek")
    (setq erc-away-nickname "jsynacek|away")
    (setq erc-log-insert-log-on-open nil)
    (setq erc-log-channels t)
    (setq erc-log-channels-directory "~/.irclogs/")
    (setq erc-save-buffer-on-part t)
    (setq erc-hide-timestamps nil)
    (setq erc-fill-column 100)
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
    (setq erc-auto-set-away t)

    (add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)

    ;; FIXME this causes emacs to throw error when attempting to close it by
    ;;       clicking on the window's close button
    ;; (defadvice save-buffers-kill-emacs (before save-logs (arg) activate)
    ;;   (save-some-buffers t (lambda ()
    ;;                          (when (and (eq major-mode 'erc-mode)
    ;;                                     (not (null buffer-file-name)))))))
    ))

(use-package helm
  :config
  (progn))

(use-package ibuffer
  :config
  (progn
    (setq ibuffer-formats
          '((mark modified read-only " "
                  (name 32 32 :left :elide)
                  " "
                  (mode 16 16 :left :elide)
                  " "
                  filename-and-process)
            (mark " " (name))))

    ;; prevent accidentaly printing buffers
    (bind-key "P" nil ibuffer-mode-map)
    ))

(use-package ido
  :config
  (progn
    (use-package ido-vertical-mode
      :config
      (ido-vertical-mode t))

    (setq ido-enable-flex-matching t
          ido-use-filename-at-point 'guess
          ido-use-url-at-point t
          ido-case-fold t
          ido-everywhere t)

    (ido-mode t)))

(use-package magit
  :config
  (progn
    ;; don't show "MRev" in the modeline
    (setq magit-auto-revert-mode-lighter 'nil)
    (setq magit-restore-window-configuration t)

    ;; FIXME doesn't work for the first time magit status is started
    (defun jsynacek-magit-hide-untracked ()
      "Hide \"Untracked files\" section in magit status buffer."
      (and (search-forward "Untracked files" nil t nil)
           (magit-hide-section)))
    (add-hook 'magit-status-mode-hook 'jsynacek-magit-hide-untracked)

    (defun magit-toggle-whitespace ()
      (interactive)
      (if (member "-w" magit-diff-options)
          (magit-dont-ignore-whitespace)
        (magit-ignore-whitespace)))

    (defun magit-ignore-whitespace ()
      (interactive)
      (add-to-list 'magit-diff-options "-w")
      (magit-refresh))

    (defun magit-dont-ignore-whitespace ()
      (interactive)
      (setq magit-diff-options (remove "-w" magit-diff-options))
      (magit-refresh))

    (bind-key "q" 'magit-mode-quit-window magit-status-mode-map)
    (bind-key "W" 'magit-toggle-whitespace magit-status-mode-map)))

(use-package multiple-cursors
  :config
  (progn
    (bind-key "C-S-c C-S-c" 'mc/edit-lines)
    (bind-key "C->" 'mc/mark-next-like-this)
    (bind-key "C-<" 'mc/mark-previous-like-this)
    (bind-key "C-c C-<" 'mc/mark-all-like-this)))

(use-package notmuch
  :config
  (progn
    (defun jsynacek-notmuch-search-unread ()
      (interactive)
      (notmuch-hello-search "tag:unread"))
    (defun jsynacek-notmuch-search-inbox ()
      (interactive)
      (notmuch-hello-search "tag:inbox"))
    (defun jsynacek-notmuch-mark-read ()
      (interactive)
      (notmuch-search-tag '("-unread")))

    (setq notmuch-fcc-dirs "Sent"
          notmuch-search-oldest-first nil
          notmuch-show-logo nil)

    (bind-key "<XF86Mail>" 'notmuch)
    (bind-key "u" 'jsynacek-notmuch-search-unread notmuch-hello-mode-map)
    (bind-key "i" 'jsynacek-notmuch-search-inbox notmuch-hello-mode-map)
    (bind-key "k" 'jsynacek-notmuch-mark-read notmuch-search-mode-map)))

(use-package org
  :config
  (progn
    ;; enable syntax highlighting in html exports
    (require 'htmlize)
    (add-to-list 'load-path "/home/jsynacek/work/git/upstream/org-mode/contrib/lisp/")
    (require 'org-notmuch)

    (add-to-list 'auto-mode-alist '("\\.\\(org\\|txt\\)$" . org-mode))

    ;; don't leave empty lines in collapsed view
    (setq org-cycle-separator-lines 0)

    (setq org-use-speed-commands t)

    (setq org-completion-use-ido t
          org-log-done 'time
          ;; start my week on monday
          calendar-week-start-day 1
          ;; org-clock-idle-time 2
          org-clock-persist t
          org-clock-history-length 10
          ;; catch invisible edits
          org-catch-invisible-edits 'error
          org-clock-into-drawer t
          )

    (org-clock-persistence-insinuate)

    (setq org-refile-targets '(("~/Dropbox/orgfiles/inbox.org.gpg" :level . 1)
                               ("~/Dropbox/orgfiles/someday.org" :level . 2)))

    (let ((font (if (boundp 'jsynacek/font)
                    (make-symbol jsynacek/font)
                  'fixed-pitch)))
      (set-face-attribute 'org-level-1 nil :inherit font :weight 'bold)
      (set-face-attribute 'org-level-2 nil :inherit font :weight 'bold)
      (set-face-attribute 'org-level-3 nil :inherit font :weight 'bold)
      (set-face-attribute 'org-level-4 nil :inherit font :weight 'bold)
      (set-face-attribute 'org-level-5 nil :inherit font :weight 'bold))

    (setq org-tags-column jsynacek/org-tags-column)
    (setq org-default-notes-file (if (boundp 'jsynacek/org-default-notes-file)
                                     jsynacek/org-default-notes-file))

    (setq org-agenda-files `(,org-default-notes-file
                             "~/Dropbox/orgfiles/birthday.org"
                             "~/Dropbox/orgfiles/bugzilla.org")
          org-capture-templates '(("t" "New item into Inbox" entry
                                   (file+headline org-agenda-files "Inbox")
                                   "** %?\n   added:%U" :empty-lines-after 1)
                                  ("n" "Note" entry
                                   (file+headline "~/Dropbox/orgfiles/notes.org" "Work notes")
                                   "** %?\n   added:%U" :empty-lines-after 1)
                                  ("i" "Interruption" entry
                                   (file+headline org-agenda-files "Inbox")
                                   "** interruption: %?\n   %U" :clock-in :clock-resume))
          org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE")
                              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))
          org-todo-keyword-faces '(("STARTED" . (:foreground "#af8700" :weight bold)))
          )

    ;; confirm plantuml source evaluation by default
    (defun jsynacek/org-confirm-plantuml-link-function (lang body)
      (not (string= lang "plantuml")))
    (setq org-confirm-babel-evaluate 'jsynacek/org-confirm-plantuml-link-function)

    ;; allow additional languages
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (plantuml . t)
       (python . t)
       (sh . t)))

    ;; do not export the validation link
    (setq org-html-validation-link nil)

    ;; allow #+BIND
    (setq org-export-allow-bind-keywords t)

    ;; allow source color highlighting in org buffers
    (setq org-src-fontify-natively t)

    (setq org-plantuml-jar-path "/usr/share/java/plantuml.jar")

    ;; global keybindings
    (bind-key "C-c a" 'org-agenda)
    (bind-key "C-c c" 'org-capture)
    (bind-key "C-c l" 'org-store-link)
    (bind-key "C-c b" 'org-iswitchb)
    (define-prefix-command 'jsynacek-f9-keymap)
    (bind-key "<f9>" 'jsynacek-f9-keymap)
    (bind-key "<f9> C-i" 'org-clock-in)
    (bind-key "<f9> C-o" 'org-clock-out)
    (bind-key "<f9> C-j" 'org-clock-jump-to-current-clock)))

(use-package projectile
  :config
  (progn
    (projectile-global-mode 1)))

(use-package recentf
  :config
  (progn
    ;; 200 files ought to be enough.
    (setq recentf-max-saved-items 200)

    (defun ido-recentf-open ()
      "Use `ido-completing-read' to \\[find-file] a recent file"
      (interactive)
      (if (find-file (ido-completing-read "Find recent file: " recentf-list))
          (message "Opening file...")
        (message "Aborting")))

    (recentf-mode t)))

(use-package smartparens
  :config
  (progn
    (require 'smartparens-config)
    (show-smartparens-global-mode t)
    ;; (smartparens-global-strict-mode t)
    (add-hook 'emacs-lisp-mode-hook 'turn-on-smartparens-strict-mode)
    (add-hook 'c-mode-hook 'turn-on-smartparens-strict-mode)))

(use-package smex
  :init (smex-initialize))

(use-package undo-tree
  :init (undo-tree-mode))

(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'post-forward))

;; Show the current function name in the header line
;; (which-function-mode 1)
;; (setq header-line-format
;;       '((which-func-mode ("" which-func-format " "))))
;; (setq mode-line-misc-info
;;       ;; We remove Which Function Mode from the mode line, because it's mostly
;;       ;; invisible here anyway.
;;       (assq-delete-all 'which-func-mode mode-line-misc-info))

;;; hooks

(defun jsynacek-c-mode-hook-defaults ()
  (setq c-default-style "linux"
        indent-tabs-mode t
        subword-mode t)
  (auto-complete-mode))
(add-hook 'c-mode-hook 'jsynacek-c-mode-hook-defaults)

(defun jsynacek-sh-mode-hook-defaults ()
  (setq sh-indentation 2
        indent-tabs-mode nil))
(add-hook 'sh-mode-hook 'jsynacek-sh-mode-hook-defaults)

;; highlight code annotations
(defun jsynacek-highlight-code-annotations ()
  (font-lock-add-keywords nil
			  '(("\\<\\(FIXME\\|TODO\\|BUG\\|XXX\\)" 1 font-lock-warning-face t))))
(add-hook 'prog-mode-hook 'jsynacek-highlight-code-annotations)

;; highlight trailing whitespace with ugly pink
(defun jsynacek-highlight-trailing-whitespace ()
;  (highlight-regexp "\\s-+$" 'hi-pink))
  (setq-local show-trailing-whitespace t))
(add-hook 'find-file-hook 'jsynacek-highlight-trailing-whitespace)

;;; remappings

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))
(bind-key [remap goto-line] 'goto-line-with-feedback)
(bind-key [remap list-buffers] 'ibuffer)
(defun dired-back-to-start-of-files ()
  (interactive)
  (backward-char (- (current-column) 2)))
(define-key dired-mode-map [remap beginning-of-line] 'dired-back-to-start-of-files)
(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 4))
(define-key dired-mode-map [remap beginning-of-buffer] 'dired-back-to-top)
(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))
(define-key dired-mode-map [remap end-of-buffer] 'dired-jump-to-bottom)
(bind-key [remap other-window] 'ace-window)

;;; requires

(require 'private)
;; (require 'defuns)
;; (require 'setup-bbdb)
;; (require 'setup-dired)
;; (require 'setup-ediff)

;;; keybindings
;; TODO use C-, as leader instead of <menu>?
(bind-key "M-`" 'other-frame)
(bind-key "M-/" 'hippie-expand) ;TODO customize a bit
(bind-key "C-." 'ace-jump-mode)
(bind-key "<f12>" 'recompile)
(bind-key "C-M-<backspace>" 'backward-kill-sexp)
(bind-key "M-x" 'helm-M-x)
(bind-key "M-X" 'smex-major-mode-commands)

;; (bind-key "M-a" 'helm-M-x)
;; (bind-key "C-f" 'helm-occur)
(defun jsynacek-find-file-or-recentf (arg)
  "Call `find-file' if run with nil argument. If called with a
universal argument, run `helm-recentf' if bound, otherwise
`ido-recentf-open'."
  (interactive "P")
  (if (null arg)
      (if (fboundp 'ffap)
          (call-interactively 'ffap)
        (call-interactively 'find-file))
    (if (fboundp 'helm-recentf)
        (helm-recentf)
      (ido-recentf-open))))
(bind-key [remap find-file] 'jsynacek-find-file-or-recentf)

;; (bind-key* "C-o" 'jsynacek-find-file-or-recentf)
;; (bind-key "C-d" 'dired)
;; (bind-key "M-z" 'undo-tree-undo)
;; (bind-key "M-Z" 'undo-tree-redo)
;; (bind-key "M-SPC" 'set-mark-command)
;; (bind-key "M-t" 'completion-at-point)

;; basic movement
;; (bind-key "M-i" 'previous-line)
;; (bind-key "M-k" 'next-line)
;; (bind-key "M-j" 'backward-char)
;; (bind-key "M-l" 'forward-char)

;; (bind-key "M-u" 'backward-word)
;; (bind-key "M-o" 'forward-word)

(defun jsynacek-open-line ()
  ""
  (interactive)
  (end-of-line)
  (newline-and-indent))
(bind-key [(shift return)] 'jsynacek-open-line)

(defun jsynacek-beginning-of-line ()
  ""
  (interactive)
  (if (= (point) (+ (line-beginning-position) (current-indentation)))
      (beginning-of-line)
    (back-to-indentation)))
(bind-key [remap move-beginning-of-line] 'jsynacek-beginning-of-line) ;TODO this makes M-m (back-to-indentation) free to use

;; (bind-key* "M-h" 'jsynacek-beginning-of-line)
;; (bind-key* "M-H" 'end-of-line)

;; (bind-key "M-K" 'scroll-up-command)
;; (bind-key "M-I" 'scroll-down-command)

;; ;; killing and yanking
;; (bind-key "M-d" 'backward-delete-char-untabify)
;; (bind-key "M-f" 'delete-forward-char)

;; (bind-key "M-e" 'backward-kill-word)
;; (bind-key "M-r" 'kill-word)
;; (bind-key "M-E" 'backward-kill-sexp)
;; (bind-key "M-R" 'kill-sexp)
(defun jsynacek-kill-line-or-region ()
  "Kill region if it is active. Otherwise kill current line."
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (let ((column (current-column)))
      (kill-whole-line)
      (move-to-column column))))
(bind-key [remap kill-region] 'jsynacek-kill-line-or-region)

;; (bind-key* "M-x" 'jsynacek-kill-line-or-region)
(defun jsynacek-copy-line-or-region ()
  "Copy region if it is active. Otherwise copy current line."
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (kill-ring-save (line-beginning-position) (line-beginning-position 2))))
(bind-key [remap kill-ring-save] 'jsynacek-copy-line-or-region)
;; (bind-key* "M-c" 'jsynacek-copy-line-or-region)
;; (bind-key* "M-v" 'yank)
;; (bind-key* "M-V" 'yank-pop)

(defun jsynacek-kill-line-backward ()
  "Kill the line backward."
  (interactive)
  (kill-line 0))
;; (bind-key "M-g" 'kill-line)
;; (bind-key "M-G" 'jsynacek-kill-line-backward)

;; selection
(defun jsynacek-mark-line ()
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position)))
;; (bind-key "C-a" 'mark-whole-buffer)
(bind-key "M-6" 'mark-defun)
(bind-key "M-7" 'jsynacek-mark-line)
(bind-key "M-8" 'er/expand-region)
(bind-key "M-9" 'er/contract-region)

;; search and replace
;; (bind-key "M-y" 'isearch-forward)
;; (bind-key "M-Y" 'isearch-backward)
;; (bind-key "M-5" 'query-replace)
;; (bind-key "M-%" 'query-replace-regexp)

;; buffers, windows and frames
(defun jsynacek-switch-to-buffer (arg)
  ""
  (interactive "P")
  (if (null arg)
      (call-interactively 'switch-to-buffer)
    (call-interactively 'switch-to-buffer-other-window)))
;; (bind-key "C-b" 'jsynacek-switch-to-buffer)
(defun jsynacek-save-buffer-or-write-file (arg)
  ""
  (interactive "P")
  (if (null arg)
      (call-interactively 'save-buffer)
    (call-interactively 'write-file)))
;; (bind-key "C-s" 'jsynacek-save-buffer-or-write-file)
;; (bind-key "C-w" 'kill-buffer)
;; (bind-key "C-r" 'revert-buffer)

(bind-key "M-2" 'delete-window)
(bind-key "M-3" 'delete-other-windows)
;; (bind-key* "M-s" 'ace-window)
(defun jsynacek-make-frame-and-select ()
  (interactive)
  (select-frame (make-frame-command)))
;; (bind-key "C-n" 'jsynacek-make-frame-and-select)

;; info and help
(bind-key "C-h 1" 'describe-function)
(bind-key "C-h 2" 'describe-variable)
(bind-key "C-h 3" 'describe-key)
(bind-key "C-h 5" 'man)
(bind-key "C-h `" 'elisp-index-search)
(bind-key "C-m" 'discover-my-major 'help-command)
(bind-key "C-f" 'find-function 'help-command)
(bind-key "C-k" 'find-function-on-key 'help-command)
(bind-key "C-v" 'find-variable 'help-command)
(bind-key "C-l" 'find-library 'help-command)

(defun describe-thing-at-point ()
  (interactive)
  (let ((function (function-called-at-point))
        (variable (variable-at-point)))
    (cond
     ((/= variable 0) (describe-variable variable))
     (function (describe-function function)))))
(bind-key "C-h ." 'describe-thing-at-point)

;;; mode-specific keybindings

(progn
  ;; (define-key eshell-command-map (kbd "M-G") 'eshell-kill-input)

  ;; (define-key minibuffer-local-map (kbd "M-i") 'previous-history-element)
  ;; (define-key minibuffer-local-map (kbd "M-k") 'next-history-element)

  ;; (define-key prog-mode-map (kbd "M-J") 'backward-sexp)
  ;; (define-key prog-mode-map (kbd "M-L") 'forward-sexp)
  ;; (define-key prog-mode-map (kbd "M-[") 'backward-up-list)
  ;; (define-key prog-mode-map (kbd "M-]") 'down-list)

  ;; (define-key isearch-mode-map (kbd "M-y") 'isearch-repeat-forward)
  ;; (define-key isearch-mode-map (kbd "M-Y") 'isearch-repeat-backward)

  ;; (define-key dired-mode-map (kbd "M-z") 'dired-undo)
  ;; (define-key dired-mode-map (kbd "M-J") 'dired-prev-subdir)
  ;; (define-key dired-mode-map (kbd "M-L") 'dired-next-subdir)

  ;; (define-key helm-map (kbd "M-i") 'helm-previous-line)
  ;; (define-key helm-map (kbd "M-k") 'helm-next-line)
  ;; (define-key helm-map (kbd "M-I") 'helm-previous-page)
  ;; (define-key helm-map (kbd "M-K") 'helm-next-page)

  (define-key erc-mode-map (kbd "RET") nil)
  (define-key erc-mode-map (kbd "C-<return>") 'erc-send-current-line)
)

;;; key sequences

(define-prefix-command 'jsynacek-menu-keymap)
(bind-key "<menu>" 'jsynacek-menu-keymap)
;; M-x
(bind-key "<menu> M-x" 'execute-extended-command)
;; apps
(bind-key "<menu> a c" 'calc)
(bind-key "<menu> a e" 'eshell)
(bind-key "<menu> a g" 'magit-status)
;; evaluations
(bind-key "<menu> e b" 'eval-buffer)
(bind-key "<menu> e d" 'eval-defun)
(bind-key "<menu> e e" 'eval-last-sexp)
(bind-key "<menu> e r" 'eval-region)
;; marking
(bind-key "<menu> m d" 'mark-defun)
;; transpositions
(bind-key "<menu> t l" 'transpose-lines)
(bind-key "<menu> t s" 'transpose-sexps)
(bind-key "<menu> t w" 'transpose-words)

;;; unbindings

;; (unbind-key "C-z")
;; (unbind-key "C-e")
;; (unbind-key "C-v")
;; (unbind-key "C-y")
;; (unbind-key "C-x b")
;; (unbind-key "C-x k")
;; (unbind-key "C-x C-f")

;; (unbind-key "<left>")
;; (unbind-key "<right>")
;; (unbind-key "<up>")
;; (unbind-key "<down>")
