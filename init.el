;;; init

(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))
(setq custom-file (expand-file-name "emacs-custom.el" user-emacs-directory))
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
(setenv "EDITOR" "emacsclient")

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

(require 'package)
(package-initialize)
(require 'use-package)

(let ((pkg-list '(buffer-move
                  diminish
                  helm
                  magit
                  projectile
                  pydoc-info
                  smex
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
    ;; TODO fix
    ;; (diminish 'hi-lock-mode)
    ))

(use-package dired
  :config
  (progn
    (require 'dired-x)
    (setq dired-dwim-target t)))

(use-package helm
  :config
  (progn
    ))

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

    (recentf-mode t)

    (bind-key "C-x C-r" 'ido-recentf-open)))

(use-package smex
  :init (smex-initialize))

(use-package undo-tree
  :init (undo-tree-mode))

(use-package uniquify)

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
        subword-mode t))
(add-hook 'c-mode-hook 'jsynacek-c-mode-hook-defaults)

(defun jsynacek-sh-mode-hook-defaults ()
  (setq sh-indentation 2))
(add-hook 'sh-mode-hook 'jsynacek-sh-mode-hook-defaults)

;; highlight code annotations
(defun jsynacek-highlight-code-annotations ()
  (font-lock-add-keywords nil
			  '(("\\<\\(FIXME\\|TODO\\|BUG\\|XXX\\)" 1 font-lock-warning-face t))))
(add-hook 'prog-mode-hook 'jsynacek-highlight-code-annotations)

;; highlight trailing whitespace with ugly pink
(defun jsynacek-highlight-trailing-whitespace ()
  (highlight-regexp "\\s-+$" 'hi-pink))
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
(global-set-key [remap goto-line] 'goto-line-with-feedback)
(global-set-key [remap list-buffers] 'ibuffer)
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


;;; requires

(require 'private)
;; (require 'defuns)
;; (require 'setup-bbdb)
;; (require 'setup-dired)
;; (require 'setup-ediff)
(require 'setup-erc)
(require 'setup-org)

;;; keybindings

(global-set-key (kbd "M-a") 'helm-M-x)
(global-set-key (kbd "C-f") 'helm-occur)
(global-set-key (kbd "C-o") 'find-file)
(global-set-key (kbd "M-z") 'undo-tree-undo)
(global-set-key (kbd "M-Z") 'undo-tree-redo)
(global-set-key (kbd "M-SPC") 'set-mark-command)
(global-set-key (kbd "M-t") 'completion-at-point)

;; basic movement
(global-set-key (kbd "M-i") 'previous-line)
(global-set-key (kbd "M-k") 'next-line)
(global-set-key (kbd "M-j") 'backward-char)
(global-set-key (kbd "M-l") 'forward-char)

(global-set-key (kbd "M-u") 'backward-word)
(global-set-key (kbd "M-o") 'forward-word)

(global-set-key (kbd "M-h") 'beginning-of-line)
(global-set-key (kbd "M-H") 'end-of-line)

(global-set-key (kbd "M-K") 'scroll-up-command)
(global-set-key (kbd "M-I") 'scroll-down-command)

;; killing and yanking
(global-set-key (kbd "M-d") 'backward-delete-char-untabify)
(global-set-key (kbd "M-f") 'delete-forward-char)

(global-set-key (kbd "M-e") 'backward-kill-word)
(global-set-key (kbd "M-r") 'kill-word)
(global-set-key (kbd "M-E") 'backward-kill-sexp)
(global-set-key (kbd "M-R") 'kill-sexp)
(defun jsynacek-kill-line-or-region ()
  "Kill region if it is active. Otherwise kill current line."
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (let ((column (current-column)))
      (kill-whole-line)
      (move-to-column column))))
(global-set-key (kbd "M-x") 'jsynacek-kill-line-or-region)
(defun jsynacek-copy-line-or-region ()
  "Copy region if it is active. Otherwise copy current line."
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (kill-ring-save (line-beginning-position) (line-beginning-position 2))))
(global-set-key (kbd "M-c") 'jsynacek-copy-line-or-region)
(global-set-key (kbd "M-v") 'yank)
(global-set-key (kbd "M-V") 'yank-pop)

(defun jsynacek-kill-line-backward ()
  "Kill the line backward."
  (interactive)
  (kill-line 0))
(global-set-key (kbd "M-g") 'kill-line)
(global-set-key (kbd "M-G") 'jsynacek-kill-line-backward)

;; selection
(defun jsynacek-mark-line ()
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position)))
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "M-7") 'jsynacek-mark-line)
(global-set-key (kbd "M-8") 'er/expand-region)
(global-set-key (kbd "M-9") 'er/contract-region)

;; search and replace
(global-set-key (kbd "M-y") 'isearch-forward)
(global-set-key (kbd "M-Y") 'isearch-backward)
(global-set-key (kbd "M-5") 'query-replace)
(global-set-key (kbd "M-%") 'query-replace-regexp)

;; buffers, windows and frames
(global-set-key (kbd "C-b") 'switch-to-buffer)
(global-set-key (kbd "C-s") 'save-buffer)
;; (global-set-key (kbd "C-S") 'write-file)
(global-set-key (kbd "C-w") 'kill-buffer)

(global-set-key (kbd "M-2") 'delete-window)
(global-set-key (kbd "M-3") 'delete-other-windows)
(global-set-key (kbd "M-s") 'other-window)

;; info and help
(global-set-key (kbd "C-/") 'info)
(global-set-key (kbd "C-h 1") 'describe-function)
(global-set-key (kbd "C-h 2") 'describe-variable)
(global-set-key (kbd "C-h 3") 'describe-key)
(global-set-key (kbd "C-h 5") 'man)
(global-set-key (kbd "C-h `") 'elisp-index-search)
(defun describe-thing-at-point ()
  (interactive)
  (let ((function (function-called-at-point))
        (variable (variable-at-point)))
    (cond
     ((/= variable 0) (describe-variable variable))
     (function (describe-function function)))))
(global-set-key (kbd "C-h .") 'describe-thing-at-point)

;;; mode-specific keybindings

(progn
  (define-key minibuffer-local-map (kbd "M-i") 'previous-history-element)
  (define-key minibuffer-local-map (kbd "M-k") 'next-history-element)
  (define-key minibuffer-local-map (kbd "M-j") 'previous-line)
  (define-key minibuffer-local-map (kbd "M-l") 'next-line)

  (define-key prog-mode-map (kbd "M-J") 'backward-sexp)
  (define-key prog-mode-map (kbd "M-L") 'forward-sexp)
  (define-key prog-mode-map (kbd "M-[") 'backward-up-list)
  (define-key prog-mode-map (kbd "M-]") 'down-list)

  (define-key isearch-mode-map (kbd "M-y") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "M-Y") 'isearch-repeat-backward)

  (define-key dired-mode-map (kbd "M-s") 'other-window)
  (define-key dired-mode-map (kbd "C-o") 'find-file)

  (define-key helm-map (kbd "M-i") 'helm-previous-line)
  (define-key helm-map (kbd "M-k") 'helm-next-line)
  (define-key helm-map (kbd "M-I") 'helm-previous-page)
  (define-key helm-map (kbd "M-K") 'helm-next-page)

  (define-key erc-mode-map (kbd "RET") nil)
  (define-key erc-mode-map (kbd "C-<return>") 'erc-send-current-line)
)

;;; key sequences

(define-prefix-command 'jsynacek-menu-keymap)
(global-set-key (kbd "<menu>") 'jsynacek-menu-keymap)
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
;; org mode?

;;; unbindings

(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "C-a") nil)
(global-set-key (kbd "C-e") nil)
(global-set-key (kbd "C-v") nil)
(global-set-key (kbd "C-y") nil)
(global-set-key (kbd "C-x b") nil)
(global-set-key (kbd "C-x k") nil)
(global-set-key (kbd "C-x C-f") nil)

(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))
