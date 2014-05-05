;;; init

(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))
(setq custom-file (expand-file-name "emacs-custom.el" user-emacs-directory))

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
(auto-fill-mode 1)
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

(setq ergoemacs-theme nil)
(setq ergoemacs-keyboard-layout "us")
(use-package ergoemacs-mode
  :config
  (progn
    (bind-key "M-;" 'comment-dwim)
    ;; TODO rebind M-e/M-r to backward-kill-sexp/kill-sexp ?
    (bind-key "M-D" 'backward-kill-sexp)
    (bind-key "M-F" 'kill-sexp)
    (ergoemacs-mode 1)))

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

    (bind-key "C-x C-g" 'magit-status)
    (bind-key "q" 'magit-mode-quit-window magit-status-mode-map)
    (bind-key "W" 'magit-toggle-whitespace magit-status-mode-map)))

(use-package projectile
  :config
  (progn
    (projectile-global-mode 1)))

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

(require 'private)
;; (require 'defuns)
;; (require 'setup-bbdb)
;; (require 'setup-dired)
;; (require 'setup-ediff)
;; (require 'setup-erc)
;; (require 'setup-recentf)
;; (require 'setup-org)

(load custom-file)
