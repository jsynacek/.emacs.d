;;; init
(setq user-full-name "Jan Synáček")
(setq user-mail-address "jsynacek@redhat.com")
(setq user-nick "jsynacek")

(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp/"))

;;; requires
(package-initialize)
(require 'ace-jump-mode)
(require 'browse-kill-ring)
(require 'buffer-move)
(require 'dired-x)
(require 'expand-region)
(require 'highlight-symbol)
(require 'magit)
(require 'package)
(require 'python)
(require 'saveplace)
(require 'shell-pop)
(require 'smartparens)
(require 'smartparens-config)
(require 'server)
(require 'uniquify)
(require 'keybindings)

;;; settings
(setq inhibit-startup-message t
      inhibit-startup-echo-area t)
(set-default-font "Terminus-12")
(setq default-frame-alist '((font . "Terminus-12")))
(prefer-coding-system 'utf-8)
(setenv "EDITOR" "emacsclient")
(setq fill-column 80)
(fset 'yes-or-no-p 'y-or-n-p)
(setq frame-title-format
      '((:eval (concat "e: " (if (buffer-file-name)
                                 (abbreviate-file-name (buffer-file-name))
                               "%b")))))
;; modes
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)
(auto-fill-mode 1)
(winner-mode t)
(smartparens-global-mode 1)

;;; package settings
;; org
(setq org-todo-keywords
      '((sequence "TODO" "INPROGRESS" "|" "DONE" ))
      org-todo-keyword-faces
      '(("INPROGRESS" . (:foreground "#af8700" :weight bold))))

;; server
(unless (server-running-p)
  (server-start))

;; edebug
(setq edebug-trace t)

;; ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function (if (> (frame-width) 140)
                                      'split-window-horizontally
                                    'split-window-vertically))

;; smartparens
(sp-with-modes sp--lisp-modes
  (sp-local-pair "`" "'")
  (sp-local-pair "'" nil :actions nil))

;;; defuns
(defun install-my-packages ()
  (interactive)
  (let ((pkg-list '(ace-jump-mode
                    buffer-move
                    cl-lib
                    expand-region
                    fill-column-indicator
                    highlight-symbol
                    magit
                    saveplace
                    smartparens
                    solarized-theme
                    undo-tree
                    yasnippet)))
    (dolist (pkg pkg-list)
      (if (package-installed-p pkg)
          (message (concat (symbol-name pkg) " already installed."))
        (package-install pkg)))))

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

(defun describe-thing-at-point ()
  (interactive)
  (let ((function (function-called-at-point))
        (variable (variable-at-point)))
    (cond
     ((/= variable 0) (describe-variable variable))
     (function (describe-function function)))))

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

(defun font-lock-restart ()
  (interactive)
  (setq font-lock-mode-major-mode nil)
  (font-lock-fontify-buffer))

(defun hippie-expand-line ()
  (interactive)
  (let ((hippie-expand-try-functions-list '(try-expand-line
                                            try-expand-line-all-buffers)))
    (hippie-expand nil)))

(defun save-region-or-current-line (arg)
  (interactive "P")
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (kill-ring-save (line-beginning-position) (line-end-position))))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

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

;; full screen magit-status
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

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

(defun google-search ()
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string
     (if mark-active
         (buffer-substring (region-beginning) (region-end))
       (read-string "Google:"))))))

; stolen from python-mode and modified
; TODO fix
(defun pydoc (w)
  "Launch PyDOC on the Word at Point"
  (interactive
   (list (let* ((word (thing-at-point 'word))
                (input (read-string
                        (format "pydoc entry%s: "
                                (if (not word) "" (format " (default %s)" word))))))
           (if (string= input "")
               (if (not word) (error "No pydoc args given")
                 word) ;sinon word
             input)))) ;sinon input
  (shell-command (concat python-shell-interpreter " -c \"from pydoc import help;help(\'" w "\')\"") "*PYDOCS*")
  (view-buffer-other-window "*PYDOCS*" t 'kill-buffer-and-window))

; indent buffer
(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

; lmi-specific
(defun lmi-debug-associators ()
  (interactive)
  (let ((params '(assocClass resultClass role resultRole))
        (debug-str "printf(\"DEBUG: %s: %%s\\n\", %s);\n")
        (opoint (point)))
    (insert " /* TODO: DEBUG */\n"
            (format debug-str
                    "cop"
                    "CMGetCharsPtr(cop->ft->toString(cop, NULL), NULL)"))
    (dolist (param params)
      (insert (format debug-str param param)))
    (indent-region opoint (point))))

(defun insert-debug-statement ()
  (interactive)
  (insert "printf(\"DEBUG: \\n\");")
  (goto-char (- (point) 5))
  (c-indent-line-or-region))

(defun sudo-edit (file)
  (interactive "fSudo edit: ")
  (let ((sudo-prefix "/sudo:root@localhost:"))
    (find-file (concat sudo-prefix file))))

(defun zap-up-to-char (arg char)
  "Kill up to ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found."
  (interactive (list (prefix-numeric-value current-prefix-arg)
             (read-char "Zap to char: " t)))
  ;; Avoid "obsolete" warnings for translation-table-for-input.
  (with-no-warnings
    (if (char-table-p translation-table-for-input)
	(setq char (or (aref translation-table-for-input char) char))))
  (kill-region (point) (progn
             (search-forward (char-to-string char) nil nil arg)
             (goto-char (1- (point)))
             (point))))

; unscroll
(defvar unscroll-point (make-marker))
(defvar unscroll-window-start (make-marker))
(defvar unscroll-hscroll)

(put 'scroll-up   'unscrollable t)
(put 'scroll-down 'unscrollable t)

(defadvice scroll-up (before remember-for-unscroll
                             activate compile)
  (unscroll-maybe-remember))

(defadvice scroll-down (before remember-for-unscroll
                             activate compile)
  (unscroll-maybe-remember))


(defun unscroll-maybe-remember ()
  (if (not (get last-command 'unscrollable))
      (progn
        (set-marker unscroll-point (point))
        (set-marker unscroll-window-start (window-start))
        (setq unscroll-hscroll))))

(defun unscroll ()
  (interactive)
  (goto-char unscroll-point)
  (set-window-start nil unscroll-window-start)
  (set-window-hscroll nil unscroll-hscroll))

;;; hooks
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

;; highlight code annotations
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(FIXME\\|TODO\\|BUG\\|XXX\\)" 1 font-lock-warning-face t)))))

(global-set-key [remap goto-line] 'goto-line-with-feedback)
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))


;;; customized
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote (("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" "/tmp/\\2" t) ("\\(.*\\)" "/home/jsynacek/.emacs.d/autosave/\\2" t))))
 '(backup-directory-alist (quote (("." . "/home/jsynacek/emacsbackup"))))
 '(bookmark-save-flag 1)
 '(c-backslash-column 78)
 '(c-backslash-max-column 78)
 '(c-macro-prompt-flag t)
 '(column-number-mode t)
 '(compilation-ask-about-save nil)
 '(confirm-kill-emacs nil)
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes (quote ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(diff-switches "-u")
 '(ediff-custom-diff-options "-up")
 '(ediff-diff-options "")
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(electric-pair-mode nil)
 '(eshell-buffer-maximum-lines 10240)
 '(fill-column 80)
 '(global-auto-revert-mode t)
 '(helm-always-two-windows nil)
 '(helm-boring-file-regexp-list (quote ("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "~$" "\\.pyc" "\\.o")))
 '(ibuffer-saved-filter-groups (quote (("openlmi" ("openlmi-scripts" (filename . "openlmi-scripts")) ("openlmi-storage" (filename . "openlmi-storage")) ("openlmi-providers" (filename . "openlmi-providers"))))))
 '(ibuffer-saved-filters (quote (("gnus" ((or (mode . message-mode) (mode . mail-mode) (mode . gnus-group-mode) (mode . gnus-summary-mode) (mode . gnus-article-mode)))) ("programming" ((or (mode . emacs-lisp-mode) (mode . cperl-mode) (mode . c-mode) (mode . java-mode) (mode . idl-mode) (mode . lisp-mode)))))))
 '(ido-case-fold nil)
 '(ido-create-new-buffer (quote always))
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-use-filename-at-point (quote guess))
 '(ido-work-directory-list-ignore-regexps (quote (".*")))
 '(indent-tabs-mode nil)
 '(kill-ring-max 1024)
 '(mark-even-if-inactive t)
 '(max-lisp-eval-depth 6000)
 '(max-specpdl-size 13000)
 '(mouse-yank-at-point t)
 '(next-screen-context-lines 4)
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("marmalade" . "http://marmalade-repo.org/packages/") ("melpa" . "http://melpa.milkbox.net/packages/"))))
 '(projectile-global-mode t)
 '(save-place t nil (saveplace))
 '(save-place-file "~/.emacs.d/places")
 '(scroll-preserve-screen-position 1)
 '(shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell)))))
 '(shell-pop-universal-key "C-x C-m")
 '(shell-pop-window-height 60)
 '(show-paren-mode t)
 '(split-height-threshold nil)
 '(split-width-threshold 160)
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

;;; TODO this does not work at startup with the scratch buffer
;(add-hook 'emacs-lisp-mode-hook
;         (lambda ()
;           eldoc-mode))
;(add-hook 'after-save-hook 'whitespace-cleanup)
;; todo org mode dont export postamble
;; todo auto-revert-non-file-buffers (dired?)
;; todo ido-goto-symbol
;; todo bind meta-tab to complete-tag?
;; todo sudo-editb
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
