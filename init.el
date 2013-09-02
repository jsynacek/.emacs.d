;;; init
(setq user-full-name "Jan Synáček")
(setq user-mail-address "jsynacek@redhat.com")
(setq user-nick "jsynacek")

(add-to-list 'load-path (expand-file-name "~/.emacs.d/"))

;;; requires
(package-initialize)
(require 'ace-jump-mode)
(require 'browse-kill-ring)
(require 'buffer-move)
(require 'expand-region)
(require 'highlight-symbol)
(require 'ido-hacks)
(require 'magit)
(require 'package)
(require 'python)
(require 'saveplace)
(require 'smartparens)
(require 'smartparens-config)
(require 'server)
(require 'uniquify)

;;; settings
(setq inhibit-startup-message t
      inhibit-startup-echo-area t)
(set-default-font "Terminus-12")
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
(ido-mode 1)
(ido-hacks-mode 1)
(auto-fill-mode 1)
(winner-mode t)

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
                    ido-hacks
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

;; whitespace-cleanup
(add-hook 'before-save-hook 'whitespace-cleanup)

;;; keybindings
; general
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "M-O") 'vi-open-line-above)
(global-set-key (kbd "M-o") 'vi-open-line-below)
(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))
(global-set-key (kbd "C-x e") 'eval-and-replace)
(global-set-key (kbd "<f5>") 'recompile)
(global-set-key (kbd "<f6>") 'compile)
(global-set-key (kbd "<f7>") 'whitespace-cleanup)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)
(global-set-key (kbd "C-x w") 'write-region)
(global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)
(global-set-key (kbd "C-x C-k") 'kill-buffer-and-window)
(global-set-key (kbd "C-x \\") 'align-regexp)
(global-set-key (kbd "C-x m") 'eshell)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer-other-window)
(global-set-key (kbd "M-i") 'imenu)
(global-set-key (kbd "C-.") 'describe-thing-at-point)
(global-set-key (kbd "C-M-/") 'hippie-expand-line)
(global-set-key (kbd "M-w") 'save-region-or-current-line)
(global-set-key (kbd "M-8") 'er/expand-region)
(global-set-key (kbd "M-9")
                (lambda ()
                  (interactive)
                  (er/expand-region -1)))
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(define-prefix-command 'menukey-prefix-map)
; custom prefix
(global-set-key (kbd "<menu>") 'menukey-prefix-map)
(define-key menukey-prefix-map (kbd "r") 'rgrep)
; my multi-occur
(global-set-key (kbd "M-s /") 'my-multi-occur-in-matching-buffers)
; highlight-symbol
(global-set-key (kbd "M-n") 'highlight-symbol-next)
(global-set-key (kbd "M-p") 'highlight-symbol-prev)
; numbers
(global-set-key (kbd "C-c +") 'increment-number-at-point)
(global-set-key (kbd "C-c -") 'decrement-number-at-point)
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
; magit
(global-set-key (kbd "C-x g") 'magit-status)
(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
(define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)
; smartparens
(define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp)
(define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
(define-key sp-keymap (kbd "<C-left>") 'sp-forward-barf-sexp)
(define-key sp-keymap (kbd "<C-right>") 'sp-forward-slurp-sexp)
(define-key sp-keymap (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
(define-key sp-keymap (kbd "C-M-<right>") 'sp-backward-barf-sexp)
(define-key sp-keymap (kbd "M-F") 'sp-forward-symbol)
(define-key sp-keymap (kbd "M-B") 'sp-backward-symbol)
(define-key sp-keymap (kbd "C-}") 'sp-select-next-thing-exchange)
(define-key sp-keymap (kbd "C-{") 'sp-select-previous-thing)
(define-key sp-keymap (kbd "C-M-}") 'sp-select-next-thing)
; python-mode
(define-key python-mode-map (kbd "C-c d") 'pydoc)
(define-key python-mode-map (kbd "M-e") 'python-next-statement)
(define-key python-mode-map (kbd "M-a") 'python-previous-statement)

;;; customized
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-directory-alist (quote (("." . "/home/jsynacek/emacsbackup"))))
 '(bookmark-save-flag 1)
 '(column-number-mode t)
 '(compilation-ask-about-save nil)
 '(confirm-kill-emacs nil)
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes (quote ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(diff-switches "-u")
 '(ediff-diff-options "")
 '(electric-pair-mode nil)
 '(global-auto-revert-mode t)
 '(ibuffer-saved-filter-groups (quote (("openlmi" ("openlmi-storage" (filename . "openlmi-storage")) ("openlmi-providers" (filename . "openlmi-providers"))))))
 '(ibuffer-saved-filters (quote (("gnus" ((or (mode . message-mode) (mode . mail-mode) (mode . gnus-group-mode) (mode . gnus-summary-mode) (mode . gnus-article-mode)))) ("programming" ((or (mode . emacs-lisp-mode) (mode . cperl-mode) (mode . c-mode) (mode . java-mode) (mode . idl-mode) (mode . lisp-mode)))))))
 '(ido-case-fold nil)
 '(ido-create-new-buffer (quote always))
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(indent-tabs-mode nil)
 '(kill-ring-max 1024)
 '(mark-even-if-inactive t)
 '(max-lisp-eval-depth 6000)
 '(max-specpdl-size 13000)
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("marmalade" . "http://marmalade-repo.org/packages/") ("melpa" . "http://melpa.milkbox.net/packages/"))))
 '(save-place t nil (saveplace))
 '(save-place-file "~/.emacs.d/places")
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
