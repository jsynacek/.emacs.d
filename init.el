;;; init
(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path (expand-file-name "elisp/"
                                          user-emacs-directory))

;;; requires
(package-initialize)
(require 'ace-jump-mode)
(require 'browse-kill-ring)
(require 'buffer-move)
(require 'expand-region)
(require 'highlight-symbol)
(require 'magit)
(require 'package)
(require 'python)
(require 'saveplace)
(require 'server)
(require 'uniquify)

;;; settings
(setq inhibit-startup-message t
      inhibit-startup-echo-area t)
(prefer-coding-system 'utf-8)
(setenv "EDITOR" "emacsclient")
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
(ido-mode 1)
(ido-vertical-mode 1)

;;; package settings
;; server
(unless (server-running-p)
  (server-start))

;; edebug
(setq edebug-trace t)

;;; defuns
(defun install-my-packages ()
  (interactive)
  (let ((pkg-list '(ace-jump-mode
                    browse-kill-ring
                    buffer-move
                    cl-lib
                    expand-region
                    fill-column-indicator
                    highlight-symbol
                    ido-vertical-mode
                    magit
                    saveplace
                    smartparens
                    smex
                    solarized-theme
                    undo-tree
                    yasnippet)))
    (dolist (pkg pkg-list)
      (if (package-installed-p pkg)
          (message (concat (symbol-name pkg) " already installed."))
        (package-install pkg)))))

(require 'private)
(require 'defuns)
(require 'setup-dired)
(require 'setup-ediff)
(require 'setup-erc)
(require 'setup-smartparens)
(require 'setup-org)
(require 'keybindings)

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
 '(dired-dwim-target t)
 '(ediff-custom-diff-options "-up")
 '(ediff-diff-options "")
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(electric-pair-mode nil)
 '(fill-column 80)
 '(git-commit-summary-max-length 72)
 '(global-auto-revert-mode t)
 '(global-subword-mode t)
 '(helm-always-two-windows nil)
 '(helm-boring-file-regexp-list (quote ("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "~$" "\\.pyc" "\\.o")))
 '(ibuffer-saved-filter-groups (quote (("basic" ("dired" (derived-mode . dired-mode)) ("ERC" (used-mode . erc-mode))) ("openlmi" ("openlmi-scripts" (filename . "openlmi-scripts")) ("openlmi-storage" (filename . "openlmi-storage")) ("openlmi-providers" (filename . "openlmi-providers"))))))
 '(ibuffer-saved-filters (quote (("dired" ((used-mode . dired-mode))) ("erc-filter" ((used-mode . erc-mode))) ("gnus" ((or (mode . message-mode) (mode . mail-mode) (mode . gnus-group-mode) (mode . gnus-summary-mode) (mode . gnus-article-mode)))) ("programming" ((or (mode . emacs-lisp-mode) (mode . cperl-mode) (mode . c-mode) (mode . java-mode) (mode . idl-mode) (mode . lisp-mode)))))))
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
 '(safe-local-variable-values (quote ((c-backslash-max-column . 78))))
 '(save-place t nil (saveplace))
 '(save-place-file "~/.emacs.d/places")
 '(scroll-preserve-screen-position 1)
 '(show-paren-mode t)
 '(split-height-threshold nil)
 '(split-width-threshold 160)
 '(tramp-default-method "ssh")
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(whitespace-style (quote (face tabs spaces trailing lines newline empty space-after-tab space-mark tab-mark newline-mark)))
 '(woman-fill-column 80))
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
