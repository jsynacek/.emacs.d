;; init
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
(fset 'yes-or-no-p 'y-or-n-p)

;; start the server
(unless (server-running-p)
  (server-start))

;; edebug
(setq edebug-trace t)

;; modes
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)
(auto-fill-mode 1)
(winner-mode t)
(ido-mode 1)
(ido-vertical-mode 1)

;; packages
;; TODO use Cask??
(package-initialize)

(require 'use-package)
(use-package ace-jump-mode
  :bind ("C-c SPC" . ace-jump-mode))

(require 'highlight-symbol)
(require 'ibuffer)
(require 'magit)
(require 'package)
(require 'python)
(require 'saveplace)
(require 'server)

(use-package smex
  :init (smex-initialize)
  :bind (("M-x"         . smex)
         ("M-X"         . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command)))

(require 'uniquify)

;; TODO
;; (defun install-my-packages ()
;;   (interactive)
;;   (let ((pkg-list '(ace-jump-mode
;;                     browse-kill-ring
;;                     buffer-move
;;                     cl-lib
;;                     expand-region
;;                     fill-column-indicator
;;                     highlight-symbol
;;                     ido-vertical-mode
;;                     magit
;;                     pydoc-info
;;                     saveplace
;;                     smartparens
;;                     smex
;;                     solarized-theme
;;                     undo-tree
;;                     yasnippet)))
;;     (dolist (pkg pkg-list)
;;       (if (package-installed-p pkg)
;;           (message (concat (symbol-name pkg) " already installed."))
;;         (package-install pkg)))))

(require 'private)
(require 'defuns)
(require 'setup-bbdb)
(require 'setup-dired)
(require 'setup-ediff)
(require 'setup-erc)
(require 'setup-smartparens)
(require 'setup-recentf)
(require 'setup-org)
(require 'keybindings)
(load custom-file)
