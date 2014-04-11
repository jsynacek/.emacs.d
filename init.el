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
(require 'server)
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

;; packages
;; TODO use Cask??
(package-initialize)

(require 'use-package)
(use-package ace-jump-mode
  :bind ("C-c SPC" . ace-jump-mode))

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
;    (diminish 'hi-lock-mode)
;    (diminish 'magit-auto-revert-mode)))
    ))

(require 'highlight-symbol)

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

    (bind-key "C-x C-b" 'ibuffer)
    (bind-key "P" nil ibuffer-mode-map)   ; prevent accidentaly printing buffers
    (bind-key "M-o" nil ibuffer-mode-map) ; make global M-o work
))

(use-package ido-vertical-mode
  :init (ido-vertical-mode 1))

(require 'magit)
(require 'package)
(require 'python)
(require 'saveplace)

(use-package smex
  :init (smex-initialize)
  :bind (("M-x"         . smex)
         ("M-X"         . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command)))

(require 'uniquify)

(bind-key "M-o" 'other-window)
(bind-key "M-o" nil diff-mode-map)
(bind-key "C-c M-o" 'diff-goto-source diff-mode-map)


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
