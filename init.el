(setq gc-cons-threshold (* 4 1024 1024))

(setq jsynacek-lisp-directory (concat user-emacs-directory "lisp/"))
(add-to-list 'load-path jsynacek-lisp-directory)
(byte-recompile-directory jsynacek-lisp-directory)

;;; packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'load-path "/usr/share/emacs/site-lisp")

;;; server
(require 'server)
(unless (server-running-p)
  (server-start))

;;; ui
;;"Iosevka Fixed Extended 11"
(defconst jsynacek-ui-alist '((font . "Liberation Mono 11")
                              (width . 101)
                              (height . 61)
                              (internal-border-width . 10)))
(setq default-frame-alist `((vertical-scroll-bars . nil)
                            ,@jsynacek-ui-alist)
      inhibit-startup-message t
      inhibit-startup-echo-area t
      initial-scratch-message nil
      x-stretch-cursor t
      bidi-inhibit-bpa t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(show-paren-mode t)
;; TODO: (setq mouse-drag-and-drop-region t)
;; TODO: Figure out how the system clipboard actually works in emacs.
(column-number-mode t)
(load (concat user-emacs-directory "jsynacek-theme.el"))

;;; user
(setq backup-directory-alist '(("." . "/home/jsynacek/.emacs.d/backup/"))

      display-buffer-alist '(("\\*Help\\*" display-buffer-reuse-window (reusable-frames . visible))
                             ("\\*Apropos\\*" display-buffer-same-window))
      even-window-sizes nil
      scroll-preserve-screen-position t
      mouse-yank-at-point t
      help-window-select t
      ;; display-buffer-alist doesn't really work for man buffers.
      Man-notify-method 'aggressive
      compilation-scroll-output 'first-error
      dired-listing-switches "-alh --group-directories-first --time-style=long-iso"
      dired-create-destination-dirs 'ask
      tab-always-indent 'complete
      sh-basic-offset 2)
(global-so-long-mode t)

(setq-default fill-column 90
              indent-tabs-mode nil)

;;; email
(load (concat user-emacs-directory "jsynacek-secret.el"))
;; (require 'smtpmail)
;; (setq message-send-mail-function #'smtpmail-send-it)
;; ;; See ~/.authinfo.
;; (setq smtpmail-smtp-server "smtp.gmail.com")
;; (setq smtpmail-smtp-service 587)

(require 'jsynacek-elisp)
(require 'jsynacek-misc)

;;; commands and keys

(global-set-key (kbd "C-b") #'switch-to-buffer)
(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "C-z") #'undo)
(global-set-key (kbd "C-.") #'isearch-forward-symbol-at-point)
(global-set-key (kbd "C-h l") #'find-library)
(global-set-key (kbd "C-c r") #'recompile)
(define-key emacs-lisp-mode-map (kbd "C-c C-c") #'eval-defun)


(load "~/.emacs.d/jsynacek-work.el")

(require 'avy)
(global-set-key (kbd "C-,") #'avy-goto-word-1)

(require 'compile)
(define-key compilation-mode-map (kbd "C-o") #'cmd/other-window-or-frame)

(require 'dired)
;; This is a direct redefinition of the same function from dired.el. It's the easiest way
;; to make left mouse click open the file/directory in the same window.
(defun dired-mouse-find-file-other-window (event)
  "In Dired, visit the file or directory name you click on in another window."
  (interactive "e")
  (dired-mouse-find-file event 'find-file 'dired))
(define-key dired-mode-map (kbd "C-o") #'cmd/other-window-or-frame)

(require 'expand-region)
(global-set-key (kbd "C-;") #'er/expand-region)

(require 'ivy)
(setq ivy-height 15
      ivy-on-del-error-function 'ignore)
(ivy-define-key ivy-minibuffer-map (kbd "TAB") #'ivy-partial)
;; (global-set-key [remap switch-to-buffer] #'ivy-switch-buffer)
;; (global-set-key [remap switch-to-buffer-other-window] #'ivy-switch-buffer-other-window)
;;(global-set-key [remap bookmark-jump] #'counsel-bookmark)
;;(global-set-key [remap yank-pop] #'counsel-yank-pop)
;;(global-set-key (kbd "C-p") #'counsel-git)
(global-set-key (kbd "C-p") #'project-find-file)

(require 'jsynacek-elisp)
(global-set-key [remap eval-last-sexp] #'cmd/eval-region-or-last-sexp)
(global-set-key (kbd "C-h e") #'cmd/info-elisp)
(define-key emacs-lisp-mode-map (kbd "C-c C-c") #'cmd/eval-defun)

(require 'jsynacek-misc)
(global-set-key (kbd "C-o") #'cmd/other-window-or-frame)
(global-set-key (kbd "<f12>") #'cmd/start-timer)

(require 'jsynacek-term)
(global-set-key (kbd "C-t") #'cmd/toggle-terminal)
(global-set-key (kbd "C-x 5 t") #'cmd/make-terminal-frame)
(define-key term-raw-map (kbd "C-x") ctl-x-map)
(define-key term-raw-map (kbd "C-u") #'universal-argument)
(define-key term-raw-map (kbd "C-h") help-mode-map)
(define-key term-raw-map (kbd "C-b") #'switch-to-buffer)
(define-key term-raw-map (kbd "C-c C-y") #'term-paste)
(define-key term-raw-map (kbd "C-p") #'project-find-file)
(define-key term-raw-map (kbd "M-x") #'execute-extended-command)
(define-key term-raw-map (kbd "C-o") #'cmd/other-window-or-frame)
(define-key term-raw-map (kbd "C-t") #'cmd/toggle-terminal)
(define-key dired-mode-map (kbd "C-t") #'cmd/toggle-terminal)

(require 'jsynacek-text)
(global-set-key [remap move-beginning-of-line] #'cmd/start-of-line)
(global-set-key [remap kill-region] #'cmd/cut-line-or-region)
(global-set-key [remap yank] #'cmd/paste)
(global-set-key [remap kill-ring-save] #'cmd/copy-line-or-region)
;; TODO: Replace with electric-pair-mode?
(global-set-key "\"" #'cmd/insert-quote-or-wrap-with-quotes)
(global-set-key "\(" #'cmd/insert-paren-or-wrap-with-parens)
(global-set-key "\)" #'cmd/insert-paren-or-wrap-with-parens)

(require 'magit)
(setq magit-section-initial-visibility-alist '((untracked . hide)
                                               (stashes . hide)))

(require 'orderless)
(setq completion-category-overrides
      '((buffer (styles orderless basic partial-completion))
        (project-file (styles orderless basic partial-completion))))

(require 'pulse)
(setq pulse-delay .06)
(face-spec-set 'pulse-highlight-start-face
               `((t :background ,(oc-color 'lime1))))

(require 'rg)
(global-set-key (kbd "C-c .") #'rg-dwim)
(global-set-key (kbd "C-c f") #'rg-project)
(define-key rg-mode-map (kbd "C-o") #'cmd/other-window-or-frame)

(require 'slime)
(setq inferior-lisp-program "sbcl --no-inform")

(require 'vc-dir)
(define-key vc-dir-mode-map (kbd "C-o") #'cmd/other-window-or-frame)

(require 'vertico "/home/jsynacek/src/vertico/vertico.el")
;; This requires a custom patch. See
;; https://github.com/jsynacek/vertico/commit/99dbdd641d20b86e2e6dbc400167e45e0c884e72.
;; I still don't know if I like this.
(setq vertico-activate-functions
      '(find-library
        execute-extended-command
        switch-to-buffer
        project-switch-project
        project-find-file))
(vertico-mode t)

(require 'org)
(setq org-todo-keywords '((sequence "TODO" "DOING" "WAITING" "|" "DONE")))
(setq org-src-fontify-natively t)
;; (setq org-agenda-files '("~/todo.org"))
;; (setq org-agenda-include-diary t)
;; (add-hook 'org-mode-hook 'turn-on-auto-fill)

(require 'project)
(setq project-switch-commands
      '((project-find-file "Find file" ?f)
        (rg-project "Find regexp" ?g)
        (project-find-dir "Find directory" ?d)
        (project-vc-dir "VC dir" ?v)
        (magit-status "Magit status" ?m)))

;;; calendar and diary
(setq calendar-holidays
      '((holiday-fixed 1 1 "Den obnovy samostatného českého státu; Nový rok")
        (holiday-easter-etc -2 "Velký pátek")
        (holiday-easter-etc 1 "Velikonoční pondělí")
        (holiday-fixed 5 1 "Svátek práce")
        (holiday-fixed 5 8 "Den vítězství")
        (holiday-fixed 7 5 "Den slovanských věrozvěstů Cyrila a Metoděje")
        (holiday-fixed 7 6 "Den upálení mistra Jana Husa")
        (holiday-fixed 9 28 "Den české státnosti")
        (holiday-fixed 10 28 "Den vzniku samostatného československého státu")
        (holiday-fixed 11 17 "Den boje za svobodu a demokracii")
        (holiday-fixed 12 24 "Štědrý den")
        (holiday-fixed 12 25 "Svátek vánoční")
        (holiday-fixed 12 26 "Svátek vánoční")))

;;; custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount '(4 ((shift) . hscroll) ((meta)) ((control) . text-scale)))
 '(package-selected-packages '(slime rg orgit expand-region counsel-etags avy))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
(put 'list-timers 'disabled nil)
