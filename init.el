;; TODO something for moving forward in camel-case (haskell...) "words"

(setq gc-cons-threshold (* 64 1024 1024))
(setq-default bidi-inhibit-bpa t)

;;; ui
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area t)
(setq initial-scratch-message nil)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(show-paren-mode t)
(column-number-mode t)
(setq x-stretch-cursor t)
(setq default-frame-alist '((vertical-scroll-bars . nil)
                            (internal-border-width . 20)
                            (font . "Liberation Mono 11")))
;;(set-default 'cursor-type  '(bar . 2))
;;(set-window-margins (selected-window) 10)
(load (concat user-emacs-directory "jsynacek-theme.el"))

;;; packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'load-path "/usr/share/emacs/site-lisp")

;;; server
(require 'server)
(unless (server-running-p)
  (server-start))

;;; user
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)
(setq mouse-yank-at-point t)
(setq-default fill-column 90)
(setq-default indent-tabs-mode nil)
;; (setq scroll-preserve-screen-position t)
(setq even-window-sizes nil)
(setq backup-directory-alist '(("." . "/home/jsynacek/.emacs.d/backup/")))

;; display action = (FUNCTIONS . ALIST)
;; (setq display-buffer-base-action '(display-buffer-same-window))
(setq display-buffer-alist '(("\\*Help\\*" display-buffer-reuse-window (reusable-frames . visible))))
                             ;; ("\\*Completions\\*" (display-buffer-reuse-window) (window-height . 20))))
(setq help-window-select t)

;;(setq display-buffer-alist nil)

;; TODO
;; (setq split-window-keep-point nil)

;; Try this poor man's completion "framework" for now.
;; The 'flex' style is sometimes not what I would expect, so I don't know
;; if I like it, yet. For example, if the completions contain both "init.el"
;; "minibuffer.el.gz", typing "ini" will still select both of them, not "init.el"
;; as I would expect. In other words, sometimes it's too aggressive.
;; TODO: Doesn't really work for project-find-file (too slow).
;; (setq completion-category-overrides '((buffer (styles flex basic substring))))
;; (defun hook/completion-help ()
;;   (when (eq major-mode 'minibuffer-mode)
;;     (minibuffer-completion-help)))
;; (add-hook 'post-command-hook #'hook/completion-help)
;; (temp-buffer-resize-mode)
;; ;; I'm not sure what can possibly break if I set a static value like this.
;; (setq temp-buffer-max-height 15)

(require 'dired)
(setq dired-listing-switches "-alh --group-directories-first --time-style=long-iso")
(setq dired-create-destination-dirs 'ask)
(setq Man-notify-method 'aggressive)
;; (setq completions-format 'one-column)
;; (require 'grep)
;; (electric-indent-mode -1)

;; email
(load (concat user-emacs-directory "jsynacek-secret.el"))
(require 'smtpmail)
(setq message-send-mail-function #'smtpmail-send-it)
;; See ~/.authinfo.
(setq smtpmail-smtp-server "smtp.gmail.com")
(setq smtpmail-smtp-service 587)

;; sh
(setq sh-basic-offset 2)

;; ;; TODO: Investigate if so long mode can be made the default for buffers with lines that are of
;; ;; certain size.

;;; commands and keys
(global-set-key (kbd "C-b") #'switch-to-buffer)
;; TODO: Bind pop-to-buffer to something? Like C-u C-b?
(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "C-z") #'undo)
(defun jsynacek-other-window-or-frame (arg)
  (interactive "P")
  (if (equal arg '(4))
      (other-frame 1)
    (other-window 1)))
(global-set-key (kbd "C-o") #'jsynacek-other-window-or-frame)
(global-set-key (kbd "M-f") #'forward-sexp)
(global-set-key (kbd "M-b") #'backward-sexp)
(global-set-key (kbd "M-d") #'kill-sexp)
;; (global-set-key (kbd "M-`") #'other-frame)
(global-set-key (kbd "C-.") #'isearch-forward-symbol-at-point)
(global-set-key (kbd "C-x C-<right>") #'windmove-right)
(global-set-key (kbd "C-x C-<left>") #'windmove-left)
(global-set-key (kbd "C-x C-<down>") #'windmove-down)
(global-set-key (kbd "C-x C-<up>") #'windmove-up)
(global-set-key (kbd "C-x M-<right>") #'windmove-swap-states-right)
(global-set-key (kbd "C-x M-<left>") #'windmove-swap-states-left)
(global-set-key (kbd "C-x M-<down>") #'windmove-swap-states-down)
(global-set-key (kbd "C-x M-<up>") #'windmove-swap-states-up)
(global-set-key (kbd "C-x c") #'recompile)
(global-set-key (kbd "C-h l") #'find-library)
(defun jsynacek-make-terminal-frame ()
  (interactive)
  (let ((frame
         ;; TODO: Generate unique titles if frame already exists.
         (make-frame '((title . "Emacs terminal")
                       ;; TODO: Good enough for now, but make this a proper computation
                       ;; based on the current resolution, font size, etc...
                       (left . 550)
                       (width . 101)
                       (height . 60)))))
    (select-frame-set-input-focus frame)
    (term "/usr/bin/bash")))
(global-set-key (kbd "C-x 5 t") #'jsynacek-make-terminal-frame)

;; ;; ;; (load "~/.emacs.d/jsynacek-open.el")
;; ;; (load "~/.emacs.d/jsynacek-eval.el")
(load "~/.emacs.d/jsynacek-cut-and-paste.el")
;; ;;(load "~/.emacs.d/jsynacek-wind.el")
;; ;; (load "~/.emacs.d/jsynacek-term.el")
;; ;; (load "~/.emacs.d/jsynacek-work.el")
;; ;; (global-set-key [remap dabbrev-expand] 'hippie-expand)

;; (require 'expand-region)
;; (global-set-key (kbd "C-;") 'er/expand-region)

;; ;; haskell
;; (defun jsynacek-shake-fix-formatting ()
;;   (interactive)
;;   (when (vc-root-dir)
;;     (async-shell-command "./shake.sh fix-formatting-quick")))

;; (defun jsynacek-hlint ()
;;   (interactive)
;;   (when (vc-root-dir)
;;     (async-shell-command "./shake.sh hlint-quick")))

(defun jsynacek-init-tags-tables ()
  (interactive)
  (let ((dirs '("/home/jsynacek/scrive/kontrakcja/"
                "/home/jsynacek/scrive/hpqtypes/"
                "/home/jsynacek/scrive/hpqtypes-extras/"))
        out)
    (mapc #'(lambda (dir)
              (start-process-shell-command
               (concat "ghc-tags " dir)
               "*ghc-tags*"
               (concat "cd " dir " && " "ghc-tags -e")))
          dirs)
    (display-buffer "*ghc-tags*")
    (setq tags-table-list dirs)))


(require 'ivy)
(setq ivy-on-del-error-function 'ignore)
(ivy-define-key ivy-minibuffer-map (kbd "TAB") #'ivy-partial)
(global-set-key [remap switch-to-buffer] #'ivy-switch-buffer)
(global-set-key [remap switch-to-buffer-other-window] #'ivy-switch-buffer-other-window)
(global-set-key [remap bookmark-jump] #'counsel-bookmark)
(global-set-key [remap yank-pop] #'counsel-yank-pop)

(require 'vc)
(require 'vc-git)
(require 'counsel)
(defun jsynacek-project-find-file ()
  (interactive)
  (if (counsel--git-root)
      (counsel-git)
    (project-find-file)))
(global-set-key (kbd "C-p") #'jsynacek-project-find-file)

;; (defun jsynacek-rg ()
;;   (interactive)
;;   ;; counsel--git-root works much better than vc-root-dir.
;;   (if (counsel--git-root)
;;       (call-interactively #'rg-project)
;;     (call-interactively #'rg)))
;; (global-set-key (kbd "C-f") 'jsynacek-rg)

;; (require 'avy)
;; (global-set-key (kbd "C-j") 'avy-goto-word-1)

;; (require 'dired)
;; (setq dired-create-destination-dirs 'ask)
;; (define-key dired-mode-map "/" 'dired-isearch-filenames)

;; ;; See 'org-show-notification' in org-clock.el for more details on how org timers are implemented.
;; (defvar jsynacek-timer nil)
;; (defun jsynacek-start-timer ()
;;   (interactive)
;;   (if jsynacek-timer
;;       (message "Timer already set.")
;;     (let ((time (* 45 60)))
;;       (setq jsynacek-timer
;;             (run-with-timer
;;              time nil
;;              (lambda ()
;;                (setq jsynacek-timer nil)
;;                (call-process "notify-send" nil nil nil
;;                              "-u" "critical"
;;                              "-i" "emacs"
;;                              "Stop working now!"
;;                              "Take a break."))))
;;       (pcase (decode-time time)
;;         (`(,secs ,mins . ,_) (message "Timer set to %02d:%02d." mins secs))))))

(defun jsynacek-start-of-line ()
  "Move to the beginning of indentation or line. Toggle between the positions if the point
is already on either of them."
  (interactive)
  (let ((b (line-beginning-position))
        (i (current-indentation))
        (p (point)))
    (cond ((= p (+ b i))
           (goto-char b))
          (t
           (back-to-indentation)))))
(global-set-key [remap move-beginning-of-line] #'jsynacek-start-of-line)

;; (defun jsynacek-delete-line-forward (arg)
;;   "Delete s-expression forward. If prefixed with a universal argument,
;; delete the current line forward."
;;   (interactive "P")
;;   (if (equal arg '(4))
;;       (kill-line)
;;     (kill-sexp)))
;; (global-set-key (kbd "C-<delete>") #'jsynacek-delete-line-forward)

;; terminal overrides
(require 'term)
(require 'help-mode)
(define-key term-raw-map (kbd "C-x") ctl-x-map)
(define-key term-raw-map (kbd "C-u") #'universal-argument)
(define-key term-raw-map (kbd "C-h") help-mode-map)
(define-key term-raw-map (kbd "C-b") #'switch-to-buffer)
(define-key term-raw-map (kbd "C-y") #'term-paste)
(define-key term-raw-map (kbd "M-x") #'execute-extended-command)
;; (define-key term-raw-map (kbd "M-f") #'ffap)
(define-key term-raw-map (kbd "C-t") #'jsynacek-toggle-terminal)
(define-key term-raw-map (kbd "C-p") #'jsynacek-project-find-file)
;; (define-key term-raw-map (kbd "C-<up>") #'windmove-up)
;; (define-key term-raw-map (kbd "C-<down>") #'windmove-down)
;; (define-key term-raw-map (kbd "C-f") 'jsynacek-rg)
(define-key term-raw-map (kbd "C-o") #'jsynacek-other-window-or-frame)
(define-key dired-mode-map (kbd "C-t") #'jsynacek-toggle-terminal)
(global-set-key (kbd "C-t") #'jsynacek-toggle-terminal)
(defun jsynacek-toggle-terminal ()
  (interactive)
  (if (eq major-mode 'term-mode)
      (switch-to-buffer nil)
    (term "/bin/bash")))


;; (require 'org)
;; (setq org-todo-keywords '((sequence "TODO" "DOING" "WAITING" "|" "DONE")))
;; (setq org-todo-keyword-faces '(("DOING" . "dodger blue")))
;; (setq org-src-fontify-natively t)
;; (setq org-agenda-files '("~/todo.org"))
;; (setq org-agenda-include-diary t)
;; (add-hook 'org-mode-hook 'turn-on-auto-fill)

(require 'magit)
(setq magit-section-initial-visibility-alist '((untracked . hide)
                                               (stashes . hide)))

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
 '(package-selected-packages
   '(nord-theme dockerfile-mode yaml-mode slime orgit rg mini-frame consult haskell-mode ivy-hydra projectile company org-tree-slide selectrum elm-mode counsel-etags go-mode expand-region avy nix-mode ivy magit))
 '(warning-suppress-types '((comp))))
(put 'dired-find-alternate-file 'disabled nil)
