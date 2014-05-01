(require 'recentf)

;; shamelessly stolen from
;; http://www.masteringemacs.org/articles/2011/01/27/find-files-faster-recent-files-package/

;; get rid of `find-file-read-only' and replace it with something
;; more useful.

;; enable recent files mode.
(recentf-mode t)

; 200 files ought to be enough.
(setq recentf-max-saved-items 200)

;; (defun ido-recentf-open ()
;;   "Use `ido-completing-read' to \\[find-file] a recent file"
;;   (interactive)
;;   (if (find-file (ido-completing-read "Find recent file: " recentf-list))
;;       (message "Opening file...")
;;     (message "Aborting")))

;; (global-set-key (kbd "C-x C-r") 'ido-recentf-open)

(provide 'setup-recentf)
