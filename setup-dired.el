(require 'dired)
(require 'dired-x)

(setq dired-dwim-target t)

(defun dired-back-to-start-of-files ()
  (interactive)
  (backward-char (- (current-column) 2)))


(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 4))

(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(define-key dired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)
(define-key dired-mode-map [remap beginning-of-buffer] 'dired-back-to-top)
(define-key dired-mode-map [remap end-of-buffer] 'dired-jump-to-bottom)

(provide 'setup-dired)
